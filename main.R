# Package ----
library(sf)
library(dplyr)
library(mapview)
library(tmap)
library(tidyr)
library(ggplot2)
library(showtext)
showtext_auto()

# Data ----
## Basic ----
# 漏洞：各个图层的mesh位置不一致。鹿数据和土地利用几乎一致，mesh人口数据和空mesh一致，相对偏右下，且和geosense网站似乎一致。目前将所有数据都对齐到全日本空mesh数据上。

# 提取城市内的鹿数据：以指定都市为例。
city <- st_read("data_raw/JapanAdmin2022", "N03-22_220101") %>%
  select(pref = N03_001, city = N03_003) %>%
  filter(city %in% c(
    "大阪市", "名古屋市", "京都市", "横浜市", "神戸市", "北九州市",
    "札幌市", "川崎市", "福岡市", "広島市", "仙台市", "千葉市",
    "さいたま市", "静岡市", "堺市", "新潟市", "浜松市", "岡山市",
    "相模原市", "熊本市"
  )) %>%
  group_by(pref, city) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_transform(crs = 6690)

# 读取日本空mesh数据作为底图，并进行裁切和筛选。
city_mesh <-
  st_intersection(
    city, st_read("data_raw/mesh5") %>% st_transform(crs = 6690)
  ) %>%
  select(pref, city, mesh = Descriptio) %>%
  mutate(
    area = as.numeric(st_area(.))/1e+06, .before = "geometry"
  ) %>%
  # 漏洞：一个完整mesh面积为25平方千米，只留下大于5平方千米的mesh。
  filter(area > 5)

# 读取日本鹿分布数据。
jp_deer <-
  st_read(dsn = "data_raw/japan_deer", layer = "生息密度2022") %>%
  mutate(mesh = as.character(mesh))

## Population ----
# 各mesh人口数据。
city_pop <- list.files("data_raw/MeshPop") %>%
  lapply(function(x) st_read(paste0("data_raw/MeshPop/", x))) %>%
  bind_rows() %>%
  # 转为普通数据框。
  st_drop_geometry() %>%
  select(mesh_8d = MESH_ID, pop_2015 = PTN_2015) %>%
  # 构建7位数mesh编号。
  mutate(
    mesh_6d = substr(mesh_8d, 1, 6),
    mesh_10d_7 = substr(mesh_8d, 7, 7),
    mesh_10d_8 = substr(mesh_8d, 8, 8),
    mesh_7d_7 = case_when(
      mesh_10d_7 <= 4 & mesh_10d_8 <= 4 ~ 1,
      mesh_10d_7 <= 4 & mesh_10d_8 >= 5 ~ 2,
      mesh_10d_7 >= 5 & mesh_10d_8 <= 4 ~ 3,
      mesh_10d_7 >= 5 & mesh_10d_8 >= 5 ~ 4
    ),
    mesh = paste0(mesh_6d, mesh_7d_7)
  ) %>%
  # 分组计算每个7位数mesh的总人口密度。每个7位数mesh应包含25个8位数mesh，若一个7位数mesh内含8位数mesh不满25个，意味着无人口mesh的地方常住人口为0。
  group_by(mesh) %>%
  summarise(
    mesh_8d_num = n(),
    pop_2015 = sum(pop_2015, na.rm = TRUE),
    .groups = "drop"
  )

## Land ----
# 读取土地利用mesh数据，并且提取7位mesh编号。
read_land <- function(dsn_x) {
  st_read(
    paste0("data_raw/LandUse/", dsn_x),
    options = "ENCODING=Shift-JIS"
  ) %>%
    rename(
      "mesh_10d" = "メッシュ", "land_code" = "土地利用種", date = "撮影年月日"
    ) %>%
    # 构造7位数mesh：在6位数mesh基础上增加第7位。第7位数字由原10位数mesh的第7和第8位数字决定。
    mutate(
      mesh_6d = substr(mesh_10d, 1, 6),
      mesh_10d_7 = substr(mesh_10d, 7, 7),
      mesh_10d_8 = substr(mesh_10d, 8, 8),
      mesh_7d_7 = case_when(
        mesh_10d_7 <= 4 & mesh_10d_8 <= 4 ~ 1,
        mesh_10d_7 <= 4 & mesh_10d_8 >= 5 ~ 2,
        mesh_10d_7 >= 5 & mesh_10d_8 <= 4 ~ 3,
        mesh_10d_7 >= 5 & mesh_10d_8 >= 5 ~ 4
      ),
      mesh = paste0(mesh_6d, mesh_7d_7)
    ) %>%
    st_drop_geometry()
}

# 读取所有土地利用数据。
jp_land <- lapply(
  list.files("data_raw/LandUse/"),
  read_land
) %>%
  bind_rows()

# 计算各城市每个mesh中各类目标土地利用的比例。
city_deer_mesh_land <- city_deer %>%
  st_drop_geometry() %>%
  select(mesh) %>%
  # 漏洞：应该用inner_join还是left_join呢？
  inner_join(jp_land, by = "mesh") %>%
  # 每个mesh中包含多少个mesh_10d。
  group_by(mesh) %>%
  mutate(mesh_10d_num = n()) %>%
  ungroup() %>%
  # 每个mesh中各类土地利用mesh的数量。
  group_by(mesh, mesh_10d_num, land_code) %>%
  summarise(
    land_mesh_10d_num = n(),
    .groups = "drop"
  ) %>%
  # 各类土地利用在各个mesh中的比例。
  mutate(land_prop = land_mesh_10d_num / mesh_10d_num)

city_deer_mesh_land_wide <- city_deer_mesh_land %>%
  mutate(land_code = paste0("lu_", land_code)) %>%
  pivot_wider(
    id_cols = "mesh", names_from = "land_code",
    values_from = "land_prop", values_fill = 0
  )

## Risk ----
# 人-鹿潜在冲突。
city_deer_risk <- city_mesh %>%
  # 将鹿数据映射到底图上。
  left_join(st_drop_geometry(jp_deer), by = "mesh") %>%
  # 漏洞：原数据12920行中，116行鹿密度为0，4885行鹿密度为NA。根据数据说明，NA来源于3种情况：令和2年调查结果数量为0；森林面积为0；密度不足。因此，此处将NA都作为0处理。
  mutate(d_2022 = case_when(is.na(d_2022) ~ 0, TRUE ~ d_2022)) %>%
  # 加入人口数据。
  left_join(city_pop, by = "mesh") %>%
  # 加入农田和森林数据。
  left_join(city_deer_mesh_land_wide, by = "mesh") %>%
  mutate(
    risk_human = d_2022 * pop_2015,
    risk_agr = d_2022 * lu_0100,
    risk_forest = d_2022 * lu_0500
  )

# Analysis ----
# 各城市网格内鹿的数量。
mapview(city_deer, zcol = "d_2022")

# 各城市各网格的各种风险。
mapview(city_deer_risk, zcol = "risk_human")

# 不同风险之间的关系。
ggplot(st_drop_geometry(city_deer_risk)) +
  geom_point(aes(risk_human, risk_forest), alpha = 0.5) +
  facet_wrap(.~ city)
ggplot(st_drop_geometry(city_deer_risk)) +
  geom_point(aes(risk_human, risk_agr), alpha = 0.5) +
  facet_wrap(.~ city)
ggplot(st_drop_geometry(city_deer_risk)) +
  geom_point(aes(risk_forest, risk_agr), alpha = 0.5) +
  facet_wrap(.~ city)

# 各个城市的平均风险。
# 漏洞：应该算平均值吗？NA值也尚未处理。
st_drop_geometry(city_deer_risk) %>%
  group_by(city) %>%
  summarise(
    risk_human = mean(risk_human, na.rm = TRUE),
    risk_agr = mean(risk_agr, na.rm = TRUE),
    risk_forest = mean(risk_forest, na.rm = TRUE)
  ) %>%
  ggplot(aes(risk_forest, risk_agr)) +
  geom_point(aes(size = risk_human), alpha = 0.3) +
  geom_text(aes(label = city), vjust = -1, size = 2)

# 各城市鹿密度和人口及土地利用的关系。
# 漏洞：有些城市基本没有数据。
st_drop_geometry(city_deer_risk) %>%
  ggplot() +
  geom_point(aes(pop_2015, d_2022), alpha = 0.5) +
  geom_smooth(aes(pop_2015, d_2022), method = "lm") +
  facet_wrap(.~ city, scales = "free")
# 漏洞：看不清图。
st_drop_geometry(city_deer_risk) %>%
  select(city, mesh, area, d_2022) %>%
  left_join(city_deer_mesh_land, by = "mesh") %>%
  ggplot() +
  geom_point(aes(land_prop, d_2022), alpha = 0.5) +
  geom_smooth(aes(land_prop, d_2022), method = "lm") +
  facet_grid(city ~ land_code, scales = "free")
