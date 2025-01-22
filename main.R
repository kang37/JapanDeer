# Package ----
library(sf)
library(dplyr)
library(mapview)
library(tmap)
library(tidyr)
library(ggplot2)
library(DescTools)
library(showtext)
showtext_auto()

# Data ----
## Basic ----
# 漏洞：各个图层的mesh位置不一致。鹿数据和土地利用几乎一致，mesh人口数据和空mesh一致，相对偏右下，且和geosense网站似乎一致。目前将所有数据都对齐到全日本空mesh数据上。

# 提取城市内的鹿数据：以指定都市为例。由于北海道无鹿调查数据，故不计入。
city <- st_read("data_raw/JapanAdmin2022", "N03-22_220101") %>%
  select(pref = N03_001, city = N03_003) %>%
  filter(city %in% c(
    "大阪市", "名古屋市", "京都市", "横浜市", "神戸市", "北九州市",
    "川崎市", "福岡市", "広島市", "仙台市", "千葉市", "さいたま市",
    "静岡市", "堺市", "新潟市", "浜松市", "岡山市", "相模原市", "熊本市"
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
city_pop <- list.files("data_raw/mesh_pop_100m") %>%
  .[!grepl("zip", .)] %>%
  lapply(function(x) st_read(paste0("data_raw/mesh_pop_100m/", x))) %>%
  bind_rows() %>%
  # 转为普通数据框。
  st_drop_geometry() %>%
  mutate(mesh_8d = substr(MESH_CODE, 1, 8), pop_2020 = PopT) %>%
  select(mesh_8d, pop_2020) %>%
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
    pop_2020 = sum(pop_2020, na.rm = TRUE),
    .groups = "drop"
  )

## Land ----
# 读取土地利用mesh数据，并且提取7位mesh编号。
read_land <- function(dsn_x) {
  st_read(
    paste0("data_raw/LandUse2021/", dsn_x),
    options = "ENCODING=Shift-JIS"
  ) %>%
    rename(
      "mesh_10d" = "L03b_001", "land_code" = "L03b_002", date = "L03b_003"
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
  list.files("data_raw/LandUse2021/"),
  read_land
) %>%
  bind_rows()

# 计算各城市每个mesh中各类目标土地利用的比例。
city_deer_mesh_land <- city_mesh %>%
  st_drop_geometry() %>%
  select(mesh) %>%
  distinct() %>%
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
  # 加入人口数据。
  left_join(city_pop, by = "mesh") %>%
  # 加入农田和森林数据。
  left_join(city_deer_mesh_land_wide, by = "mesh") %>%
  # 数据缺值补充。
  mutate(
    # 漏洞：原数据12920行中，116行鹿密度为0，4885行鹿密度为NA。根据数据说明，NA来源于3种情况：令和2年调查结果数量为0；森林面积为0；密度不足。因此，此处将NA都作为0处理。不过对于北海道而言，并非没有风险，而是没有调查数据。
    d_2022 = case_when(is.na(d_2022) ~ 0, TRUE ~ d_2022),
    # 漏洞：人口数据缺失值通常意味着对应mesh内无常住人口，因此作为0处理。
    pop_2020 = case_when(is.na(pop_2020) ~ 0, TRUE ~ pop_2020)
  ) %>%
  # 计算风险值。
  mutate(
    risk_human = d_2022 * pop_2020,
    risk_agr = d_2022 * lu_0100,
    risk_forest = d_2022 * lu_0500
  )
# 漏洞：需要确保数值无缺。
apply(city_deer_risk, 2, function(x) sum(is.na(x)))

# 筛选无风险城市。
zero_risk_city <-
  city_deer_risk %>%
  st_drop_geometry() %>%
  select("city", "mesh", "risk_human", "risk_agr", "risk_forest") %>%
  pivot_longer(
    cols = c("risk_human", "risk_agr", "risk_forest"),
    names_to = "risk_cat", values_to = "risk_val"
  ) %>%
  group_by(city) %>%
  summarise(risk_sum = sum(risk_val)) %>%
  filter(risk_sum == 0) %>%
  pull(city)

# 筛选最终分析城市。
city_deer_risk_tar <- city_deer_risk %>%
  filter(!city %in% zero_risk_city)

# Analysis ----
## General ----
# 各城市网格内鹿的数量。
mapview(city_deer_risk_tar, zcol = "d_2022")

## Risk ----
# 各城市各网格的各种风险。
mapview(city_deer_risk_tar %>% select(mesh, risk_human), zcol = "risk_human")
mapview(city_deer_risk_tar %>% select(mesh, risk_agr), zcol = "risk_agr")
mapview(city_deer_risk_tar %>% select(mesh, risk_forest), zcol = "risk_forest")
# 漏洞：如果城市的所有mesh的所有风险都为0，那么应该去掉。
city_deer_risk_tar %>%
  st_drop_geometry() %>%
  select("city", "mesh", "risk_human", "risk_agr", "risk_forest") %>%
  pivot_longer(
    cols = c("risk_human", "risk_agr", "risk_forest"),
    names_to = "risk_cat", values_to = "risk_val"
  ) %>%
  ggplot(aes(city, risk_val)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5, col = "pink") +
  facet_wrap(.~ risk_cat, scales = "free", ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# 不同风险之间的关系。
ggplot(st_drop_geometry(city_deer_risk_tar)) +
  geom_point(aes(risk_human, risk_forest), alpha = 0.5) +
  facet_wrap(.~ city)
ggplot(st_drop_geometry(city_deer_risk_tar)) +
  geom_point(aes(risk_human, risk_agr), alpha = 0.5) +
  facet_wrap(.~ city)
ggplot(st_drop_geometry(city_deer_risk_tar)) +
  geom_point(aes(risk_forest, risk_agr), alpha = 0.5) +
  facet_wrap(.~ city)

# 各个城市的风险中位数。
# 漏洞：应该算中位数吗？NA值也尚未处理。
st_drop_geometry(city_deer_risk_tar) %>%
  group_by(city) %>%
  summarise(
    risk_human = median(risk_human, na.rm = TRUE),
    risk_agr = median(risk_agr, na.rm = TRUE),
    risk_forest = median(risk_forest, na.rm = TRUE)
  ) %>%
  ggplot(aes(risk_forest, risk_agr)) +
  geom_point(aes(size = risk_human), alpha = 0.3) +
  geom_text(aes(label = city), vjust = -1, size = 2)

## Gini ----
city_deer_risk_tar %>%
  st_drop_geometry() %>%
  select("city", "mesh", "risk_human", "risk_agr", "risk_forest") %>%
  pivot_longer(
    cols = c("risk_human", "risk_agr", "risk_forest"),
    names_to = "risk_cat", values_to = "risk_val"
  ) %>%
  group_by(city, risk_cat) %>%
  summarise(risk_gini = Gini(risk_val), .groups = "drop") %>%
  ggplot() +
  geom_col(aes(city, risk_gini)) +
  facet_wrap(.~ risk_cat, ncol = 1, scales = "free")

## Gini and average risk ----
# 漏洞：和上面的重复了。
city_deer_risk_tar %>%
  st_drop_geometry() %>%
  select("city", "mesh", "risk_human", "risk_agr", "risk_forest") %>%
  pivot_longer(
    cols = c("risk_human", "risk_agr", "risk_forest"),
    names_to = "risk_cat", values_to = "risk_val"
  ) %>%
  group_by(city, risk_cat) %>%
  summarise(
    risk_mean = median(risk_val), risk_gini = Gini(risk_val), .groups = "drop"
  ) %>%
  ggplot() +
  geom_point(aes(city, risk_mean, size = risk_gini), alpha = 0.5) +
  facet_wrap(.~ risk_cat, ncol = 1, scales = "free") +
  theme_bw()

## Habitat preference ----
# 各城市鹿密度和人口及土地利用的关系。
# 漏洞：有些城市基本没有数据。
st_drop_geometry(city_deer_risk_tar) %>%
  ggplot() +
  geom_point(aes(pop_2020, d_2022), alpha = 0.5) +
  geom_smooth(aes(pop_2020, d_2022), method = "lm") +
  facet_wrap(.~ city, scales = "free")
# 漏洞：看不清图。
st_drop_geometry(city_deer_risk_tar) %>%
  select(city, mesh, area, d_2022) %>%
  left_join(city_deer_mesh_land, by = "mesh") %>%
  ggplot() +
  geom_point(aes(land_prop, d_2022), alpha = 0.5) +
  geom_smooth(aes(land_prop, d_2022), method = "lm") +
  facet_grid(city ~ land_code, scales = "free")

# 计算鹿和人口密度、土地利用的关系。
# 总体关系。
lapply(
  c(
    "pop_2020", "lu_0100", "lu_0500", "lu_0600", "lu_0700", "lu_0901", "lu_1100"
  ),
  function(x) {
    cor_res <- cor.test(city_deer_risk_tar$d_2022, city_deer_risk_tar[[x]])
    return(c(x, cor_res$estimate, cor_res$p.value))
  }
) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = TRUE) %>%
  data.frame() %>%
  rename_with(~ c("variable", "estimate", "p")) %>%
  mutate(
    p = as.numeric(p),
    p_mark = case_when(
      p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p >= 0.05 ~ ""
    )
  )

# 分城市统计。
get_cor <- function(x) {
  city_deer_risk_tar %>%
    st_drop_geometry() %>%
    group_by(city) %>%
    summarise(
      smp_size = n(),
      cor_res = list(cor.test(d_2022, {{x}}))
    ) %>%
    mutate(
      est = lapply(cor_res, function(x) x$estimate) %>% unlist(),
      p = lapply(cor_res, function(x) x$p.value) %>% unlist(),
    )
}
rbind(
  get_cor(pop_2020) %>% mutate(grp = "pop_2020"),
  get_cor(lu_0100) %>% mutate(grp = "lu_0100"),
  get_cor(lu_0500) %>% mutate(grp = "lu_0500"),
  get_cor(lu_0600) %>% mutate(grp = "lu_0600"),
  get_cor(lu_0700) %>% mutate(grp = "lu_0700"),
  get_cor(lu_0901) %>% mutate(grp = "lu_0901"),
  get_cor(lu_1100) %>% mutate(grp = "lu_1100")
) %>%
  mutate(est = case_when(p < 0.05 ~ est, TRUE ~ NA)) %>%
  ggplot() +
  geom_tile(aes(city, grp, fill = c(est > 0)), col = "white")

# Export ----
# 各个变量中位数。
city_deer_risk_tar %>%
  st_drop_geometry() %>%
  group_by(city) %>%
  summarise(
    across(d_2022:risk_forest, median, .names = "{.col}_mid"),
    across(risk_human:risk_forest, Gini, .names = "{.col}_gini")
  ) %>%
  write.csv(paste0("data_proc/city_deer_risk_tar_", Sys.Date(), ".csv"))

