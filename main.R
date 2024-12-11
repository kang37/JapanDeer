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
# 读取日本鹿分布数据。
jp_deer <- st_read(dsn = "data_raw/japan_deer", layer = "生息密度2022") %>%
  mutate(mesh = as.character(mesh))

# 日本鹿数量变化。
jp_deer_hist <- read.csv(
  "data_raw/japan_deer/【取り扱い注意】2014･2022年度全国推定生息密度.csv"
) %>%
  tibble() %>%
  rename("mesh" = "X5kmメッシュ番号") %>%
  mutate(mesh = as.character(mesh))

# 提取城市内的鹿数据：以人口排名前20城市为例。
city <- st_read("data_raw/JapanAdmin2022", "N03-22_220101") %>%
  filter(N03_003 %in% c(
    "横浜市", "大阪市", "名古屋市", "札幌市", "福岡市",
    "川崎市", "神戸市", "京都市", "さいたま市", "広島市",
    "仙台市", "千葉市", "北九州市", "堺市", "浜松市",
    "新潟市", "熊本市", "相模原市", "岡山市", "静岡市"
  )) %>%
  select(N03_001, N03_003) %>%
  rename(pref = N03_001, city = N03_003) %>%
  group_by(pref, city) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_transform(crs = 6690)
city_deer <- st_join(jp_deer, city) %>%
  filter(!is.na(city)) %>%
  mutate(mesh = as.character(mesh))

# 各城市鹿种群历史数据。
city_deer_hist <- city_deer %>%
  st_drop_geometry() %>%
  left_join(jp_deer_hist, by = "mesh") %>%
  rename_with(~ gsub("年度当初推定生息密度.頭...", "", .x)) %>%
  rename_with(~ gsub("X", "", .x)) %>%
  pivot_longer(cols = c(as.character(2014:2022)), names_to = "year", values_to = "deer_d")

# 各城市人口数据。
city_pop <- list.files("data_raw/MeshPop") %>%
  lapply(function(x) st_read(paste0("data_raw/MeshPop/", x))) %>%
  bind_rows() %>%
  st_transform(crs = 6690) %>%
  select(mesh_8d = MESH_ID, pop_2015 = PTN_2015)
# 漏洞：如果用地理数据叠加，会出现对不齐的情况。
# city_pop_agg <-
#   st_intersection(select(city_deer, mesh, geometry), city_pop) %>%
#   group_by(mesh) %>%
#   summarise(pop_2015 = sum(pop_2015), .groups = "drop")

city_pop_agg <-
  city_deer %>%
  # 漏洞：边界上的mesh会同时被划入两个城市。
  left_join(
    city_pop %>%
      st_drop_geometry() %>%
      mutate(mesh = substr(mesh_8d, 1, 7)) %>%
      group_by(mesh) %>%
      summarise(pop_2015 = sum(pop_2015), .groups = "drop"),
    by = "mesh"
  )

# Analysis ----
# 各城市网格内鹿的数量。
mapview(city_deer, zcol = "d_2022")

# 各城市鹿和人口比率。
# 漏洞：如何构建人兽潜在冲突指数？
city_pop_agg %>%
  mutate(deer_pre = d_2022 * pop_2015) %>%
  mapview(zcol = "deer_pre")

# 如果按城市排名呢？
city_pop_agg %>%
  st_drop_geometry() %>%
  mutate(deer_pre = d_2022 * pop_2015) %>%
  group_by(city) %>%
  summarise(
    deer_pre_sum = sum(deer_pre, na.rm = TRUE),
    deer_pre_mid = median(deer_pre, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(-deer_pre_sum)

# 各城市鹿和人口关系。
city_pop_agg %>%
  st_drop_geometry() %>%
  ggplot() +
  geom_point(aes(d_2022, pop_2015), alpha = 0.5) +
  geom_smooth(aes(d_2022, pop_2015), method = "lm") +
  facet_wrap(.~ city, scales = "free")

# 各城市中各网格的鹿种群变化。
ggplot(city_deer_hist) +
  geom_line(aes(
    year, deer_d, col = as.character(mesh), group = as.character(mesh)
  )) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  ) +
  facet_wrap(.~ city, scales = "free_y")

# Kyoto test ----
kyo_land <- st_read(
  "data_raw/LandUse/L03-b-16_5235-tky_GML", "L03-b-16_5235-tky",
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
  )

# 计算京都每个mesh中各类目标土地利用的比例。
deer_mesh_land <- city_deer %>%
  st_drop_geometry() %>%
  select(mesh) %>%
  # 漏洞：应该用inner_join还是left_join呢？
  inner_join(st_drop_geometry(kyo_land), by = "mesh") %>%
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

# 京都市鹿-森林潜在风险指数。
city_deer %>%
  inner_join(deer_mesh_land %>% filter(land_code == "0500"), by = "mesh") %>%
  mutate(risk_forest = land_prop * d_2022) %>%
  mapview(zcol = "risk_forest")
# 京都市鹿-农田潜在风险指数。
city_deer %>%
  inner_join(deer_mesh_land %>% filter(land_code == "0100"), by = "mesh") %>%
  mutate(risk_forest = land_prop * d_2022) %>%
  mapview(zcol = "risk_forest")

# 各类土地利用和鹿的关系。
city_deer %>%
  inner_join(deer_mesh_land, by = "mesh") %>%
  st_drop_geometry() %>%
  ggplot() +
  geom_point(aes(land_prop, d_2022), alpha = 0.5) +
  geom_smooth(aes(land_prop, d_2022), method = "lm") +
  facet_grid(city ~ land_code, scales = "free")
