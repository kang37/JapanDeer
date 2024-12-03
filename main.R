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
jp_deer <- st_read(dsn = "data_raw/japan_deer", layer = "生息密度2022")

# 日本鹿数量变化。
jp_deer_hist <- read.csv(
  "data_raw/japan_deer/【取り扱い注意】2014･2022年度全国推定生息密度.csv"
) %>% tibble()

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
city_deer <- st_intersection(jp_deer, city)

# 各城市鹿种群历史数据。
city_deer_hist <- city_deer %>%
  st_drop_geometry() %>%
  left_join(jp_deer_hist, by = c("mesh" = "X5kmメッシュ番号")) %>%
  rename_with(~ gsub("年度当初推定生息密度.頭...", "", .x)) %>%
  rename_with(~ gsub("X", "", .x)) %>%
  pivot_longer(cols = c(as.character(2014:2022)), names_to = "year", values_to = "deer_d")

# 各城市人口数据。
city_pop <- list.files("data_raw/MeshPop") %>%
  lapply(function(x) st_read(paste0("data_raw/MeshPop/", x))) %>%
  bind_rows() %>%
  st_transform(crs = 6690) %>%
  select(mesh_8d = MESH_ID, pop_2015 = PTN_2015)
city_pop_agg <-
  st_intersection(select(city_deer, mesh, geometry), city_pop) %>%
  group_by(mesh) %>%
  summarise(pop_2015 = sum(pop_2015), .groups = "drop")

# Analysis ----
# 各城市网格内鹿的数量。
mapview(city_deer, zcol = "d_2022")

# 各城市鹿和人口比率。
city_deer %>%
  left_join(st_drop_geometry(city_pop_agg), by = "mesh") %>%
  mutate(deer_pre = d_2022 / pop_2015) %>%
  mapview(zcol = "deer_pre")

# 各城市鹿和人口关系。
city_deer %>%
  left_join(st_drop_geometry(city_pop_agg), by = "mesh") %>%
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
