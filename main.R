# Package ----
library(sf)
library(dplyr)
library(mapview)
library(tmap)
library(tidyr)
library(ggplot2)
library(ggrepel)
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

# 读取日本空5千米mesh数据作为底图，并进行裁切和筛选。
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
  # GIS数据。
  st_read(dsn = "data_raw/japan_deer", layer = "生息密度2022") %>%
  mutate(mesh = as.character(mesh)) %>%
  left_join(
    # 添加Excel表格中的历年鹿数据。
    read.csv("data_raw/japan_deer/2014_2022年度全国推定生息密度.csv") %>%
      tibble() %>%
      select(contains("メッシュ"), contains("2015"), contains("2021")) %>%
      rename_with(~ c("mesh", "d_2015", "d_2021")) %>%
      mutate(mesh = as.character(mesh)),
    by = "mesh"
  ) %>%
  # 删除无数据网格。
  filter(!is.na(d_2015) | !is.na(d_2021) | !is.na(d_2022))

## Population ----
# 各mesh人口数据。
city_pop <- list.files("data_raw/Mesh100Pop2020") %>%
  .[!grepl("zip", .)] %>%
  lapply(function(x) st_read(paste0("data_raw/Mesh100Pop2020/", x))) %>%
  bind_rows() %>%
  # 转为普通数据框。
  st_drop_geometry() %>%
  mutate(mesh_8d = substr(MESH_CODE, 1, 8), pop = PopT) %>%
  select(mesh_8d, pop) %>%
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
    pop = sum(pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(stage = 2)
# 2015年各网格人口数据。
city_pop_2015 <- list.files("data_raw/Mesh100Pop2015") %>%
  .[!grepl("zip", .)] %>%
  lapply(function(x) st_read(paste0("data_raw/Mesh100Pop2015/", x))) %>%
  bind_rows() %>%
  # 转为普通数据框。
  st_drop_geometry() %>%
  # Bug: Name "Meshcode" in following code is different from above 2021 data.
  mutate(mesh_8d = substr(Meshcode, 1, 8), pop = PopT) %>%
  select(mesh_8d, pop) %>%
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
    pop = sum(pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(stage = 1)
# 合并人口数据。
city_pop <- rbind(city_pop_2015, city_pop_2020)
rm(city_pop_2015, city_pop_2020)

## Land ----
# 读取土地利用mesh数据，并且提取7位mesh编号。
read_land <- function(year_x, dsn_x) {
  st_read(
    paste0("data_raw/LandUse", year_x, "/", dsn_x),
    options = "ENCODING=Shift-JIS"
  ) %>%
    rename_with(~ c("mesh_10d", "land_code", "date"), .cols = -geometry) %>%
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

# 读取2021年所有土地利用数据。
jp_land_2021 <- lapply(
  list.files("data_raw/LandUse2021/"),
  read_land, year_x = 2021
) %>%
  bind_rows()

# 读取2016年所有土地利用数据：需要10分钟。
jp_land_2016 <- lapply(
  list.files("data_raw/LandUse2016/"),
  read_land, year_x = 2016
) %>%
  bind_rows()

# 合并两个年份的土地利用数据。
jp_land <- rbind(
  jp_land_2016 %>% mutate(year = 2016, stage = 1),
  jp_land_2021 %>% mutate(year = 2021, stage = 2)
)
rm(jp_land_2016)
rm(jp_land_2021)

# 计算各城市每个mesh中各类目标土地利用的比例。
city_deer_mesh_land <- city_mesh %>%
  st_drop_geometry() %>%
  select(mesh) %>%
  distinct() %>%
  # 漏洞：应该用inner_join还是left_join呢？
  inner_join(jp_land, by = "mesh") %>%
  # 每个mesh中包含多少个mesh_10d。
  group_by(stage, mesh) %>%
  mutate(mesh_10d_num = n()) %>%
  ungroup() %>%
  # 每个mesh中各类土地利用mesh的数量。
  group_by(stage, mesh, mesh_10d_num, land_code) %>%
  summarise(
    land_mesh_10d_num = n(),
    .groups = "drop"
  ) %>%
  # 各类土地利用在各个mesh中的比例。
  mutate(land_prop = land_mesh_10d_num / mesh_10d_num)

city_deer_mesh_land_wide <- city_deer_mesh_land %>%
  mutate(land_code = paste0("lu_", land_code)) %>%
  pivot_wider(
    id_cols = c("stage", "mesh"), names_from = "land_code",
    values_from = "land_prop", values_fill = 0
  )

## Risk ----
# 人-鹿潜在冲突。
city_deer_risk <- city_mesh %>%
  # 将鹿数据映射到底图上。
  left_join(
    st_drop_geometry(jp_deer) %>%
      select(mesh, d_2015, d_2021) %>%
      rename_with(~ gsub("d_", "", .x)) %>%
      pivot_longer(cols = c("2015", "2021"), names_to = "year", values_to = "deer") %>%
      mutate(stage = case_when(year == "2015" ~ 1, year == "2021" ~ 2)),
    by = "mesh"
  ) %>%
  # 加入人口数据。
  left_join(city_pop, by = c("stage", "mesh")) %>%
  # 加入农田和森林数据。
  left_join(city_deer_mesh_land_wide, by = c("stage", "mesh")) %>%
  # 数据缺值补充。
  mutate(
    # 漏洞：原数据12920行中，116行鹿密度为0，4885行鹿密度为NA。根据数据说明，NA来源于3种情况：令和2年调查结果数量为0；森林面积为0；密度不足。因此，此处将NA都作为0处理。不过对于北海道而言，并非没有风险，而是没有调查数据。
    deer = case_when(is.na(deer) ~ 0, TRUE ~ deer),
    # 漏洞：人口数据缺失值通常意味着对应mesh内无常住人口，因此作为0处理。
    pop = case_when(is.na(pop) ~ 0, TRUE ~ pop)
  ) %>%
  # 计算风险值。
  mutate(
    risk_human = deer * pop,
    risk_agr = deer * lu_0100,
    risk_forest = deer * lu_0500
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
mapview(city_deer_risk_tar %>% filter(stage == 1), zcol = "deer")
mapview(city_deer_risk_tar %>% filter(stage == 2), zcol = "deer")

## Risk ----
# 各城市各网格的各种风险。
mapview(city_deer_risk_tar %>% select(mesh, risk_human), zcol = "risk_human")
mapview(city_deer_risk_tar %>% select(mesh, risk_agr), zcol = "risk_agr")
mapview(city_deer_risk_tar %>% select(mesh, risk_forest), zcol = "risk_forest")
# 漏洞：如果城市的所有mesh的所有风险都为0，那么应该去掉。
city_deer_risk_tar %>%
  st_drop_geometry() %>%
  select("stage", "city", "mesh", "risk_human", "risk_agr", "risk_forest") %>%
  # Bug: What to filt?
  filter(!is.na(stage)) %>%
  pivot_longer(
    cols = c("risk_human", "risk_agr", "risk_forest"),
    names_to = "risk_cat", values_to = "risk_val"
  ) %>%
  # Bug: What to filt?
  filter(risk_val < 5000000) %>%
  ggplot(aes(city, risk_val, col = as.character(stage))) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, col = "grey") +
  facet_wrap(~ risk_cat, scales = "free", ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# 不同风险之间的关系。
ggplot(st_drop_geometry(city_deer_risk_tar)) +
  geom_point(aes(risk_human, risk_forest, col = as.factor(stage)), alpha = 0.5) +
  facet_wrap(.~ city)
ggplot(st_drop_geometry(city_deer_risk_tar)) +
  geom_point(aes(risk_human, risk_agr, col = as.factor(stage)), alpha = 0.5) +
  facet_wrap(.~ city)
ggplot(st_drop_geometry(city_deer_risk_tar)) +
  geom_point(aes(risk_forest, risk_agr, col = as.factor(stage)), alpha = 0.5) +
  facet_wrap(.~ city)

# 各个城市的风险中位数。
# 漏洞：应该算中位数吗？NA值也尚未处理。
st_drop_geometry(city_deer_risk_tar) %>%
  # Bug.
  filter(!is.na(stage)) %>%
  group_by(stage, city) %>%
  summarise(
    risk_human = median(risk_human, na.rm = TRUE),
    risk_agr = median(risk_agr, na.rm = TRUE),
    risk_forest = median(risk_forest, na.rm = TRUE)
  ) %>%
  ggplot(aes(risk_forest, risk_agr)) +
  geom_point(aes(size = risk_human), alpha = 0.3) +
  geom_text_repel(aes(label = city), size = 2) +
  facet_wrap(.~ stage)

## Gini ----
city_deer_risk_tar %>%
  st_drop_geometry() %>%
  # Bug.
  filter(!is.na(stage)) %>%
  select("stage", "city", "mesh", "risk_human", "risk_agr", "risk_forest") %>%
  pivot_longer(
    cols = c("risk_human", "risk_agr", "risk_forest"),
    names_to = "risk_cat", values_to = "risk_val"
  ) %>%
  group_by(stage, city, risk_cat) %>%
  summarise(risk_gini = Gini(risk_val), .groups = "drop") %>%
  ggplot() +
  geom_col(aes(city, risk_gini)) +
  facet_grid(risk_cat ~ stage, scales = "free") +
  theme(axis.text.x = element_text(angle = 90))

## Gini and average risk ----
# 漏洞：和上面的重复了。
city_deer_risk_tar %>%
  # Bug.
  filter(!is.na(stage)) %>%
  st_drop_geometry() %>%
  select("stage", "city", "mesh", "risk_human", "risk_agr", "risk_forest") %>%
  pivot_longer(
    cols = c("risk_human", "risk_agr", "risk_forest"),
    names_to = "risk_cat", values_to = "risk_val"
  ) %>%
  group_by(stage, city, risk_cat) %>%
  summarise(
    risk_mean = median(risk_val), risk_gini = Gini(risk_val), .groups = "drop"
  ) %>%
  ggplot() +
  geom_point(aes(city, risk_mean, size = risk_gini, col = as.factor(stage)), alpha = 0.5) +
  facet_wrap(.~ risk_cat, ncol = 1, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

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

## Change rate ----
# 每个网格的变化率。
# Bug: 应该挪到前面。
city_deer_risk_tar_rate <-
  # Bug：应该用空网格来叠加。
  city_deer_risk_tar %>%
  select(mesh, geometry) %>%
  left_join(
    city_deer_risk_tar %>%
      st_drop_geometry() %>%
      select(stage, city, mesh, risk_human, risk_agr, risk_forest) %>%
      pivot_longer(
        cols = c(risk_human, risk_agr, risk_forest),
        names_to = "risk_cat", values_to = "risk_val"
      ) %>%
      # Bug.
      filter(!is.na(stage)) %>%
      pivot_wider(
        id_cols = c(city, mesh, risk_cat),
        names_from = stage, values_from = risk_val, names_prefix = "stage_"
      ) %>%
      mutate(chg_rate = (stage_2 - stage_1) / stage_1),
    by = "mesh"
  )

# 各个城市的增长率。
city_deer_risk_tar_rate %>%
  st_drop_geometry() %>%
  # Bug.
  filter(!is.na(chg_rate)) %>%
  ggplot(aes(city, chg_rate)) +
  geom_boxplot() +
  geom_point(alpha = 0.3) +
  facet_wrap(.~ risk_cat, ncol = 1) +
  theme(axis.text.x = element_text(angle = 90))

# 增长率的空间分布。
tm_shape(city_deer_risk_tar_rate) +
  tm_polygons(col = "chg_rate") +
  tm_facets(by = "city", along = "risk_cat")

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

