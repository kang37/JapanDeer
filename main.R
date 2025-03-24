# Package ----
library(sf)
library(dplyr)
library(mapview)
library(tmap)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(DescTools)
library(showtext)
showtext_auto()

# Data ----
tar_load(city_mesh)
tar_load(city_pop)
tar_load(jp_deer)
# Bug: 应该在数据构建阶段填充？
jp_deer <- jp_deer %>%
  mutate(across(c(d_2015, d_2021, d_2022), ~replace_na(.x, 0)))
tar_load(city_deer_mesh_land_wide)

# Risk ----
# 人-鹿潜在冲突。
city_deer_risk <- rbind(
  city_mesh %>% mutate(stage = 1), city_mesh %>% mutate(stage = 2)
) %>%
  # 将鹿数据映射到底图上。
  left_join(
    st_drop_geometry(jp_deer) %>%
      select(mesh, d_2015, d_2021) %>%
      rename_with(~ gsub("d_", "", .x)) %>%
      pivot_longer(cols = c("2015", "2021"), names_to = "year", values_to = "deer") %>%
      mutate(stage = case_when(year == "2015" ~ 1, year == "2021" ~ 2)) %>%
      select(-year),
    by = c("mesh", "stage")
  ) %>%
  # 补充：缺失值一般是没有调查或者多年没有鹿出现的地方，因此填充为0.
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

# Analysis ----
## General ----
# Bug: 研究区域作图。

# 各城市网格内鹿的数量。
mapview(city_deer_risk %>% filter(stage == 1), zcol = "deer")
mapview(city_deer_risk %>% filter(stage == 2), zcol = "deer")

## Risk ----
# 各城市各网格的各种风险。
mapview(city_deer_risk %>% select(mesh, risk_human), zcol = "risk_human")
mapview(city_deer_risk %>% select(mesh, risk_agr), zcol = "risk_agr")
mapview(city_deer_risk %>% select(mesh, risk_forest), zcol = "risk_forest")
# 漏洞：如果城市的所有mesh的所有风险都为0，那么应该去掉。
city_deer_risk %>%
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
  facet_wrap(~ risk_cat, scales = "free", ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
# Bug: 最后一个子图的Y轴范围需要更改。
# 函数：用于做风险对比箱形图。
risk_boxplot <- function(risk_name, y_name) {
  city_deer_risk %>%
    st_drop_geometry() %>%
    ggplot() +
    # Bug: 早点把stage变成character比较好。
    geom_boxplot(aes(city, get(risk_name), col = as.character(stage))) +
    theme_bw() +
    labs(x = NULL, y = y_name, col = "Stage")
}
(
  risk_boxplot("risk_human", "Human-deer\nrisk") +
    coord_cartesian(ylim = c(0, 1e6))
) /
  risk_boxplot("risk_forest", "Forest-deer\nrisk") /
  risk_boxplot("risk_agr", "Agri-deer\nrisk") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# 不同风险之间的关系。
ggplot(st_drop_geometry(city_deer_risk)) +
  geom_point(aes(risk_human, risk_forest, col = as.factor(stage)), alpha = 0.5) +
  facet_wrap(.~ city)
ggplot(st_drop_geometry(city_deer_risk)) +
  geom_point(aes(risk_human, risk_agr, col = as.factor(stage)), alpha = 0.5) +
  facet_wrap(.~ city)
ggplot(st_drop_geometry(city_deer_risk)) +
  geom_point(aes(risk_forest, risk_agr, col = as.factor(stage)), alpha = 0.5) +
  facet_wrap(.~ city)

# 各个城市的风险中位数。
# 漏洞：应该算中位数吗？NA值也尚未处理。
st_drop_geometry(city_deer_risk) %>%
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
city_deer_risk %>%
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
city_deer_risk %>%
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
st_drop_geometry(city_deer_risk) %>%
  ggplot() +
  geom_point(aes(pop_2020, d_2022), alpha = 0.5) +
  geom_smooth(aes(pop_2020, d_2022), method = "lm") +
  facet_wrap(.~ city, scales = "free")
# 漏洞：看不清图。
st_drop_geometry(city_deer_risk) %>%
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
    cor_res <- cor.test(city_deer_risk$d_2022, city_deer_risk[[x]])
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
  city_deer_risk %>%
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
city_deer_risk_rate <-
  # Bug：应该用空网格来叠加。
  city_deer_risk %>%
  select(mesh, geometry) %>%
  left_join(
    city_deer_risk %>%
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
city_deer_risk_rate %>%
  st_drop_geometry() %>%
  # Bug.
  filter(!is.na(chg_rate)) %>%
  ggplot(aes(city, chg_rate)) +
  geom_boxplot() +
  geom_point(alpha = 0.3) +
  facet_wrap(.~ risk_cat, ncol = 1) +
  theme(axis.text.x = element_text(angle = 90))

# 增长率的空间分布。
tm_shape(city_deer_risk_rate) +
  tm_polygons(col = "chg_rate") +
  tm_facets(by = "city", along = "risk_cat")

# Export ----
# 各个变量中位数。
city_deer_risk %>%
  st_drop_geometry() %>%
  group_by(city) %>%
  summarise(
    across(d_2022:risk_forest, median, .names = "{.col}_mid"),
    across(risk_human:risk_forest, Gini, .names = "{.col}_gini")
  ) %>%
  write.csv(paste0("data_proc/city_deer_risk_", Sys.Date(), ".csv"))

