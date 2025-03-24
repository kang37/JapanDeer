# Package ----
library(sf)
library(dplyr)
library(mapview)
library(tmap)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(cowplot)
library(DescTools)
library(showtext)
showtext_auto()

# Data ----
# Research area.
# Bug: 可以移动到targets中。
# 日本边界。
japan_boundary <- st_read(
  dsn = "data_raw/JapanBoundary", layer = "Japan_boundary"
)
# 城市列表。
city_loc <- c(
  "熊本市", "Kumamoto", 32.80300, 130.7079, 0, 0,
  "福岡市", "Fukuoka", 33.59000, 130.4017, 0, 0,
  "北九州市", "Kitakyushu", 33.88342, 130.8752, 0, 0,
  "広島市", "Hiroshima", 34.39140, 132.4519, 0, 0,
  "岡山市", "Okayama", 34.65511, 133.9196, 0, 0,
  "神戸市", "Kobe", 34.69017, 135.1954, 0, 0,
  "堺市", "Sakai", 34.57333, 135.4830, 0, 0,
  "大阪市", "Osaka", 34.69375, 135.5021, 0, 0,
  "浜松市", "Hamamatsu", 34.71089, 137.7262, 0, 0,
  "京都市", "Kyoto", 35.01161, 135.7681, 0, 0,
  "名古屋市", "Nagoya", 35.18140, 136.9064, 0, 0,
  "静岡市", "Shizuoka", 34.97560, 138.3825, 0, 0,
  "横浜市", "Yokohama", 35.45033, 139.6342, 0, 0,
  "相模原市", "Sagamihara", 35.56667, 139.3667, 0, 0,
  "川崎市", "Kawasaki", 35.53089, 139.7030, 0, 0,
  "さいたま市", "Saitama", 35.86140, 139.6456, 0, 0,
  "千葉市", "Chiba", 35.60728, 140.1064, 0, 0,
  "新潟市", "Niigata", 37.90247, 139.0232, 0, 0,
  "仙台市", "Sendai", 38.26822, 140.8694, 0, 0
) %>%
  matrix(byrow = TRUE, ncol = 6) %>%
  as.data.frame() %>%
  rename_with(~ c(
    "city_jp", "city_en", "latitude", "longitude", "x_adj", "y_adj"
  )) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(japan_boundary)) %>%
  mutate(
    x_adj = as.numeric(x_adj),
    y_adj = as.numeric(y_adj),
    city_en = factor(city_en, levels = city_en)
  )

# 研究区域。
jpeg(
  filename = paste0("data_proc/map_", Sys.Date(), ".jpg"),
  res = 300, width = 160, height = 80, units = "mm"
)
ggplot() +
  geom_sf(data = japan_boundary, fill = "lightgrey", col = NA) +
  geom_sf(
    data = city_loc, size = 1.5, fill = "#fd8a93", col = "white",
    shape = 21, stroke = 0.2
  ) +
  # Bug: 可以删除。
  # geom_sf_text(data = city_loc, aes(label = city_en), size = 2) +
  theme_bw()
dev.off()

# 载入数据。
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
  # 加入城市英文名。
  left_join(st_drop_geometry(city_loc), by = c("city" = "city_jp")) %>%
  # 将鹿数据映射到底图上。
  left_join(
    st_drop_geometry(jp_deer) %>%
      select(mesh, d_2015, d_2021) %>%
      rename_with(~ gsub("d_", "", .x)) %>%
      pivot_longer(
        cols = c("2015", "2021"), names_to = "year", values_to = "deer"
      ) %>%
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
  ) %>%
  # 计算风险标准化值。
  mutate(across(
    c(risk_human, risk_agr, risk_forest),
    ~(. - min(.))/(max(.) - min(.)),
    .names = "{.col}_scale"
  ))
# 漏洞：需要确保数值无缺。
apply(city_deer_risk, 2, function(x) sum(is.na(x)))

# Analysis ----
## General ----
# Bug: 研究区域作图。

## Risk ----
### Map ----
# 提取每个城市的经纬度范围，据此调整作图范围，以保证各图范围一致。
city_bbox <- city_mesh %>%
  group_by(city) %>%
  summarise(geometry = st_union(geometry)) %>%
  mutate(bbox = lapply(geometry, st_bbox)) %>%
  st_drop_geometry() %>%
  mutate(
    xmin = lapply(bbox, function(x) x["xmin"]) %>% unlist(),
    xmax = lapply(bbox, function(x) x["xmax"]) %>% unlist(),
    ymin = lapply(bbox, function(x) x["ymin"]) %>% unlist(),
    ymax = lapply(bbox, function(x) x["ymax"]) %>% unlist(),
  ) %>%
  ungroup() %>%
  select(city, bbox, xmin, xmax, ymin, ymax) %>%
  # 调整纬度范围：每个城市都以最大纬度跨度城市的纬度差进行调整。
  mutate(
    y_diff = ymax - ymin,
    ymin_adj = ymin - (max(y_diff) - y_diff) / 2,
    ymax_adj = ymax + (max(y_diff) - y_diff) / 2,
    x_diff = xmax - xmin,
    xmin_adj = xmin - (max(x_diff) * 1.2 - x_diff) / 2,
    xmax_adj = xmax + (max(x_diff) * 1.2 - x_diff) / 2
  ) %>%
  left_join(
    st_drop_geometry(select(city_loc, city_jp, city_en)),
    by = c("city" = "city_jp")
  ) %>%
  arrange(city_en)

map_risk <- function(risk_scale_name, risk_name, stage_n) {
  purrr::pmap(
    list(
      city_bbox$city_en,
      city_bbox$xmin_adj, city_bbox$xmax_adj,
      city_bbox$ymin_adj, city_bbox$ymax_adj
    ),
    function(city_name, xmin, xmax, ymin, ymax) {
      city_deer_risk %>%
        filter(city_en == city_name, stage == stage_n) %>%
        mutate(risk_scale = case_when(
          get(risk_name) == 0 ~ NA, TRUE ~ get(risk_scale_name)
        )) %>%
        # Bug: 名字太长起缩写。
        mutate(city_en = case_when(
          city_en == "Kumamoto" ~ "Kuma.",
          city_en == "Kitakyushu" ~ "Kitakyu.",
          city_en == "Hiroshima" ~ "Hiroshi.",
          city_en == "Okayama" ~ "Okaya.",
          city_en == "Hamamatsu" ~ "Hamama.",
          city_en == "Yokohama" ~ "Yokoha.",
          city_en == "Sagamihara" ~ "Sagami.",
          city_en == "Kawasaki" ~ "Kawasa.",
          TRUE ~ city_en
        )) %>%
        ggplot() +
        geom_sf(aes(fill = risk_scale)) +
        scale_fill_gradient(
          low = "lightyellow", high = "red", na.value = "lightgreen",
          limits = c(0, 1)
        ) +
        facet_wrap(.~ city_en) +
        coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
        theme_bw() +
        theme(
          legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank()
        )
    }
  ) %>%
    plot_grid(plotlist = ., nrow = 1, align = "h")
}
# 作图。
jpeg(
  filename = paste0("data_proc/map_risk_", Sys.Date(), ".jpg"),
  res = 300, width = 3800, height = 2000
)
map_risk("risk_human_scale", "risk_human", 1) /
  map_risk("risk_human_scale", "risk_human", 2) /
  map_risk("risk_agr_scale", "risk_agr", 1) /
  map_risk("risk_agr_scale", "risk_agr", 2) /
  map_risk("risk_forest_scale", "risk_forest", 1) /
  map_risk("risk_forest_scale", "risk_forest", 2)
dev.off()

# Bug: 图例另画。
# 提取图例。
# plt_legend <- city_deer_risk %>%
#   filter(stage == stage_n) %>%
#   mutate(risk_scale = case_when(
#     get(risk_name) == 0 ~ NA, TRUE ~ get(risk_scale_name)
#   )) %>%
#   ggplot() +
#   geom_sf(aes(fill = risk_scale)) +
#   scale_fill_gradient(
#     low = "yellow", high = "red", na.value = "lightgreen", limits = c(0, 1)
#   )
# legend <- get_legend(plt_legend)

### Box plot ----
# Bug: 如果城市的所有mesh的所有风险都为0，那么应该去掉。
city_deer_risk %>%
  st_drop_geometry() %>%
  select("stage", "city", "mesh", "risk_human_scale", "risk_agr_scale", "risk_forest_scale") %>%
  pivot_longer(
    cols = c("risk_human_scale", "risk_agr_scale", "risk_forest_scale"),
    names_to = "risk_cat", values_to = "risk_val"
  ) %>%
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
    geom_boxplot(aes(city_en, get(risk_name), col = as.character(stage))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
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
# 标准化数值作图。
(
  risk_boxplot("risk_human_scale", "Human-deer\nrisk") +
    coord_cartesian(ylim = c(0, 0.1)) +
    theme(axis.text.x = element_blank())
) / (
  risk_boxplot("risk_agr_scale", "Agri-deer\nrisk") +
    theme(axis.text.x = element_blank())
) /
  risk_boxplot("risk_forest_scale", "Forest-deer\nrisk") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# 各个城市的风险中位数、平均值、基尼系数。
# 漏洞：应该算中位数吗？NA值也尚未处理。
risk_smry <-
  st_drop_geometry(city_deer_risk) %>%
  # Bug.
  filter(!is.na(stage)) %>%
  group_by(stage, city_en) %>%
  summarise(
    across(
      c(risk_human_scale, risk_agr_scale, risk_forest_scale),
      ~ median(.x, na.rm = TRUE),
      .names = "{.col}_mid"
    ),
    across(
      c(risk_human_scale, risk_agr_scale, risk_forest_scale),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    across(
      c(risk_human_scale, risk_agr_scale, risk_forest_scale),
      ~ Gini(.x, na.rm = TRUE),
      .names = "{.col}_gini"
    ),
    .groups = "drop"
  )

# 可视化中位数、平均值、基尼系数的变化。
segment_plt_smry <- function(smry_x) {
  risk_smry %>%
    select(stage, city_en, ends_with(smry_x)) %>%
    pivot_longer(
      cols = ends_with(smry_x), names_to = "risk_cat", values_to = "risk_val"
    ) %>%
    pivot_wider(names_from = stage, values_from = risk_val) %>%
    ggplot() +
    geom_segment(aes(x = city_en, y = `1`, yend = `2`)) +
    geom_point(
      aes(city_en, `1`), fill = "lightgreen", col = "darkgreen", shape = 21
    ) +
    geom_point(
      aes(city_en, `2`), fill = "pink", col = "red", shape = 21, alpha = 0.8
    ) +
    facet_grid(risk_cat ~., scales = "free") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
}
segment_plt_smry("mid")
segment_plt_smry("mean")
segment_plt_smry("gini")

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

