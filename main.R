# Package ----
pacman::p_load(
  dplyr, tidyr, sf, DescTools, ggplot2, ggspatial, ggrepel, tmap,
  mapview, patchwork, cowplot, showtext
)
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
  annotation_north_arrow(
    location = "tl", style = north_arrow_fancy_orienteering()
  ) +
  # Bug: 可以删除。
  # geom_sf_text(data = city_loc, aes(label = city_en), size = 2) +
  theme_bw()
dev.off()

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

map_risk <- function(city_id_seq, risk_scale_name, risk_name, stage_n) {
  # 筛选城市，如第1到第6个城市。
  city_bbox_sub <- city_bbox[city_id_seq, ]
  # 循环画图并且组合。
  purrr::pmap(
    list(
      city_bbox_sub$city_en,
      city_bbox_sub$xmin_adj, city_bbox_sub$xmax_adj,
      city_bbox_sub$ymin_adj, city_bbox_sub$ymax_adj
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
          city_en == "Fukuoka" ~ "Fukuo.",
          city_en == "Kitakyushu" ~ "Kitakyu.",
          city_en == "Hiroshima" ~ "Hiroshi.",
          city_en == "Okayama" ~ "Okaya.",
          city_en == "Hamamatsu" ~ "Hama.",
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
# 第一张分图。
jpeg(
  filename = paste0("data_proc/map_risk_1_", Sys.Date(), ".jpg"),
  res = 300, width = 1286, height = 2000
)
map_risk(1:6, "risk_human_scale", "risk_human", 1) /
  map_risk(1:6, "risk_human_scale", "risk_human", 2) /
  map_risk(1:6, "risk_agr_scale", "risk_agr", 1) /
  map_risk(1:6, "risk_agr_scale", "risk_agr", 2) /
  map_risk(1:6, "risk_forest_scale", "risk_forest", 1) /
  map_risk(1:6, "risk_forest_scale", "risk_forest", 2)
dev.off()

# 第二张分图。
jpeg(
  filename = paste0("data_proc/map_risk_2_", Sys.Date(), ".jpg"),
  res = 300, width = 1286, height = 2000
)
map_risk(7:12, "risk_human_scale", "risk_human", 1) /
  map_risk(7:12, "risk_human_scale", "risk_human", 2) /
  map_risk(7:12, "risk_agr_scale", "risk_agr", 1) /
  map_risk(7:12, "risk_agr_scale", "risk_agr", 2) /
  map_risk(7:12, "risk_forest_scale", "risk_forest", 1) /
  map_risk(7:12, "risk_forest_scale", "risk_forest", 2)
dev.off()

# 第三张分图。
jpeg(
  filename = paste0("data_proc/map_risk_3_", Sys.Date(), ".jpg"),
  res = 300, width = 1500, height = 2000
)
map_risk(13:19, "risk_human_scale", "risk_human", 1) /
  map_risk(13:19, "risk_human_scale", "risk_human", 2) /
  map_risk(13:19, "risk_agr_scale", "risk_agr", 1) /
  map_risk(13:19, "risk_agr_scale", "risk_agr", 2) /
  map_risk(13:19, "risk_forest_scale", "risk_forest", 1) /
  map_risk(13:19, "risk_forest_scale", "risk_forest", 2)
dev.off()

# Bug: 图例另画。
# 提取图例。
city_deer_risk %>%
  filter(city_en == "kyoto", risk_human_scale %in% seq(0, 1, 0.2)) %>%
  mutate(risk_scale = case_when(
    risk_human == 0 ~ NA, TRUE ~ risk_human_scale
  )) %>%
  ggplot() +
  geom_sf(aes(fill = risk_scale)) +
  scale_fill_gradient(
    low = "lightyellow", high = "red", na.value = "lightgreen",
    limits = c(0, 1), breaks = c(0, 0.5, 1.0)
  ) +
  labs(fill = "Scaled risk") +
  theme(legend.position = "top")

### Risk box plot ----
# 各城市前后风险差异是否显著。
comp_risk_bf <- function(risk_name) {
  city_deer_risk %>%
    st_drop_geometry() %>%
    select(city_en, mesh, stage, risk_name) %>%
    pivot_wider(names_from = stage, values_from = risk_name) %>%
    group_by(city_en) %>%
    summarise(
      p = wilcox.test(`1`, `2`, paired = TRUE)$p.value, .groups = "drop"
    ) %>%
    mutate(p_mark = case_when(
      p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p >= 0.05 ~ ""
    ))
}
comp_risk_bf("risk_human_scale")
comp_risk_bf("risk_agr_scale")
comp_risk_bf("risk_forest_scale")

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
risk_boxplot <- function(risk_name, y_name, label_dt, label_y) {
  city_deer_risk %>%
    st_drop_geometry() %>%
    ggplot() +
    # Bug: 早点把stage变成character比较好。
    geom_boxplot(aes(city_en, get(risk_name), col = as.character(stage))) +
    geom_text(
      data = label_dt, aes(city_en, label_y, label = p_mark), size = 3
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = NULL, y = y_name, col = "Stage")
}
# 标准化数值作图。
(
  risk_boxplot(
    "risk_human_scale", "Human-deer\nrisk",
    comp_risk_bf("risk_human_scale"), -0.02
  ) +
    coord_cartesian(ylim = c(-0.02, 0.2)) +
    theme(axis.text.x = element_blank())
) / (
  risk_boxplot(
    "risk_agr_scale", "Agri-deer\nrisk",
    comp_risk_bf("risk_agr_scale"), -0.1
  ) +
    theme(axis.text.x = element_blank())
) /
  risk_boxplot(
    "risk_forest_scale", "Forest-deer\nrisk",
    comp_risk_bf("risk_forest_scale"), -0.1
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")
# 标准化值作图：Y轴完整版。
(
  risk_boxplot(
    "risk_human_scale", "Human-deer\nrisk",
    comp_risk_bf("risk_human_scale"), -0.1
  ) +
    theme(axis.text.x = element_blank())
) / (
  risk_boxplot(
    "risk_agr_scale", "Agri-deer\nrisk",
    comp_risk_bf("risk_agr_scale"), -0.1
  ) +
    theme(axis.text.x = element_blank())
) /
  risk_boxplot(
    "risk_forest_scale", "Forest-deer\nrisk",
    comp_risk_bf("risk_forest_scale"), -0.1
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

### Risk summary ----
# 对各阶段各城市各风险数据进行正态性检验。
lapply(
  c("risk_human_scale", "risk_agr_scale", "risk_forest_scale"),
  function(risk_name) {
    print(risk_name)
    st_drop_geometry(city_deer_risk) %>%
      group_by(stage, city_en) %>%
      summarise(
        # 人兽冲突。
        unique_val_num = length(unique(get(risk_name))),
        risk_shapiro_p = ifelse(
          unique_val_num > 5, shapiro.test(risk_human_scale)$p.value, NA
        ),
        risk_norm = c(risk_shapiro_p >= 0.05),
        .groups = "drop"
      ) %>%
      pull(risk_norm) %>%
      table(useNA = "ifany")
  }
)

# 各个城市的风险中位数和基尼系数。
# 由于数据不符合正态分布，故不计算平均值。
risk_smry <-
  st_drop_geometry(city_deer_risk) %>%
  group_by(stage, city_en) %>%
  summarise(
    across(
      c(risk_human_scale, risk_agr_scale, risk_forest_scale),
      ~ median(.x, na.rm = TRUE),
      .names = "{.col}_mid"
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
    # 对风险类别进行排序。
    mutate(
      risk_cat = case_when(
        risk_cat == paste0("risk_human_scale_", smry_x) ~ "Deer-human risk",
        risk_cat == paste0("risk_agr_scale_", smry_x) ~ "Deer-agri risk",
        risk_cat == paste0("risk_forest_scale_", smry_x) ~ "Deer-forest risk"
      ),
      risk_cat = factor(risk_cat, levels = c(
        "Deer-human risk", "Deer-agri risk", "Deer-forest risk"
      ))
    ) %>%
    pivot_wider(names_from = stage, values_from = risk_val) %>%
    ggplot() +
    geom_segment(aes(x = city_en, y = `1`, yend = `2`)) +
    geom_point(
      aes(city_en, `1`, fill = "1"), col = "red", shape = 21
    ) +
    geom_point(
      aes(city_en, `2`, fill = "2"), col = "darkgreen", shape = 21, alpha = 0.8
    ) +
    # 设置图例。
    scale_fill_manual(
      name = "Stage", values = c("1" = "#F8766D", "2" = "#56BCC2")
    ) +
    facet_grid(risk_cat ~., scales = "free") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90), legend.position = "right") +
    labs(x = NULL, y = smry_x)
}
# 中值变化。
segment_plt_smry("mid")
# 基尼系数变化。
segment_plt_smry("gini") +
  lims(y = c(0, 1)) +
  labs(y = "Gini coefficient") +
  scale_color_manual(
    name = "Legend Title", values = c("darkgreen" = "red", "red" = "blue")
  ) +
  guides(color = guide_legend(override.aes = list(shape = 16)))

# 中值和基尼系数变化综合图。
# 作图函数。
plt_mid_gini <- function(risk_type) {
  # 获得列名。
  risk_gini_name <- paste0("risk_", risk_type, "_scale_gini")
  risk_mid_name <- paste0("risk_", risk_type, "_scale_mid")
  # 画城市标签用数据。
  city_label_dt <- risk_smry %>%
    filter(
      risk_human_scale_mid + risk_agr_scale_mid + risk_forest_scale_mid != 0,
      stage == 1
    )
  # 作图。
  risk_smry %>%
    filter(
      risk_human_scale_mid + risk_agr_scale_mid + risk_forest_scale_mid != 0
    ) %>%
    ggplot() +
    geom_path(
      aes(get(risk_gini_name), get(risk_mid_name), group = city_en),
      col = "darkgrey", alpha = 0.8,
      arrow = arrow(type = "closed", length = unit(0.1, "inches"))
    ) +
    geom_point(
      aes(get(risk_gini_name), get(risk_mid_name), col = as.character(stage)),
      alpha = 0.9
    ) +
    geom_label_repel(
      data = city_label_dt,
      aes(get(risk_gini_name), get(risk_mid_name), label = city_en), size = 1
    ) +
    theme_bw() +
    labs(
      x = paste0("Deer-", risk_type, " risk\nGini coefficient"),
      y = paste0("Deer-", risk_type, " risk median value"),
      col = "Stage"
    ) +
    theme(legend.position = "none")
}
png("data_proc/mid_gini_city_stage.png", width = 2000, height = 800, res = 300)
plt_mid_gini("human") |
  plt_mid_gini("agr") |
  plt_mid_gini("forest")
dev.off()

## Change rate ----
# 每个网格的变化率。
# Bug: 应该挪到前面。
city_deer_risk_rate <-
  city_mesh %>%
  select(mesh, geometry) %>%
  left_join(
    city_deer_risk %>%
      st_drop_geometry() %>%
      select(stage, city_en, mesh, contains("scale")) %>%
      pivot_longer(
        cols = contains("scale"), names_to = "risk_cat", values_to = "risk_val"
      ) %>%
      pivot_wider(
        id_cols = c(city_en, mesh, risk_cat),
        names_from = stage, values_from = risk_val, names_prefix = "stage_"
      ) %>%
      mutate(chg_rate = (stage_2 - stage_1) / stage_1),
    by = "mesh", relationship = "many-to-many"
  ) %>%
  # 对风险进行排序。
  mutate(risk_cat = factor(risk_cat, levels = c(
    "risk_human_scale", "risk_agr_scale", "risk_forest_scale"
  )))

# 各个城市的增长率。
city_deer_risk_rate %>%
  st_drop_geometry() %>%
  ggplot(aes(city_en, chg_rate)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  facet_wrap(.~ risk_cat, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
# 对数尺度。
city_deer_risk_rate %>%
  st_drop_geometry() %>%
  ggplot(aes(city_en, log(chg_rate))) +
  geom_violin() +
  facet_wrap(.~ risk_cat, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# 增长率的空间分布。
tm_shape(city_deer_risk_rate) +
  tm_polygons(col = "chg_rate") +
  tm_facets(by = "city_en", along = "risk_cat")

# Export ----
# 各网格风险计算结果。
write.csv(
  city_deer_risk,
  paste0("data_proc/各城市各网格结果_", Sys.Date(), ".csv")
)
write.csv(
  risk_smry,
  paste0("data_proc/各城市风险中位数和基尼系数_", Sys.Date(), ".csv")
)
list(
  comp_risk_bf("risk_human_scale") %>% rename(risk_human_scale = city_en),
  comp_risk_bf("risk_agr_scale") %>% rename(risk_agr_scale = city_en),
  comp_risk_bf("risk_forest_scale") %>% rename(risk_forest_scale = city_en)
) %>%
  openxlsx::write.xlsx(
    paste0("data_proc/各城市前后风险对比_", Sys.Date(), ".xlsx")
  )
