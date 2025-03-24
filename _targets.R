# Packages.
pacman::p_load(targets, sf, dplyr, tidyr)

# Function.
# 函数：读取人口分布数据。
read_pop <- function(file_x, stage_x) {
  res <- list.files(paste0("data_raw/", file_x)) %>%
    .[!grepl("zip", .)] %>%
    lapply(function(x) st_read(paste0("data_raw/", file_x, "/", x))) %>%
    bind_rows() %>%
    # 转为普通数据框。
    st_drop_geometry()
  # 因不同年份列名不同，此处用不同处理。
  if(grepl("2015", file_x)) {
    res <- res %>% mutate(mesh_8d = substr(Meshcode, 1, 8), pop = PopT)
  } else {
    res <- res %>% mutate(mesh_8d = substr(MESH_CODE, 1, 8), pop = PopT)
  }
  # 继续处理。
  res <- res %>%
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
    mutate(stage = stage_x)
  return(res)
}

# 函数：读取土地利用mesh数据，并且提取7位mesh编号。
read_land <- function(dsn_x, year_x) {
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
    st_drop_geometry() %>%
    mutate(year = year_x)
}

# Data list.
# Bug：各个图层的mesh位置不一致。鹿数据和土地利用几乎一致，mesh人口数据和空mesh一致，相对偏右下，且和geosense网站似乎一致。目前将所有数据都对齐到全日本空mesh数据上。
list(
  # 提取城市内的鹿数据：以指定都市为例。由于北海道无鹿调查数据，故不计入。
  tar_target(
    city,
    st_read("data_raw/JapanAdmin2022", "N03-22_220101") %>%
      select(pref = N03_001, city = N03_003) %>%
      filter(city %in% c(
        "大阪市", "名古屋市", "京都市", "横浜市", "神戸市", "北九州市",
        "川崎市", "福岡市", "広島市", "仙台市", "千葉市", "さいたま市",
        "静岡市", "堺市", "新潟市", "浜松市", "岡山市", "相模原市", "熊本市"
      )) %>%
      group_by(pref, city) %>%
      summarise(geometry = st_union(geometry), .groups = "drop") %>%
      st_transform(crs = 6690)
  ),
  # 读取日本空5千米mesh数据作为底图，并进行裁切和筛选。
  tar_target(
    city_mesh,
    st_intersection(
      city, st_read("data_raw/mesh5") %>% st_transform(crs = 6690)
    ) %>%
      select(pref, city, mesh = Descriptio) %>%
      mutate(
        area = as.numeric(st_area(.))/1e+06, .before = "geometry"
      ) %>%
      # 漏洞：一个完整mesh面积为25平方千米，只留下大于5平方千米的mesh。
      filter(area > 5)
  ),
  # 读取日本鹿分布数据。
  tar_target(
    jp_deer,
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
      # Bug: 删除无数据网格。应该填充为0更合理。
      filter(!is.na(d_2015) | !is.na(d_2021) | !is.na(d_2022))
  ),
  # 人口数据。
  tar_target(
    city_pop,
    rbind(read_pop("Mesh100Pop2015", 1), read_pop("Mesh100Pop2020", 2))
  ),
  # 土地利用数据。
  tar_target(
    jp_land,
    rbind(
      lapply(list.files("data_raw/LandUse2016/"), read_land, year_x = 2016) %>%
        bind_rows() %>%
        mutate(stage = 1),
      lapply(list.files("data_raw/LandUse2021/"), read_land, year_x = 2021) %>%
        bind_rows() %>%
        mutate(stage = 2)
    )
  ),
  # 各城市每个mesh中各类目标土地利用的比例。
  tar_target(
    city_deer_mesh_land,
    city_mesh %>%
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
  ),
  # 数据变换。
  tar_target(
    city_deer_mesh_land_wide,
    city_deer_mesh_land %>%
      mutate(land_code = paste0("lu_", land_code)) %>%
      pivot_wider(
        id_cols = c("stage", "mesh"), names_from = "land_code",
        values_from = "land_prop", values_fill = 0
      )
  )
)
