# Packages.
pacman::p_load(targets, sf, dplyr, tidyr)

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
      # 删除无数据网格。
      filter(!is.na(d_2015) | !is.na(d_2021) | !is.na(d_2022))
  )
)
