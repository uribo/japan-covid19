library(jpcovid19)
df_corona_gojp <-
  c("agoop", "docomo", "kddi", "yahoo") %>% 
  purrr::set_names(c("株式会社Agoop", 
                     "NTTドコモ「モバイル空間統計」分析レポート", 
                     "KDDI株式会社",
                     "ヤフー・データソリューション")) %>% 
  purrr::map_dfr(~ collect_corona_go_jp(.x),
                 .id = "source") %>% 
  tibble::as_tibble()

if (!file.exists("data-raw/corona_gojp_mobility.csv")) {
  df_corona_gojp %>%
    readr::write_csv("data-raw/corona_gojp_mobility.csv")
} else {
  rbind(readr::read_csv("data-raw/corona_gojp_mobility.csv"),
        df_corona_gojp) %>%
    readr::write_csv("data-raw/corona_gojp_mobility.csv")
}

# 観測位置 --------------------------------------------------------------------
# Agoop ... 全国主要繁華街エリア --> 原則として500mのバッファ
# NTT Docomo ... 全国主要都市 --> 500mメッシュ
# KDDI ... 全国主要観光地
# 対象メッシュコード
# https://www.nttdocomo.co.jp/utility/demographic_analytics/
if (!file.exists("data-raw/mobility_nttdocomo_locations.rds")) {
  library(rvest)
  # library(dplyr)
  library(assertr)
  library(jpmesh)
  df_nttdocomo_locations <- 
    read_html("https://mobaku.jp/covid-19/mesh-area/") %>% 
    html_nodes(css = "p") %>% 
    .[-1] %>% 
    html_text() %>% 
    tibble::enframe() %>% 
    tidyr::extract(value,
                   into = c("value", "meshcode"),
                   regex = c("(.+)[:space:]([0-9]{9})")) %>% 
    purrr::update_list(value = stringr::str_replace(.$value, "羽田空港 第", "羽田空港_第")) %>% 
    tidyr::extract(value,
                   into = c("prefecture", "location_name"),
                   regex = c("(.+)[:space:](.+)")) %>% 
    verify(dim(.) == c(95, 4)) %>% 
    jpmesh::meshcode_sf(meshcode)
  df_nttdocomo_locations %>% 
    readr::write_rds("data-raw/mobility_nttdocomo_locations.rds")
}
