library(jpcovid19)
df_corona_gojp <-
  c("agoop", "docomo", "kddi") %>% 
  purrr::set_names(c("株式会社Agoop", 
                     "NTTドコモ「モバイル空間統計」分析レポート", 
                     "KDDI株式会社")) %>% 
  purrr::map_dfr(~ collect_corona_go_jp(.x),
                 .id = "source") %>% 
  tibble::as_tibble()

if (!file.exists("data-raw/corona_gojp_mobility.csv")) {
  df_corona_gojp %>%
    readr::write_csv("data-raw/corona_gojp_mobility.csv")
} else {
  rbind(readr::read_csv("data-raw/corona_gojp_mobility.csv",
                        col_types = "cDdcdddd"),
        df_corona_gojp) %>%
    readr::write_csv("data-raw/corona_gojp_mobility.csv")
}

# 観測位置 --------------------------------------------------------------------
# Agoop ... 全国主要繁華街エリア --> 原則として500mのバッファ
# NTT Docomo ... 全国主要都市 --> 500mメッシュ
# KDDI ... 全国主要観光地 --> 500mメッシュ
# 対象メッシュコード
library(assertr)
library(jpmesh)
# https://www.nttdocomo.co.jp/utility/demographic_analytics/
if (!file.exists("data-raw/mobility_nttdocomo_locations.rds")) {
  library(rvest)
  # library(dplyr)
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
if (!file.exists("data-raw/mobility_kddi_locations.rds")) {
  # https://www.au.com/content/dam/au-com/information/covid-19/pdf/KDDI_area_analysis.pdf
  df_kddi_locations <- 
    tibble::tibble(
      prefecture = c("北海道", "北海道", "北海道", "北海道",
                     "東京都", 
                     rep("栃木県", 4),
                     rep("神奈川県", 7),
                     "神奈川県",
                     rep("神奈川県", 4),
                     rep("石川県", 3),
                     "長野県",
                     rep("長野県", 2),
                     rep("静岡県", 2),
                     rep("静岡県", 3),
                     rep("三重県", 3),
                     "京都府",
                     rep("兵庫県", 3),
                     rep("兵庫県", 2),
                     "奈良県",
                     "広島県",
                     "愛媛県",
                     "長崎県",
                     rep("熊本県", 2),
                     rep("大分県", 6),
                     rep("沖縄県", 2)),
      location_name = c("函館駅前", 
                        "小樽運河", "小樽運河", "小樽運河",
                        "浅草雷門", 
                        rep("日光東照宮", 4),
                        rep("江の島", 7),
                        "鎌倉",
                        rep("箱根湯本", 4),
                        rep("金沢市 兼六園", 3),
                        "松本駅",
                        rep("軽井沢", 2),
                        rep("熱海温泉街", 2),
                        rep("浜名湖内浦湾", 3),
                        rep("伊勢神宮", 3),
                        "京都円山公園",
                        rep("神戸市メリケンパーク", 3),
                        rep("淡路島 明石海峡大橋", 2),
                        "奈良市 大和西大寺",
                        "広島市 本通り",
                        "道後温泉",
                        "⾧崎市 観光通り",
                        rep("熊本城公園", 2),
                        rep("由布院", 6),
                        rep("石垣島 ゆいロード", 2)),
      meshcode = c("644142881", 
                   "644160302", "644160204", "644160303",
                   "533946532",
                   "553914081", "553914074", "553904993", "553904994",
                   "523973774", "523973781", "523973684", "523973681", 
                   "523973682", "523973583", "523973584",
                   "523974841",
                   "523960774", "523960783", "523960772", "523960771",
                   "543665721", "543665722", "543665731",
                   "543727773",
                   "543845102", "543845004",
                   "523950261", "523950154",
                   "523714292", "523714193", "523715103",
                   "513655574", "513655572", "513655474",
                   "523546021",
                   "523501153", "523501251", "523501144",
                   "513560913", "513570011",
                   "523506322",
                   "513243762",
                   "503266222",
                   "492907901",
                   "493015663", "493015664",
                   "493172183", "493172184", "493172193", "493172291", "493172182", "493172191",
                   "362441023", "362441024")
    ) %>% 
    meshcode_sf(meshcode) %>% 
    verify(dim(.) == c(55, 4))
  df_kddi_locations %>% 
    readr::write_rds("data-raw/mobility_kddi_locations.rds")
}
