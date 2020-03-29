####################################
# COVID-19 日本国内の感染者マッピング
# データ: 都道府県別新型コロナウイルス感染者数マップ（ジャッグジャパン株式会社提供）
# (Creative Commons BY-NC 国際4.0)
# https://gis.jag-japan.com/covid19jp/
####################################
if (!requireNamespace("RCurl", quietly = TRUE)) {
  renv::install("RCurl")
}
if (!requireNamespace("rgeos", quietly = TRUE)) {
  renv::install("rgeos")
}
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  renv::install("ropensci/rnaturalearthhires")
}
library(sf)
library(dplyr)
library(sfheaders)
library(rnaturalearth)
library(assertr)
library(ensurer)
library(drake)
ne_jpn <- 
  ne_states(country = "Japan", returnclass = "sf") %>% 
  tibble::new_tibble(nrow = nrow(.), subclass = "sf") %>% 
  arrange(iso_3166_2)

plan_data <- 
  drake::drake_plan(
  df_raw =
    readr::read_csv("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv") %>% 
    assertr::verify(ncol(.) == 50)  )
drake::make(plan_data)
drake::loadd(list = c("df_raw"))

glimpse(df_raw)