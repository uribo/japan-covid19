library(jpcovid19)

sources <- 
  c("agoop", "docomo", "kddi")

df_corona_gojp <-
  sources %>% 
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
  rbind(readr::read_csv("data-raw/corona_gojp_mobility.csv"),
        df_corona_gojp) %>% 
    readr::write_csv("data-raw/corona_gojp_mobility.csv")
}
