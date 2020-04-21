library(jpcovid19)

df_corona_gojp <-
  c("agoop", "docomo") %>%
  purrr::map_dfr(collect_corona_go_jp) %>% 
  tibble::as_tibble() %>% 
  purrr::discard(names(.) == "category")

if (!file.exists("data-raw/corona_gojp_mobility.csv")) {
  df_corona_gojp %>% 
    readr::write_csv("data-raw/corona_gojp_mobility.csv")
} else {
  rbind(readr::read_csv("data-raw/corona_gojp_mobility.csv"),
        df_corona_gojp) %>% 
    readr::write_csv("data-raw/corona_gojp_mobility.csv")
}
