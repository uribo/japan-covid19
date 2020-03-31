####################################
# COVID-19 日本国内の感染者マッピング
# データ: 都道府県別新型コロナウイルス感染者数マップ（ジャッグジャパン株式会社提供）
# (Creative Commons BY-NC 国際4.0)
# https://gis.jag-japan.com/covid19jp/
# 項目について https://jag-japan.com/covid19map-readme/
####################################
if (!requireNamespace("RCurl", quietly = TRUE)) {
  renv::install("RCurl")
}
if (!requireNamespace("rgeos", quietly = TRUE)) {
  renv::install("rgeos")
}
if (!requireNamespace("tabularmaps", quietly = TRUE)) {
  renv::install("uribo/tabularmaps")
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
library(tabularmaps)
library(ggplot2)
library(readxl)
library(drake)
ne_jpn <- 
  ne_states(country = "Japan", returnclass = "sf") %>% 
  tibble::new_tibble(nrow = nrow(.), subclass = "sf") %>% 
  arrange(iso_3166_2)
plan_data <- 
  drake::drake_plan(
    dl_pops_2018h30_prefs = {
      if (!file.exists("data-raw/2018h30_a00400.xls")) {
        download.file(
          "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031807141&fileKind=0",
          destfile = "data-raw/2018h30_a00400.xls"
        )  
      }
    },
    # 13822000
    # 1382万2000
    df_pop_201810 =
      # 都道府県，男女別人口及び人口性比－総人口，日本人人口(平成30年10月1日現在)
      readxl::read_xls("data-raw/2018h30_a00400.xls",
                       range = "I20:T66",
                       col_types = c(rep("text", 2),
                                     "skip",
                                     rep("text", 9)),
                       col_names = c("jis_code", "prefecture_kanji", "prefecture",
                                     paste0("total_",
                                            c("both_sexes",
                                              "male",
                                              "female",
                                              "sex_rario")),
                                     paste0("japanese_",
                                            c("both_sexes",
                                              "male",
                                              "female",
                                              "sex_rario")))) %>% 
      readr::type_convert(col_types = "ccciiidiiid") %>% 
      select(-prefecture) %>% 
      mutate_if(is.character,
                stringr::str_trim),
  df_raw =
    readr::read_csv("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv") %>% 
    assertr::verify(ncol(.) == 50),
  data_lastupdate = 
    lubridate::mdy_hm(c(na.omit(unique(df_raw$更新日時))), tz = "Asia/Tokyo"),
  df =
    df_raw %>% 
    # 予備項目
    select(-num_range("Field", seq.int(2, 10)),
           -contains("累計"),
           -contains("前日比"),
           -contains("正誤確認用"),
           -contains("Pref"),
           # -ends_with("都道府県コード"),
           -Release,
           -Gender,
           -`確定日YYYYMMDD`,
           -`国内`,
           -`発表`,
           -`都道府県内症例番号`,
           -`人数`,
           -`退院数`,
           -`発症数`,
           -`PCR検査実施人数`,
           -`無症状病原体保有者`,
           -`死者合計`) %>%
    filter(X != "0") %>% 
    mutate_at(vars(`確定日`, `発症日`),
              lubridate::mdy),
  sf_df = 
    df %>% 
    filter(!is.na(`居住市区町村`)) %>% 
    sfheaders::sf_point("X", "Y", keep = TRUE) %>% 
    st_set_crs(4326) %>% 
    st_crop(ne_jpn) %>% 
    tibble::new_tibble(subclass = "sf", nrow = nrow(.)),
  df_pref_ratio = 
    df_pop_201810 %>% 
    select(jis_code, prefecture_kanji, total_both_sexes) %>% 
    mutate(total_both_sexes = total_both_sexes * 1000) %>% 
    left_join(
      df %>% 
        count(`居住都道府県`, `居住都道府県コード`) %>% 
        filter(!is.na(`居住都道府県コード`)) %>% 
        filter(`居住都道府県コード` != "0"),
      by = c("jis_code" = "居住都道府県コード",
             "prefecture_kanji" = "居住都道府県")) %>% 
    mutate(ratio = (n / total_both_sexes) * 100),
  df_plot =
    jpn77 %>% 
    left_join(df_pref_ratio,
              by = c("jis_code" = "jis_code",
                     "prefecture_kanji" = "prefecture_kanji")) %>% 
    mutate(n = tidyr::replace_na(n, 0)))
drake::make(plan_data)
drake::loadd(list = c("df_raw", "df", "sf_df", "df_pref_ratio", "df_plot", "data_lastupdate"))
# mapview::mapview(df)

library(patchwork)
p1_a <- 
  df_plot %>% 
  mutate(label = glue::glue("{prefecture_kanji}\n({n})")) %>% 
  tabularmap(fill = n, 
             label = label, 
             family = "IPAexGothic",
             color = "white",
             size = 2) +
  theme_tabularmap(base_family = "IPAexGothic") +
  nord::scale_fill_nord("halifax_harbor", 
                        discrete = FALSE,
                        name = "人数") +
  # labs(title = "都道府県別コロナウイルス感染者数",
  #      subtitle = "感染者の居住地",
  #      caption = glue::glue("作成: Shinya Uryu (@u_ribo)
  #      データソース: 都道府県別新型コロナウイルス感染者数マップ（ジャッグジャパン株式会社提供）
  #      (CC BY-NC 4.0, https://gis.jag-japan.com/covid19jp/)\n{data_lastupdate}時点
  #      レイアウト: カラム地図 (CC0, https://github.com/tabularmaps/hq)
  #                           括弧内の数値は感染者数")) +
  theme(plot.caption = element_text(size = 6)) +
  guides(fill = FALSE)

p2_a <- 
  df_plot %>% 
  mutate(label = glue::glue("{prefecture_kanji}\n({n})")) %>% 
  tabularmap(fill = ratio, label = label, 
             family = "IPAexGothic",
             color = "white",
             size = 2) +
  theme_tabularmap(base_family = "IPAexGothic") +
  nord::scale_fill_nord("halifax_harbor", 
                        discrete = FALSE,
                        name = "人口比率(%)") +
  theme(plot.caption = element_text(size = 6)) +
  guides(fill = FALSE)

p1_b <-
  df_plot %>% 
  filter(n > 0) %>% 
  arrange(desc(ratio)) %>% 
  ggplot(aes(forcats::fct_reorder(prefecture_kanji, n), n)) +
  geom_bar(stat = "identity", aes(fill = n)) +
  nord::scale_fill_nord("halifax_harbor", 
                        discrete = FALSE,
                        name = "感染者数") +
  coord_flip() +
  theme_gray(base_family = "IPAexGothic", base_size = 8) +
  xlab(NULL)
p2_b <- 
  df_plot %>% 
  filter(n > 0) %>% 
  arrange(desc(ratio)) %>% 
  ggplot(aes(forcats::fct_reorder(prefecture_kanji, ratio), ratio)) +
  geom_bar(stat = "identity", aes(fill = ratio)) +
  nord::scale_fill_nord("halifax_harbor", 
                        discrete = FALSE,
                        name = "人口比率(%)") +
  coord_flip() +
  theme_gray(base_family = "IPAexGothic", base_size = 8) +
  xlab(NULL)

p1_a + p1_b +
  plot_layout(ncol = 2) +
  plot_annotation(
    theme = theme(text = element_text(family = "IPAexGothic"),
                  plot.caption = element_text(size = 6)),
    title = "都道府県別コロナウイルス感染者数",
    subtitle = "1) 感染者の居住地",
    caption = glue::glue("作成: Shinya Uryu (@u_ribo)
       データソース: 都道府県別新型コロナウイルス感染者数マップ（ジャッグジャパン株式会社提供）
       (CC BY-NC 4.0, https://gis.jag-japan.com/covid19jp/)\n{data_lastupdate}時点
       レイアウト: カラム地図 (CC0, https://github.com/tabularmaps/hq)
                            括弧内の数値は感染者数"))
ggsave(last_plot(),
       filename = glue::glue("figures/{datetime}_prefecture_count.png",
                             datetime = stringr::str_replace(as.character(data_lastupdate), " ", "_") %>% 
                               stringr::str_replace_all(":", "")),
       width = 10,
       height = 8)

p2_a + p2_b +
  plot_layout(ncol = 2) +
  plot_annotation(
    theme = theme(text = element_text(family = "IPAexGothic"),
                  plot.caption = element_text(size = 6)),
    title = "都道府県別コロナウイルス感染者数",
    subtitle = "2) 感染者の居住地と人口の比率",
    caption = glue::glue("作成: Shinya Uryu (@u_ribo)
       データソース: 都道府県別新型コロナウイルス感染者数マップ（ジャッグジャパン株式会社提供）
       (CC BY-NC 4.0, https://gis.jag-japan.com/covid19jp/)\n{data_lastupdate}時点、
       人口推計 (2018年10月1日現在) 総人口
       レイアウト: カラム地図 (CC0, https://github.com/tabularmaps/hq)
                            括弧内の数値は感染者数"))
ggsave(last_plot(),
       filename = glue::glue("figures/{datetime}_prefecture_population_ratio.png",
                             datetime = stringr::str_replace(as.character(data_lastupdate), " ", "_") %>% 
                               stringr::str_replace_all(":", "")),
       width = 10,
       height = 8)
