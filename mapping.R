####################################
# COVID-19 日本国内の感染者マッピング
# データ: 都道府県別新型コロナウイルス感染者数マップ（ジャッグジャパン株式会社提供）
# (Creative Commons BY-NC 国際4.0)
# https://gis.jag-japan.com/covid19jp/
# 項目について https://jag-japan.com/covid19map-readme/
####################################
if (!requireNamespace("tabularmaps", quietly = TRUE)) {
  renv::install("uribo/tabularmaps")
}
library(dplyr)
library(assertr)
library(ensurer)
library(tabularmaps)
library(ggplot2)
library(patchwork)
library(readxl)
library(drake)
library(nord)
plot_font <-
  dplyr::if_else(grepl("apple",
                                     sessionInfo()$platform),
                               "IPAexGothic",
                               "IPAPGothic")
download.file(
  "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031807141&fileKind=0",
  destfile = "data-raw/2018h30_a00400.xls")
plan_data <- 
  drake::drake_plan(
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
    assertr::verify(ncol(.) == 51),
  df =
    df_raw %>% 
    # 予備項目
    select(-num_range("Field", seq.int(2, 10)),
           -contains("累計"),
           -contains("前日比"),
           -contains("正誤確認用"),
           -contains("Pref"),
           -starts_with("ソース"),
           # -ends_with("都道府県コード"),
           -Release,
           -Gender,
           -`キー`,
           -`確定日YYYYMMDD`,
           -`国内`,
           -`発表`,
           -`居住管内`,
           -`都道府県内症例番号`,
           -`人数`,
           -`退院数`,
           -`発症数`,
           -`PCR検査実施人数`,
           -`無症状病原体保有者`,
           -`死者合計`) %>%
    filter(X != "0") %>% 
    filter(!is.na(`居住都道府県コード`)) %>% 
    filter(`居住都道府県コード` != "0") %>% 
    mutate_at(vars(`確定日`, `発症日`),
              lubridate::mdy) %>% 
    rename(no = `通し`,
           mhlw_no = `厚労省NO`,
           charter = `チャーター便`,
           ages = `年代`,
           gender = `性別`,
           date = `確定日`,
           resident_pref = `居住都道府県`,
           resident_city = `居住市区町村`,
           status = `ステータス`,
           notes = `備考`,
           jis_code = `居住都道府県コード`,
           last_update = `更新日時`),
  df_pref_ratio = 
    df_pop_201810 %>% 
    select(jis_code, prefecture_kanji, total_both_sexes) %>% 
    mutate(total_both_sexes = total_both_sexes * 1000) %>% 
    left_join(
      df %>% 
        count(resident_pref, `jis_code`),
      by = c("jis_code" = "jis_code",
             "prefecture_kanji" = "resident_pref")) %>% 
    mutate(ratio = (n / total_both_sexes) * 100) %>% 
    verify(dim(.) == c(47, 5)),
  df_plot =
    jpn77 %>% 
    left_join(df_pref_ratio,
              by = c("jis_code" = "jis_code",
                     "prefecture_kanji" = "prefecture_kanji")) %>% 
    mutate(n = tidyr::replace_na(n, 0)) %>% 
    verify(dim(.) == c(47, 11)) %>% 
    mutate(label = glue::glue("{prefecture_kanji}\n({n})")),
  data_lastupdate = 
    lubridate::mdy_hm(c(na.omit(unique(df$last_update))), 
                      tz = "Asia/Tokyo"),
  # 累積であることの注意
  data_period = 
    range(df$date) %>% 
    purrr::map_chr(
      ~ format(.x, "%Y年%m月%d日")) %>% 
    paste(collapse = "から"),
  path2prefecture_population_ratio = 
    glue::glue("figures/{datetime}_prefecture_population_ratio.png",
               datetime = stringr::str_replace(as.character(data_lastupdate), " ", "_") %>% 
                 stringr::str_replace_all(":", "")))
drake::make(plan_data)
drake::loadd(list = c("df_raw", "df", "df_pref_ratio", "df_plot", 
                      "data_lastupdate", "data_period",
                      "path2prefecture_population_ratio"))
plot_tabular_covid19 <- function(data, type, ...) {
  p <-
    data %>%
    tabularmaps::tabularmap(
      fill = !!rlang::enquo(type),
      label = label,
      family = plot_font,
      color = "white",
      size = 2
    ) +
    tabularmaps::theme_tabularmap(base_family = plot_font) +
    ggplot2::theme(plot.caption = element_text(size = 6)) +
    nord::scale_fill_nord("halifax_harbor",
                          discrete = FALSE,
                          ...)
  p
}
plot_bar_covid19 <- function(data, vars, ...) {
  vars <- 
    rlang::enquo(vars)
  data %>% 
    dplyr::filter(n > 0) %>% 
    dplyr::arrange(desc(ratio)) %>% 
    ggplot2::ggplot(aes(forcats::fct_reorder(prefecture_kanji, !!vars), !!vars)) +
    ggplot2::geom_bar(stat = "identity", aes(fill = !!vars)) +
    nord::scale_fill_nord("halifax_harbor", 
                    discrete = FALSE,
                    ...) +
    ggplot2::coord_flip() +
    ggplot2::theme_gray(base_family = plot_font, base_size = 8) +
    ggplot2::xlab(NULL)
}

p1_a <- 
  plot_tabular_covid19(df_plot, n, name = "人数") +
  guides(fill = FALSE)
p2_a <- 
  plot_tabular_covid19(df_plot, ratio, name = "人口比率(%)") +
  guides(fill = FALSE)
p1_b <- 
  plot_bar_covid19(df_plot, n, name = "感染者数")
p2_b <-
  plot_bar_covid19(df_plot, ratio, name = "人口比率(%)")

plot_caps <- 
  list(
    title = "都道府県別 コロナウイルス感染者数(累計)",
    caption = glue::glue("作成: Shinya Uryu (@u_ribo)
       データソース: 都道府県別新型コロナウイルス感染者数マップ（ジャッグジャパン株式会社提供）
       (CC BY-NC 4.0, https://gis.jag-japan.com/covid19jp/)\n{data_lastupdate}時点
       レイアウト: カラム地図 (CC0, https://github.com/tabularmaps/hq)
                            括弧内の数値は感染者数
                         {data_period}のデータに基づく値"))

p1_a + p1_b +
  plot_layout(ncol = 2) +
  plot_annotation(
    theme = theme(text = element_text(family = plot_font),
                  plot.caption = element_text(size = 6)),
    title = plot_caps$title,
    subtitle = "1) 感染者の居住地",
    caption = plot_caps$caption)
ggsave(last_plot(),
       filename = glue::glue("figures/{datetime}_prefecture_count.png",
                             datetime = stringr::str_replace(as.character(data_lastupdate), " ", "_") %>% 
                               stringr::str_replace_all(":", "")),
       width = 10,
       height = 8)

p2_a + p2_b +
  plot_layout(ncol = 2) +
  plot_annotation(
    theme = theme(text = element_text(family = plot_font),
                  plot.caption = element_text(size = 6)),
    title = plot_caps$title,
    subtitle = "2) 感染者の居住地と人口の比率",
    caption = plot_caps$caption)
ggsave(last_plot(),
       filename = path2prefecture_population_ratio,
       width = 10,
       height = 8)
file.copy(path2prefecture_population_ratio,
          "figures/latest_prefecture_population_ratio.png", 
          overwrite = TRUE)
