####################################
# ヤフー・データソリューション
# 東京23区滞在人口推計値の日別遷移（全体・来訪者・住人）
# https://ds.yahoo.co.jp/report/
# 2020年6月30日をもって公開終了
####################################
# library(readxl)
# library(dplyr)
# library(tidyr)
# library(purrr)
# library(lubridate)
# library(ggplot2)
# library(patchwork)
# library(jmastats)
# library(jpcovid19)
# library(rvest)
# library(ensurer)
# library(assertr)
# data("tky23", package = "tabularmaps")
# 
# dl_file_url <- 
#   read_html("https://ds.yahoo.co.jp/report/") %>% 
#   html_nodes(css = "body > main > main > section.df-MainContent > div > div.df-MainContent__inner > a") %>% 
#   html_attr("href")
# 
# dl_file <- 
#   glue::glue("data-raw/{file}",
#              file = URLdecode(basename(dl_file_url)))
# download.file(dl_file_url,
#               destfile = dl_file)
# 
# gg_plot_cal <- function(data, var, text_size = 2, text_color = "black", ...) {
#   var <- rlang::enquo(var)
#   data %>% 
#     ggplot(aes(x = wkdy, y = wkn_mo, color = !!var)) +
#     geom_point(...) +
#     geom_point(shape = 1, ..., colour = "black") +
#     geom_text(aes(label = day), 
#               family = "IPAexGothic", 
#               size = text_size,
#               color = text_color) +
#     scale_y_reverse(breaks = NULL, expand = expansion(add = 1)) +
#     xlab(NULL) +
#     ylab(NULL)
# }
# 
# df_long <- 
#   read_yds_tky23(dl_file, sheet = NULL, long = TRUE) %>% 
#   rename(area = `エリア`,
#          scope = `対象分類`)
# invisible(
#   df_long %>% 
#     pull(area) %>% 
#     unique() %>% 
#     ensure(length(.) == 24L)
# )
# invisible(
#   df_long %>% 
#     pull(scope) %>% 
#     unique() %>% 
#     ensure(length(.) == 3L)
# )
# 
# df_days <-
#   df_long %>% 
#   distinct(date) %>%
#   mutate(isoweek = isoweek(date),
#          is_holiday = if_else(wday(date) %in% c(1, 7),
#                               TRUE,
#                               zipangu::is_jholiday(date)),
#          wkdy = wday(date, label = TRUE, week_start = 7),
#          day = day(date),
#          wkn_mo = ceiling(day(date) / 7))
# 
# df_days <- 
#   df_days %>%
#   mutate(month = month(date)) %>% 
#   group_by(month) %>% 
#   mutate(wkn_mo = isoweek - min(isoweek),
#          wkn_mo = if_else(wkdy == "日", wkn_mo + 1, wkn_mo)) %>% 
#   ungroup()
# 
# df_long <- 
#   df_long %>% 
#   left_join(df_days, by = "date")
# 
# df_long %>% 
#   count(date, sort = TRUE) %>% 
#   filter(n != 72)
# 
# df_visitors <- 
#   df_long %>% 
#   filter(scope == "来訪者") %>% 
#   arrange(isoweek, wkdy, wkn_mo) %>% 
#   group_by(area, wkdy) %>% 
#   mutate(diff = visitors - lag(visitors),
#          comp_lw_pct = (visitors / lag(visitors) - 1) * 100) %>% 
#   ungroup() %>% 
#   arrange(area, date) %>% 
#   select(-scope) %>% 
#   mutate(area = forcats::fct_relevel(area,
#                                      tky23 %>% 
#                                        arrange(desc(y), x) %>% 
#                                        pull(ward_kanji) %>% 
#                                        unique())) %>% 
#   mutate_at(vars(diff, comp_lw_pct),
#             list(~ replace_na(., 0)))
# 
# df_visitors %>% 
#   filter(area == "千代田区") %>% 
#   select(date, visitors, diff, comp_lw_pct) %>% 
#   ggplot(aes(date, comp_lw_pct)) +
#   geom_point()
# 
# gg_plot_visitor_cal <- function(data, var, year, month) {
#   var <- rlang::enquo(var)
#   data %>% 
#     gg_plot_cal(!!var, alpha = 0.8, size = 2, text_size = 0.8) +
#     #rcartocolor::scale_color_carto_c(name = "(%)", type = "diverging", palette = "TealRose") +
#     scale_color_gradient2(name = "(%)", 
#                           high = scales::muted("red"), 
#                           low = scales::muted("blue"), 
#                           midpoint = 0) +
#     facet_wrap(~ area) + 
#     labs(title = glue::glue("東京23区 訪問者の推移 ({year}年{month}月)"),
#          subtitle = "同一区内、先週の値（同曜日との比較）に対する割合") +
#     theme(plot.margin = margin(5, 10, 20, 10))
# }
# 
# p1 <- 
#   df_visitors %>%
#   filter(month == 2, area != "東京23区全体") %>% 
#   gg_plot_visitor_cal(comp_lw_pct, 2020, 2) +
#   labs(title = "2月",
#        subtitle = NULL) +
#   theme_gray(base_size = 6, base_family = "IPAexGothic") +
#   theme(plot.title = element_text(size = 5),
#         plot.caption = element_text(size = 3)) +
#   guides(color = FALSE)
# p2 <- 
#   df_visitors %>%
#   filter(month == 3, area != "東京23区全体") %>% 
#   gg_plot_visitor_cal(comp_lw_pct, 2020, 3) +
#   labs(title = "3月",
#        subtitle = NULL) +
#   theme_gray(base_size = 6, base_family = "IPAexGothic") +
#   theme(plot.title = element_text(size = 5),
#         plot.caption = element_text(size = 3))
# p1 + p2 +
#   plot_layout(guides = "collect", ncol = 1) +
#   plot_annotation(subtitle = "同一区内、先週の値（同曜日との比較）に対する来訪者の割合",
#                   caption = "作成: Shinya Uryu (@u_ribo)\nデータ: ヤフー・データソリューション\n丸の中の数値は日付")
# ggsave(last_plot(),
#        filename = "figures/tokyo23wards_visitor_calendar.png",
#        width = 5,
#        height = 7,
#        dpi = 300)
# 
# # weather -----------------------------------------------------------------
# stations %>% 
#   filter(area == "東京", station_type == "官", block_no == "47662") %>% 
#   pull(station_no) %>% 
#   unique()
# 
# df_tky_weather <- 
#   seq.int(2, 3) %>% 
#   purrr::map_dfr(
#     ~ jma_collect(item = "daily", block_no = "47662", year = 2020, month = .x)
#   ) %>% 
#   select(date, 
#          `pressure_average(hPa)`, `precipitation_sum(mm)`, 
#          starts_with("temperature"), 
#          `sunshine_duration_(h)`) %>% 
#   jmastats:::convert_variable_unit() %>% 
#   left_join(df_days, by = "date") %>% 
#   mutate(temperature_average = as.numeric(temperature_average)) %>% 
#   mutate_at(vars(starts_with("precipitation")),
#             as.numeric) %>% 
#   mutate(precipitation_sum_c = case_when(
#     between(precipitation_sum, 0, 1) ~ "0〜1",
#     between(precipitation_sum, 1, 5) ~ "1〜5",
#     between(precipitation_sum, 5, 10) ~ "5〜10",
#     between(precipitation_sum, 10, 20) ~ "10〜20",
#     between(precipitation_sum, 20, 30) ~ "20〜30",
#     between(precipitation_sum, 30, 50) ~ "30〜50",
#     between(precipitation_sum, 50, 80) ~ "50〜80",
#     precipitation_sum > 80 ~ "80以上",
#   ))
# 
# gg_plot_cal_weather <- function(data, var, year, month, source_url) {
#   var <- rlang::enquo(var)
#   data %>% 
#     gg_plot_cal(!!var, alpha = 0.7, size = 2.6, text_size = 1.4) +
#     scale_color_manual(values = jmastats:::jma_pal(palette = "absolute", .attribute = TRUE) %>% 
#                          purrr::pluck("colors") %>% 
#                          rev()) +
#     theme_void(base_family = "IPAexGothic") +
#     labs(title = glue::glue("{year}年{month}月の降水量"),
#          subtitle = "地点「東京」の値",
#          caption = glue::glue("URL: {source_url}\n丸の中の数値は日付")) +
#     theme(plot.caption = element_text(size = 1))
# }
# 
# p1 <-
#   df_tky_weather %>% 
#   filter(month(date) == 2) %>%
#   gg_plot_cal_weather(precipitation_sum_c, 
#                       year = 2020, 
#                       month = 2,
#                       source_url = "http://www.data.jma.go.jp/obd/stats/etrn/view/daily_s1.php?prec_no=44&block_no=47662&year=2020&month=2&day=&view=") +
#   guides(color = FALSE) +
#   labs(title = "2月",
#        subtitle = NULL) +
#   theme_void(base_size = 6, base_family = "IPAexGothic") +
#   theme(plot.caption = element_text(size = 3))
# p2 <-
#   df_tky_weather %>% 
#   filter(month(date) == 3) %>%
#   gg_plot_cal_weather(precipitation_sum_c, 
#                       year = 2020, 
#                       month = 3,
#                       source_url = "http://www.data.jma.go.jp/obd/stats/etrn/view/daily_s1.php?prec_no=44&block_no=47662&year=2020&month=3&day=&view=") +
#     guides(color = guide_legend(title = "降水量(mm)合計",
#                                 reverse = TRUE)) +
#   labs(title = "3月",
#        subtitle = NULL) +
#   theme_void(base_size = 6, base_family = "IPAexGothic") +
#   theme(plot.caption = element_text(size = 3))
# p1 + p2 +
#   plot_layout(guides = "collect", ncol = 1) +
#   plot_annotation(title = "気象庁アメダス 観測データ",
#                   subtitle = "地点「東京」の値")
# ggsave(last_plot(),
#        filename = "figures/tokyo_weather_calendar202002-202003.png",
#        width = 4,
#        height = 4,
#        dpi = 300)
