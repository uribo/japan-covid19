library(tidyverse)
library(lubridate)

df_kinkyu_all <-
  read_csv("data-raw/緊急事態宣言の発令.csv", col_types = "ccDD")
df_manbou_all <-
  read_csv("data-raw/まん延防止等重点措置の発令.csv", col_types = "cccDD")

seq_sengen_days <- function(area, source) {
  if (source == "緊急事態宣言") {
    df <- 
      df_kinkyu_all %>% 
      dplyr::distinct(area, date_from, date_to) %>% 
      dplyr::filter(area == {{ area }})    
  } else if (source == "まん延防止等重点措置") {
    df <- 
      df_manbou_all %>% 
      dplyr::distinct(prefecture, date_from, date_to) %>% 
      dplyr::filter(prefecture == {{ area }})
  }
  df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(days = list(seq(date_from, date_to, by = 1))) %>% 
    dplyr::ungroup() %>% 
    tidyr::unnest(cols = days) %>% 
    dplyr::pull(days)
}

seq_sengen_days("東京都", source = "緊急事態宣言")
seq_sengen_days("東京都", source = "まん延防止等重点措置")

make_covid_emergency_df <- function(prefecture) {
  df_full_days <- tibble(
    date = seq(ymd("2021-01-01"), ymd("2021-09-30"), by = 1)
  )
  tibble::tibble(
    state = "緊急事態宣言",
    date = seq_sengen_days({{ prefecture }}, source = "緊急事態宣言")) %>% 
    dplyr::bind_rows(
      tibble::tibble(
        state = "まん延防止等重点措置",
        date = seq_sengen_days({{ prefecture }}, source = "まん延防止等重点措置")
      )
    ) %>% 
    dplyr::arrange(date) %>% 
    dplyr::right_join(df_full_days, by = "date") %>% 
    arrange(date) %>% 
    mutate(day = yday(date),
           state = if_else(is.na(state),
                           "発令なし",
                           state))
}

# make_covid_emergency_df("福岡県")

df_tokyo <- 
  make_covid_emergency_df("東京都") %>% 
  filter(year(date) == 2021L)

# df_tokyo %>% 
#   filter(year(date) == 2021L) %>% 
#   mutate(week = isoweek(date),
#          month = month(date)) %>% 
#   distinct(week, month) %>% 
#   count(month)

# df_tokyo %>% 
#   filter(year(date) == 2021L) %>% 
#   mutate(week = isoweek(date),
#          month = month(date)) %>% 
#   distinct(week, month) %>% 
#   mutate(labels = c("1月",
#                     rep("", 4),
#                     "2月",
#                     rep("", 3),
#                     "3月",
#                     rep("", 4),
#                     "4月",
#                     rep("", 4),
#                     "5月",
#                     rep("", 5),
#                     "6月",
#                     rep("", 4),
#                     "7月",
#                     rep("", 4),
#                     "8月",
#                     rep("", 5),
#                     "9月",
#                     rep("", 4)))
#   View()

theme_set(theme(text = element_text(family = "Hiragino Sans")))


# 東京都 ---------------------------------------------------------------------
plot_emergency_calendar <- function(data, ...) {
  data %>% 
    filter(year(date) == 2021L) %>% 
    mutate(state = if_else(date > today(),
                           paste0(state, "(予定)"),
                           state)) %>% 
    mutate(week = isoweek(date)) %>% 
    mutate(week = fct_inorder(as.character(week)),
           wday = fct_rev(wday(date, week_start = 1, label = TRUE, locale = "ja_JP.UTF-8"))) %>% 
  ggplot(aes(week, wday, fill = state)) + 
    geom_tile(color = "white", size = 0.1) + 
    theme_tufte(...) + 
    scale_x_discrete("週番号", labels = c("", "1",
                                       rep("", 9),
                                       "10",
                                       rep("", 9),
                                       "20",
                                       rep("", 9),
                                       "30",
                                       rep("", 7),
                                       "39")) +
    coord_equal() + 
    scale_fill_manual(values = c("発令なし" = "gray",
                                 "まん延防止等重点措置" = "#FFA500",
                                 "まん延防止等重点措置(予定)" = "#FFA50070",
                                 "緊急事態宣言" = "#FF0000",
                                 "緊急事態宣言(予定)" = "#FF000070"))
}
p_out <- 
  df_tokyo %>% 
  plot_emergency_calendar(base_family = "Hiragino Sans") +
  labs(x = NULL, 
       y = NULL, 
       title = "2021年 緊急事態宣言及びまん延防止等重点措置の発令状況",
       subtitle = "東京都",
       caption = "2021年9月18日時点") +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 5),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 5, colour = "gray60"),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, "lines"),
        legend.position = "bottom",
        legend.text = element_text(size = 6))

ggsave(here::here("figures/state_of_emergency_in_tokyo2021.png"),
       p_out,
       bg = "white",
       width = 8,
       height = 4,
       dpi = 320)


# 地方都市 --------------------------------------------------------------------
prefs <- c("北海道", "宮城県", "神奈川県", "愛知県", "大阪府", "広島県", "福岡県", "沖縄県")
df_tgt_prefs <- 
  prefs %>% 
  purrr::set_names(prefs) %>% 
  purrr::map_dfr(make_covid_emergency_df, .id = "prefecture") %>% 
  filter(year(date) == 2021L) %>% 
  right_join(expand_grid(prefecture =  prefs,
                         date = seq(ymd("2021-01-01"), ymd("2021-09-30"), by = 1)),
             by = c("prefecture", "date")) %>% 
  arrange(date) %>% 
  mutate(day = yday(date),
         state = if_else(is.na(state),
                         "発令なし",
                         state)) %>% 
  mutate(prefecture = fct_inorder(prefecture))

p_out <- 
  df_tgt_prefs %>% 
  plot_emergency_calendar(base_family = "Hiragino Sans") +
  labs(x = NULL, 
       y = NULL, 
       title = "2021年 緊急事態宣言及びまん延防止等重点措置の発令状況",
       caption = "2021年9月18日時点") +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 5),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 4, colour = "gray60"),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "lines"),
        legend.position = "bottom",
        legend.text = element_text(size = 6)) +
  facet_wrap(~ prefecture, ncol = 3)

ggsave(here::here("figures/state_of_emergency_2021.png"),
       p_out,
       bg = "white",
       width = 8,
       height = 6,
       dpi = 320)
