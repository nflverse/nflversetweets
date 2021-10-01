Sys.setlocale("LC_ALL","English")
nflversetweets <- rtweet::search_tweets("#nflverse", n = 18000, include_rts = FALSE)

stop <- lubridate::today("America/New_York") - 1
start <- lubridate::today("America/New_York") - 7

library(ggplot2)
dummy <- data.frame(post_day = 1:7, n = 0)
last_week <- nflversetweets |>
  dplyr::filter(created_at < stop) |>
  dplyr::filter(created_at > start) |>
  dplyr::transmute(post_day = lubridate::wday(created_at, label = FALSE, week_start = 4)) |>
  dplyr::count(post_day) |>
  dplyr::bind_rows(dummy) |>
  dplyr::group_by(post_day) |>
  dplyr::slice_max(n, n = 1) |>
  dplyr::ungroup() |>
  dplyr::mutate(post_day_word = c("Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed"))

last_week |>
  ggplot2::ggplot(aes(x = forcats::fct_reorder(post_day_word, post_day), y = n)) +
  ggplot2::geom_col(width = 0.75, color = "white", fill = "#1B95E0") +
  # ggplot2::geom_label(y_nudge = 1, aes(label = n)) +
  ggplot2::labs(
    title = "Daily Number of Tweets using the <span style = 'color:#1B95E0;'>#nflverse</span> Hashtag",
    subtitle = glue::glue("Total of {sum(last_week$n)} tweets between {start} and {stop}"),
    y = "Number of Tweets",
    caption = "Data: rtweet | Plot: @mrcaseb"
  ) +
  ggplot2::scale_y_continuous(breaks = scales::breaks_pretty()) +
  # ggplot2::scale_x_discrete(breaks = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  hrbrthemes::theme_modern_rc(plot_margin = ggplot2::margin(10, 10, 10, 10), grid = "Y") +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    plot.title.position = "plot",
    plot.title = ggtext::element_markdown()
  )

ggsave("plot.png", width = 15, height = 10, units = "cm", dpi = 600)
