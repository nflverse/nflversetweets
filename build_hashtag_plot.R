suppressPackageStartupMessages({
  library(rtweet)
  library(dplyr)
  library(ggplot2)
  library(showtext)
})
options(warn = 1)
# Have to set locale for time stamp parsing
Sys.setlocale("LC_TIME", "C")

cli::cli_alert_info("Manage Fonts...")
sysfonts::font_add_google("Roboto Condensed")
showtext::showtext_auto()

cli::cli_alert_info("Setup Bot...")
api_key <- Sys.getenv("TWITTERAPIKEY")
api_secret <- Sys.getenv("TWITTERAPISECRET")
access_token <- Sys.getenv("TWITTERACCESSTOKEN")
access_secret <- Sys.getenv("TWITTERACCESSTOKENSECRET")
bot <- rtweet::rtweet_bot(
  api_key = api_key,
  api_secret = api_secret,
  access_token = access_token,
  access_secret = access_secret
)

rtweet::auth_as(bot)

cli::cli_alert_info("Load and Prepare Data...")
nflversetweets <- rtweet::search_tweets(
  "#nflverse",
  n = 1000,
  include_rts = FALSE,
  retryonratelimit = TRUE
)

# latest Sunday
stop <- lubridate::today("America/New_York") - lubridate::wday(lubridate::today("America/New_York")) + 1
# last Monday
start <- lubridate::floor_date(stop, "week", 1)
all_days <- data.frame(created_at = lubridate::as_date(start:stop))

last_week <- nflversetweets |>
  dplyr::filter(!is.na(created_at)) |>
  dplyr::mutate(
    created_at = lubridate::parse_date_time(created_at, c("abdHMSzY", "YmdHMS"))
  )

# Try to catch failed date parsing because we don't need a plot that shows
# all zeros
if(all(is.na(last_week$created_at))){
  cli::cli_abort("The date parser failed. Please check it. Here are dates {.val {nflversetweets$created_at}}")
} else {
  last_week <- last_week |>
    dplyr::filter(created_at < stop) |>
    dplyr::filter(created_at > start) |>
    dplyr::bind_rows(all_days) |>
    dplyr::transmute(post_day = lubridate::wday(created_at, label = TRUE, week_start = 1)) |>
    dplyr::count(post_day) |>
    dplyr::mutate(n = n - 1L)

  cli::cli_alert_info("Build Plot...")
  plot <- last_week |>
    ggplot2::ggplot(aes(x = post_day, y = n)) +
    ggplot2::geom_col(width = 0.75, color = "white", fill = "#1B95E0") +
    ggplot2::geom_text(aes(label = paste0(n, "\n")), vjust = 0, color = "white", fontface = "bold", lineheight = ggplot2::unit(0.2, "lines")) +
    ggplot2::labs(
      title = "Daily Number of Tweets using the <span style = 'color:#1B95E0;'>#nflverse</span> Hashtag",
      subtitle = glue::glue("Total of {sum(last_week$n)} Tweets Between {format(start, '%b %d, %Y')} and {format(stop, '%b %d, %Y')}"),
      y = "Number of Tweets",
      caption = "Data: rtweet | Plot: @mrcaseb"
    ) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty()) +
    hrbrthemes::theme_modern_rc(
      plot_margin = ggplot2::margin(10, 10, 10, 10),
      grid = "Y"
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.title = ggtext::element_markdown()
    )

  cli::cli_alert_info("Save Plot...")
  ggplot2::ggsave("plot.png", width = 17, height = 12, units = "cm", dpi = 600)

  cli::cli_alert_info("Post Plot...")
  rtweet::post_tweet(
    status = "Number of nflverse tweets over the last week, posted through #rstats courtesy of #rtweet!",
    media = "plot.png",
    media_alt_text = "Number of tweets using the #nflverse hashtag."
  )

  cli::cli_alert_info("DONE!")
}

