suppressPackageStartupMessages({
  library(rtweet)
  library(dplyr, warn.conflicts = FALSE)
  library(purrr, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
})

#' search_tweets() FOR #nflverse WHERE status_id > status_from_last_run EVERY fifteen minutes
#'
#' apply cleaning:
#' a) do not retweet where more than 3 hashtags are used
#' b) do not retweet more than one message from the same author (author vatiable is missing?)

# TODO maintain a list of banned/blocked users
# TODO remove swearing and/or not-pg tweets? (censor it)

# api_key <- Sys.getenv("TWITTERAPIKEY")
# api_secret <- Sys.getenv("TWITTERAPISECRET")
# access_token <- Sys.getenv("TWITTERACCESSTOKEN")
# access_secret <- Sys.getenv("TWITTERACCESSTOKENSECRET")
# bot <- rtweet::rtweet_bot(
#   api_key = api_key,
#   api_secret = api_secret,
#   access_token = access_token,
#   access_secret = access_secret
# )
#
# rtweet::auth_as(bot)

last_id_saved <- readLines("last_id.txt")

retweet_nflverse <- function(last_id_saved){

  message(paste("Starting retweet job:",Sys.time()))

  tweets_raw <- rtweet::search_tweets(
    q = "#nflverse",
    n = 1000,
    include_rts = FALSE,
    parse = FALSE,
    since_id = last_id_saved,
    retryonratelimit = TRUE
  )

  tweets_filtered <- tweets_raw |>
    purrr::pluck(1,"statuses")

  if(is.null(tweets_filtered)) return("No tweets to retweet")

  tweets_filtered <- tweets_filtered |>
    tidyr::unpack(user,names_sep = "_") |>
    tidyr::unpack(entities) |>
    dplyr::filter(purrr::map_lgl(hashtags, ~nrow(.x) <= 3)) |>
    dplyr::select(
      created_at,
      user_id = user_id_str,
      screen_name = user_screen_name,
      status_id = id_str,
      full_text
    ) |>
    dplyr::group_by(user_id) |>
    dplyr::slice_min(order_by = status_id, n = 3) |>
    dplyr::ungroup()

  writeLines(max(tweets_filtered$status_id), "last_id.txt")

  if(nrow(tweets_filtered) == 0) return("No tweets to retweet")

  write.table(tweets_filtered,
              file = paste0("data/",Sys.Date(),".csv"),
              qmethod = "double",
              row.names = FALSE,
              append = file.exists(paste0("data/",Sys.Date(),".csv")),
              col.names = !file.exists(paste0("data/",Sys.Date(),".csv")),
              sep = ",")

  r <- purrr::map(tweets_filtered$status_id,
                  purrr::possibly(~rtweet::post_tweet(retweet_id = .x),
                                  otherwise = NA))

  message(paste(nrow(tweets_filtered), "#nflverse messages retweeted! --", Sys.time()))

  return(NULL)
}

retweet_nflverse(last_id_saved)
