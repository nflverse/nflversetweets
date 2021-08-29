library(rtweet)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

#' search_tweets() FOR #nflverse WHERE status_id > status_from_last_run EVERY fifteen minutes
#'
#' apply cleaning:
#' a) do not retweet where more than 3 hashtags are used
#' b) do not retweet more than one message from the same author (author vatiable is missing?)
#' c) remove swearing and/or not-pg tweets? (censor it)? (FUTURE)

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

last_id_saved <- readLines("last_id.txt")

retweet_nflverse <- function(last_id_saved){

  tweets_raw <- search_tweets(
    q = "#nflverse",
    n = 1000,
    include_rts = FALSE,
    since_id = last_id_saved,
    retryonratelimit = TRUE
  )

  tweets_filtered <- tweets_raw %>%
    tidyr::unnest_wider(entities) |>
    dplyr::filter(!is.na(id)) |>
    dplyr::filter(purrr::map_lgl(hashtags, ~nrow(.x) <= 3))

  writeLines(max(tweets_raw$id_str), "last_id.txt")

  if(!nrow(tweets_filtered)>0) return("No tweets to retweet")

  r <- purrr::map(tweets_filtered$id_str,~rtweet::post_tweet(retweet_id = .x))

  m <- paste(nrow(tweets_filtered)," #nflverse messages retweeted!")

  message(m)

  return(NULL)
}

retweet_nflverse(last_id_saved)
