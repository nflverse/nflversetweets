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

api_key <- Sys.getenv("TWITTERAPIKEY")
api_secret <- Sys.getenv("TWITTERAPISECRET")
access_token <- Sys.getenv("TWITTERACCESSTOKEN")
access_secret <- Sys.getenv("TWITTERACCESSTOKENSECRET")

# TODO figure out how to auth as nflversetweets bot
# TODO remove swearing and/or not-pg tweets? (censor it)

bot <- rtweet::rtweet_bot(
  api_key = api_key,
  api_secret = api_secret,
  access_token = access_token,
  access_secret = access_secret
)

rtweet::auth_as(bot)

last_id_saved <- readLines("last_id.txt")

retweet_nflverse <- function(last_id_saved){

    message(paste("Starting retweet job:",Sys.time()))

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

  # tweets_filtered <- tweets_raw %>%
  #   filter(
  #     !is_retweet,
  #     status_id > last_id_saved,
  #     map_lgl(hashtags, ~length(.x) <= 3)
  #   ) %>%
  #   group_by(user_id) %>%
  #   slice_min(order_by = status_id, n = 3) %>%
  #   ungroup() %>%
  #   select(status_id,user_id,created_at,screen_name,text)


  writeLines(max(tweets_raw$id_str), "last_id.txt")

  if(nrow(tweets_filtered) == 0) return("No tweets to retweet")

  write.table(tweets_filtered,
              file = paste0("data/",Sys.Date(),".csv"),
              qmethod = "double",
              row.names = FALSE,
              append = file.exists(paste0("data/",Sys.Date(),".csv")),
              col.names = !file.exists(paste0("data/",Sys.Date(),".csv")),
              sep = ",")

  r <- purrr::map(tweets_filtered$id_str,~rtweet::post_tweet(retweet_id = .x))

  message(paste(nrow(tweets_filtered), "#nflverse messages retweeted!", Sys.time()))

  return(NULL)
}

retweet_nflverse(last_id_saved)
