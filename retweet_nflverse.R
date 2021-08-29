library(rtweet)
library(dplyr)
library(purrr)

#' search_tweets() FOR #nflverse WHERE status_id > status_from_last_run EVERY fifteen minutes
#'
#' apply cleaning:
#' a) do not retweet where more than 3 hashtags are used
#' b) do not retweet more than one message from the same author
#' c) remove swearing and/or not-pg tweets? (censor it)? (FUTURE)

# TODO figure out how to auth as nflversetweets bot
# TODO GHA

twitter_token <- rtweet::create_token(
  app = "Tan-DataCollection",
  consumer_key = Sys.getenv("TWITTERAPIKEY"),
  consumer_secret = Sys.getenv("TWITTERAPISECRET"),
  access_token = Sys.getenv("TWITTERACCESSTOKEN"),
  access_secret = Sys.getenv("TWITTERACCESSTOKENSECRET")
)

last_id_saved <- readLines("last_id.txt")

retweet_nflverse <- function(last_id_saved){

  tweets_raw <- search_tweets(q = "#nflverse", n = 18000)

  tweets_filtered <- tweets_raw %>%
    filter(!is_retweet,
           as.numeric(status_id) > as.numeric(last_id_saved),
           map_lgl(hashtags, ~length(.x) <= 3)
    ) %>%
    group_by(user_id) %>%
    slice_min(order_by = status_id, n = 3) %>%
    ungroup()

  writeLines(max(tweets_raw$status_id), "last_id.txt")

  if(nrow(tweets_filtered)>0) return("No tweets to retweet")

  r <- map(x,~post_tweet(retweet_id = .x))

  m <- paste(nrow(tweets_filtered)," #nflverse messages retweeted!")

  message(m)

  return(NULL)
}

retweet_nflverse(last_id_saved)
