# devtools::install_github("mkearney/rtweet")
library(rtweet)
library(tweetbotornot)
library(tidyverse)
library(readr)

# set variables
api_key <- '6vw************Mu2nR'
api_secret_key <- 'EwzoHGQxg************sVVrUynbeqowFN5'
app_name <- 'RT Disinfo'
my_access_token <- '829729224***************q7fkSXOpOrJjQGfRshGR'
my_access_secret <- 'S9ooUCHTW****************dKnb00uqvqKYvNqDj'
my_bearer_token <- "AAAAAAAAAAAAAAAAAAAAAOLMNwEAAAAAzmqm7yB7********************hxZZENPuzQEyF1Wc2CYwzWGLzTuosj7UJ6n1RFX0FYO"

# create token
token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = my_access_token,
  access_secret = my_access_secret
)

# read in twint data

# RT Tweets between 2015 and 2017
rt_tweets <- readr::read_csv("C:/Users/wtfst/OneDrive - McGill University/Documents/pythonProjects/tweepy/RT_2015-2016tweets.csv")

# RT mentions between 2015 and 2017
rt_mentions <- readr::read_csv("data_clean/RT_retweets_2015_2017.csv")

# keep only tweets/mentions about white helmets by hashtag
wh_tweets <- rt_tweets  %>% 
  filter(stringr::str_detect(hashtags, "whitehelmets"))

mentions <- rt_mentions %>% filter(str_detect(hashtags, "whitehelmets"))

# use rtweet to get retweets because this feature is not supported in twint

# retweets
wh_retweets <- list()
for(i in 1 :nrow(wh_tweets)) {
  wh_retweets[[i]] <- get_retweets(as.character(wh_tweets$id[i]))
}

# get bot probabilities for mentioners
bot_probs_mentions <- botornot(mentions$username)

write.csv(bot_probs_mentions, "./data_clean/bot_probs_mentions.csv")

bot_probs_retweets <- list()
for(i in 4:length(wh_retweets)) {
  bot_probs_retweets[[i]] <- botornot(wh_retweets[[i]]$screen_name)
}

write.csv(bot_probs_retweets[[1]], 
          paste0("./data_clean/bot_probs_retweets_", wh_retweets[[1]]$retweet_status_id[1], ".csv"))

bot_probs_retweets[[2]] <- botornot(wh_retweets[[2]]$screen_name)
write.csv(bot_probs_retweets[[2]], 
          paste0("./data_clean/bot_probs_retweets_", wh_retweets[[2]]$retweet_status_id[1], ".csv"))

# 3 is null
# bot_probs_retweets[[3]] <- botornot(wh_retweets[[4]]$screen_name)
# write.csv(bot_probs_retweets[[3]], 
#           paste0("./data_clean/bot_probs_retweets_", wh_retweets[[3]]$retweet_status_id[1], ".csv"))

write.csv(bot_probs_retweets[[4]], 
          paste0("./data_clean/bot_probs_retweets_", wh_retweets[[4]]$retweet_status_id[1], ".csv"))

write.csv(bot_probs_retweets[[5]], 
          paste0("./data_clean/bot_probs_retweets_", wh_retweets[[5]]$retweet_status_id[1], ".csv"))

# stops at 6
write.csv(bot_probs_retweets[[6]], 
          paste0("./data_clean5bot_probs_retweets_", wh_retweets[[6]]$retweet_status_id[1], ".csv"))
write.csv(bot_probs_retweets[[7]], 
          paste0("./data_clean/bot_probs_retweets_", wh_retweets[[7]]$retweet_status_id[1], ".csv"))