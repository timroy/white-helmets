# install and load packags ####
# devtools::install_github("mkearney/rtweet")
library(rtweet)
library(tweetbotornot)
library(tidyverse)
library(readr)

# set variables
# api_key <- '6vw************Mu2nR'
# api_secret_key <- 'EwzoHGQxg************sVVrUynbeqowFN5'
# app_name <- 'RT Disinfo'
# my_access_token <- '829729224***************q7fkSXOpOrJjQGfRshGR'
# my_access_secret <- 'S9ooUCHTW****************dKnb00uqvqKYvNqDj'
# my_bearer_token <- "AAAAAAAAAAAAAAAAAAAAAOLMNwEAAAAAzmqm7yB7********************hxZZENPuzQEyF1Wc2CYwzWGLzTuosj7UJ6n1RFX0FYO"

# create token ####
token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = my_access_token,
  access_secret = my_access_secret
)

# read in twint data ####

# RT Tweets between 2015 and 2017
rt_tweets <- readr::read_csv("data_clean/RT_tweets_2015_2017.csv")

# RT mentions between 2015 and 2017
rt_mentions <- readr::read_csv("data_clean/RT_mentions_2015_2017.csv")

# keep only tweets/mentions about white helmets by hashtag
wh_tweets <- rt_tweets  %>% 
  filter(stringr::str_detect(hashtags, "whitehelmets"))

mentions <- rt_mentions %>% filter(str_detect(hashtags, "whitehelmets"))

# use rtweet to get retweets because this feature is not supported in twint ####

# retweets
# wh_retweets <- list()
# for(i in 1 :nrow(wh_tweets)) {
#   wh_retweets[[i]] <- get_retweets(as.character(wh_tweets$id[i]))
#   # write.csv(wh_retweets[[i]], paste0("./data_clean/WH_retweets_", i, ".csv"))
#   save(wh_retweets, file = paste0("./data_clean/WH_retweets_", i, "_.Rdata"))
# }

load("./data_clean/WH_retweets_9_.Rdata")

wh_retweets <- wh_retweets[sapply(wh_retweets, function(x) dim(x)[1]) > 0]
names(wh_retweets) <- seq_along(wh_retweets)

# get bot probabilities for mentioners
bot_probs_mentions <- botornot(mentions$username)

write.csv(bot_probs_mentions, "./data_clean/bot_probs_mentions.csv")


# get bot probs retweets ####
# for loop causes rate limit to exceed
bot_probs_retweets <- list()
# for(i in 4:length(wh_retweets)) {
#   bot_probs_retweets[[i]] <- botornot(wh_retweets[[i]]$screen_name)
# }

# go one by one, wait 15 minutes upon reaching rate limit 
# bot_probs_retweets[[1]] <- botornot(wh_retweets[[1]]$screen_name)
# write.csv(bot_probs_retweets[[1]], 
#           paste0("./data_clean/bot_probs_retweets_", wh_retweets[[1]]$retweet_status_id[1], ".csv")) # done
# 
# bot_probs_retweets[[2]] <- botornot(wh_retweets[[2]]$screen_name)
# write.csv(bot_probs_retweets[[2]], 
#           paste0("./data_clean/bot_probs_retweets_", wh_retweets[[2]]$retweet_status_id[1], ".csv")) # done
# 
# bot_probs_retweets[[3]] <- botornot(wh_retweets[[3]]$screen_name)
# write.csv(bot_probs_retweets[[3]],
#           paste0("./data_clean/bot_probs_retweets_", wh_retweets[[3]]$retweet_status_id[1], ".csv")) # done
# 
# bot_probs_retweets[[4]] <- botornot(wh_retweets[[4]]$screen_name)
# write.csv(bot_probs_retweets[[4]], 
#           paste0("./data_clean/bot_probs_retweets_", wh_retweets[[4]]$retweet_status_id[1], ".csv")) # done
# 
# bot_probs_retweets[[5]] <- botornot(wh_retweets[[5]]$screen_name)
# write.csv(bot_probs_retweets[[5]], 
#           paste0("./data_clean/bot_probs_retweets_", wh_retweets[[5]]$retweet_status_id[1], ".csv")) # done
# 
# bot_probs_retweets[[6]] <- botornot(wh_retweets[[6]]$screen_name)
# write.csv(bot_probs_retweets[[6]], 
#           paste0("./data_clean/bot_probs_retweets_", wh_retweets[[6]]$retweet_status_id[1], ".csv")) # done
# 
# bot_probs_retweets[[7]] <- botornot(wh_retweets[[7]]$screen_name)
# write.csv(bot_probs_retweets[[7]], 
#           paste0("./data_clean/bot_probs_retweets_", wh_retweets[[7]]$retweet_status_id[1], ".csv")) # done
# 
# bot_probs_retweets[[8]] <- botornot(wh_retweets[[8]]$screen_name)
# write.csv(bot_probs_retweets[[8]], 
#           paste0("./data_clean/bot_probs_retweets_", wh_retweets[[8]]$retweet_status_id[1], ".csv")) # done

# load in bot probs retweets ####
bot_probs_retweets_all <-
  rbind(
    read_csv("data_clean/bot_probs_retweets_784491890104205312.csv") %>% mutate(id = 784491890104205312), # 8
    read_csv("data_clean/bot_probs_retweets_784574039582908416.csv") %>% mutate(id = 784574039582908416), # 7
    read_csv("data_clean/bot_probs_retweets_801122310266589184.csv") %>% mutate(id = 801122310266589184), # 6
    read_csv("data_clean/bot_probs_retweets_801285670186532864.csv") %>% mutate(id = 801285670186532864), # 5
    read_csv("data_clean/bot_probs_retweets_801532028076310528.csv") %>% mutate(id = 801532028076310528),
    read_csv("data_clean/bot_probs_retweets_801719512219484160.csv") %>% mutate(id = 801719512219484160),
    read_csv("data_clean/bot_probs_retweets_802075170534002688.csv") %>% mutate(id = 802075170534002688),
    read_csv("data_clean/bot_probs_retweets_810640795640463360.csv") %>% mutate(id = 810640795640463360)
  ) %>% 
  mutate(type = "Retweets") %>% 
  left_join(wh_tweets, by = "id") %>% 
  mutate(
    group = case_when(
      str_detect(tweet, "evidence of execution") ~ 1,
      str_detect(tweet, "Massive evidence") ~ 2,
      str_detect(tweet, "deserve an") ~ 3,
      str_detect(tweet, "judgement") ~ 4,
      str_detect(tweet, "Confusing") ~ 5,
    )
  )

duped_bots <- bot_probs_retweets_all %>% filter(duplicated(screen_name)) %>% select(screen_name)

dupes <- bot_probs_retweets_all %>% filter(screen_name %in% duped_bots$screen_name) %>% arrange(screen_name, id)

# number of bots that double-posted duplicate RT tweets ####
sum(table(dupes$screen_name, dupes$group) == 2)

nrow(duped_bots) # 10 bots retweeted twice, 1 bot retweeted 3 times


bot_probs_mentions <- read_csv("data_clean/bot_probs_mentions.csv") %>% mutate(type = "Mentions")

bot_probs_full <- rbind(bot_probs_retweets_all, bot_probs_mentions)

# Output for paper ####

# Density plot ####
density_p <- bot_probs_full %>% 
  group_by(type) %>% 
  mutate(avg = mean(prob_bot)) %>% 
  ggplot(aes(x = prob_bot, 
             color = type,
             group = type)) +
  geom_histogram(aes(y = ..density.., fill = type), alpha = 0.2, position = position_dodge2(width = 0.9)) +
  geom_density(size = 1) +
  geom_vline(aes(xintercept = avg, col = type), size = 1, lty = "dashed") +
  labs(x = "Probability of Being a Bot",
       y = "Density",
       # title = "Bot Probabilities for RT WH \"Tweeters\"",
       caption = "Probabilities estimated using tweetbotornot. Dashed lines indicate average.") +
  scale_fill_discrete(name = "Tweet Type") +
  scale_color_discrete(guide = FALSE) +
  theme_minimal() +
  theme(legend.position = c(0.15, 0.8), legend.background = element_rect());density_p
ggsave("./figs/density.png", density_p, width = 6, height = 5, device = "png")


full_retweets <- bind_rows(wh_retweets, .id = "Tweet_ID")

# Summary Statistics table ####
data.frame(
  ` ` = c(
    "N. of Tweets",
    "N. of Users",
    "Mean bot score",
    "Bot score >= 0.95",
    "Bot score >= 0.9",
    "Bot score >= 0.75",
    "Bot score >= 0.5"
  ),
  `RT Tweets` = c(
    nrow(wh_tweets), 
    NA,
    NA,
    NA,
    NA,
    NA,
    NA
  ),
  Mentions = c(
    nrow(mentions), 
    length(unique(mentions$username)),
    mean(bot_probs_mentions$prob_bot),
    sum(bot_probs_mentions$prob_bot >= 0.95) / nrow(bot_probs_mentions),
    sum(bot_probs_mentions$prob_bot >= 0.9) / nrow(bot_probs_mentions),
    sum(bot_probs_mentions$prob_bot >= 0.75) / nrow(bot_probs_mentions),
    sum(bot_probs_mentions$prob_bot >= 0.5) / nrow(bot_probs_mentions)
  ),
  Retweets =  c(
    nrow(full_retweets), 
    length(unique(full_retweets$screen_name)),
    mean(bot_probs_retweets_all$prob_bot),
    sum(bot_probs_retweets_all$prob_bot >= 0.95) / nrow(bot_probs_retweets_all),
    sum(bot_probs_retweets_all$prob_bot >= 0.9) / nrow(bot_probs_retweets_all),
    sum(bot_probs_retweets_all$prob_bot >= 0.75) / nrow(bot_probs_retweets_all),
    sum(bot_probs_retweets_all$prob_bot >= 0.5) / nrow(bot_probs_retweets_all)
  ),
  `Total Engagement` = c(
    nrow(full_retweets) + nrow(mentions),
    length(unique(full_retweets$screen_name)) + length(unique(mentions$username)),
    mean(bot_probs_full$prob_bot),
    sum(bot_probs_full$prob_bot >= 0.95) / nrow(bot_probs_full),
    sum(bot_probs_full$prob_bot >= 0.9) / nrow(bot_probs_full),
    sum(bot_probs_full$prob_bot >= 0.75) / nrow(bot_probs_full),
    sum(bot_probs_full$prob_bot >= 0.5) / nrow(bot_probs_full)
  )
) %>% stargazer::stargazer(digits = 3, 
                           #type = "text", 
                           summary = FALSE,
                           header = FALSE,
                           out = "./tables/sumstats.tex")



# get RT-WH intersection of twitter users removed by Twitter ####
hashed_df <- read_csv("./data_clean/russian_linked_tweets_csv_hashed.csv")


date1 <- as.POSIXct("2015-01-01 00:00:00") #lower bound
date2 <- as.POSIXct("2017-01-01 00:00:00") #upper bound
rng <- interval(date1, date2)   
hashed <- hashed_df[hashed_df$tweet_time %within% rng,]
colnames(hashed)
hashed %>% filter(str_detect(hashtags, "whitehelmets"))
hashed %>%  filter(str_detect(retweet_userid, "64643056"))

range(hashed$tweet_time)


# make table of tweets for appendix ####
wh_tweets %>% 
  mutate(tweet = gsub("&", "", tweet)) %>% 
  select(tweet#,
         #replies_count,
         #retweets_count, 
         #likes_count#, 
         #hashtags#, 
         #created_at
  ) %>% 
  stargazer::stargazer(summary = FALSE,
                       header = FALSE, 
                       out = "./tables/wh_tweets.tex", 
                       font.size = "scriptsize")


# get standard errors of distribution ####
std <- function(x) sd(x)/sqrt(length(x))
std(bot_probs_mentions$prob_bot)
1.96*0.09

std(bot_probs_retweets_all$prob_bot)

# plot RT tweets over time
wh_tweets <-
  wh_tweets %>% 
  mutate(
    group = case_when(
      str_detect(tweet, "evidence of execution") ~ 1,
      str_detect(tweet, "Massive evidence") ~ 2,
      str_detect(tweet, "deserve an") ~ 3,
      str_detect(tweet, "judgement") ~ 4,
      str_detect(tweet, "Confusing") ~ 5,
    )
  )
ts_tweets <- 
  wh_tweets %>% 
  ggplot(aes(x = as.Date(created_at), y = retweets_count, color = factor(group))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  labs(y = "Retweet Count", x = "Date-time in 2016", caption = "Points indicate a tweet. Matching colors indicate the same text in different tweets.") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("./figs/tweets_over_time.png", ts_tweets, "png", width = 6.5, height = 2)

39/529
1/17
unique()

bots <- filter(bot_probs_retweets_all, prob_bot >= 0.9)

39/length(unique(bots$screen_name))

unique()

t.result <- t.test(bot_probs_retweets_all$prob_bot, 
                   bot_probs_mentions$prob_bot, var.equal = TRUE)
t.tidy <- t.result %>% broom::tidy()

round(t.tidy[, c(-8, -9)], 3) %>%   stargazer::stargazer(digits = 3,
                       summary = FALSE,
                       header = FALSE,
                       title = "Joint T.Test for Bot Probs on Mentions and Retweet Samples",
                       out = "./tables/t_test.tex")
