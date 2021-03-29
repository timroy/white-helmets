# white-helmets

## Repo for RT-Disinfo white-helmets 
### Author: Tim Roy

### scripts 
#### 1. run `get_tweets_mentions.py`
* get all Tweets by RT_com between 2015 and 2017 by uncommenting the line starting with `c.Username` 
* comment it back out after and remove the .csv and .txt files from the folder
* get all mentions to RT_com between 2015 and 2017 by uncommenting the line starting with `c.To`
#### 2. run `get_bot_probs.R`
* set your API keys
* load in the python data, make sure you rename it correctly. By default it is `tweets.csv`.
* BotOrNot might exceed its rate limit for retweeters, wait 15 minutes and move on to the next set of retweeters


 
