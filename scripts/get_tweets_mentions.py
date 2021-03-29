import nest_asyncio
import twint
import pandas as pd

nest_asyncio.apply()

# Configure
c = twint.Config()
# c.Username = "RT_com" # search profile of RT_com
# c.Limit = 1000 # turn off limit because we can
# c.Search = "whitehelmets" # search whitehelmets in text of Tweet
c.To = "RT_com" # search for mentions to RT_com
c.Since = '2015-01-01'
c.Until = '2017-01-01'
c.Store_json = True
c.Output = 'tweets.json'

# Run
twint.run.Search(c)

df = pd.read_json('tweets.json', lines = True)

df.to_csv("tweets.csv")
