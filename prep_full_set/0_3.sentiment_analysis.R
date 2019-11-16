library("sentimentSetsR")


rm(list=ls())
setwd("~/GitHub/samunico/Apart/data")
df_window <- read.csv("df_window_30th_dail.csv")

## Sentiment analysis of windows
corpus_window<- corpus(df_window, text_field = 'window') #first transform df to corpus
vader <- getVaderRuleBasedSentiment(df_window$window, compound=TRUE)
sentanalysis <- dfm(corpus_window, dictionary=data_dictionary_LSD2015[1:2]) #sentiment analysis
df_window <- cbind(df_window, convert(sentanalysis_30th_dail, to="data.frame")) # add sentiment analysis to df_window


## Sentiment score = (positive words - negative words)/total tokens in that window
df_window$ntoken_window <- ntoken(df_window$window) # number of tokens per window
df_window$sentiment_score <- (df_window$positive - df_window$negative)/df_window$ntoken_window # sentiment score
# Now 1 row = 1 window, with docvars + sentiment score

## add score from Proksch et al (log(count+0.5))
