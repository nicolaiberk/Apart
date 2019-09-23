###### Sentiment Analysis of 31st Dáil ########

library(quanteda)
library(readtext)
library(Hmisc)


## Import dataset, subset for only recent 31st legislature, create corpus

df <- readtext("dail_debates.tab", text_field="speech") # import dataset
df$date <- as.Date(df$date, format = "%Y-%m-%d") # specify dates 
df.small <- df[df$date > as.Date("2011-03-08"),] # only speeches in most recent legislature
corp_dail <- corpus(df.small) # construct quanteda corpus from dataframe

#### Sentiment Analysis of speeches that refer to the Government ####

## First subset corpus - only speeches by non-government(party) MPs

govt.parties <- c("Fine Gael", "The Labour Party") ## these two parties were in govt
corp_opposition <- corpus_subset(corp_dail, party_name %nin% govt.parties)

## Tokenize corpus, and remove stop punctuation and stopwords
opp.tokens <- tokens(corp_opposition, remove_punct = T)
opp.tokens <- tokens_select(opp.tokens, stopwords('english'), selection='remove')

## Targeted sentiment analysis by mentions of ministries, party govts and govt (super primitive)

govt <- c("[Gg]overnment", "Fine Gael", "Labour", "Taoiseach" "[Mm]inist*")
against_govt_tokens <- tokens_keep(opp.tokens, pattern=phrase(govt), window=20) ## 'window' of 20 tokens accompanying mention

## Transform targeted tokens to dfm, apply quanteda dictionary and aggregate results by party
dfm_opp_lsd <- dfm(against_govt_tokens, dictionary=data_dictionary_LSD2015[1:2]) %>%
  dfm_group(group='party_name', fill=T) 

## Total number of tokens by party (needed to calculcate polarity ratio)

n_party <- ntoken(dfm(against_govt_tokens, group=docvars(against_govt_tokens, 'party_name')))

## Transforming it all to dataframe

df.sentiment <- convert(dfm_opp_lsd, to="data.frame")
df.sentiment <- cbind(df.sentiment, n_party) ## add number of tokens for each party
df.sentiment$score <- (df.sentiment$positive - df.sentiment$negative)/n_party ## sentiment score
df.sentiment <- df.sentiment[df.sentiment$document != "Progressive Democrats",]  ## too few tokens

## Visualising sentiment by party through a dot plot

ggplot(df.sentiment, aes(x=score, y=reorder(document, score)))+
  geom_point(aes(size=n_party)) +
  theme_bw() +
  labs(size = "Number of tokens (words)") +  ## edit legend title
  scale_x_continuous("Sentiment Score") +
  scale_y_discrete("Parties") +
  ggtitle(label = "Sentiment Score by Party in 31st Dáil (until 2013)",
          subtitle = "Sentiment score for opposition parties whenever they refer to the Government")
