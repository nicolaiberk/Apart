# __global vars  -----------------------------------------------------
# 1) original dataset fromat Harvard dataverse
# If you want to start from the original Harvard Dataverse dataset you'll have to download it manually first & paste it into the current file directory: https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/6MZN76/CRUNF0

DATASRC <-  3 # set to x to start from: 
#   1) [NOT WORKING] Harvard Dataverse compressed file ("Dail_debates_1919-2013.tar.gz")
#   2) Google Drive compressed file
#   3) Ready-made local dataset
UPLOAD <-  F  # Upload Entity dictionary to drive?
PLOT <- F     # Plot distribution of speeches across legislative periods


# __Loading Data -----------------------------------------------------
## 1. Data frame of Dáil Periods & speakers
daildict <- read.csv("DailPeriods.csv", encoding="UTF-8")
daildict <- daildict %>% mutate(
  begin = begin %>% as.Date(),
  end = end %>% as.Date()
)
## 2.Universe of speeches
if (DATASRC<3) {
  if (DATASRC==1) { # From Harvard Dataverse
    # These lines downlad the original data from the website [not working atm; somehow I can unpack the manually downloaded file, but not the one downloaded via R]; unpack them; and read them into the program
    
    # download [not working atm]
    # download.file(destfile = "Dail_debates_1919-2013.tar.gz", 
    #               url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/6MZN76/CRUNF0") # https://stat.ethz.ch/R-manual/R-devel/library/utils/html/download.file.html
  }
  if (DATASRC==2) { # From Google Drive
    file_id <- drive_find("Dail_debates_1919-2013.tar.gz", n_max = 10)$id
    drive_download(paste0("https://drive.google.com/open?id=", file_id), overwrite = T) 
  }
  # unpack
  untar("Dail_debates_1919-2013.tar.gz"); beep(10)
  
  # read 
  df <- fread("Dail_debates_1919-2013.tab", sep="\t", 
              quote="", header=TRUE, showProgress = TRUE, data.table=T, verbose = TRUE)
  
  # Adding legislative period variable:
  df$datef <- as.Date(df$date); beep(10) # make sure that date variable is in date format
  # ad end-date as current date for the current period
  daildict$end[daildict$dail==max(daildict$dail)] <- Sys.Date() %>% as.character()
  df$legper <- NA
  for (i in 1:nrow(daildict)){
    df <- df %>% 
      mutate(legper = ifelse(datef >= daildict$begin[i] &
                               datef <= daildict$end[i], 
                             daildict$dail[i], legper))
  }
  
  
  #2. Reading entities

entities <- read.csv("EntitiesDict.csv", header=T, stringsAsFactors = F) 
entities <- entities %$% 
  c(fullname, match, alternativematch, nicknamematch, party_name) %>% 
    unique() 
entities <- entities[!is.na(entities)]



#___ Sentiment Analysis------

## 13th leg period onward

df <- df %>% 
  filter(legper>12)

## Random sample of full dataset
df_sample <- sample_n(df, 10000)

#remove(df)
  
## Preparation
corpus <- corpus(df_sample, text_field = 'speech', docid_field = 'speechID')
tokens <- tokens(corpus, remove_punct = T)
tokens <- tokens_select(tokens, stopwords('english'), selection='remove')
system.time({kwic  <- kwic(tokens, pattern=phrase(entities), window=20, case_insensitive = F)})
df_window <- merge(df_sample, kwic, by.y="docname", by.x="speechID")

df_window$window <- paste(df_window$pre, df_window$keyword, df_window$post, sep=" ")
df_window$window_noname <- paste(df_window$pre, df_window$post, sep=" ")
corpus_window<- corpus(df_window, text_field = 'window_noname')


sent_analysis <-dfm(corpus_window, dictionary=data_dictionary_LSD2015[1:2]) 
df_window <- cbind(df_window, convert(sent_analysis, to="data.frame"))
df_window$ntoken_window <- ntoken(df_window$window_noname) 
df_window$original_lsd <- (df_window$positive - df_window$negative)/df_window$ntoken_window
df_window$unweighted_lsd <- (df_window$positive - df_window$negative)
df_window$original_log <- log((df_window$positive + 0.5)/(df_window$negative + 0.5))
df_window$weighted_log <- (log((df_window$positive + 0.5)/(df_window$negative + 0.5)))/df_window$ntoken_window

#sentimentr

sentences_sentr <- get_sentences(df_window$window_noname)
sent_r <- sentiment(sentences_sentr)
df_window$unweighted_sentr <- sent_r$sentiment
df_window$weighted_sentr <- sent_r$sentiment/df_window$ntoken_window

# ranks

df_window <- df_window %>% 
  mutate(
         percentrank_original_lsd = percent_rank(original_lsd),
         percentrank_unweighted_lsd = percent_rank(unweighted_lsd),
         percentrank_original_log = percent_rank(original_log),
         percentrank_weighted_log = percent_rank(weighted_log),
         percentrank_unweighted_sentr = percent_rank(unweighted_sentr),
         percentrank_weighted_sentr = percent_rank(weighted_sentr))



