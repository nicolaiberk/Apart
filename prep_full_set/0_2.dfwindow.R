"
Construct df window
"

## setup
rm(list=ls())
setwd("~/GitHub/samunico/Apart/data")
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
pkgs <- c('beepr', 'tidyverse', 'rio', 'tidylog', 'skimr',
          'quanteda', 'readtext', 'Hmisc',
          'googledrive', 'readtext', 'data.table', 'stringr', 'qdap'); for (i in pkgs){usePackage(i)}




# __Loading Data -----------------------------------------------------
# Download again vs. subsample file already existing?
# Get Data from: 
# 1) Website 
# 2) Drive
# 3) local
downl <- 3

# subset (if T: sample 1000 cases)
subset <- T

if (downl == 1){
  # These lines downlad the original data from the website; unpack them; and read them into the program
  # Following line not yet working, somehow I can unpack the manually downloaded file, but not the one downloaded via R
  download.file(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/6MZN76/CRUNF0",
                destfile = "Dail_debates_1919-2013.tar.gz") # https://stat.ethz.ch/R-manual/R-devel/library/utils/html/download.file.html
  # unpacking the file
  untar(tarfile = "Dail_debates_1919-2013.tar.gz")

} else if (downl==2) {
  fn <- "~/Internship AffPol in Text/Data/Ireland/Dail_debates_1919-2013.tar.gz"
  drive_get(fn)
  drive_download(file = fn, overwrite = T)
  
  ## unzip
  untar("Dail_debates_1919-2013.tar.gz")

} else if (downl==3) {
  
}

## read data in
dfs <- fread("Dail_debates_1919-2013.tab", 
             sep="\t", 
             quote="", 
             header=TRUE, 
             showProgress = TRUE, 
             data.table=FALSE, 
             verbose = TRUE)

dfs$datef <- as.Date(dfs$date) # making sure that date variable is in date format


if (subset == T){
  dfs <- dfs[sample(nrow(dfs), 1000), ]
}


# 2. Corpus preparation ----------------------------------------------------------
corpus <- corpus(dfs, text_field = "speech")

## Tokenize corpus, and remove stop punctuation and stopwords
tokens <- tokens(corpus, remove_punct = T)
tokens <- tokens_select(tokens, stopwords('english'), selection='remove')


# 3. Entity Detection --------------------

## update data
if (downl > 1){
  fn <- "~/Internship AffPol in Text/Data/Ireland/entities_Dail.csv"
  drive_get(fn)
  drive_download(file = fn, overwrite = T)
}
## Cleaning dictionary
entities <- read_csv("entities_30th_Dail.csv")
entities <- as.vector(entities_30th_dail[[2]])
entities <- entities_30th_dail[!is.na(entities_30th_dail)]
entities <- rm_stopwords(entities_30th_dail, Top25Words, separate = F, strip=T) # remove stop words and punctuation
entities <- tools::toTitleCase(entities_30th_dail) # capitalise all words
entities <- gsub("(O'.)","\\U\\1",entities_30th_dail,perl=TRUE) # capitalise names starting with O'

## Creating windows (match of entities)
kwic <- kwic(tokens, pattern=phrase(entities), window=20, case_insensitive = F)



# Analyses ----------------------------------------------------------

#####################################################################
### PROBLEMS here, I think dfs != df in SentAnalysis_30Dail
### also make only df_window here, SentimentAnalysis in different file 
#####################################################################


## Create df_window where 1 row = 1 window preserving original docvars
df_window <- merge(dfs, kwic, by.y="docname", by.x="doc_id")
df_window <- paste(dfs$pre, df_window_30th_dail$keyword, df_window_30th_dail$post, sep=" ")


## Sentiment analysis of windows
corpus_window_30th_dail<- corpus(df_window_30th_dail, text_field = 'window') #first transform df to corpus
sentanalysis_30th_dail <- dfm(corpus_window_30th_dail, dictionary=data_dictionary_LSD2015[1:2]) #sentiment analysis
df_window_30th_dail <- cbind(df_window_30th_dail, convert(sentanalysis_30th_dail, to="data.frame")) # add sentiment analysis to df_window


## Sentiment score = (positive words - negative words)/total tokens in that window
df_window_30th_dail$ntoken_window <- ntoken(df_window_30th_dail$window) # number of tokens per window
df_window_30th_dail$sentiment_score <- (df_window_30th_dail$positive - df_window_30th_dail$negative)/df_window_30th_dail$ntoken_window # sentiment score
# Now 1 row = 1 window, with docvars + sentiment score


## Write df_window to .csv and upload it to Drive

write.csv(df_window_30th_dail, "df_window_30th_dail.csv")
drive_upload("df_window_30th_dail.csv",
             path="~/Internship AffPol in Text/Data/Ireland/",
             overwrite = T)
