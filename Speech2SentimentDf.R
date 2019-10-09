# Title: Speech 2 Sentiment Df
# Context: APART
# Author: J. Areal, P. Mendoza
# Date: Fri Oct 04 12:06:30 2019
# Dataset used: Herzog, Alexander; Mikhaylov, Slava, 2017, "Dail_debates_1919-2013.tar.gz", Database of Parliamentary Speeches in Ireland, 1919-2013, https://doi.org/10.7910/DVN/6MZN76/CRUNF0, Harvard Dataverse, V2
# 0. Content ------------------------------------------------------------
#  1. Preparation
#  2. Corpus Preparation
#  3. Entity Detection
#
#
#
# 1. Preparation --------------------------------------------------------
# __Loading Packages -------------------------------------------------
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
pkgs <- c('beepr', 'tidyverse', 'rio', 'tidylog', 'skimr',
          'quanteda', 'readtext', 'Hmisc',
          'googledrive', 'readtext', 'data.table', 'stringr'); for (i in pkgs){usePackage(i)}

# __Loading Data -----------------------------------------------------
# Download again vs. subsample file already existing?
# Get Data from: 
    # 1) Website (original) 
    # 2) Drive (subset) 
    # 3) local (subset)
downl <- 3

if (downl == 1){
# These lines downlad the original data from the website; unpack them; and read them into the program
# Following line not yet working, somehow I can unpack the manually downloaded file, but not the one downloaded via R
download.file(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/6MZN76/CRUNF0",
              destfile = "Dail_debates_1919-2013.tar.gz") # https://stat.ethz.ch/R-manual/R-devel/library/utils/html/download.file.html
# unpacking the file
untar(tarfile = "Dail_debates_1919-2013.tar.gz")

## Read file 
df <- fread("Dail_debates_1919-2013.tab", sep="\t", 
            quote="", header=TRUE, showProgress = TRUE, data.table=FALSE, verbose = TRUE, encoding = "UTF-8")

## Subsetting
# Take only the ones of the latest 2 legislative periods
# latest recorded speech: 2013-03-28
# Adding legislative period variable:
df$datef <- as.Date(df$date) # making sure that date variable is in date format
per <- import("DailPeriods.csv")
per$end[per$dail==max(per$dail)] <- Sys.Date() %>% as.character()
for (i in 1:nrow(per)){df$legper[(df$datef >= per$begin[i] & df$datef <= per$end[i])] <- per$dail[i]}
dfs <- df %>% filter(legper >= 30); beep(2)
# dfs$datef %>% min # double checking: ✓ 
export(dfs, "dail_subset.Rdata")
export(dfs, "dail_subset.csv")
drive_upload(media="dail_subset.csv", 
             path="~/Internship AffPol in Text/Data/Ireland/dail_subset.csv")
} else if (downl==2) {
  # drive_get("https://drive.google.com/open?id=1jWNIx3PYKEvqzB8iZXgZe_36MmvgTmEd")
  drive_download("https://drive.google.com/open?id=1jWNIx3PYKEvqzB8iZXgZe_36MmvgTmEd", overwrite=T)
  dfs <- readtext("dail_subset.csv", text_field = "speech")
} else if (downl==3) {
  dfs <- readtext("dail_subset.csv", text_field = "speech")
}

# 2. Corpus preparation ----------------------------------------------------------
### Sentiment Analysis of speeches that refer to the Government ###

df.small <- dfs[dfs$legper==30,]
corp_dail <- corpus(df.small)

## First subset corpus - only speeches by non-government(party) MPs
govt.parties <- c("Fine Gael", "The Labour Party") ## these two parties were in govt
corp_opposition <- corpus_subset(corp_dail, party_name %nin% govt.parties)

## Tokenize corpus, and remove stop punctuation and stopwords
opp.tokens <- tokens(corp_opposition, remove_punct = T)
opp.tokens <- tokens_select(opp.tokens, stopwords('english'), selection='remove')


# 3. Entity Detection #kwic --------------------
# Keywords in context (major difference STARTS HERE)
govt <- c("Government", "Fine Gael", "Labour", "Taoiseach", "Minist*")

kw_govt <- kwic(opp.tokens, pattern=phrase(govt), window=20) # key word in context

# head(kw_govt)
df_test <- merge(df.small, kw_govt, by.y="docname", by.x="doc_id")
df_test$window <- paste(df_test$pre, df_test$keyword, df_test$post, sep=" ")
df_test$text <- df_test$window

corpus.test <- corpus(df_test)
sent.test <- dfm(corpus.test, dictionary=data_dictionary_LSD2015[1:2])

sent.test %>% head()
df_test %>% head()

df_test <- cbind(df_test, as.data.frame(sent.test))
df_test <- df_test[-19]
df_test <- df_test[-c(18, 16, 14, 13, 12)]




# Analyses ----------------------------------------------------------



