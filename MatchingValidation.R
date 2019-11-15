# Title:
# Context: APART
# Author: Nicolai B.
# Date: Fri Nov 15 13:42:14 2019
# 0. Content ------------------------------------------------------------
# 1. Preparation
#
#
# 1. Preparation -----------------------------------------------------
# __Loading Packages -------------------------------------------------
setwd("~/GitHub/samunico/Apart/data")
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('googledrive', 'data.table', 'tidyverse', 'tidylog')){
  usePackage(i)}

# __global vars  -----------------------------------------------------
update = F

# __Loading Data -----------------------------------------------------
if (update == T) {
  drive_download(file = "~/Internship AffPol in Text/Data/Ireland/df_window_30th_dail.csv", overwrite = T)
  df_w <- fread("df_window_30th_dail.csv")
} else {
  df_w <- fread("df_window_30th_dail.csv")
}


# __Transform

## draw random subsample of 250 speeches
  # extract 250 random unique speeches
sample <- df_w$speechID %>%
  unique() %>%
  sample(250)
  
  # keep only those windows that are part of those speeches
df_sub <- df_w %>% filter(df_w$speechID %in% sample)


## create table were case = speech, with the number of mentions of each entity
df_c <- df_sub %>% group_by(speechID, df_sub$keyword) %>% 
  summarise(n=n())

# get into wide format
df_c_wide <- df_c %>% spread(`df_sub$keyword`, n)

# append speech text
df_c_matched <- left_join(df_c_wide,
                  df_w %>% select(speechID,text) %>% distinct,
                  by=c("speechID"))


## for each speech, display speech + number and string of entities matched
    ## count number of TP, FP, FN matches in each speech
      # TP: matched entity correctly identified 
      # FP: matched entity incorrectly identified
      # FN: entity not matched

df_c_mentioned <- data.frame(matrix(nrow = nrow(df_c_matched), ncol = ncol(df_c_matched)), row.names = df_c_matched$speechID)
colnames(df_c_mentioned) <- colnames(df_c_matched)

df_c_missing <- data.frame(matrix(nrow = nrow(df_c_matched), ncol = 1), row.names = df_c_matched$speechID)


for (i in (1:nrow(df_c_matched))){
  print("Speech:")
  cat(df_c_matched[i,]$text)
  print("Mentions:")
  for (j in (colnames(df_c_matched))){
    if (!is.na(df_c_matched[i,j]) & j != "text" & j != "speechID"){
      print(paste("How often was ", j, " mentioned?"))
      df_c_mentioned[i,j] <- readline(prompt = "Input: ")
      print(paste(j, " was matched ", df_c_matched[i,j], " times"))
    }
  }
  print("Was another MP/party mentioned?")
  df_c_missing[i] <- readline(prompt = "Input: ")
}


## write into csv
write.csv(x = df_c_matched, file = "validation_matches.csv")
write.csv(x = df_c_mentioned, file = "validation_mentions.csv")
write.csv(x = df_c_missing, file = "validation_missings.csv")
