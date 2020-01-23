######

# CURRENT WORKING SITE:
# Work on finding correct speaker names (see header 'find speakers ....')

######



# Title: Post-window creation cleaning
# Context: APART
# Author: Nicolai Berk
# Date: Fri Nov 15 13:42:14 2019
# 0. Content ---------------------------------------------------------
#
#
# 1. Preparation -----------------------------------------------------
# __Loading Packages -------------------------------------------------
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('data.table', 'dplyr', 'purrr', 'googledrive', 'quanteda', 'compiler', 'stringr')){usePackage(i)}; rm(i, usePackage)

# 2. identify mentions through speaker
# __global vars  -----------------------------------------------------
update <- F
subset <- F

# load data
setwd("~/GitHub/samunico/Apart")
speakers <- fread("DailPeriods.csv", encoding = "UTF-8") # load speaker data set

if (update == T){
  fn <- "https://drive.google.com/open?id=104gxp9sK-7ZEgaHI_1dLmn1YD42w0w59"
  drive_get(fn, )
  drive_download(file = fn, overwrite = T)
}


df_window <- fread("data/dail_full_nospeech.csv.gz", encoding = "UTF-8")
if(subset == T){df_window <- df_window[1:1000, ]}

member_name_r <- strsplit(df_window$member_name, split = " ")
second <- sapply(member_name_r, `[`, 2)
third <- sapply(member_name_r, `[`, 3)
df_window$new_names <- data.frame(second, third) %>%
  apply(X = ., MARGIN = 1, FUN = paste, collapse = " ")
rm(member_name_r, second, third)

# # transform dates
df_window$datef <- as.Date(df_window$date, format = "%Y-%m-%d")
speakers$begin <- as.Date(speakers$begin, format = "%d/%m/%Y")
speakers$end <- as.Date(speakers$end, format = "%d/%m/%Y")



# find speakers with differing names in the df -----
# some speaker names are listed differently in the dail periods and in the speaker list
entities <- fread("EntitiesDict.csv")
sp <- c("Count Plunkett", "Seán Lemass", "Pádraig Faulkner", "John O'Connell", "Tom Fitzpatrick", "Seán Barrett") 
sp <- sp %>% word(2)
et <- entities %>% select(originalname, fullname) %>% distinct

et[grepl(sp[1], et$originalname),] 
df_window[grepl(sp[1], df_window$member_name),] 

et[grepl(sp[2], et$originalname),] 
df_window[grepl(sp[2], df_window$speaker),] 

et[grepl(sp[3], et$originalname),] 
df_window[grepl(sp[3], df_window$speaker),] 

et[grepl(sp[4], et$originalname),] 
df_window[grepl(sp[4], df_window$speaker),] 

et[grepl(sp[5], et$originalname),] 
df_window[grepl(sp[5], df_window$speaker),] 

et[grepl(sp[6], et$originalname),] 
df_window[grepl(sp[6], df_window$speaker),]



# dummy variable for speakers
df_window$speaker <- 0
sum(df_window$speaker)

for (i in (1:nrow(speakers))){

  begin <- speakers$begin[i]
  end <- speakers$end[i]
  name <- speakers$speaker[i]
  
  df_window <-
    df_window %>%
    tidylog::mutate(
      speaker =
        ifelse(test = (new_names == name &
                         begin < datef &
                         datef < end |
                         speaker == 1),
               yes = 1,
               no = 0)
    )
}

# 3. get rid of common reference terms  -------------------------

# define pre-match terms
df_window$shortpre <- df_window$pre %>%
  lapply(., strsplit, split = " ") %>%
  lapply(., unlist) %>% 
  lapply(., tail, n = 5) %>%
  lapply(., paste, collapse = " ") %>% 
  unlist #%>% 
  # as.factor()

# get rid of missings
df_window$shortpre[df_window$shortpre == ""] <- NA

# inspect most common pre-matches (this might take a bit)
sort(table(df_window$shortpre), decreasing = T)[1:100]

# might exclude 'I call', 'I am calling', however very minor problem (~0.2 to ~0.3% of windows)