# Title: Creating a universal dictionary across legislative periods
# Context: APART
# Author: Philipp M.
# Date: Fri Nov 15 12:46:26 2019
# DESCRIPTION: 
# INPUT FILES: DailPeriods.csv; Dail_debates_1919-2013.tar.gz | dail_full.csv
# ---------------------------------------- 0. Content --------------------------------
#   1. Preparation
#   2. Creating Entity Dictionary
#   
#   
#   
#   
# ---------------------------------------- 1. Preparation --------------------------------
# ---------------------------------------- __Loading Packages ----------------------------
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('beepr', 'tidyverse', 'tidylog', 'clipr', 'googledrive', 'stringr', 'data.table', 'inops')){usePackage(i)}
# detach("package:skimr", unload=TRUE)


# ---------------------------------------- __Global Variables ----------------------------
# 1) original dataset fromat Harvard dataverse
# If you want to start from the original Harvard Dataverse dataset you'll have to download it manually first & paste it into the current file directory: https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/6MZN76/CRUNF0
DATASRC <-  3 # set to x to start from: 
              #   1) [NOT WORKING] Harvard Dataverse compressed file ("Dail_debates_1919-2013.tar.gz")
              #   2) Google Drive compressed file
              #   3) Ready-made local dataset "dail_full.csv"
UPLOAD <-  T  # Should the dictionary be uploaded to drive?
PLOT <- F     # Plot distribution of speeches across legislative periods


# ---------------------------------------- __Loading Data ----------------------------
## 1. Data frame of Dáil Periods & speakers
# daildict <- read.csv("DailPeriods.csv", encoding = "UTF-8", stringsAsFactors = F)
daildict <- import("DailPeriods.csv",  encoding = "UTF-8") # used to be object periods
daildict <- daildict %>% mutate(
  begin = begin %>% as.Date(),
  end = end %>% as.Date(),
  )

## 2.Universe of speeches
# ___updating local full speech file (if necessary) --------
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
              quote="", header=TRUE, showProgress = TRUE, data.table=FALSE, verbose = TRUE)

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
    
    # Plotting the number of speeches against the legislative period
    if (PLOT) {zz <- df %>% ggplot(.,aes(x=legper)) + 
      geom_histogram(bins = max(df$legper,na.rm=T)) + 
      scale_x_continuous(breaks = 1:max(df$legper))}
  
    
  # Save file locally
  fwrite(df, "dail_full.csv", verbose = T)
}


# ___loading local full speech file ----------
df <- fread("dail_full.csv", encoding = "UTF-8", verbose = T)

# ---------------------------------------- 2. Creating an entity dictionary ----------------------------
# Subselect relevant columns
temp <- df %>% select("memberID", "member_name", "legper", "party_name") %>% distinct() %>% as_tibble()


# find formal prefixes of names:
# temp$member_name %>% word(1) %>% table() %>% sort(decreasing = T)

## NICKNAME MATCHES
# # check which nicknames are actually Nicknames
# nick <- str_extract(temp$member_name, "\\((.*?)\\)") %>% 
#   table() %>% sort(decreasing = T) %>% names()
# nonick <- c("(Snr.)", "(Deceased)", "(Major-General)", "(Jnr.)", "(Limerick West)", "(Senior)", "(The Cope)", "(Captain)", "(Dr.)", "(Major)", "(Dublin South-Central)", "(Longford-Roscommon)", "(Tipperary)", "(Cork City)", "(Resigned)", "(Wexford)", "(Tipperary South)", "(Cork Borough)", "(Cork West)", "(deceased)", "(Dublin West)", "(Edenderry, Offaly)", "(Mayo North)", "(Snr)", NA)



# ---------------------------------------- __Find relevant titles --------------------
temp$member_name %>% word(1) %>% table %>% sort(decreasing=T) %>% names() %>% write_clip
titles <- c("Professor","Doctor","Sir","Captain","General","Major","Colonel","Countess","Count")
 
# # Which are the deputies with titles? (from legper 13 onwards)
# temp[(temp$member_name) %in~% titles & temp$legper >= 13,] %>% 
#   select(member_name) %>% distinct %>% 
#   print(n=40)
# could be added: "Sir Anthony C . Esmonde", "Deputy Dr . Esmonde"
# Dummy for title
temp <- temp %>% mutate(title = ifelse((temp$member_name) %in~% titles,1,0))

# Creating Clean Member Names & Patterns, etc. --------
temp2 <- temp %>% 
  mutate(
    originalname = member_name, # original name (not clean)
    fullname = member_name %>% 
      str_remove_all("Count |Mr. |Dr. |Professor |General |Ms. |Countess |Sir |Capt. |Major |Mrs. |Colonel | RIP"),
    # nickname = ifelse(!str_extract(fullname, "\\((.*?)\\)") %in% nonick, 
    #                   (str_extract(fullname, "\\((.*?)\\)") %>% str_remove_all("\\(|\\)")), NA),
    fullname = fullname %>% str_remove_all("\\((.*?)\\)|,") %>% str_replace("  ", " "),
      # word(2,-1), # clean full name
    surname = fullname %>% word(-1),
    titlefullname = ifelse(title==1,
                       paste0(word(member_name,1), " ",fullname)
                       , NA),
    titlesurname = ifelse(title==1,
                           paste0(word(member_name,1), " ",surname)
                           , NA),
    firstname = word(fullname,1),
    firstlast = paste(firstname,surname)
)

# Adressing the shared surnames issue:
temp2 <- temp2 %>% 
  group_by(legper, surname) %>% 
  mutate(shared = ifelse(n()>1,1,0)) %>%  # Dummy for whether surname is shared within legislative period
  ungroup() %>% 
  mutate(
    lengthofname = sapply(strsplit(fullname, " "), length),
    deputylastname = ifelse(shared==0, paste("Deputy", surname),NA),  #### DEPUTY LASTNAME if no duplicate
    # ifelse(shared==1 & lengthofname>=2, paste("Deputy", fullname)
    deputyfullname = paste("Deputy", fullname),                       #### DEPUTY FULLNAME
    deputyfirstlastn = ifelse(lengthofname>2,paste("Deputy", firstlast),NA),   #### DEPUTY FIRST LASTNAME
    # nicknamematch = ifelse(!is.na(nickname),paste("Deputy", nickname, surname),NA)
    ) %>% 
  arrange(-shared, lengthofname)

# temp2$deputyfullname[temp2$shared==1 & temp2$lengthofname ==3] <- altmat$deputyfullname



# Individual cases Treatement -------------------------------------------------------------------------------------
# could be added: "Sir Anthony C . Esmonde", "Deputy Dr . Esmonde"

# All Loftus references refer to Deputy Rockall / Loftus
temp2$deputylastname[grepl("Loftus",temp2$fullname)] <- "Loftus"
temp2$deputyfullname[grepl("Loftus",temp2$fullname)] <- NA

# Plunkett is always and only referred to as Count Plunkett!
temp2$deputylastname[temp2$surname == "Plunkett"] <- "Count Plunkett"
temp2$deputyfullname[temp2$surname == "Plunkett"] <- NA



# 3. Export ------------------------------------------------------------
# Save file
Encoding(temp2$fullname)
fwrite(temp2 %>% select(-memberID, -member_name), "EntitiesDict.csv")

# Upload file
if(UPLOAD) {
  drive_upload("EntitiesDict.csv",
               path="~/Internship AffPol in Text/Data/Ireland/",
               overwrite = T)
}
