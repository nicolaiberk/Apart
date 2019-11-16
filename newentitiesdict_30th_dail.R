# Title: Creating a universal dictionary across legislative periods
# Context: APART
# Author: Philipp M.
# Date: Fri Nov 15 12:46:26 2019
# DESCRIPTION: This code 
# 0. Content ------------------------------------------------------------
#   1. Preparation
#   2. Creating Entity Dictionary
#   
#   
#   
#   
# 1. Preparation -------------------------------------------------------
# __Loading Packages ---------------------------------------------------
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('beepr', 'tidyverse', 'tidylog', 'clipr', 'googledrive', 'stringr', 'rio', 'data.table')){usePackage(i)}
# detach("package:skimr", unload=TRUE)


# __global vars  -----------------------------------------------------
# 1) original dataset fromat Harvard dataverse
# If you want to start from the original Harvard Dataverse dataset you'll have to download it manually first & paste it into the current file directory: https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/6MZN76/CRUNF0

DATASRC <-  3 # set to x to start from: 
              #   1) [NOT WORKING] Harvard Dataverse compressed file ("Dail_debates_1919-2013.tar.gz")
              #   2) Google Drive compressed file
              #   3) Ready-made local dataset
PLOT <- F     # Plot distribution of speeches across legislative periods


# __Loading Data -----------------------------------------------------
## 1. Data frame of Dáil Periods & speakers
daildict <- read.csv("DailPeriods.csv", encoding = "UTF-8")
daildict <- daildict %>% mutate(
  begin = begin %>% as.Date("%d/%m/%Y"),
  end = end %>% as.Date("%d/%m/%Y")
)
write.csv(daildict, fileEncoding = "UTF-8")

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
    if (PLOT) {df %>% ggplot(.,aes(x=legper)) + 
      geom_histogram(bins = max(df$legper,na.rm=T)) + 
      scale_x_continuous(breaks = 1:max(daildict$dail))}
  
  # Save file locally
  export(df, "dail_full.csv")
  
} #else if (DATASRC==3) {
  

df <- read.csv("dail_full.csv", encoding = "UTF-8")
  # }

# Creating an entity dictionary ---------------------
# Subselect relevant columns
temp <- df %>% select("memberID", "member_name", "legper", "party_name") %>% distinct() %>% as_tibble()

# find formal prefixes of names:
# temp$member_name %>% word(1) %>% table() %>% sort(decreasing = T)

# check which nicknames there are
nick <- str_extract(temp$member_name, "\\((.*?)\\)") %>% 
  table() %>% sort(decreasing = T) %>% names()
nonick <- c("(Snr.)", "(Deceased)", "(Major-General)", "(Jnr.)", "(Limerick West)", "(Senior)", "(The Cope)", "(Captain)", "(Dr.)", "(Major)", "(Dublin South-Central)", "(Longford-Roscommon)", "(Tipperary)", "(Cork City)", "(Resigned)", "(Wexford)", "(Tipperary South)", "(Cork Borough)", "(Cork West)", "(deceased)", "(Dublin West)", "(Edenderry, Offaly)", "(Mayo North)", "(Snr)", NA)

# nonick %>% 
temp$party_name %>% unique()

# Create cleaned names & surname columns
temp2 <- temp %>% mutate(
  originalname = member_name, # original name (not clean)
  fullname = member_name %>% 
    str_remove_all("Count |Mr. |Dr. |Professor |General |Ms. |Countess |Sir |Capt. |Major |Mrs. |Colonel | RIP"),
  nickname = ifelse(!str_extract(fullname, "\\((.*?)\\)") %in% nonick, (str_extract(fullname, "\\((.*?)\\)") %>% str_remove_all("\\(|\\)")), NA),
  fullname = fullname %>% str_remove_all("\\((.*?)\\)"),
    # word(2,-1), # clean full name
  surname = (str_remove_all(member_name, "\\((.*?)\\)") %>%
               str_remove_all(" RIP") %>% strsplit(split=" ") %>% # clean last name
               lapply(function(x) {x[length(x)]}) %>% unlist),
       )

temp2 <- temp2 %>% group_by(legper, surname) %>% mutate(shared = ifelse(n()>1,1,0)) %>%  
  ungroup %>% mutate(
    fullname = fullname %>% str_replace("  ", " "),
    lengthofname = sapply(strsplit(fullname, " "), length),
    match = ifelse(shared==0, paste("Deputy", surname),
                   ifelse(shared==1 & lengthofname>=2, paste("Deputy", fullname),NA)),
    alternativematch = ifelse(shared==0, paste("Deputy", fullname),
                              ifelse(shared==1 & lengthofname==2,NA,-99)),
    nicknamematch = ifelse(!is.na(nickname),paste("Deputy", nickname, surname),NA)) %>% 
  arrange(-shared, lengthofname)

temp2$nicknamematch %>% unique()

tibble(x=temp2$fullname[temp2$shared==1 & temp2$lengthofname ==3] %>% strsplit(" ") %>% map(1) %>% unlist(),
y=temp2$fullname[temp2$shared==1 & temp2$lengthofname ==3] %>% strsplit(" ") %>% map(3) %>% unlist()) %>% mutate(alternativematch = paste(x,y)) %>% select(alternativematch) ->
  altmat
temp2$alternativematch[temp2$shared==1 & temp2$lengthofname ==3] <- altmat$alternativematch


# Save file
write.csv(temp2 %>% select(-memberID, -member_name), "EntitiesDict.csv", fileEncoding = "UTF-8")





temp2 %>% arrange(-shared) %>% View
temp2$alternativematch %>% is.na %>% table

temp2$fullname %>% str_split(" ")

temp2 %>% View

str_split()
with(temp2,
     alternativematch[shared==1 & lengthofname ==3] <-      
     )



  # filter(n()>1) %>% arrange(legper,surname) #


temp2$match <- NA
temp2$alternativematch <- NA
for (i in 1:nrow(temp2)) {
  if (temp2$shared[i]==0) {
    temp2$match[i] <- paste("Deputy", temp2$surname[i])
    temp2$alternativematch[i] <- paste("Deputy", temp2$fullname[i])
  }
  else if (temp2$shared[i]==1 & temp2$length[i]==2) {
    temp2$match[i] <- paste("Deputy", temp2$fullname[i])
    temp2$alternativematch[i] <- NA
  }
  else if (temp2$shared[i]==1 & temp2$length[i]==3) {
    temp2$match[i] <- paste("Deputy", temp2$fullname[i])
    temp2$alternativematch[i] <- paste("Deputy", strsplit(temp2$fullname[i]," ")[[1]][1],
                                       strsplit(temp2$fullname[i]," ")[[1]][3], sep=" ")
  }
  
  

###### Creating dictionary #######
names$match <- NA
names$alternativematch <- NA





names[170:178,'match'] <-  unique(df$party_name) # add party names to column of matches

write.csv(names, 'entities_30th_dail.csv')
drive_upload("entities_30th_dail.csv",
             path="~/Internship AffPol in Text/Data/Ireland/",
             overwrite = T)




if(upload){
  # Compress csv file
  tar("dail_full.csv", compression="gzip")
  
  # Update Google Drive File
  drive_upload("Dail_debates_1919-2013.csv", 
               path = "~/Internship AffPol in Text/Data/Ireland/",
               overwrite = T)
  drive_update()
}


