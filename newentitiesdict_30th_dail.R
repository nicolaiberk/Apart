# Title: Creating a universal dictionary across legislative periods
# Context: APART
# Author: Philipp M.
# Date: Fri Nov 15 12:46:26 2019
# 0. Content ------------------------------------------------------------
#   1. Load full set of 
#
# 1. Preparation --------------------------------------------------------
# __Loading Packages -------------------------------------------------
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('beepr', 'tidyverse', 'rio', 'tidylog', 'skimr', 'clipr',
            'quanteda', 'readtext', 'Hmisc', 'rio',
            'googledrive', 'readtext', 'data.table', 'stringr', 'qdap')){
  usePackage(i)}

# __Loading Data -----------------------------------------------------

# Get a date overview of Dáils:
daildict <- import("Original Data Files/DailPeriods.csv")
daildict <- daildict %>% mutate(
  begin = begin %>% as.Date("%d/%m/%Y"),
  end = end %>% as.Date("%d/%m/%Y")
)

# Get the universe of speeches
# Get Data from: 
# 1) Website (original)  TRUE
# 2) local (processed)  FALSE
downl <- TRUE

if (downl){
  # These lines downlad the original data from the website; unpack them; and read them into the program
  # Following line not yet working, somehow I can unpack the manually downloaded file, but not the one downloaded via R
  # downloading file
  download.file(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/6MZN76/CRUNF0",
                destfile = "Dail_debates_1919-2013.tar.gz") # https://stat.ethz.ch/R-manual/R-devel/library/utils/html/download.file.html
  # unpacking the file
  untar(tarfile = "Dail_debates_1919-2013.tar.gz")
  list.files()
  
  # read file 
  df <- fread("Dail_debates_1919-2013.tab", sep="\t", 
              quote="", header=TRUE, showProgress = TRUE, data.table=FALSE, verbose = TRUE)
  
  # latest recorded speech: 2013-03-28
  # Adding legislative period variable:
  df$datef <- as.Date(df$date) # making sure that date variable is in date format
  beep(2)
  
  daildict$end[daildict$dail==max(daildict$dail)] <- Sys.Date() %>% as.character()
  df$legper <- NA
  i <- 30
  for (i in 1:nrow(daildict)){
    df <- df %>% mutate(legper = ifelse(datef >= daildict$begin[i] & datef <= daildict$end[i], daildict$dail[i], legper))}
  
  # Plotting the number of speeches against the legislative period
  df %>% ggplot(.,aes(x=legper)) + geom_histogram(bins = max(df$legper,na.rm=T)) + scale_x_continuous(breaks = 1:max(daildict$dail))
  
  export(df, "dail_full.csv")
} 

dfs <- readtext("dail_full.csv", text_field = "speech")


##
## Subsetting
# Take only the ones of the latest 2 legislative periods
##

df <- read_csv("df_window_30th_dail.csv")


## Get names of MPs in 30th Dail
names <- tibble(originalname=unique(df$member_name), # original name (not clean)
                fullname=str_remove_all(as.character(df$member_name %>% unique()), "\\((.*?)\\)") %>% 
                 str_remove_all(" RIP") %>% 
                  word(2,-1), # clean full name
               surname=(str_remove_all(as.character(df$member_name %>% unique()), "\\((.*?)\\)") %>% 
                        str_remove_all(" RIP") %>% strsplit(split=" ") %>% # clean last name
                        lapply(function(x) {x[length(x)]}) %>% unlist)) 


## Get only MPs who share a last name
repeated_names <- names %>%
  group_by(surname) %>% 
  filter(n()>1)

## Dummy if MP shares surname

names$shared <- 0
names$shared <- ifelse(names$surname %in% repeated_names$surname, 1, 0)

###### Creating dictionary #######

names$match <- NA
names$alternativematch <- NA
names$length <- sapply(strsplit(names$fullname, " "), length)


for (i in 1:nrow(names)) {
  if (names$shared[i]==0) {
    names$match[i] <- paste("Deputy", names$surname[i])
    names$alternativematch[i] <- paste("Deputy", names$fullname[i])
  }
  else if (names$shared[i]==1 & names$length[i]==2) {
    names$match[i] <- paste("Deputy", names$fullname[i])
    names$alternativematch[i] <- NA
  }
  else if (names$shared[i]==1 & names$length[i]==3) {
    names$match[i] <- paste("Deputy", names$fullname[i])
    names$alternativematch[i] <- paste("Deputy", strsplit(names$fullname[i]," ")[[1]][1],
                                       strsplit(names$fullname[i]," ")[[1]][3], sep=" ")
  }
}


names[170:178,'match'] <-  unique(df$party_name) # add party names to column of matches

write.csv(names, 'entities_30th_dail.csv')
drive_upload("entities_30th_dail.csv",
             path="~/Internship AffPol in Text/Data/Ireland/",
             overwrite = T)




