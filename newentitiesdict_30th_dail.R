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
daildict <- import("DailPeriods.csv")
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

df <- fread("dail_full.csv",
            header=TRUE, showProgress = TRUE, data.table=FALSE, verbose = TRUE, encoding = "UTF-8")
# Creating a dictionary ---------------------
temp$member_name %>% word(1) %>% unique() %>% write_clip()


temp <- df %>% select("memberID", "member_name", "legper") %>% distinct() %>% as_tibble()
temp2 <- temp %>% mutate(
  originalname = member_name, # original name (not clean)
  fullname = member_name %>% str_remove_all("Count |Mr. |Dr. |Professor |General |Ms. |Countess |Sir |Capt. |Major |Mrs. |Colonel"), 
    # str_remove_all(as.character(member_name), "\\((.*?)\\)") %>% 
    #      str_remove_all(" RIP") %>% 
    #      word(2,-1), # clean full name
       surname=(str_remove_all(as.character(member_name), "\\((.*?)\\)") %>%
                  str_remove_all(" RIP") %>% strsplit(split=" ") %>% # clean last name
                  lapply(function(x) {x[length(x)]}) %>% unlist)
       ) 


temp2 <- temp2 %>% group_by(legper, surname) %>% mutate(shared = ifelse(n()>1,1,0)) %>%  
  ungroup %>% mutate(
    fullname = fullname %>% str_replace("  ", " "),
    lengthofname = sapply(strsplit(fullname, " "), length),
                     match = ifelse(shared==0, paste("Deputy", surname),
                             ifelse(shared==1 & lengthofname>=2, paste("Deputy", fullname),NA)),
                     alternativematch = ifelse(shared==0, paste("Deputy", fullname),
                             ifelse(shared==1 & lengthofname==2,NA,-99))) %>% 
  arrange(-shared, lengthofname)

tibble(x=temp2$fullname[temp2$shared==1 & temp2$lengthofname ==3] %>% strsplit(" ") %>% map(1) %>% unlist(),
y=temp2$fullname[temp2$shared==1 & temp2$lengthofname ==3] %>% strsplit(" ") %>% map(3) %>% unlist()) %>% mutate(alternativematch = paste(x,y)) %>% select(alternativematch) ->
  altmat
temp2$alternativematch[temp2$shared==1 & temp2$lengthofname ==3] <- altmat$alternativematch

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




