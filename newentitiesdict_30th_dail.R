library(tidyverse)
library(stringr)


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




