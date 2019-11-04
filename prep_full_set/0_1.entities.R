"
construct dictionary of entities
"

usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
pkgs <- c('googledrive', 'readtext', 'data.table', 'tidyverse', 'stringr', 'qdap'); for (i in pkgs){usePackage(i)}


## download data
rm(list = ls())
setwd("~/GitHub/samunico/Apart/data")
update = F # set F if update should be skipped
upload = F # set F if you do not want to upload changes

if (update == T){
  fn <- "~/Internship AffPol in Text/Data/Ireland/Dail_debates_1919-2013.tar.gz"
  drive_get(fn)
  drive_download(file = fn, overwrite = T)

  ## unzip
  untar("Dail_debates_1919-2013.tar.gz")
}

## inspect
my_file <- "Dail_debates_1919-2013.tab"
print(paste(round(file.info(my_file)$size  / 2^30,3), 'gigabytes'))

## read in
data <- fread(my_file, sep="\t", 
              quote="", 
              header=TRUE, 
              showProgress = TRUE, 
              data.table=FALSE, 
              verbose = TRUE)

data["date"] <- as.Date(data[, "date"], format = "%Y-%m-%d")

# clean names
data$member_name_clean <- 
  str_remove_all(as.character(data$member_name), "\\((.*?)\\)") %>%
  str_remove_all(" RIP")

# add 'deputy' to last names
data$member_last_name <- 
  str_remove_all(data$member_name_clean, ".* ") %>%
  paste('Deputy', ., sep = " ")


# add full names
data$member_full_name <- 
  str_remove_all(data$member_name_clean, "Ms. |Mr. |Dr. |Mrs. |Count ") %>%
  paste('Deputy', ., sep = " ")


dict <- c(as.character(unique(data$member_full_name))
          , as.character(unique(data$member_last_name))
          , as.character(unique(data$party_name))
)

dict <- dict[dict != ""]


data <- as.vector(dict)
data <- data[!is.na(data)]
data <- rm_stopwords(data, Top25Words, separate = F, strip=T) # remove stop words and punctuation
data <- tools::toTitleCase(data) # capitalise all words
data <- gsub("(O'.)","\\U\\1",data,perl=TRUE) # capitalise names starting with O'


## write to csv and upload to drive
write.csv(data, 'entities_full_Dail.csv')
if (upload == T){
drive_upload("entities_full_Dail.csv", 
             path = "~/Internship AffPol in Text/Data/Ireland/",
             overwrite = T)
}