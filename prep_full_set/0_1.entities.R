# Title:
# Context: APART
# Author: Nicolai B.
# Date: Fri Nov 15 13:42:14 2019
# 0. Content ---------------------------------------------------------
# 1. Preparation
#
#
# 1. Preparation -----------------------------------------------------
# __Loading Packages -------------------------------------------------
setwd("~/GitHub/samunico/Apart/data")
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('googledrive', 'readtext', 'data.table', 'tidyverse', 'stringr', 'qdap')){usePackage(i)}

# __global vars  -----------------------------------------------------
update = F # set F if update should be skipped
upload = F # set F if you do not want to upload changes


# __Loading Data -----------------------------------------------------
setwd("~/GitHub/samunico/Apart/data")

if (update == T){
  fn <- "~/Internship AffPol in Text/Data/Ireland/Dail_debates_1919-2013.tar.gz"
  drive_get(fn)
  drive_download(file = fn, overwrite = T)

  ## unzip
  untar("Dail_debates_1919-2013.tar.gz")
}

## read in
data <- fread("Dail_debates_1919-2013.tab", 
              sep="\t", 
              quote="", 
              header=TRUE, 
              showProgress = TRUE, 
              data.table=FALSE, 
              verbose = TRUE)


# 2. Cleaning ----------------------------------------------------------
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
rm(data)

dict <- dict[dict != ""]

# 3. Export ------------------------------------------------------------
write.csv(dict, 'entities_full_Dail.csv')
if (upload == T){
drive_upload("entities_full_Dail.csv", 
             path = "~/Internship AffPol in Text/Data/Ireland/",
             overwrite = T)
}