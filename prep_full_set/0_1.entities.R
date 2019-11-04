"
construct dictionary of entities
"

usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
pkgs <- c('googledrive', 'readtext', 'data.table', 'tidyverse', 'stringr'); for (i in pkgs){usePackage(i)}


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

## dictionary of speakers

if (update == T){
  fn <- "~/Internship AffPol in Text/Data/Ireland/Dail_debates_1937-2011_ministers.tab"
  drive_get(fn)
  drive_download(file = fn, overwrite = T)
}

ministers <- fread("Dail_debates_1937-2011_ministers.tab")


## attach ministry to speech set:
ministers$start_date <- as.Date(ministers$start_date, format = "%Y-%m-%d")
ministers$end_date <- as.Date(ministers$end_date, format = "%Y-%m-%d")

data$position <- NA
data$department <- NA

# maybe there is a more efficient way to assign this?
for (r in 1:nrow(ministers)){
  member <- ministers$memberID[r]
  start  <- ministers$start_date[r]
  end    <- ministers$end_date[r]
  data$position[data$memberID == member &
                  data$date >= start &
                  data$date <= end] <- ministers$position[r]
  data$department[data$memberID == member &
                    data$date >= start &
                    data$date <= end] <- ministers$department[r]
}


## write into csv
write.csv(data, file = "full_Dail_incl_positions.csv")

## upload into drive
if (upload == T){
drive_upload("full_Dail_incl_positions.csv",
             path = "https://drive.google.com/open?id=1jOF7BOTbzJwDXALbYte2uwXimfkFqwQF",
             overwrite = T)
}


## generate dictionary

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
  str_remove_all(data$member_name_clean, "Ms. |Mr. |Dr. |Mrs. ") %>%
  paste('Deputy', ., sep = " ")


# clean characters
data$position[is.na(data$position)] <- ""
data$department[is.na(data$department)] <- ""


dict <- c(as.character(unique(data$member_full_name))
          , as.character(unique(data$member_last_name))
          , as.character(unique(data$party_name))
)

dict <- dict[dict != ""]


## write to csv and upload to drive
write.csv(dict, 'entities_full_Dail.csv')
if (upload == T){
drive_upload("entities_full_Dail.csv", 
             path = "https://drive.google.com/open?id=1jOF7BOTbzJwDXALbYte2uwXimfkFqwQF",
             overwrite = T)
}