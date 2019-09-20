library("googledrive") ## see https://googledrive.tidyverse.org/
library("readtext")
library("data.table")
library("tidyverse")


## download data
rm(list = ls())
setwd("~/GitHub/samunico/Apart/data")
fn <- "https://drive.google.com/open?id=1bZy0uF8i-W3a4wQAJQ1tefek4Rcy_K0k"
drive_get(fn)
drive_download(file = fn, overwrite = T)

## unzip
untar("Dail_debates_1919-2013.tar.gz")

## inspect
my_file <- "Dail_debates_1919-2013.tab"
print(paste(round(file.info(my_file)$size  / 2^30,3), 'gigabytes'))


# read in the first few lines
data <- fread(my_file, sep="\t", 
              quote="", 
              header=TRUE, 
              showProgress = TRUE, 
              data.table=FALSE, 
              verbose = TRUE)

data["date"] <- as.Date(data[, "date"], format = "%Y-%m-%d")

# restrict to speeches in the 30th D?il
data <- data[data[, "date"] > as.Date("24-05-2007", format = "%d-%m-%Y"), ]
data <- data[data[, "date"] < as.Date("25-02-2011", format = "%d-%m-%Y"), ]


## dictionary of speakers
fn <- "https://drive.google.com/open?id=1TWyaYjbu_rz5wXsY2nmR0gQO9BAquOkE"
drive_get(fn)
drive_download(file = fn, overwrite = T)

ministers <- fread("Dail_debates_1937-2011_ministers.tab")


## attach ministry to speech set:
ministers$start_date <- as.Date(ministers$start_date, format = "%Y-%m-%d")
ministers$end_date <- as.Date(ministers$end_date, format = "%Y-%m-%d")

data$position <- NA
data$department <- NA

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
write.csv(data, file = "30th_Dail_incl_positions.csv")

## upload into drive
drive_upload("30th_Dail_incl_positions.csv", 
             path = "https://drive.google.com/open?id=1jOF7BOTbzJwDXALbYte2uwXimfkFqwQF/")



## generate dictionary (matching can be done later on w. the full set, 
##    else we would need to collect it by hand w. party switches for every 
##    legislature, though this shouldnt be hard either)
dict <- c(as.character(unique(data$member_name)),
          as.character(unique(data$party_name)),
          as.character(unique(paste(data$position, data$department))))

## write to csv and upload to drive
write.csv(dict, 'entities_30th_Dail.csv')
drive_upload("entities_30th_Dail.csv", 
             path = "https://drive.google.com/open?id=1jOF7BOTbzJwDXALbYte2uwXimfkFqwQF/")
