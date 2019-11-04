usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
pkgs <- c('googledrive', 'readtext', 'data.table', 'tidyverse', 'stringr'); for (i in pkgs){usePackage(i)}
## see https://googledrive.tidyverse.org/

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

# restrict to speeches in the 30th Dáil
data <- data[data[, "date"] > as.Date("14-06-2007", format = "%d-%m-%Y"), ]
data <- data[data[, "date"] < as.Date("29-01-2011", format = "%d-%m-%Y"), ]


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
             path = "https://drive.google.com/open?id=1jOF7BOTbzJwDXALbYte2uwXimfkFqwQF/",
             overwrite = T)



## generate dictionary (matching can be done later on w. the full set, 
##    else we would need to collect it by hand w. party switches for every 
##    legislature, though this shouldnt be hard either)

# clean names
data$member_name_clean <- 
  str_remove_all(as.character(data$member_name), "\\((.*?)\\)") %>%
  str_remove_all(" RIP")

# add last names
data$member_last_name <- str_remove_all(data$member_name_clean, ".* ")

# add full names
data$member_full_name <- str_remove_all(data$member_name_clean, "Ms. |Mr. |Dr. |Mrs. ")


# clean characters
data$position[is.na(data$position)] <- ""
data$department[is.na(data$department)] <- ""


dict <- c(as.character(unique(data$member_full_name))
          , as.character(unique(data$member_last_name))
          , as.character(unique(data$party_name))
          #, as.character(unique(data$const_name))
          #, as.character(unique(paste(data$position, data$department)))
          )

dict <- dict[dict != ""]


## write to csv and upload to drive
write.csv(dict, 'entities_30th_Dail.csv')
drive_upload("entities_30th_Dail.csv", 
             path = "https://drive.google.com/open?id=1jOF7BOTbzJwDXALbYte2uwXimfkFqwQF/",
             overwrite = T)
