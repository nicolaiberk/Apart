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
setwd("~/GitHub/samunico/Apart")
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('googledrive', 'data.table', 'dplyr', 'corrplot')){usePackage(i)}

# __global vars  -----------------------------------------------------
subsample = 400 # define sample size, 'full' for full sample

# __Loading Data -----------------------------------------------------
## include code to download random sample 
df <- fread("window_sub.csv", encoding = "UTF-8")

if (subsample != 'full'){
  df <- sample_n(df, subsample)
}

df$handcoding <- NA
df$procedural <- NA
i <- 1
while(i <= nrow(df)) {
  # for (i in (j:nrow(df))){
  cat(paste0(c("|", rep("=",((z <- i/nrow(df))*80) %>% round(0)), ">",rep(" ",(80-z*80)), "|", z*100, "% \n")), sep="", collapse="")
  cat(paste("Party:", df$party_name[i], "\n\n"))
  cat(paste("Match:", df$keyword[i], "\n\n"))
  cat("Window:\n\n")
  cat(paste(df$window[i], "\n"))
  
  # Procedural Check
  cat("IS THE SENTIMENT TOWARDS THE MATCH IN THIS TEXT NEGATIVE [1], NEUTRAL [2], OR POSITIVE [3]? \n")
  value <- readline(prompt = "Value: ")
  if(value %in% c("quit", "q")) {break #; j <- i
  } else if (value %in% c("back", "b")) {i <- i-1; next
  } else if (is.numeric(value)) {(df$handcoding[i] <- as.numeric(value)-2)
  } else {cat("No valid input"); next}
  
  # Procedural Check
  cat("IS THE WINDOW PROCEDURAL [1], OR NOT [2]? \n")
  value2 <- readline(prompt = "Value: ")
  if(value2 %in% c("quit", "q")){break #; j <- i
  } else if(is.numeric(value2)) {df$procedural[i] <- 2-as.numeric(value2)
  } else if(value2 %in% c("back", "b")) {i <- i-1; next
  } else {cat("No valid input"); next} #i <- i-1;
  i <- i + 1
}

write.csv(df, file = "nico_sample_coded.csv", fileEncoding = "UTF-8")
