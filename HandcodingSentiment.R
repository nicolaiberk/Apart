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
for (i in c('googledrive', 'data.table', 'dplyr')){usePackage(i)}

# __global vars  -----------------------------------------------------
subsample = 1000 # define sample size, 'full' for full sample

# __Loading Data -----------------------------------------------------
## include code to download random sample 
df <- fread("df_window_30th_dail.csv", encoding = "UTF-8")

if (subsample != 'full'){
  df <- sample_n(df, subsample)
}

df$handcoding <- NA

for (i in (1:nrow(df))){
  print(df$window[i])
  print("IS THE SENTIMENT IN THIS TEXT NEGATIVE [1], NEUTRAL [2], OR POSITIVE [3]?")
  value <- readline(prompt = "Value: ")
  if(value == "quit"){break}else{(df$handcoding[i] <- as.numeric(value)-2)}
}
