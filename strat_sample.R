# Title: Stratified sampling for sentiment validation
# Context: APART
# Author: Nicolai B.
# Date: Fri Jan 15 12:10 2020
# 0. Content ---------------------------------------------------------
#
#
# 1. Preparation -----------------------------------------------------
# __Loading Packages -------------------------------------------------
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('dplyr', 'beepr')){usePackage(i)}; rm(i, usePackage)

# __preprocess full set------------------------------------------------
setwd("C:/Users/samunico/Dropbox/Studium/Amsterdam/Studies/Semester 3/Block I/Internship/data")
dta <- readRDS("df_windows_full_2020-01-22_x2.rds"); beep(10)
dta_r <- dta %>% select(legper, keyword, window_for_sent, window_id)
setwd("~/GitHub/samunico/Apart/data")
write.csv(dta_r, "windows_df_230120.csv"); beep(10)



# __global vars  -----------------------------------------------------
prop <-  0.01 # proportion of original data to be sampled

# __Loading Data -----------------------------------------------------
df <- data.table::fread("windows_df_230120.csv", encoding = "UTF-8", verbose = T); beep(10)


# 2. Sampling --------------------------------------------------------
set.seed(312) # ensures reproducability
sample <- df %>% 
  group_by(legper) %>% 
  sample_frac(prop)

# 3. write to csv

# general sample
data.table::fwrite(sample,"StratSample_01-23.csv"); beep(10)

# figure eight sample
f8_sample <- as.data.frame(sample)[, c('window_id', 'window_for_sent', 'keyword')]
data.table::fwrite(f8_sample,"f8Val_StratSample_01-23.csv"); beep(10)
