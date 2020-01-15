# Title: Stratified sampling for sentiment validation
# Context: APART
# Author: Nicolai B.
# Date: Fri Jan 15 12:10 2020
# 0. Content ---------------------------------------------------------
#
#
# 1. Preparation -----------------------------------------------------
# __Loading Packages -------------------------------------------------
setwd("~/GitHub/samunico/Apart/data")
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('dplyr', 'beepr')){usePackage(i)}; rm(i, usePackage)

# __global vars  -----------------------------------------------------
load <- T # should dataset be downloaded?
prop <-  0.01 # proportion of original data to be sampled

# __Loading Data -----------------------------------------------------
df <- data.table::fread("df_windows_full_12-03.csv", encoding = "UTF-8", verbose = T); beep(10)


# 2. Sampling --------------------------------------------------------
set.seed(312) # ensures reproducability
sample <- df %>% 
  group_by(legper) %>% 
  sample_frac(prop)

# generate single string
sample$text <- paste(sample$pre, sample$keyword, sample$post)

# 3. write to csv

# general sample
data.table::fwrite(sample,"StratSample_01-15.csv"); beep(10)

# figure eight sample
f8_sample <- as.data.frame(sample)[, c('window_id', 'text', 'keyword')]
data.table::fwrite(f8_sample,"f8Val_StratSample_01-15.csv"); beep(10)
