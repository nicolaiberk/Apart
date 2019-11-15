"
Construct df window
"

## setup
rm(list=ls())
setwd("~/GitHub/samunico/Apart/data")
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
pkgs <- c('beepr', 'tidyverse', 'rio', 'tidylog', 'skimr',
          'quanteda', 'readtext', 'Hmisc',
          'googledrive', 'readtext', 'data.table', 'stringr', 'qdap'); for (i in pkgs){usePackage(i)}




# __Loading Data -----------------------------------------------------
# Download again vs. subsample file already existing?
# Get Data from: 
# 1) Website 
# 2) Drive
# 3) local
downl <- 3

# subset (if T: sample 1000 cases)
subset <- F

if (downl == 1){
  # These lines downlad the original data from the website; unpack them; and read them into the program
  # Following line not yet working, somehow I can unpack the manually downloaded file, but not the one downloaded via R
  download.file(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/6MZN76/CRUNF0",
                destfile = "Dail_debates_1919-2013.tar.gz") # https://stat.ethz.ch/R-manual/R-devel/library/utils/html/download.file.html
  # unpacking the file
  untar(tarfile = "Dail_debates_1919-2013.tar.gz")

} else if (downl==2) {
  fn <- "~/Internship AffPol in Text/Data/Ireland/Dail_debates_1919-2013.tar.gz"
  drive_get(fn)
  drive_download(file = fn, overwrite = T)
  
  ## unzip
  untar("Dail_debates_1919-2013.tar.gz")

} else if (downl==3) {
  
}

## read data in
dfs <- fread("Dail_debates_1919-2013.tab", 
             sep="\t", 
             quote="", 
             header=TRUE, 
             showProgress = TRUE, 
             data.table=FALSE, 
             verbose = TRUE)



if (subset == T){
  dfs <- dfs[sample(nrow(dfs), 1000), ]
}

dfs$date <- as.Date(dfs$date) # making sure that date variable is in date format

# 2. Corpus preparation ----------------------------------------------------------
corpus <- corpus(dfs, text_field = "speech", docid_field = "speechID")

## Tokenize corpus, and remove stop punctuation and stopwords
tokens <- tokens(corpus, remove_punct = T)
tokens <- tokens_select(tokens, stopwords('english'), selection='remove')


# 3. Entity Detection --------------------

## update data
if (downl < 3){
  fn <- "~/Internship AffPol in Text/Data/Ireland/entities_full_Dail.csv"
  drive_get(fn)
  drive_download(file = fn, overwrite = T)
}

entities <- read.csv("entities_full_Dail.csv")
entities <- entities[2]
entities <- entities[!is.na(entities)]
## not sure why this is necessary, might even be bad if last name == stopword
# entities <- rm_stopwords(entities, Top25Words, separate = F, strip=T) # remove stop words and punctuation
entities <- tools::toTitleCase(entities) # capitalise all words
entities <- gsub("(O'.)","\\U\\1",entities,perl=TRUE) # capitalise names starting with O'


## Creating windows (match of entities)
kwic <- kwic(tokens, pattern=phrase(entities), window=20, case_insensitive = F)



# Analyses ----------------------------------------------------------


## Create df_window where 1 row = 1 window preserving original docvars
df_window <- merge(dfs, kwic, by.x="speechID", by.y="docname") # THIS DOES NOT WORK
df_window <- paste(dfs$pre, df_window$keyword, df_window$post, sep=" ")


## Write df_window to .csv and upload it to Drive
write.csv(df_window, "df_window.csv")
drive_upload("df_window.csv",
             path="~/Internship AffPol in Text/Data/Ireland/",
             overwrite = T)
