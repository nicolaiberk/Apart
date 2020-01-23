# Title: Keyword in Context & Sentiment
# Context: APART
# Author: Philipp M.
# Date: Sat Nov 16 20:57:36 2019
# ---------------------------------------- 0. Content --------------------------------------
# 1. Preparation
# 2. Data Cleaning
# 3. Analyses
#
#
# ---------------------------------------- 1. Preparation --------------------------------------
# __Loading Packages -------------------------------------------------
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('beepr', 'tidyverse', 'rio', 'tidylog', 'clipr', 'quanteda', 'data.table', 'rlist', 
            'parallel', 'doSNOW', 'foreach', 'sentimentr', 'progress', 'inops')){
  usePackage(i)}

# __Loading Data -----------------------------------------------------
dfo <- data.table::fread("dail_full.csv", encoding = "UTF-8", verbose = T); beep(10)
entities <- data.table::fread("EntitiesDict.csv", encoding = "UTF-8", verbose = T)

# ---------------------------------------- 2. KWIC --------------------------------------
# __Global Variables -----------------------------------------------------
SUBS <- F # Subset or Full
    P <- 1 # change either n or proportion in decimals, e.g. 0.01
    LP <- 13 # start of data frame in terms of legislative periods / Dails
W <-  70 # Windowsize
# Upload to drive?
drive <- F

# __Defining Analysis set -----------------------------------------------------
if(SUBS){
  if(P>1){df <- dfo %>% tidylog::filter(legper>=LP) %>% tidylog::group_by(legper) %>% dplyr::sample_n(P) %>% dplyr::ungroup() %>% as_tibble()
  } else {df <- dfo %>% tidylog::filter(legper>=LP) %>% tidylog::group_by(legper) %>% dplyr::sample_frac(P) %>% dplyr::ungroup() %>% as_tibble()}
} else {df <- dfo %>% tidylog::filter(legper>=13); LP <- 13}
df$legper %>% table(useNA = "always") # double check
    rm(dfo)

# __Data cleaning -----------------------------------------------------
# remove everything with parenthesis:
system.time(df$speech <- df$speech %>% 
              stringr::str_remove_all("\\([^\\(\\)]*\\)") %>% 
              stringr::str_remove_all("\\[[^\\(\\)]*\\]")); beep(10)

# __Setup Parallel Processing -----------------------------------------------------
## Slice task for foreach looping: # has to happen within each thingy (legper?).
x <- 3000 # size of slice
lp <- df$legper %>% unique %>% sort()
for (i in lp){
  if (i == lp[1]){j <- 1}
  y <- df %>% filter(legper==i) %>% nrow
  z <- (y/x) %>% ceiling()
  ju <- z+j-1
  df$randomslice[df$legper==i] <- rep(j:ju,x)[1:y] %>% sample()
  j <- z+j
}
# df$randomslice %>% unique %>% sort()    

# For subsampling
# fwrite(df, "n1000_subsample_13-30thdail_raw.csv")

# __Progress bar -----------------------------------------------------
  iterations <- df$randomslice %>% max() # Number of iterations
  pb <- progress_bar$new(
    format = "legper = :letter [:bar] :elapsed | eta: :eta",
    total = iterations,   
    width = 60)
  progress_letter <- rep(LETTERS[1:5],(iterations/5)+1)[1:iterations]  # token reported in progress bar
  progress <- function(n){pb$tick(tokens = list(letter = progress_letter[n]))} # allowing progress bar to be used in foreach
  opts <- list(progress = progress)

# __Actual Loop -----------------------------------------------------
# Ready, Set, Go!!!
# setup parallel backend to use many processors
numCores<-detectCores(); cl <- makeCluster(numCores-1);registerDoSNOW(cl)
start <- Sys.time()

# Actual loop
df_windows <- foreach (i = 1:iterations,
                       .combine = rbind,
                       .packages = c('tidyverse', 'quanteda'),
                       # .verbose = T,
                       .options.snow = opts
                       ) %dopar% {
                         dfs <- df %>% filter(randomslice==i) %>% mutate(speechID = speechID %>% as.character)
                         currentdail <- dfs$legper %>% unique 
                         # Searchterms
                         entits <- c("Esmonde") # to test for specific matches
                         # entity <- entities %>% filter(legper==currentdail)
                         # entits <- c(entity$match, 
                         #             entity$alternativematch, 
                         #             entity$party_name[!entity$party_name %in% "Independent"]) %>% unique #entity$nicknamematch
                         # entits <- entits[!entits %in% c("", "Independent")]
                         dfs %>% corpus(text_field = 'speech', docid_field = 'speechID') %>% 
                           tokens(remove_punct = F) %>% 
                           kwic(pattern=phrase(entits), window=W, case_insensitive = F) 
                         # %>% merge(dfs,., by.x = "speechID", by.y="docname") # matching can either happen in the loop or after the loop
                       }; print(Sys.time()-start); stopCluster(cl); beep(10)

df_windows1 <- right_join(df, df_windows %>% mutate(speechID = as.numeric(docname)), by = c("speechID"))


df_windows1 <- merge(df, df_windows, by.x = "speechID", by.y="docname") # this was docname...

df_windows %>% names()
df_windows1 %>% names()

# backup <- df_windows

# ###
# tempppp <- .Last.value
# tempppp <- tempppp %>% mutate(window=paste(pre, pattern, post))
# tempppp$window[!(tempppp$window %>% grepl("Count Plunkett",.))]
# ###

# Creating unique Window ID
df_window <- df_windows %>% group_by(speechID) %>% mutate(row = row_number()) %>% ungroup() %>% mutate(window_id = paste(speechID, row, sep="_")) %>% select(-row)


# ---------------------------------------- Saving Data --------------------------------
data.table::fwrite(df_window,"df_windows_full_12-05.csv")
data.table::fwrite(df_window,"df_windows_full_12-05.csv.gz")
if (drive){
googledrive::drive_upload(media = "df_windows_full_12-05.csv.gz",
                          path = "~/Internship AffPol in Text/Data/Ireland/",
                          overwrite = T)1;}

## Getting the data

# __Viz Dist of Windows X Legper --------
df_window %>% ggplot(.,aes(x=legper)) + 
  geom_histogram(bins = max(df_window$legper,na.rm=T)) + 
  scale_x_continuous(breaks = 1:max(df_window$legper))

# ---------------------------------------- Windows for Sentiment Analysis --------------------------------

# Load Data from saved file
# df_window <- fread("df_windows_full_12-03.csv", encoding = "UTF-8")

## Position of Match in Window
print("Preparation for Sentiment Analyses"); start <- Sys.time()
df_window$window <- paste(df_window$pre, df_window$pattern, df_window$post, sep=" ")
df_window$ntoken_speech <- ntoken(df_window$speech); beep(8)
df_window$from_token_window <- ifelse(df_window$from<(W+1),
                                      df_window$from,
                                      W+1); beep(8)
df_window$ntokenofmatch <- ntoken(df_window$pattern); beep(8)
df_window$to_token_window <- df_window$from_token_window+df_window$ntokenofmatch-1
fwrite(df_window,"df_windows_full_12-05_01.csv"); beep(8)
# df_window <- fread("df_windows_full_12-03_01.csv", encoding = "UTF-8")

## DOUBLE CHECK HERE
# for (i in 1:7){(df_window$window[i] %>% str_split(" ") %>% unlist)[df_window$from_token_window[i]:df_window$to_token_window[i]]}

## Sentence-ise the window 
df_window$window_sentences <- get_sentences(df_window$window); beep(8)
saveRDS(df_window, "df_windows_full_12-05_02.rds"); beep(8) # because we have a data.table with a list, we better use rds instead of csv!

# df_window <- readRDS("df_windows_full_12-03_02.rds")
df_window$n_sentences <- sapply(df_window$window_sentences, length); beep(8) # n of sentences in each window
numCores <- detectCores(); cl <- makeCluster(numCores-1)
system.time(ntokenssentences <- parLapply(cl, df_window$window_sentences, ntoken)); beep(8)
ntokenssentences %>% saveRDS("ntokenssentences_full.rds")
ntokenssentences1 <- lapply(ntokenssentences, as.numeric)
df_window$n_token_sentence <- lapply(ntokenssentences1,cumsum); beep(8) # cumulative sum of tokens in each sentence of each window
saveRDS(df_window, "df_windows_full_12-05_03.rds"); beep(8) # because we have a data.table with a list, we better use rds instead of csv!

## Getting sentence index of where match occurs
df_window$temp_list <- apply(df_window, 1,function(x) {x$n_token_sentence-x$from_token_window}); beep(8)
df_window$match_sentence_index <- as.numeric(lapply(df_window$temp_list, function(x) {which(x==min(x[x>=0]))})); beep(8)

## Sentence before & after match 
df_window$window_for_sent <- apply(df_window, 1, function(x) {
  a <- 0
  if(x$match_sentence_index>1){a <- c(a,-1)}
  if(x$match_sentence_index<x$n_sentences){a <- c(a,+1)}
  a <- sort(a)
  x$window_sentences[x$match_sentence_index+a] %>% paste0(collapse=" ")
}
)

df_window$window_noname <- df_window %>% apply(1,function(x){str_remove(x$window,x$pattern)})
Sys.time()-start;beep(8)
saveRDS(df_window, "df_windows_full_12-05_x.rds"); beep(10)

# Sentiment Analyses ------------------------
# 1. LSD Lexicoder Sentiment Dictionary
# print("1. Lexicoder Sentiment Analysis"); start <- Sys.time()
# sent_analysis <- dfm(corpus_window, dictionary=data_dictionary_LSD2015[1:4]) 
# df_window <- cbind(df_window, quanteda::convert(sent_analysis, to="data.frame"))
# df_window$ntoken_window <- ntoken(df_window$window_noname) 
# df_window$weighted_lsd <- (df_window$positive - df_window$negative)/df_window$ntoken_window
# df_window$unweighted_lsd <- (df_window$positive - df_window$negative)
# df_window$unweighted_log <- log((df_window$positive + 0.5)/(df_window$negative + 0.5))
# df_window$weighted_log <- (log((df_window$positive + 0.5)/(df_window$negative + 0.5)))/df_window$ntoken_window
# Sys.time()-start

# 2. sentimentr
# backup <- df_window
# df_window <- readRDS("df_windows_full_12-05_x.rds")
print("2. SentimentR Sentiment Analysis"); start <- Sys.time()
# sentences_sentr <- get_sentences(df_window$window_noname)

sentences <- get_sentences(df_window$window_noname); beep(8)
saveRDS(sentences, "sentences_noname_full.rds"); beep(10)
df_window$unweighted_sentr <- sentiment_by(sentences)[,4]; beep(8)
saveRDS(df_window, "df_windows_full_12-05_x1.rds");beep(10)
df_window$ntoken_window <- quanteda::ntoken(df_window$window_noname)
df_window$weighted_sentr <- df_window$unweighted_sentr/df_window$ntoken_window
saveRDS(df_window, "df_windows_full_12-05_x2.rds");beep(10)
# Sys.time()-start

saveRDS(df_window, "df_windows_full_12-05_x2.rds");beep(10)

# ranks
df_window <- df_window %>% ungroup %>% 
  mutate(
    percentrank_weighted_lsd = percent_rank(weighted_lsd),
    percentrank_unweighted_lsd = percent_rank(unweighted_lsd),
    percentrank_unweighted_log = percent_rank(unweighted_log),
    percentrank_weighted_log = percent_rank(weighted_log),
    percentrank_unweighted_sentr = percent_rank(unweighted_sentr),
    percentrank_weighted_sentr = percent_rank(weighted_sentr))
fwrite(df_window, "window_sub.csv")


## Trashfile -----
# # Use Stopwords?
# stopp <- stopwords('english')
# stopws <- stopp[!stopp %in% c("not", "very")]
# stopwords('english')[!stopwords('english') %in% c("not", "very")]


# to create unique pattern identifier
tempp <- fread("dail_full_nospeech.csv.gz")

getXbeforeKWIC <- function(df,z){ 
  from <- match("from",names(df)) # get index of "from" column
  speech <- match("speech",names(df)) # get index of "from" column
  apply(df, 1, function(x) {
    if(as.numeric(x[from])<(z+1)) {a <- 0} else {a <- as.numeric(x[from])-1}
    quanteda::tokens(x[speech])[[1]][(a-z):(a)]} %>% t %>% apply(1,paste,collapse=" ")
  )}
getXbeforeKWIC(tempp, 4)


apply(tempppp, 1, print)
tempp <- tempp %>% group_by(speechID) %>% mutate(row = row_number()) %>% ungroup() %>% mutate(window_id = paste(speechID, row, sep="_")) %>% select(-row)
tempp <- tempp %>% group_by(window_id) %>% mutate(window = paste(pre, pattern,post, collapse=" "))

tempp[1:5,c("pre", "post", "window")]

tempp <- tempp %>% mutate(window = paste(sentence[[match-1]], ,post, collapse=" "))



apply(df_window[1:5,], 1, function(x) {
  a <- 0
  if(x$match_sentence_index>1){a <- c(a,-1)}
  if(x$match_sentence_index<x$n_sentences){a <- c(a,+1)}
  x$window_sentences[x$match_sentence_index+a] %>% paste0()
}
)


tempp <- tempp %>% mutate(pos_in_window)
tempp$window %>% head


## Getting all the expressions in parentheses
system.time(temp <- df$speech %>% str_extract_all("\\([^\\(\\)]*\\)")); beep(10)
tempp <- temp %>% unlist; beep(10)
temp2 <- tempp %>% unique; beep(10)

tempp <- tempp %>% table(); beep(10)
temppt <- tempp %>% sort(decreasing = T)
temppt %>% head(400)
temp2[grepl("\\. | \\! |\\?", temp2)]





### TRASH -
####
df_window$randomslice <- rep(1:10000,(nrow(df_window)/10000) %>% ceiling)[1:nrow(df_window)] %>% sample()

numCores<-detectCores(); cl <- makeCluster(numCores);registerDoSNOW(cl)
# progress bar ------------------------------------------------------------
iterations <- df_window$randomslice %>% max() 
pb <- progress_bar$new(format = "iteration = :letter [:bar] :elapsed | eta: :eta",total = iterations,width = 60)
progress_letter <- 1:1000
progress <- function(n){pb$tick(tokens = list(letter = progress_letter[n]))} 
opts <- list(progress = progress)
#####
start <- Sys.time()
test <- foreach (i = 1:100,.combine = rbind,.packages = c('quanteda'),.verbose = T,.options.snow = opts
) %dopar% {
  ntoken(df_window$speech)
};beep(8);Sys.time()-start;stopCluster(cl)
####