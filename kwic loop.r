# Title: Keyword in Context & Sentiment
# Context: APART
# Author: Philipp M.
# Date: Sat Nov 16 20:57:36 2019
# 0. Content ------------------------------------------------------------
# 1. Preparation
# 2. Data Cleaning
# 3. Analyses
#
#
# 1. Preparation --------------------------------------------------------
# __Loading Packages -------------------------------------------------
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('beepr', 'tidyverse', 'rio', 'tidylog', 'clipr', 'quanteda', 'data.table', 'rlist', 
            'parallel', 'doSNOW', 'foreach', 'sentimentr', 'progress')){
  usePackage(i)}

# __Loading Data -----------------------------------------------------
dfo <- data.table::fread("dail_full.csv", encoding = "UTF-8", verbose = T); beep(10)
entities <- data.table::fread("EntitiesDict.csv", encoding = "UTF-8", verbose = T)

# __Global Variables -----------------------------------------------------
SUBS <- T # Subset or Full
    P <- 1 # change either n or proportion in decimals, e.g. 0.01
    LP <- 25 # start of data frame in terms of legislative periods / Dails
    
# __Defining Analysis set -----------------------------------------------------
if(SUBS){
  if(P>1){df <- dfo %>% tidylog::filter(legper>=LP) %>% tidylog::group_by(legper) %>% dplyr::sample_n(P) %>% dplyr::ungroup() %>% as_tibble()
  } else {df <- dfo %>% tidylog::filter(legper>=LP) %>% tidylog::group_by(legper) %>% dplyr::sample_frac(P) %>% dplyr::ungroup() %>% as_tibble()}
} else {df <- dfo %>% tidylog::filter(legper>=13); LP <- 1}
df$legper %>% table(useNA = "always") # double check
    rm(dfo)

# __ Prepare Data Slices for parallel -----------------------------------------------------
    # Slice task for foreach looping: # has to happen within each thingy.
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

# 2. Data cleaning -----------------------------------------------------
# remove everything with parenthesis:
system.time(df$speech <- df$speech %>% stringr::str_remove_all("\\([^\\(\\)]*\\)")); beep(10)


# 2. KWIC -----------------------------------------------------
# Set up Progress bar ####
    numCores<-detectCores(); cl <- makeCluster(numCores);registerDoSNOW(cl)
    # progress bar ------------------------------------------------------------
    iterations <- df$randomslice %>% max() # Number of iterations
    pb <- progress_bar$new(
      format = "legper = :letter [:bar] :elapsed | eta: :eta",
      total = iterations,   
      width = 60)
    progress_letter <- rep(LETTERS[1:5],(iterations/5)+1)[1:iterations]  # token reported in progress bar
    progress <- function(n){pb$tick(tokens = list(letter = progress_letter[n]))} # allowing progress bar to be used in foreach
    opts <- list(progress = progress)
#####

# Ready, Set, Go!!!
start <- Sys.time()
# i <- 14
windowsize <- 50
# setup parallel backend to use many processors
# cores <- detectCores(); cl <- makeCluster(cores[1]-1); registerDoParallel(cl)
# Actual loop
df_windows <- foreach (i = 1:iterations,
                       .combine = rbind,
                       .packages = c('tidyverse', 'quanteda'),
                       # .verbose = T,
                       .options.snow = opts
                       ) %dopar% {
                         dfs <- df %>% filter(randomslice==i) %>% mutate(speechID = speechID %>% as.character)
                         currentdail <- dfs$legper %>% unique 
                         entity <- entities %>% filter(legper==currentdail)
                         entits <- c(entity$match, entity$alternativematch, entity$party_name) %>% unique #entity$nicknamematch
                         entits <- entits[!entits %in% c("", "Independent")]
                         dfs %>% corpus(text_field = 'speech', docid_field = 'speechID') %>% 
                           tokens(remove_punct = F) %>% 
                           kwic(pattern=phrase(entits), window=windowsize, case_insensitive = F) #%>% 
                           # merge(dfs,., by.x = "speechID", by.y="docname")
                       }; print(Sys.time()-start); stopCluster(cl); beep(10)

df_windows <- merge(df, df_windows, by.x = "speechID", by.y="docname")

# backup <- df_windows


# #####
# tempppp <- .Last.value
# tempppp <- tempppp %>% mutate(window=paste(pre, pattern, post))
# tempppp$window[!(tempppp$window %>% grepl("Count Plunkett",.))]
# #####

# Creating unique Window ID
df_window <- df_windows %>% group_by(speechID) %>% mutate(row = row_number()) %>% ungroup() %>% mutate(window_id = paste(speechID, row, sep="_")) %>% select(-row)


# Saving Data -------------------------------
data.table::fwrite(df_window,"df_windows_full_12-03.csv")
data.table::fwrite(df_window,"df_windows_full_12-03.csv.gz")
googledrive::drive_upload(media = "df_windows_full_12-03.csv.gz",
                          path = "~/Internship AffPol in Text/Data/Ireland/",
                          overwrite = T)


# Visualizing Distribution of window --------
df_window %>% ggplot(.,aes(x=legper)) + 
  geom_histogram(bins = max(df_window$legper,na.rm=T)) + 
  scale_x_continuous(breaks = 1:max(df_window$legper))


# -----
df_window_sub <- df_window %>%  sample_n(1000)
# Windows for Sentiment Analysis

print("Preparation for Sentiment Analyses"); start <- Sys.time()
df_window_13_sub$window <- paste(df_window_13_sub$pre, df_window_13_sub$keyword, df_window_13_sub$post, sep=" ")
df_window_13_sub$window_noname <- paste(df_window_13_sub$pre, df_window_13_sub$post, sep=" ")
corpus_window <- corpus(df_window_13_sub, text_field = 'window_noname')
Sys.time()-start

# Sentiment Analyses ------------------------
# 1. LSD Lexicoder Sentiment Dictionary
print("1. Lexicoder Sentiment Analysis"); start <- Sys.time()
sent_analysis <- dfm(corpus_window, dictionary=data_dictionary_LSD2015[1:4]) 
df_window_13_sub <- cbind(df_window_13_sub, quanteda::convert(sent_analysis, to="data.frame"))
df_window_13_sub$ntoken_window <- ntoken(df_window_13_sub$window_noname) 
df_window_13_sub$weighted_lsd <- (df_window_13_sub$positive - df_window_13_sub$negative)/df_window_13_sub$ntoken_window
df_window_13_sub$unweighted_lsd <- (df_window_13_sub$positive - df_window_13_sub$negative)
df_window_13_sub$unweighted_log <- log((df_window_13_sub$positive + 0.5)/(df_window_13_sub$negative + 0.5))
df_window_13_sub$weighted_log <- (log((df_window_13_sub$positive + 0.5)/(df_window_13_sub$negative + 0.5)))/df_window_13_sub$ntoken_window
Sys.time()-start

# 2. sentimentr
print("2. SentimentR Sentiment Analysis"); start <- Sys.time()
sentences_sentr <- get_sentences(df_window_13_sub$window_noname)
sent_r <- sentiment(sentences_sentr)
df_window_13_sub$unweighted_sentr <- sent_r$sentiment
df_window_13_sub$weighted_sentr <- sent_r$sentiment/df_window_13_sub$ntoken_window
Sys.time()-start

# ranks
df_window_13_sub <- df_window_13_sub %>% ungroup %>% 
  mutate(
    percentrank_weighted_lsd = percent_rank(weighted_lsd),
    percentrank_unweighted_lsd = percent_rank(unweighted_lsd),
    percentrank_unweighted_log = percent_rank(unweighted_log),
    percentrank_weighted_log = percent_rank(weighted_log),
    percentrank_unweighted_sentr = percent_rank(unweighted_sentr),
    percentrank_weighted_sentr = percent_rank(weighted_sentr))
fwrite(df_window_13_sub, "window_sub.csv")


## Trashfile
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