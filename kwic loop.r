# Title: Keyword in Context & Sentiment
# Context: APART
# Author: Philipp M.
# Date: Sat Nov 16 20:57:36 2019
# 0. Content ------------------------------------------------------------
# 1. Preparation
# 2. Recoding
# 3. Analyses
#
#
# 1. Preparation --------------------------------------------------------
# __Loading Packages -------------------------------------------------
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('beepr', 'tidyverse', 'rio', 'tidylog', 'clipr', 'quanteda', 'data.table', 'rlist', 'foreach', 'doParallel', 'sentimentr')){
  usePackage(i)}

# __Loading Data -----------------------------------------------------
df <- fread("dail_full.csv", encoding = "UTF-8", verbose = T)
entities <- fread("EntitiesDict.csv", encoding = "UTF-8", verbose = T)



# KWIC --------------------------------------

# Create Subset for testing

df_sub <- df %>% group_by(legper) %>% 
  mutate(take = sample(c(T,F), replace=TRUE, size=n(), prob = c((p=0.01), 1-p))) %>% # (40/n())
  ungroup() %>% filter(take) %>% as.tibble() 

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]) #not to overload your computer
registerDoParallel(cl)
#

## Run KWIC by Legislative period!
# Ready, Set, Go!!!
start <- Sys.time()
df_windows <- foreach (i=13:16, #max(df$legper)
                       .combine = "rbind",
                       .packages=c('tidyverse', 'quanteda')) %dopar% {
  dfs <- df %>% filter(legper==i)
  entity <- entities %>% filter(legper==i) %>% select(match, alternativematch, alternativematch, fullname, party_name)
  entits <- entity[[1]]
  for (j in 2:ncol(entity)){
    entits <- c(entits, entity[[j]])
  }; entits <- entits %>% unique
  entits <- entits[!entits == ""]
  corpus <- corpus(dfs, text_field = 'speech', docid_field = 'speechID')
  tokens <- tokens(corpus, remove_punct = T)
  # tokens <- tokens_select(tokens, stopws, selection='remove')
  kwic  <- kwic(tokens, pattern=phrase(entits), window=20, case_insensitive = F)
  if (nrow(kwic)>0) {
  dfs <- dfs %>% mutate(speechID = speechID %>% as.character)
  # df_windows[[i]] <- inner_join(dfs %>% mutate(speechID = speechID %>% as.character), 
  #                               kwic, by= c("speechID"="docname"))
  df_windows <- merge(dfs, kwic, by.x = "speechID", by.y="docname")
  }
  df_windows
# }
}; print(Sys.time()-start); stopCluster(cl)

# Creating unique Window ID
df_window <- df_windows %>% group_by(speechID) %>% mutate(row = row_number()) %>% ungroup() %>% mutate(window_id = paste(speechID, row, sep="_")) %>% select(-row)

# Windows for Sentiment Analysis
print("Preparation for Sentiment Analyses"); start <- Sys.time()
df_window$window <- paste(df_window$pre, df_window$keyword, df_window$post, sep=" ")
df_window$window_noname <- paste(df_window$pre, df_window$post, sep=" ")
corpus_window <- corpus(df_window, text_field = 'window_noname')
Sys.time()-start

# Sentiment Analyses ------------------------
# 1. LSD Lexicoder Sentiment Dictionary
print("1. Lexicoder Sentiment Analysis"); start <- Sys.time()
sent_analysis <- dfm(corpus_window, dictionary=data_dictionary_LSD2015[1:4]) 
df_window <- cbind(df_window, convert(sent_analysis, to="data.frame"))
df_window$ntoken_window <- ntoken(df_window$window_noname) 
df_window$original_lsd <- (df_window$positive - df_window$negative)/df_window$ntoken_window
df_window$unweighted_lsd <- (df_window$positive - df_window$negative)
df_window$original_log <- log((df_window$positive + 0.5)/(df_window$negative + 0.5))
df_window$weighted_log <- (log((df_window$positive + 0.5)/(df_window$negative + 0.5)))/df_window$ntoken_window
Sys.time()-start

# 2. sentimentr
print("2. SentimentR Sentiment Analysis"); start <- Sys.time()
sentences_sentr <- get_sentences(df_window$window_noname)
sent_r <- sentiment(sentences_sentr)
df_window$unweighted_sentr <- sent_r$sentiment
df_window$weighted_sentr <- sent_r$sentiment/df_window$ntoken_window
Sys.time()-start

# ranks
df_window <- df_window %>% 
  mutate(
    percentrank_original_lsd = percent_rank(original_lsd),
    percentrank_unweighted_lsd = percent_rank(unweighted_lsd),
    percentrank_original_log = percent_rank(original_log),
    percentrank_weighted_log = percent_rank(weighted_log),
    percentrank_unweighted_sentr = percent_rank(unweighted_sentr),
    percentrank_weighted_sentr = percent_rank(weighted_sentr))




## Trashfile
# # Use Stopwords?
# stopp <- stopwords('english')
# stopws <- stopp[!stopp %in% c("not", "very")]
# stopwords('english')[!stopwords('english') %in% c("not", "very")]