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
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
for (i in c('beepr', 'tidyverse', 'rio', 'tidylog', 'clipr', 'quanteda', 'data.table', 'rlist', 'foreach', 'doParallel', 'sentimentr')){
  usePackage(i)}


# __Loading Data -----------------------------------------------------
dfo <- data.table::fread("dail_full.csv", encoding = "UTF-8", verbose = T); beep(10)
entities <- data.table::fread("EntitiesDict.csv", encoding = "UTF-8", verbose = T)


# __Global Variables -----------------------------------------------------
SUBS <- F # Subset or Full
P <- 0.01 #change either n or proportion in decimals, e.g. 0.01


# __Defining Analysis set -----------------------------------------------------
if(SUBS){
  if(P>1){df <- dfo %>% sample_n(P) %>% as.tibble()
  } else {df <- dfo %>% sample_frac(P) %>% as.tibble()}
} else {df <- dfo}


# 2. Data cleaning -----------------------------------------------------
# kick out (Interruption[s])
temp <- df$speech %>% str_extract("\\(.*\\)"); beep(10)
tempp <- temp[!is.na(temp)]; beep(10)
unique(tempp)
temp2 <- temp %>% unique
tempp <- tempp %>% table(); beep(10)
tempp %>% sort(decreasing = T)
# str_remove_all("\\((.*?)\\)")
# str_remove_all("Count |Mr. |Dr. |Professor |General |Ms. |Countess |Sir |Capt. |Major |Mrs. |Colonel | RIP")


# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
#
## Run KWIC by Legislative period!
# Ready, Set, Go!!!
start <- Sys.time()
df_windows <- foreach (i=2:max(df$legper), # 
                       .combine = "rbind",
                       .packages=c('tidyverse', 'quanteda')) %dopar% {
                         dfs <- df %>% filter(legper==i) %>% mutate(speechID = speechID %>% as.character)
                         entity <- entities %>% filter(legper==i)
                         #entity$nicknamematch
                         entits <- c(entity$match, entity$alternativematch, entity$party_name) %>% unique
                         entits <- entits[!entits == ""]
                         dfs %>% corpus(text_field = 'speech', docid_field = 'speechID') %>% 
                           tokens(remove_punct = T) %>% 
                           kwic(pattern=phrase(entits), window=20, case_insensitive = F) %>% 
                           merge(dfs,., by.x = "speechID", by.y="docname")
                       }; print(Sys.time()-start); stopCluster(cl)



# Creating unique Window ID
df_window <- df_windows %>% group_by(speechID) %>% mutate(row = row_number()) %>% ungroup() %>% mutate(window_id = paste(speechID, row, sep="_")) %>% select(-row)

# Saving Data
data.table::fwrite(df_window,"df_windows_full.csv")
data.table::fwrite(df_window,"df_windows_full.csv.gz")
googledrive::drive_upload(media = "df_windows_full.csv.gz",
                          path = "~/Internship AffPol in Text/Data/Ireland/",
                          overwrite = T)

df_window %>% ggplot(.,aes(x=legper)) + 
  geom_histogram(bins = max(df_window$legper,na.rm=T)) + 
  scale_x_continuous(breaks = 1:max(df_window$legper))

df_window_13 <- df_window %>% filter(legper>12)
df_window_13_sub <- df_window_13 %>%  sample_n(1000)


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

df_window_13_sub %>% names



## Trashfile
# # Use Stopwords?
# stopp <- stopwords('english')
# stopws <- stopp[!stopp %in% c("not", "very")]
# stopwords('english')[!stopwords('english') %in% c("not", "very")]