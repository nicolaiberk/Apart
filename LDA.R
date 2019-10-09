# LDA
rm(list = ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
pkgs <- c('googledrive', 'topicmodels','tidytext', 'dplyr', 'quanteda', 'ggplot2'); for (i in pkgs){usePackage(i)}

## download/update data
setwd("~/GitHub/samunico/Apart/data")
fn <- "https://drive.google.com/open?id=1tPCLrJeOTzW2SwbvBx64aKYOFCfKGVJ-"
drive_get(fn)
drive_download(file = fn, overwrite = T)

## load data
dail30 <- read.csv('30th_Dail_incl_positions.csv')

### subset for smoother workflow (get rid of later)
dail30 <- dail30[1:100,]

## transform into document-term-matrix
dail30_dtm <- dail30$speech %>%
  as.character() %>%
  dfm(groups = dail30$speechID,
      tolower = T,
      stem = T,
      remove = stopwords('english'),
      # remove frequent words
      remove_punct = T)

dail30_dtm <- dail30_dtm[rowSums(dail30_dtm) > 20, ]



## lda
dail30_lda <- LDA(dail30_dtm, k = 30, control = list(seed = 1234))

## generate term probabilities per topic
dail30_topics <- tidy(dail30_lda, matrix = "beta")

dail30_top_terms <- dail30_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

dail30_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

ggsave('plots/LDA_topwords.png', width = 1000, height = 500, units = 'mm')

## extract probability of each topic for each document


## add topics to initial dataframe, based on speechID's
