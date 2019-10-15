# LDA
rm(list = ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
pkgs <- c('googledrive', 'topicmodels','tidytext', 'dplyr', 'quanteda', 'ggplot2', 'ldatuning', 'tidyr'); for (i in pkgs){usePackage(i)}

## download/update data
setwd("~/GitHub/samunico/Apart/data")
fn <- "https://drive.google.com/open?id=1tPCLrJeOTzW2SwbvBx64aKYOFCfKGVJ-"
drive_get(fn)
drive_download(file = fn, overwrite = T)

## load data
dail30 <- read.csv('30th_Dail_incl_positions.csv')

### subset for smoother workflow (get rid of later)
dail30 <- dail30[sample(nrow(dail30), size = 1000, replace = F),]

## transform into document-term-matrix
dail30_dtm <- dail30$speech %>%
  as.character() %>%
  dfm(groups = dail30$speechID,
      tolower = T,
      stem = T,
      remove = stopwords('english'),
      remove_punct = T) %>%
  dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

dail30_dtm <- dail30_dtm[rowSums(dail30_dtm) > 10, ]



## get perfect n of topics
result <- FindTopicsNumber(
  dail30_dtm,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

#### elbow plot
FindTopicsNumber_plot(result)

ggsave('plots/LDA_bestfit.png', width = 400, height = 150, units = 'mm')



## lda
dail30_lda <- LDA(dail30_dtm, k = 37, control = list(seed = 1234))

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
  scale_x_reordered() + 
  theme(text = element_text(size=20))

ggsave('plots/LDA_topwords.png', width = 1000, height = 500, units = 'mm')



## extract probability of each topic for each document
dail30_documents <- tidy(dail30_lda, matrix = "gamma")

### rows to columns
dail30_wide <- spread(dail30_documents, topic, gamma)

### sort
dail30 <- dail30[order(dail30$speechID), ]
dail30_wide <- dail30_wide[order(dail30_wide$document), ]
dail30_wide$document <- as.integer(dail30_wide$document)
dail30 <- dail30[dail30$speechID %in% dail30_wide$document, ] # remove dropped obs (due to short speech)

### add topics to initial dataframe, based on speechID's
dail30_topics <- cbind(dail30, dail30_wide)
write.csv(dail30_topics, file = "dail30_topics.csv")
drive_upload("dail30_topics.csv", 
             path = "https://drive.google.com/open?id=1jOF7BOTbzJwDXALbYte2uwXimfkFqwQF/",
             overwrite = T)