# Title: Network DF construction & Visualization
# Context: APART
# Author: P. Mendoza
# Date: Fri Oct 09 2019
# Dataset used: Herzog, Alexander; Mikhaylov, Slava, 2017, "Dail_debates_1919-2013.tar.gz", Database of Parliamentary Speeches in Ireland, 1919-2013, https://doi.org/10.7910/DVN/6MZN76/CRUNF0, Harvard Dataverse, V2
# 0. Content ------------------------------------------------------------
#  1. Preparation
#
#
#
# 1. Preparation --------------------------------------------------------
# __Loading Packages -------------------------------------------------
rm(list=ls())
usePackage <- function(p) {if (!is.element(p, installed.packages()[,1]))install.packages(p,dep = TRUE, repos = "http://cran.wu.ac.at"); library(p, character.only = TRUE)}
pkgs <- c('beepr', 'tidyverse', 'tidylog','rio', 'tidylog', 'skimr',
          'quanteda', 'readtext', 'clipr',
          'igraph', 'extrafont', 'RColorBrewer',
          'googledrive', 'readtext', 'data.table', 'stringr'); for (i in pkgs){usePackage(i)}

# clipr to read and write clipboard content

# Analyses ----------------------------------------------------------

# whilst we already have the sender partyIDs, we will need a partyID for the recipient!




### GET ONLINE DATA
drive_download("https://drive.google.com/open?id=18MkmIgE8d5kzZRxB_57uW23Ke4amnE1T", overwrite=T)
wind_df <- import("df_window_30th_dail.csv", encoding = "UTF-8")
# entity <- import("entities_30th_Dail.csv", encoding = "UTF-8")

# CORRECT MISSPECIFICATION OF PARTIES:
    # "Mr. Eamon Gilmore" > Worker's party > labour
    # "Mr. Pat Rabbitte" > Democratic Left > labour
    # "Ms. Kathleen Lynch" > Democratic Left > labour
    # "Ms. Liz McManus" > Democratic Left > labour

wind_df$party_name[wind_df$member_name %in% c("Mr. Eamon Gilmore", "Mr. Pat Rabbitte", "Ms. Kathleen Lynch", "Ms. Liz McManus")] <- "The Labour Party"

# Namedictionary take the names from the speakers 
nams <- tibble(name=str_remove_all(as.character(wind_df$member_name %>% unique()), "\\((.*?)\\)") %>% 
                 str_remove_all(" RIP"),
               surname=(str_remove_all(as.character(wind_df$member_name %>% unique()), "\\((.*?)\\)") %>% 
                        str_remove_all(" RIP") %>% strsplit(split=" ") %>% 
                        lapply(function(x) {x[length(x)]}) %>% unlist),
               party=(wind_df[c("member_name", "party_name")] %>% distinct)[,2])
nams$oname <- as.character(wind_df$member_name %>% unique())

# Keep only MP references - Exclude all party/ministry/county references
wind_df_mp <- wind_df %>% filter(pattern %in% nams$surname)

# Check if remaining can be recognized as names
# MPs who were talked about but not talking
(wind_df_mp$pattern %>% unique)[!(wind_df_mp$pattern %>% unique) %in% nams$surname]

# MPs who were talking but not talked about
(nams$surname %>% unique)[!(nams$surname %>% unique) %in% (wind_df_mp$pattern %>% unique)]

# Add surnames to the speakers
wind_df_mp_nams <- left_join(wind_df_mp, nams %>% select(-party),
                             by=c("member_name"="oname"))
names(wind_df_mp_nams)[names(wind_df_mp_nams) == c("name", "surname")] <- c("name_speaker", "surname_speaker")

# MATCHING DIDNT WORK bc there are several names associated with the same surname
# Wait for reparation of dictionary for kwic   ----------------------------------------------------------------------------
# for now I'll kick out all the MP's who share surnames
temp <- wind_df_mp_nams[,c("member_name","surname_speaker")] %>% unique %>% arrange(surname_speaker)
duplics <- (temp$surname_speaker %>% unique)[temp$surname_speaker %>% table>1] %>% sort
df <- wind_df_mp_nams %>% tidylog::filter(!surname_speaker %in% duplics & !pattern %in% duplics)

# Match full names to the recipients
df <- left_join(df, nams,
                by=c("pattern"="surname"))
names(df)[match(c("name", "oname", "party"), names(df))] <- c("name_recipient", "oname_recipient", "party_recipient")


# Create Network dataframe ------------------
# Creating Nodes datasets
nodes_party <- df %>% select(party_name, name_speaker) %>% distinct %>% group_by(party_name) %>% summarize(seats=n())
nodes <- df[c("name_speaker","surname_speaker","party_name")] %>% distinct

# Creating Edges datasets
# Parties
net_df_party <- df %>% 
  group_by(party_name, doc_id, party_recipient) %>%                       # per speech, per outgroup reference, 1 sentiment score.
  summarize(sentiment = mean(sentiment_score, na.rm=T)) %>%   #
  group_by(party_name, party_recipient) %>%                   # per party, per outgourp reference, 1 sentiment score & number of mentions.
  summarize(mentions = n(),                                   #
            sentiment = mean(sentiment, na.rm=T)) %>%         #
  mutate(senti2 = sentiment^2) %>% 
  arrange(senti2) # to have the thinnest lines in the foreground


cv <- function(x, na.rm=F) {sd(x, na.rm=na.rm)/mean(x, na.rm=na.rm)} # Coefficient of Variation Formula
marg <- function(x,z=20) {
  return((log(x*(x>0)+1)/log(z+1))*(x<=z)*(x>0)+(x>z))}

net_df %>% 
  mutate(certaintycv=(1-sentimentcv)*(sentimentcv<1)*(sentimentcv>0),
         certaitnymentions=marg(mentions),
         certainty=certaintycv*certaitnymentions) %>% summary

(net_df$sentimentcv==Inf) %>% table()

net_df$sentiment

# MPs
net_df <- df %>% 
  group_by(name_speaker, name_recipient, doc_id) %>% 
  summarize(sentiment = mean(sentiment_score, na.rm=T),
            sentimentcv = cv(sentiment_score, na.rm=T),
            mentions = n()) %>%
  group_by(name_speaker, name_recipient) %>%
  summarize(mentionssum = sum(mentions),
            mpties= n(),
            sentimentcv = cv(sentiment, na.rm=T),
            sentiment = mean(sentiment, na.rm=T),
            ) %>%
  mutate(senti2 = sentiment^2) %>% 
  arrange(senti2) # to have the thinnest lines in the foreground

# Parties
netp <- graph_from_data_frame(d=net_df_party, directed=T, vertices=nodes_party)

# MPs
net_df_noloop <- net_df %>% filter(!name_recipient==name_speaker) # remove loops # net <- simplify(net, remove.loops = T)
net <- graph_from_data_frame(d = net_df_noloop, vertices = nodes, directed = T)
net <- set.vertex.attribute(net, "name", value=nodes$surname_speaker)
wind_df$party_name %>% unique %>% write_clip()


parties <- tibble(party_name=c("Fianna Fáil", "Fine Gael", "The Labour Party", "Green Party", 
                               "Sinn Féin", "Progressive Democrats", "Independent"),
                  party_color=c("#66BB66", "#6699FF", "#CC0000", "#99CC33", "#326760", "#3333CC", "#DDDDDD"),
                  govt=c("square", "circle", "circle", "square", "circle", "square", "circle"),
                  seats = c(56,46,17,6,5,2,3))

# Calculate degrees
V(net)$outdegree <- degree(net, mode = c("out"), loops = F, normalized = T)
V(net)$indegree <- degree(net, mode = c("in"), loops = F, normalized = T)

## DATAVIZ
usePackage('RColorBrewer')
display.brewer.all()


## create distance matrix for mds algorithm
E(net)$wweight <- E(net)$weight
(E(net)$weight <- (E(net)$sentiment*100+21)) %>% summary()
distmat <- net[]
##

(E(net)$sentiment*-100+21) %>% summary()
layout_with_kk(net, dim=2, weights = (E(net)$sentiment*-100+21)) 

# Color for edges:
rbPal <- colorRampPalette(c('red','green'))
E(net)$sentiment %>% summary()

# the alpha stuff for the edges only works if you run the whole thing for the first time...
# png("APART_net.png", width=200, height=200, res=300, units = "mm", family = "Garamond")
plot.igraph(net,
            # layout=layout_with_kk(net, weights = E(net)$weight)),
            layout=layout_with_mds(net,dist = distmat),
            # vertex.size = 10*V(net)$outdegree,
            vertex.label.family = "Garamond", 
            vertex.label.color = "darkblue", 
            vertex.label.cex = 0.4,
            vertex.color=parties$party_color[match(V(net)$party_name, parties$party_name)],
            vertex.shape=parties$govt[match(V(net)$party_name, parties$party_name)],
            edge.arrow.size=.4,
            edge.color = brewer.pal(11,name="RdBu")[as.numeric(cut(E(net)$sentiment,breaks = 11))],
            )
# dev.off()

a <- plot.igraph(net, 
            vertex.color = "blue",
            vertex.size = nodes$vote_share*100,
            vertex.label.color = "black",
            vertex.label=nodes$party_name,
            edge.curved = 0.15, 
            edge.color = adjustcolor("SkyBlue2", alpha.f = .7),
            vertex.label.degree zaggdcvc y = -pi/1.5,
            vertex.label.dist = 4,
            edge.arrow.size = 9,
            edge.arrow.width = 2,
            edge.width = net_df$weight*2
            )


plot.igraph(net,
            layout=layout_with_fr(net, weights = (-net_df$sentiment)+4),
            edge.arrow.size=.4,
            vertex.color = "blue",
            vertex.size = nodes$vote_share*100,
            vertex.label.family = "Garamond", 
            vertex.label.color = "black",
            vertex.label=nodes$party_name,
            vertex.label.cex = 2,
            edge.curved = 0.15,
            edge.color = adjustcolor("SkyBlue2", alpha.f = 1),
            vertex.label.degree = -pi/1.5,
            vertex.label.dist = 4,
            edge.arrow.size = 4,
            edge.arrow.width = 2,
            edge.width = net_df$weight*2)

net_df %>% head

plot(net, layout_with_fr(net))

# cols <- function(a) image(1:5, 1, as.matrix(1:5), col=a, axes=FALSE , xlab="", ylab=""); cols(rgb(.5,.5,.5, 0.5))

### find out about weights!!!



# asdf

links <- read.csv("C:/Users/Philipp/Downloads/netscix2016/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
nodes <- read.csv("C:/Users/Philipp/Downloads/netscix2016/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)

nodes$type.label
nodes %>% head()
links %>% head()

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL


# Trashfile ---------------------------------
# Simulation of final dataset ####
doc_id <- sample(1:25,40,replace = T) %>% sort() %>% dense_rank()
win_id <- paste(doc_id,1:40, sep="_")
parties <- LETTERS[1:5]
temp <- left_join(data.frame(doc_id),
                  tibble(doc_id=doc_id %>% unique,
                         sender_party=LETTERS[sample(1:5, doc_id %>% unique %>% length, replace=T)]),
                  by=c("doc_id"))

temp <- left_join(temp,
                  tibble(doc_id=doc_id %>% unique,
                         recip_party=LETTERS[sample(1:5, doc_id %>% unique %>% length, replace=T)]),
                  by=c("doc_id"))

wind_df <- tibble(win_id = paste(doc_id,1:40, sep="_"),
                  doc_id = sample(1:25,40,replace = T) %>% sort() %>% dense_rank(),
                  from = temp$sender_party,
                  to = temp$recip_party,
                  sentiment = rnorm(40,mean = 0, sd = 0.5) %>% round(0)
)
#####

# get matching patterns for non-mps
# nonmps <- c(entity[147:nrow(entity),2] %>% 
#               str_remove_all(" of") %>% 
#               str_remove_all(" and") %>% str_remove_all("[,()]") %>% 
#               append(c("Labour Party", "Longford Westmeath")), # labour party not in there and Longford was wrongly written
#             entity[147:nrow(entity),2])


(wind_df[c("member_name", "party_name")] %>% unique %>% select(member_name) %>% table()>1) %>% sum()
wind_df$party_name[wind_df$member_name=="Mr. Noel Grealish"] %>% unique

wind_df$member_name[wind_df$party_name=="Independent"] %>% unique


Jackie Healy-Rae ✓
Finian McGrath ✓
Tony Gregory ?
  Michael Lowry ?	
  "Beverley Flynn",NaN,8.4.2008
"Joe Behan", 17.10.2008, NaN
"Ned O'Keeffe", 28.11.2007, 27.02.2008
"Jim McDaid", 13.11.2008


wind_df[wind_df$member_name=="Mr. Joe Behan" & wind_df$date < "2008-10-17", "party_name"]

(wind_df$member_name=="Mr. Joe Behan") %>% table()