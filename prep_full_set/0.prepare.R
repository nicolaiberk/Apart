"
0. prepare window frame based on entire dataset
We could also do this with functions, would require some effort though (and benefit unclear)
"

setwd("~/GitHub/samunico/Apart/prep_full_set")

## load and prepare data
source("0_1.entities.R")

## run lda tuning to decide n of topics (optional, low priority)

## run lda, assign topic probabilities to documents (optional, low priority)

## construct window frame

## subset & aggregate window frame to desired level
# startperiod = 
# endperiod =
# topics = 
# entity = "party"  ## else 'member', or maybe also 'legislature'
# granulation = 'monthly' ## else weekly or yearly
