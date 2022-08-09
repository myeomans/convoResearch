#######################################
# Natural Language for Social Science #
#        Prof Michael Yeomans         #
#######################################

# Run only once, when you first download the data
install.packages(c("tidyverse","quanteda","politeness",
                 "doc2concrete","glmnet","ggrepel","stm",
                 "syuzhet","sentimentr","doMC","spacyr"))

library(tidyverse) # Contains MANY useful functions
library(quanteda)  # many useful text analysis tools
library(politeness) # structural features from text
library(doc2concrete) # Contains the ngramTokens function - mostly a wrapper around quanteda tools
library(glmnet) # A simple machine learning model (the LASSO)
library(ggrepel) # useful for plotting later on
library(stm)  # High-performance topic modelling
library(syuzhet) # a benchmark for sentiment detection
library(sentimentr) # a benchmark for sentiment detection
library(doMC) # to speed up some code with parallel processing
library(spacyr) # to parse grammar


########################################################

# Let's load the data from memory
rev_small<-readRDS("data/rev_small.RDS")


source("text_basics.R")    

# ngrams, model training, dictionaries
source("ngram_model.R")

# using SpaCy pre-trained model
source("sentence_structure.R")

# politeness, conversation
source("conversations.R") 
 
# receptiveness example
source("receptiveness.R") 
