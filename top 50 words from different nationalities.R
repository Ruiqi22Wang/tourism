library(gridExtra)
library(tidyverse)
library(tidytext)
library(dplyr)
library(gtools)
library(ggplot2)
library(lubridate)
library(SnowballC)
library(rlang)

setwd("~/Desktop/703 prj")

#read in data
hotels <- read_csv('Hotel_Reviews.csv')
          
#make unique identifier for each review
hotels$X = seq(1:515738)

# functions for data pre-processing

#dropNA
dropNA <-function(df,text_column){
  df %>%
    drop_na(!!sym(text_column)) %>%
    return()
}

# Tokenization
tokenization <-function(df,text_column,token = "word"){
  df %>%
    unnest_tokens(!!sym(token),!!sym(text_column)) %>%
    return()
}

# Removing stopwords
data("stop_words")
rem_stopwords <-function(df,df_stop = stop_words){
  df %>%
    anti_join(df_stop) %>%
    return()
}
# Removing numbers
rem_numbers <- function(df,token="word"){
  df %>%
    .[-grep("\\d",.[[token]]),] %>%
    return()
}
# Removing whitespaces
rem_whitespaces <-function(df,token="word"){
  df %>%
    mutate(!!sym(token) := gsub("\\s+","",.[[token]] ) ) %>%
    return()
}
# Stemming
stemming <- function(df,token = "word"){
  df %>%
    mutate_at(token, funs(wordStem((.), language="en"))) %>%
    return()
}

# get Nationality data
get_cn = function(nationality){
  hotels %>% 
    filter(Reviewer_Nationality == as.character(nationality)) %>% 
    select(c(7,10,18))
}

# Remove no negative/no positive placeholders
rem_no = function(df){
  df$Negative_Review[grep("No Negative", A_UK$Negative_Review)] <- NA
  df$Positive_Review[grep("No Positive", A_UK$Negative_Review)] <- NA
}

# get negtive/ positive words
get_neg = function(df){
  df %>% 
    select(X, Negative_Review) %>% 
    dropNA(., "Negative_Review") %>%
    unnest_tokens("word", Negative_Review) %>%
    rem_stopwords(., df_stop = stop_words) %>%
    rem_numbers() %>%
    rem_whitespaces() %>% 
    stemming()
}

get_pos = function(df){
  df %>% 
    select(X, Positive_Review) %>% 
    dropNA(., "Positive_Review") %>%
    unnest_tokens("word", Positive_Review) %>%
    rem_stopwords(., df_stop = stop_words) %>%
    rem_numbers() %>%
    rem_whitespaces() %>% 
    stemming()
}

# get positive and negative and overall top 50 word counts
get_n_count = function(nationality){
  cn = get_cn(as.character(nationality))
  rem_no(cn)
  neg = get_neg(cn) 
  pos = get_pos(cn)
  all = rbind(neg, pos)
  neg = neg %>% 
    group_by(word) %>% 
    count(sort = T) %>% 
    head(50)
  pos = pos %>% 
    group_by(word) %>% 
    count(sort = T) %>% 
    head(50)
  all = all %>% 
    group_by(word) %>% 
    count(sort = T) %>% 
    head(50)
  temp = list(neg, pos, all)
  return(temp)
}

# visualization
nt_vis = function(results, att, nationality ){
  ggplot(results, aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab(paste('Number of Times Word Appears in', att, 'Hotel Reviews from', nationality, sep=' '))+
  xlab("")+
  guides(fill=FALSE) + coord_flip()
}

# find most common nationalities overall
top10_all = hotels %>% 
  group_by(Reviewer_Nationality) %>% 
  count(sort = T) %>% 
  head(10)

# see the top 10 nationalities
top10_all

# get top 50 words from each nationality
#  1 United Kingdom           245246
UK = get_n_count('United Kingdom')
nt_vis(UK[[1]], 'Negative', 'United Kingdom')
nt_vis(UK[[2]], 'Positive', 'United Kingdom')
nt_vis(UK[[3]], 'Overall', 'United Kingdom')

#  2 United States of America  35437
US = get_n_count('United States of America')
nt_vis(US[[1]], 'Negative', 'United States of America')
nt_vis(US[[2]], 'Positive', 'United States of America')
nt_vis(US[[3]], 'Overall', 'United States of America')

#  3 Australia                 21686
AUS = get_n_count('Australia')
nt_vis(AUS[[1]], 'Negative', 'Australia')
nt_vis(AUS[[2]], 'Positive', 'Australia')
nt_vis(AUS[[3]], 'Overall', 'Australia')

#  4 Ireland                  14827
IL = get_n_count('Ireland')
nt_vis(IL[[1]], 'Negative', 'Ireland')
nt_vis(IL[[2]], 'Positive', 'Ireland')
nt_vis(IL[[3]], 'Overall', 'Ireland')

#  5 United Arab Emirates      10235
UAE = get_n_count('United Arab Emirates')
nt_vis(UAE[[1]], 'Negative', 'United Arab Emirates')
nt_vis(UAE[[2]], 'Positive', 'United Arab Emirates')
nt_vis(UAE[[3]], 'Overall', 'United Arab Emirates')

#  6 Saudi Arabia               8951
SA = get_n_count('Saudi Arabia')
nt_vis(SA[[1]], 'Negative', 'Saudi Arabia')
nt_vis(SA[[2]], 'Positive', 'Saudi Arabia')
nt_vis(SA[[3]], 'Overall', 'Saudi Arabia')

#  7 Netherlands                8772
NL = get_n_count('Netherlands')
nt_vis(NL[[1]], 'Negative', 'Netherlands')
nt_vis(NL[[2]], 'Positive', 'Netherlands')
nt_vis(NL[[3]], 'Overall', 'Netherlands')

#  8 Switzerland                8678
SWL = get_n_count('Switzerland')
nt_vis(SWL[[1]], 'Negative', 'Switzerland')
nt_vis(SWL[[2]], 'Positive', 'Switzerland')
nt_vis(SWL[[3]], 'Overall', 'Switzerland')

#  9 Germany                    7941
GMN = get_n_count('Germany')
nt_vis(GMN[[1]], 'Negative', 'Germany')
nt_vis(GMN[[2]], 'Positive', 'Germany')
nt_vis(GMN[[3]], 'Overall', 'Germany')

# 10 Canada                     7894
CND = get_n_count('Canada')
nt_vis(CND[[1]], 'Negative', 'Canada')
nt_vis(CND[[2]], 'Positive', 'Canada')
nt_vis(CND[[3]], 'Overall', 'Canada')

