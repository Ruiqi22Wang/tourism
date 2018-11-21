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

#separate data frames for hotels in each city
hotels$City <- ''
hotels$City[grepl("Amsterdam Netherlands", hotels$Hotel_Address)] <- 'Amsterdam'
hotels$City[grepl("Milan Italy", hotels$Hotel_Address)] <- 'Milan'
hotels$City[grepl("United Kingdom", hotels$Hotel_Address)] <- 'London'
hotels$City[grepl("Barcelona Spain", hotels$Hotel_Address)] <- 'Barcelona'
hotels$City[grepl("Vienna Austria", hotels$Hotel_Address)] <- 'Vienna'
hotels$City[grepl("Paris France", hotels$Hotel_Address)] <- 'Paris'


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

# get City_Nationality data
get_cn = function(city, nationality){
  hotels %>% 
    filter(City == as.character(city), Reviewer_Nationality == as.character(nationality)) %>% 
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

# get positive and negative and overall top 40 word counts
get_cn_count = function(city, nationality){
  cn = get_cn(as.character(city), as.character(nationality))
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
cnt_vis = function(results, att, city, nationality ){
  ggplot(results, aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab(paste('Number of Times Word Appears in', att, city, 'Hotel Reviews from', nationality, sep=' '))+
  xlab("")+
  guides(fill=FALSE) + coord_flip()
  
}

#########################################    ggplot: grid, save pic

# find most common nationalities overall and for each city
unique(hotels$Reviewer_Nationality)

top10_all = hotels %>% 
  group_by(Reviewer_Nationality) %>% 
  count(sort = T) %>% 
  head(10)

top10_Amsterdam = hotels %>% 
  filter(City == 'Amsterdam') %>% 
  group_by(Reviewer_Nationality) %>% 
  count(sort = T) %>% 
  head(10)

top10_Barcelona = hotels %>% 
  filter(City == 'Barcelona') %>% 
  group_by(Reviewer_Nationality) %>% 
  count(sort = T) %>% 
  head(10)

top10_London = hotels %>% 
  filter(City == 'London') %>% 
  group_by(Reviewer_Nationality) %>% 
  count(sort = T) %>% 
  head(10)

top10_Milan = hotels %>% 
  filter(City == 'Milan') %>% 
  group_by(Reviewer_Nationality) %>% 
  count(sort = T) %>% 
  head(10)

top10_Paris = hotels %>% 
  filter(City == 'Paris') %>% 
  group_by(Reviewer_Nationality) %>% 
  count(sort = T) %>% 
  head(10)

top10_Vienna = hotels %>% 
  filter(City == 'Vienna') %>% 
  group_by(Reviewer_Nationality) %>% 
  count(sort = T) %>% 
  head(10)


### Reviews of Amsterdam

# see the top 10 nationalities in Amsterdam
top10_Amsterdam

# get top 50 words from each nationality

# 1. United Kingdom
A_UK = get_cn_count('Amsterdam', 'United Kingdom')
cnt_vis(A_UK[[1]], 'Negative', 'Amsterdam', 'United Kingdom')
cnt_vis(A_UK[[2]], 'Positive','Amsterdam', 'United Kingdom')
cnt_vis(A_UK[[3]], 'Overall', 'Amsterdam', 'United Kingdom')

# 2. United States of America 
A_US = get_cn_count('Amsterdam', 'United States of America')
cnt_vis(A_US[[1]], 'Negative', 'Amsterdam', 'United States of America')
cnt_vis(A_US[[2]], 'Positive', 'Amsterdam', 'United States of America')
cnt_vis(A_US[[3]], 'Overall', 'Amsterdam', 'United States of America')

# 3. Ireland
A_IL = get_cn_count('Amsterdam', 'Ireland')
cnt_vis(A_IL[[1]], 'Negative', 'Amsterdam', 'Ireland')
cnt_vis(A_IL[[2]], 'Positive', 'Amsterdam', 'Ireland')
cnt_vis(A_IL[[3]], 'Overall', 'Amsterdam', 'Ireland')

# 4. Australia
A_AUS = get_cn_count('Amsterdam', 'Australia')
cnt_vis(A_AUS[[1]], 'Negative', 'Amsterdam', 'Australia')
cnt_vis(A_AUS[[2]], 'Positive', 'Amsterdam', 'Australia')
cnt_vis(A_AUS[[3]], 'Overall', 'Amsterdam', 'Australia')

# 5. Netherlands
A_NL = get_cn_count('Amsterdam', 'Netherlands')
cnt_vis(A_NL[[1]], 'Negative', 'Amsterdam', 'Netherlands')
cnt_vis(A_NL[[2]], 'Positive', 'Amsterdam', 'Netherlands')
cnt_vis(A_NL[[3]], 'Overall', 'Amsterdam', 'Netherlands')

# 6. Germany
A_GM = get_cn_count('Amsterdam', 'Germany')
cnt_vis(A_GM[[1]], 'Negative', 'Amsterdam', 'Germany')
cnt_vis(A_GM[[2]], 'Positive', 'Amsterdam', 'Germany')
cnt_vis(A_GM[[3]], 'Overall', 'Amsterdam', 'Germany')

# 7. Israel
A_IR = get_cn_count('Amsterdam', 'Israel')
cnt_vis(A_IR[[1]], 'Negative', 'Amsterdam', 'Israel')
cnt_vis(A_IR[[2]], 'Positive', 'Amsterdam', 'Israel')
cnt_vis(A_IR[[3]], 'Overall', 'Amsterdam', 'Israel')

# 8. Canada
A_CND = get_cn_count('Amsterdam', 'Canada')
cnt_vis(A_CND[[1]], 'Negative', 'Amsterdam', 'Canada')
cnt_vis(A_CND[[2]], 'Positive', 'Amsterdam', 'Canada')
cnt_vis(A_CND[[3]], 'Overall', 'Amsterdam', 'Canada')

# 9. United Arab Emirates
A_UAE = get_cn_count('Amsterdam', 'United Arab Emirates')
cnt_vis(A_UAE[[1]], 'Negative', 'Amsterdam', 'United Arab Emirates')
cnt_vis(A_UAE[[2]], 'Positive', 'Amsterdam', 'United Arab Emirates')
cnt_vis(A_UAE[[3]], 'Overall', 'Amsterdam', 'United Arab Emirates')

# 10. Belgium
A_BG = get_cn_count('Amsterdam', 'Belgium')
cnt_vis(A_BG[[1]], 'Negative', 'Amsterdam', 'Belgium')
cnt_vis(A_BG[[2]], 'Positive', 'Amsterdam', 'Belgium')
cnt_vis(A_BG[[3]], 'Overall', 'Amsterdam', 'Belgium')



### Reviews across Milan
# see the top 10 nationalities in Milan
top10_Milan

# get top 50 words from each nationality
# 1. United Kingdom
M_UK = get_cn_count('Milan', 'United Kingdom')
cnt_vis(M_UK[[1]], 'Negative', 'Milan', 'United Kingdom')
cnt_vis(M_UK[[2]], 'Positive', '', 'United Kingdom')
cnt_vis(M_UK[[3]], 'Overall', '', 'United Kingdom')

# 2. United States of America
M_US = get_cn_count('Milan', 'United States of America')
cnt_vis(M_US[[1]], 'Negative', 'Milan', 'United States of America')
cnt_vis(M_US[[2]], 'Positive', 'Milan', 'United States of America')
cnt_vis(M_US[[3]], 'Overall', 'Milan', 'United States of America')

# 3. Australia
M_AUS = get_cn_count('Milan', 'Australia')
cnt_vis(M_AUS[[1]], 'Negative', 'Milan', 'Australia')
cnt_vis(M_AUS[[2]], 'Positive', 'Milan', 'Australia')
cnt_vis(M_AUS[[3]], 'Overall', 'Milan', 'Australia')

# 4. Switzerland 
M_SZL = get_cn_count('Milan', 'Switzerland ')
cnt_vis(M_SZL[[1]], 'Negative', 'Milan', 'Switzerland ')
cnt_vis(M_SZL[[2]], 'Positive', 'Milan', 'Switzerland ')
cnt_vis(M_SZL[[3]], 'Overall', 'Milan', 'Switzerland ')

# 5. Italy      
M_ITL = get_cn_count('Milan', 'Italy')
cnt_vis(M_ITL[[1]], 'Negative', 'Milan', 'Italy')
cnt_vis(M_ITL[[2]], 'Positive', 'Milan', 'Italy')
cnt_vis(M_ITL[[3]], 'Overall', 'Milan', 'Italy')

# 6. United Arab Emirates  
M_UAE= get_cn_count('Milan', 'United Arab Emirates')
cnt_vis(M_UAE[[1]], 'Negative', 'Milan', 'United Arab Emirates')
cnt_vis(M_UAE[[2]], 'Positive', 'Milan', 'United Arab Emirates')
cnt_vis(M_UAE[[3]], 'Overall', 'Milan', 'United Arab Emirates')

# 7. Saudi Arabia     
M_SA = get_cn_count('Milan', 'Saudi Arabia')
cnt_vis(M_SA[[1]], 'Negative', 'Milan', 'Saudi Arabia')
cnt_vis(M_SA[[2]], 'Positive', 'Milan', 'Saudi Arabia')
cnt_vis(M_SA[[3]], 'Overall', 'Milan', 'Saudi Arabia')

# 8. Germany            
M_GM = get_cn_count('Milan', 'Germany')
cnt_vis(M_GM[[1]], 'Negative', 'Milan', 'Germany')
cnt_vis(M_GM[[2]], 'Positive', 'Milan', 'Germany')
cnt_vis(M_GM[[3]], 'Overall', 'Milan', 'Germany')

# 9. Turkey            
M_TK = get_cn_count('Milan', 'Turkey')
cnt_vis(M_TK[[1]], 'Negative', 'Milan', 'Turkey')
cnt_vis(M_TK[[2]], 'Positive', 'Milan', 'Turkey')
cnt_vis(M_TK[[3]], 'Overall', 'Milan', 'Turkey')

# 10. Romania 
M_RMN = get_cn_count('Milan', 'Romania')
cnt_vis(M_RMN[[1]], 'Negative', 'Milan', 'Romania')
cnt_vis(M_RMN[[2]], 'Positive', 'Milan', 'Romania')
cnt_vis(M_RMN[[3]], 'Overall', 'Milan', 'Romania')


### Reviews across London
top10_London
# get top 50 words from each nationality
# 1 United Kingdom           170649
L_UK = get_cn_count('London', 'United Kingdom')
cnt_vis(L_UK[[1]], 'Negative', 'London', 'United Kingdom')
cnt_vis(L_UK[[2]], 'Positive', 'London', 'United Kingdom')
cnt_vis(L_UK[[3]], 'Overall', 'London', 'United Kingdom')

# 2 United States of America  11011
L_US = get_cn_count('London', 'United States of America')
cnt_vis(L_US[[1]], 'Negative', 'London', 'United States of America')
cnt_vis(L_US[[2]], 'Positive', 'London', 'United States of America')
cnt_vis(L_US[[3]], 'Overall', 'London', 'United States of America')

# 3 Australia                  8473
L_AUS = get_cn_count('London', 'Australia')
cnt_vis(L_AUS[[1]], 'Negative', 'London', 'Australia')
cnt_vis(L_AUS[[2]], 'Positive', 'London', 'Australia')
cnt_vis(L_AUS[[3]], 'Overall', 'London', 'Australia')

# 4 Ireland                    7020
L_IL = get_cn_count('London', 'Ireland')
cnt_vis(L_IL[[1]], 'Negative', 'London', 'Ireland')
cnt_vis(L_IL[[2]], 'Positive', 'London', 'Ireland')
cnt_vis(L_IL[[3]], 'Overall', 'London', 'Ireland')

# 5 United Arab Emirates       3864
L_UAE = get_cn_count('London', 'United Arab Emirates')
cnt_vis(L_UAE[[1]], 'Negative', 'London', 'United Arab Emirates')
cnt_vis(L_UAE[[2]], 'Positive', 'London', 'United Arab Emirates')
cnt_vis(L_UAE[[3]], 'Overall', 'London', 'United Arab Emirates')

# 6 Saudi Arabia               3202
L_SA = get_cn_count('London', 'Saudi Arabia')
cnt_vis(L_SA[[1]], 'Negative', 'London', 'Saudi Arabia')
cnt_vis(L_SA[[2]], 'Positive', 'London', 'Saudi Arabia')
cnt_vis(L_SA[[3]], 'Overall', 'London', 'Saudi Arabia')

# 7 Switzerland                2992
L_SWL = get_cn_count('London', 'Switzerland')
cnt_vis(L_SWL[[1]], 'Negative', 'London', 'Switzerland')
cnt_vis(L_SWL[[2]], 'Positive', 'London', 'Switzerland')
cnt_vis(L_SWL[[3]], 'Overall', 'London', 'Switzerland')

# 8 Netherlands                2737
L_NL = get_cn_count('London', 'Netherlands')
cnt_vis(L_NL[[1]], 'Negative', 'London', 'Netherlands')
cnt_vis(L_NL[[2]], 'Positive', 'London', 'Netherlands')
cnt_vis(L_NL[[3]], 'Overall', 'London', 'Netherlands')

# 9 Canada                     2718
L_CND = get_cn_count('London', 'Canada')
cnt_vis(L_CND[[1]], 'Negative', 'London', 'Canada')
cnt_vis(L_CND[[2]], 'Positive', 'London', 'Canada')
cnt_vis(L_CND[[3]], 'Overall', 'London', 'Canada')

# 10 Kuwait                     2397
L_KW = get_cn_count('London', 'Kuwait')
cnt_vis(L_KW[[1]], 'Negative', 'London', 'Kuwait')
cnt_vis(L_KW[[2]], 'Positive', 'London', 'Kuwait')
cnt_vis(L_KW[[3]], 'Overall', 'London', 'Kuwait')



### Reviews across Barcelona
top10_Barcelona
# get top 50 words from each nationality
#  1 United Kingdom           20961
B_UK = get_cn_count('Barcelona', 'United Kingdom')
cnt_vis(B_UK[[1]], 'Negative', 'Barcelona', 'United Kingdom')
cnt_vis(B_UK[[2]], 'Positive', 'Barcelona', 'United Kingdom')
cnt_vis(B_UK[[3]], 'Overall', 'Barcelona', 'United Kingdom')

#  2 United States of America  6169
B_US = get_cn_count('Barcelona', 'United States of America')
cnt_vis(B_US[[1]], 'Negative', 'Barcelona', 'United States of America')
cnt_vis(B_US[[2]], 'Positive', 'Barcelona', 'United States of America')
cnt_vis(B_US[[3]], 'Overall', 'Barcelona', 'United States of America')

#  3 Australia                 2907
B_AUS = get_cn_count('Barcelona', 'Australia')
cnt_vis(B_AUS[[1]], 'Negative', 'Barcelona', 'Australia')
cnt_vis(B_AUS[[2]], 'Positive', 'Barcelona', 'Australia')
cnt_vis(B_AUS[[3]], 'Overall', 'Barcelona', 'Australia')

#  4 Ireland                   2134
B_IL = get_cn_count('Barcelona', 'Ireland')
cnt_vis(B_IL[[1]], 'Negative', 'Barcelona', 'Ireland')
cnt_vis(B_IL[[2]], 'Positive', 'Barcelona', 'Ireland')
cnt_vis(B_IL[[3]], 'Overall', 'Barcelona', 'Ireland')


#  5 Spain                     1778
B_SPN = get_cn_count('Barcelona', 'Spain')
cnt_vis(B_SPN[[1]], 'Negative', 'Barcelona', 'Spain')
cnt_vis(B_SPN[[2]], 'Positive', 'Barcelona', 'Spain')
cnt_vis(B_SPN[[3]], 'Overall', 'Barcelona', 'Spain')

#  6 Canada                    1457
B_CND = get_cn_count('Barcelona', 'Canada')
cnt_vis(B_CND[[1]], 'Negative', 'Barcelona', 'Canada')
cnt_vis(B_CND[[2]], 'Positive', 'Barcelona', 'Canada')
cnt_vis(B_CND[[3]], 'Overall', 'Barcelona', 'Canada')

#  7 United Arab Emirates      1331
B_UAE = get_cn_count('Barcelona', 'United Arab Emirates')
cnt_vis(B_UAE[[1]], 'Negative', 'Barcelona', 'United Arab Emirates')
cnt_vis(B_UAE[[2]], 'Positive', 'Barcelona', 'United Arab Emirates')
cnt_vis(B_UAE[[3]], 'Overall', 'Barcelona', 'United Arab Emirates')

#  8 Netherlands               1232
B_NL = get_cn_count('Barcelona', 'Netherlands')
cnt_vis(B_NL[[1]], 'Negative', 'Barcelona', 'Netherlands')
cnt_vis(B_NL[[2]], 'Positive', 'Barcelona', 'Netherlands')
cnt_vis(B_NL[[3]], 'Overall', 'Barcelona', 'Netherlands')

#  9 Saudi Arabia              1202
B_SA = get_cn_count('Barcelona', 'Saudi Arabia')
cnt_vis(B_SA[[1]], 'Negative', 'Barcelona', 'Saudi Arabia')
cnt_vis(B_SA[[2]], 'Positive', 'Barcelona', 'Saudi Arabia')
cnt_vis(B_SA[[3]], 'Overall', 'Barcelona', 'Saudi Arabia')

# 10 Israel                    1173
B_IR = get_cn_count('Barcelona', 'Israel')
cnt_vis(B_IR[[1]], 'Negative', 'Barcelona', 'Israel')
cnt_vis(B_IR[[2]], 'Positive', 'Barcelona', 'Israel')
cnt_vis(B_IR[[3]], 'Overall', 'Barcelona', 'Israel')


### Reviews across Vienna
top10_Vienna
# get top 50 words from each nationality
# 1 United Kingdom            7507
V_UK = get_cn_count('Vienna', 'United Kingdom')
cnt_vis(V_UK[[1]], 'Negative', 'Vienna', 'United Kingdom')
cnt_vis(V_UK[[2]], 'Positive', 'Vienna', 'United Kingdom')
cnt_vis(V_UK[[3]], 'Overall', 'Vienna', 'United Kingdom')

#  2 United States of America  3104
V_US = get_cn_count('Vienna', 'United States of America')
cnt_vis(V_US[[1]], 'Negative', 'Vienna', 'United States of America')
cnt_vis(V_US[[2]], 'Positive', 'Vienna', 'United States of America')
cnt_vis(V_US[[3]], 'Overall', 'Vienna', 'United States of America')

#  3 Romania                   1807
V_RMN = get_cn_count('Vienna', 'Romania')
cnt_vis(V_RMN[[1]], 'Negative', 'Vienna', 'Romania')
cnt_vis(V_RMN[[2]], 'Positive', 'Vienna', 'Romania')
cnt_vis(V_RMN[[3]], 'Overall', 'Vienna', 'Romania')

#  4 Australia                 1680
V_AUS = get_cn_count('Vienna', 'Australia')
cnt_vis(V_AUS[[1]], 'Negative', 'Vienna', 'Australia')
cnt_vis(V_AUS[[2]], 'Positive', 'Vienna', 'Australia')
cnt_vis(V_AUS[[3]], 'Overall', 'Vienna', 'Australia')

#  5 United Arab Emirates      1163
V_UAE = get_cn_count('Vienna', 'United Arab Emirates')
cnt_vis(V_UAE[[1]], 'Negative', 'Vienna', 'United Arab Emirates')
cnt_vis(V_UAE[[2]], 'Positive', 'Vienna', 'United Arab Emirates')
cnt_vis(V_UAE[[3]], 'Overall', 'Vienna', 'United Arab Emirates')

#  6 Germany                   1040
V_GM = get_cn_count('Vienna', 'Germany')
cnt_vis(V_GM[[1]], 'Negative', 'Vienna', 'Germany')
cnt_vis(V_GM[[2]], 'Positive', 'Vienna', 'Germany')
cnt_vis(V_GM[[3]], 'Overall', 'Vienna', 'Germany')

#  7 Israel                    1029
V_IR = get_cn_count('Vienna', 'Israel')
cnt_vis(V_IR[[1]], 'Negative', 'Vienna', 'Israel')
cnt_vis(V_IR[[2]], 'Positive', 'Vienna', 'Israel')
cnt_vis(V_IR[[3]], 'Overall', 'Vienna', 'Israel')

#  8 Austria                    932
V_ATR = get_cn_count('Vienna', 'Austria')
cnt_vis(V_ATR[[1]], 'Negative', 'Vienna', 'Austria')
cnt_vis(V_ATR[[2]], 'Positive', 'Vienna', 'Austria')
cnt_vis(V_ATR[[3]], 'Overall', 'Vienna', 'Austria')

#  9 Hungary                    908
V_HGR = get_cn_count('Vienna', 'Hungary')
cnt_vis(V_HGR[[1]], 'Negative', 'Vienna', 'Hungary')
cnt_vis(V_HGR[[2]], 'Positive', 'Vienna', 'Hungary')
cnt_vis(V_HGR[[3]], 'Overall', 'Vienna', 'Hungary')

# 10 Czech Republic             832
V_CR = get_cn_count('Vienna', 'Czech Republic')
cnt_vis(V_CR[[1]], 'Negative', 'Vienna', 'Czech Republic')
cnt_vis(V_CR[[2]], 'Positive', 'Vienna', 'Czech Republic')
cnt_vis(V_CR[[3]], 'Overall', 'Vienna', 'Czech Republic')


### Reviews across Paris
top10_Paris
# get top 50 words from each nationality
#  1 United Kingdom           16908
P_UK = get_cn_count('Paris', 'United Kingdom')
cnt_vis(P_UK[[1]], 'Negative', 'Paris', 'United Kingdom')
cnt_vis(P_UK[[2]], 'Positive', 'Paris', 'United Kingdom')
cnt_vis(P_UK[[3]], 'Overall', 'Paris', 'United Kingdom')

#  2 United States of America  7279
P_US = get_cn_count('Paris', 'United States of America')
cnt_vis(P_US[[1]], 'Negative', 'Paris', 'United States of America')
cnt_vis(P_US[[2]], 'Positive', 'Paris', 'United States of America')
cnt_vis(P_US[[3]], 'Overall', 'Paris', 'United States of America')

#  3 Australia                 3904
P_AUS = get_cn_count('Paris', 'Australia')
cnt_vis(P_AUS[[1]], 'Negative', 'Paris', 'Australia')
cnt_vis(P_AUS[[2]], 'Positive', 'Paris', 'Australia')
cnt_vis(P_AUS[[3]], 'Overall', 'Paris', 'Australia')

#  4 Saudi Arabia              1994
P_SA = get_cn_count('Paris', 'Saudi Arabia')
cnt_vis(P_SA[[1]], 'Negative', 'Paris', 'Saudi Arabia')
cnt_vis(P_SA[[2]], 'Positive', 'Paris', 'Saudi Arabia')
cnt_vis(P_SA[[3]], 'Overall', 'Paris', 'Saudi Arabia')

#  5 France                    1860
P_FR = get_cn_count('Paris', 'France')
cnt_vis(P_FR[[1]], 'Negative', 'Paris', 'France')
cnt_vis(P_FR[[2]], 'Positive', 'Paris', 'France')
cnt_vis(P_FR[[3]], 'Overall', 'Paris', 'France')

#  6 United Arab Emirates      1539
P_UAE = get_cn_count('Paris', 'United Arab Emirates')
cnt_vis(P_UAE[[1]], 'Negative', 'Paris', 'United Arab Emirates')
cnt_vis(P_UAE[[2]], 'Positive', 'Paris', 'United Arab Emirates')
cnt_vis(P_UAE[[3]], 'Overall', 'Paris', 'United Arab Emirates')

#  7 Netherlands               1485
P_NL = get_cn_count('Paris', 'Netherlands')
cnt_vis(P_NL[[1]], 'Negative', 'Paris', 'Netherlands')
cnt_vis(P_NL[[2]], 'Positive', 'Paris', 'Netherlands')
cnt_vis(P_NL[[3]], 'Overall', 'Paris', 'Netherlands')

#  8 Canada                    1335
P_CND = get_cn_count('Paris', 'Canada')
cnt_vis(P_CND[[1]], 'Negative', 'Paris', 'Canada')
cnt_vis(P_CND[[2]], 'Positive', 'Paris', 'Canada')
cnt_vis(P_CND[[3]], 'Overall', 'Paris', 'Canada')

#  9 Ireland                   1327
P_IL = get_cn_count('Paris', 'Ireland')
cnt_vis(P_IL[[1]], 'Negative', 'Paris', 'Ireland')
cnt_vis(P_IL[[2]], 'Positive', 'Paris', 'Ireland')
cnt_vis(P_IL[[3]], 'Overall', 'Paris', 'Ireland')

# 10 Germany                   1245
P_GM = get_cn_count('Paris', 'Germany')
cnt_vis(P_GM[[1]], 'Negative', 'Paris', 'Germany')
cnt_vis(P_GM[[2]], 'Positive', 'Paris', 'Germany')
cnt_vis(P_GM[[3]], 'Overall', 'Paris', 'Germany')








