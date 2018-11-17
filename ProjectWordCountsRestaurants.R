setwd("~/Desktop/703 Project")

library(gridExtra)
library(tidytext)
library(dplyr)
library(gtools)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

restaurants <- read.csv('TA_restaurants_curated.csv')

#list of locations to be included for restaurants
locations <- c('Amsterdam', 'Paris', 'London', 'Vienna',
               'Barcelona', 'Milan')

#all restaurants in chosen cities
chosen_restaurants <- restaurants[grepl(paste(locations, collapse = '|'), restaurants$City),]

#separate data frames for restaurants in each city
Amsterdam_rest <- chosen_restaurants[chosen_restaurants$City == "Amsterdam",] 
Paris_rest <- chosen_restaurants[chosen_restaurants$City == 'Paris',]
Vienna_rest <- chosen_restaurants[chosen_restaurants$City == 'Vienna',]
London_rest <- chosen_restaurants[chosen_restaurants$City == 'London',]
Barcelona_rest <- chosen_restaurants[chosen_restaurants$City == 'Barcelona',]
Milan_rest <- chosen_restaurants[chosen_restaurants$City == 'Milan',]

###AMSTERDAM

#restaruant reviews for Amsterdam
A_rest_reviews <- Amsterdam_rest[,c(1, 9)]
A_rest_reviews$Reviews <- as.character(A_rest_reviews$Reviews)

#create a tidy text data frame for Amsterdam restaurant reviews
tidy_A_rest_reviews <- A_rest_reviews %>%
  select(X, Reviews) %>%
  unnest_tokens(word, Reviews)

#arrange words from Amsterdam restaurant reviews in decending order
tidy_A_rest_reviews %>%
  count(word) %>%
  arrange(desc(n))

#remove stop words from Amsterdam restaurant reviews
data("stop_words")
tidy_A_rest_reviews <- tidy_A_rest_reviews %>%
  anti_join(stop_words)

#remove white space from Amsterdam restaurant reviews
tidy_A_rest_reviews$word <- gsub("\\s+","",tidy_A_rest_reviews$word)

#remove numbers from Amsterdam restaurant reviews
tidy_A_rest_reviews <-tidy_A_rest_reviews[-grep("\\b\\d+\\b", tidy_A_rest_reviews$word),]

#wordcloud
tidy_A_rest_reviews %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq=20, random.order = FALSE,
                 colors = brewer.pal(6,"Set1")))

#stem words from Amsterdam restaurant reviews
library(SnowballC)
tidy_A_rest_reviews <-tidy_A_rest_reviews %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#get most common words from Amsterdam restaurant reviews
A_rest_wordcount <- tidy_A_rest_reviews %>%
  count(word, sort=TRUE)

#limit to 50 words
A_rest_top50 <- A_rest_wordcount[seq(1,50,by=1),]
A_rest_top50$word <- as.factor(A_rest_top50$word)

#plot top words
ggplot(A_rest_top50, aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Amsterdam Restaurant Reviews")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()

###BARCELONA

#restaruant reviews for Barcelona
B_rest_reviews <- Barcelona_rest[,c(1, 9)]
B_rest_reviews$Reviews <- as.character(B_rest_reviews$Reviews)

#create a tidy text data frame for Barcelona restaurant reviews
tidy_B_rest_reviews <- B_rest_reviews %>%
  select(X, Reviews) %>%
  unnest_tokens(word, Reviews)

#arrange words from Barcelona restaurant reviews in decending order
tidy_B_rest_reviews %>%
  count(word) %>%
  arrange(desc(n))

#remove stop words from Barcelona restaurant reviews
data("stop_words")
tidy_B_rest_reviews <- tidy_B_rest_reviews %>%
  anti_join(stop_words)

#remove white space from Barcelona restaurant reviews
tidy_B_rest_reviews$word <- gsub("\\s+","",tidy_B_rest_reviews$word)

#remove numbers from Barcelona restaurant reviews
tidy_B_rest_reviews <-tidy_B_rest_reviews[-grep("\\b\\d+\\b", tidy_B_rest_reviews$word),]

#wordcloud
tidy_B_rest_reviews %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq=20, random.order = FALSE,
                 colors = brewer.pal(6,"Set1")))

#stem words from Barcelona restaurant reviews
library(SnowballC)
tidy_B_rest_reviews <-tidy_B_rest_reviews %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#get most common words from Barcelona restaurant reviews
B_rest_wordcount <- tidy_B_rest_reviews %>%
  count(word, sort=TRUE)

#limit to 50 words
B_rest_top50 <- B_rest_wordcount[seq(1,50,by=1),]
B_rest_top50$word <- as.factor(B_rest_top50$word)

#plot top words
ggplot(B_rest_top50, aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Barcelona Restaurant Reviews")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()

###LONDON

#restaruant reviews for London
L_rest_reviews <- London_rest[,c(1, 9)]
L_rest_reviews$Reviews <- as.character(L_rest_reviews$Reviews)

#create a tidy text data frame for London restaurant reviews
tidy_L_rest_reviews <- L_rest_reviews %>%
  select(X, Reviews) %>%
  unnest_tokens(word, Reviews)

#arrange words from London restaurant reviews in decending order
tidy_L_rest_reviews %>%
  count(word) %>%
  arrange(desc(n))

#remove stop words from London restaurant reviews
data("stop_words")
tidy_L_rest_reviews <- tidy_L_rest_reviews %>%
  anti_join(stop_words)

#remove white space from London restaurant reviews
tidy_L_rest_reviews$word <- gsub("\\s+","",tidy_L_rest_reviews$word)

#remove numbers from London restaurant reviews
tidy_L_rest_reviews <-tidy_L_rest_reviews[-grep("\\b\\d+\\b", tidy_L_rest_reviews$word),]

#wordcloud
tidy_L_rest_reviews %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq=50, random.order = FALSE,
                 colors = brewer.pal(6,"Set1")))

#stem words from London restaurant reviews
library(SnowballC)
tidy_L_rest_reviews <-tidy_L_rest_reviews %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#get most common words from London restaurant reviews
L_rest_wordcount <- tidy_L_rest_reviews %>%
  count(word, sort=TRUE)

#limit to 50 words
L_rest_top50 <- L_rest_wordcount[seq(1,50,by=1),]
L_rest_top50$word <- as.factor(L_rest_top50$word)

#plot top words
ggplot(L_rest_top50, aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in London Restaurant Reviews")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()

###MILAN

#restaruant reviews for Milan
M_rest_reviews <- Milan_rest[,c(1, 9)]
M_rest_reviews$Reviews <- as.character(M_rest_reviews$Reviews)

#create a tidy text data frame for Milan restaurant reviews
tidy_M_rest_reviews <- M_rest_reviews %>%
  select(X, Reviews) %>%
  unnest_tokens(word, Reviews)

#arrange words from Milan restaurant reviews in decending order
tidy_M_rest_reviews %>%
  count(word) %>%
  arrange(desc(n))

#remove stop words from Milan restaurant reviews
data("stop_words")
tidy_M_rest_reviews <- tidy_M_rest_reviews %>%
  anti_join(stop_words)

#remove white space from Milan restaurant reviews
tidy_M_rest_reviews$word <- gsub("\\s+","",tidy_M_rest_reviews$word)

#remove numbers from Milan restaurant reviews
tidy_M_rest_reviews <-tidy_M_rest_reviews[-grep("\\b\\d+\\b", tidy_M_rest_reviews$word),]

#wordcloud
tidy_M_rest_reviews %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq=20, random.order = FALSE,
                 colors = brewer.pal(6,"Set1")))

#stem words from Milan restaurant reviews
library(SnowballC)
tidy_M_rest_reviews <-tidy_M_rest_reviews %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#get most common words from Milan restaurant reviews
M_rest_wordcount <- tidy_M_rest_reviews %>%
  count(word, sort=TRUE)

#limit to 50 words
M_rest_top50 <- M_rest_wordcount[seq(1,50,by=1),]
M_rest_top50$word <- as.factor(M_rest_top50$word)

#plot top words
ggplot(M_rest_top50, aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Milan Restaurant Reviews")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()


###PARIS

#restaruant reviews for Paris
P_rest_reviews <- Paris_rest[,c(1, 9)]
P_rest_reviews$Reviews <- as.character(P_rest_reviews$Reviews)

#create a tidy text data frame for Paris restaurant reviews
tidy_P_rest_reviews <- P_rest_reviews %>%
  select(X, Reviews) %>%
  unnest_tokens(word, Reviews)

#arrange words from Paris restaurant reviews in decending order
tidy_P_rest_reviews %>%
  count(word) %>%
  arrange(desc(n))

#remove stop words from Paris restaurant reviews
data("stop_words")
tidy_P_rest_reviews <- tidy_P_rest_reviews %>%
  anti_join(stop_words)

#remove white space from Paris restaurant reviews
tidy_P_rest_reviews$word <- gsub("\\s+","",tidy_P_rest_reviews$word)

#remove numbers from Paris restaurant reviews
tidy_P_rest_reviews <-tidy_P_rest_reviews[-grep("\\b\\d+\\b", tidy_P_rest_reviews$word),]

#wordcloud
tidy_P_rest_reviews %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq=50, random.order = FALSE,
                 colors = brewer.pal(6,"Set1")))

#stem words from Paris restaurant reviews
library(SnowballC)
tidy_P_rest_reviews <-tidy_P_rest_reviews %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#get most common words from Paris restaurant reviews
P_rest_wordcount <- tidy_P_rest_reviews %>%
  count(word, sort=TRUE)

#limit to 50 words
P_rest_top50 <- P_rest_wordcount[seq(1,50,by=1),]
P_rest_top50$word <- as.factor(P_rest_top50$word)

#plot top words
ggplot(P_rest_top50, aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Paris Restaurant Reviews")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()

###VIENNA

#restaruant reviews for Vienna
V_rest_reviews <- Vienna_rest[,c(1, 9)]
V_rest_reviews$Reviews <- as.character(V_rest_reviews$Reviews)

#create a tidy text data frame for Vienna restaurant reviews
tidy_V_rest_reviews <- V_rest_reviews %>%
  select(X, Reviews) %>%
  unnest_tokens(word, Reviews)

#arrange words from Vienna restaurant reviews in decending order
tidy_V_rest_reviews %>%
  count(word) %>%
  arrange(desc(n))

#remove stop words from Vienna restaurant reviews
data("stop_words")
tidy_V_rest_reviews <- tidy_V_rest_reviews %>%
  anti_join(stop_words)

#remove white space from Vienna restaurant reviews
tidy_V_rest_reviews$word <- gsub("\\s+","",tidy_V_rest_reviews$word)

#remove numbers from Vienna restaurant reviews
tidy_V_rest_reviews <-tidy_V_rest_reviews[-grep("\\b\\d+\\b", tidy_V_rest_reviews$word),]

#wordcloud
tidy_V_rest_reviews %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq=20, random.order = FALSE,
                 colors = brewer.pal(6,"Set1")))

#stem words from Vienna restaurant reviews
library(SnowballC)
tidy_V_rest_reviews <-tidy_V_rest_reviews %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#get most common words from Vienna restaurant reviews
V_rest_wordcount <- tidy_V_rest_reviews %>%
  count(word, sort=TRUE)

#limit to 50 words
V_rest_top50 <- V_rest_wordcount[seq(1,50,by=1),]
V_rest_top50$word <- as.factor(V_rest_top50$word)

#plot top words
ggplot(V_rest_top50, aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Vienna Restaurant Reviews")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()








###CUISINES

#Amsterdam
Amsterdam_rest$Cuisine.Style <- as.character(Amsterdam_rest$Cuisine.Style)

#create a tidy text data frame for Amsterdam restaurant reviews
tidy_A_Cuisines <- Amsterdam_rest %>%
  select(X, Cuisine.Style) %>%
  unnest_tokens(word, Cuisine.Style)

tidy_A_Cuisines$word[tidy_A_Cuisines$word == 'middle'] <- "middle eastern"


#arrange words from Amsterdam restaurant reviews in decending order
A_cuisine_count <- tidy_A_Cuisines %>%
  count(word) %>%
  arrange(desc(n))

non_cuisines <- c("european", "vegetarian", "friendly", "options", 
                  "gluten", "free", "fast", "vegan", "healthy", "grill", 
                  "food", "central", "south", "fusion", "eastern")

#remove words that aren't cuisines
A_cuisine_count <- A_cuisine_count[-grep(paste(non_cuisines, collapse = '|'), A_cuisine_count$word),]

#plot cuisines
ggplot(A_cuisine_count[seq(1,25, by=1),], aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Amsterdam Cuisines")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()

#wordcloud of cuisines
A_cuisine_count %>%
  with(wordcloud(word, n, min.freq=20, random.order = FALSE,
                 colors = brewer.pal(6, "Spectral")))


#Barcelona
Barcelona_rest$Cuisine.Style <- as.character(Barcelona_rest$Cuisine.Style)

#create a tidy text data frame for Amsterdam restaurant reviews
tidy_B_Cuisines <- Barcelona_rest %>%
  select(X, Cuisine.Style) %>%
  unnest_tokens(word, Cuisine.Style)

tidy_B_Cuisines$word[tidy_B_Cuisines$word == 'middle'] <- "middle eastern"
tidy_B_Cuisines$word[tidy_B_Cuisines$word == 'wine'] <- "wine bar"


#arrange words from Amsterdam restaurant reviews in decending order
B_cuisine_count <- tidy_B_Cuisines %>%
  count(word) %>%
  arrange(desc(n))

non_cuisines <- c("european", "vegetarian", "friendly", "options", 
                  "gluten", "free", "fast", "vegan", "healthy", "grill", 
                  "food", "central", "south", "fusion", "eastern")

#remove words that aren't cuisines
B_cuisine_count <- B_cuisine_count[-grep(paste(non_cuisines, collapse = '|'), B_cuisine_count$word),]

#plot cuisines
ggplot(B_cuisine_count[seq(1,25, by=1),], aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Barcelona Cuisines")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()

#wordcloud of cuisines
B_cuisine_count %>%
  with(wordcloud(word, n, min.freq=20, random.order = FALSE,
                 colors = brewer.pal(6, "Spectral")))

#London
London_rest$Cuisine.Style <- as.character(London_rest$Cuisine.Style)

#create a tidy text data frame for Amsterdam restaurant reviews
tidy_L_Cuisines <- London_rest %>%
  select(X, Cuisine.Style) %>%
  unnest_tokens(word, Cuisine.Style)

tidy_L_Cuisines$word[tidy_L_Cuisines$word == 'middle'] <- "middle eastern"
tidy_L_Cuisines$word[tidy_L_Cuisines$word == 'wine'] <- "wine bar"


#arrange words from Amsterdam restaurant reviews in decending order
L_cuisine_count <- tidy_L_Cuisines %>%
  count(word) %>%
  arrange(desc(n))

non_cuisines <- c("european", "vegetarian", "friendly", "options", 
                  "gluten", "free", "fast", "vegan", "healthy", "grill", 
                  "food", "central", "south", "fusion")

#remove words that aren't cuisines
L_cuisine_count <- L_cuisine_count[-grep(paste(non_cuisines, collapse = '|'), L_cuisine_count$word),]

#plot cuisines
ggplot(L_cuisine_count[seq(1,25, by=1),], aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("London Cuisines")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()

#wordcloud of cuisines
L_cuisine_count %>%
  with(wordcloud(word, n, min.freq=20, random.order = FALSE,
                 colors = brewer.pal(6, "Spectral")))


#Milan
Milan_rest$Cuisine.Style <- as.character(Milan_rest$Cuisine.Style)

#create a tidy text data frame for Amsterdam restaurant reviews
tidy_M_Cuisines <- Milan_rest %>%
  select(X, Cuisine.Style) %>%
  unnest_tokens(word, Cuisine.Style)

tidy_M_Cuisines$word[tidy_M_Cuisines$word == 'middle'] <- "middle eastern"
tidy_M_Cuisines$word[tidy_M_Cuisines$word == 'wine'] <- "wine bar"


#arrange words from Amsterdam restaurant reviews in decending order
M_cuisine_count <- tidy_M_Cuisines %>%
  count(word) %>%
  arrange(desc(n))

non_cuisines <- c("european", "vegetarian", "friendly", "options", 
                  "gluten", "free", "fast", "vegan", "healthy", "grill", 
                  "food", "central", "south", "fusion", "eastern")

#remove words that aren't cuisines
M_cuisine_count <- M_cuisine_count[-grep(paste(non_cuisines, collapse = '|'), M_cuisine_count$word),]

#plot cuisines
ggplot(M_cuisine_count[seq(1,25, by=1),], aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Milan Cuisines")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()

#wordcloud of cuisines
M_cuisine_count %>%
  with(wordcloud(word, n, min.freq=20, random.order = FALSE,
                 colors = brewer.pal(6, "Spectral")))



#Paris
Paris_rest$Cuisine.Style <- as.character(Paris_rest$Cuisine.Style)

#create a tidy text data frame for Amsterdam restaurant reviews
tidy_P_Cuisines <- Paris_rest %>%
  select(X, Cuisine.Style) %>%
  unnest_tokens(word, Cuisine.Style)

tidy_P_Cuisines$word[tidy_P_Cuisines$word == 'middle'] <- "middle eastern"
tidy_P_Cuisines$word[tidy_P_Cuisines$word == 'wine'] <- "wine bar"


#arrange words from Amsterdam restaurant reviews in decending order
P_cuisine_count <- tidy_P_Cuisines %>%
  count(word) %>%
  arrange(desc(n))

non_cuisines <- c("european", "vegetarian", "friendly", "options", 
                  "gluten", "free", "fast", "vegan", "healthy", "grill", 
                  "food", "central", "south", "fusion")

#remove words that aren't cuisines
P_cuisine_count <- P_cuisine_count[-grep(paste(non_cuisines, collapse = '|'), P_cuisine_count$word),]

#plot cuisines
ggplot(P_cuisine_count[seq(1,25, by=1),], aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Paris Cuisines")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()

#wordcloud of cuisines
P_cuisine_count %>%
  with(wordcloud(word, n, min.freq=20, random.order = FALSE,
                 colors = brewer.pal(6, "Spectral")))



#Vienna
Vienna_rest$Cuisine.Style <- as.character(Vienna_rest$Cuisine.Style)

#create a tidy text data frame for Amsterdam restaurant reviews
tidy_V_Cuisines <- Vienna_rest %>%
  select(X, Cuisine.Style) %>%
  unnest_tokens(word, Cuisine.Style)

tidy_V_Cuisines$word[tidy_V_Cuisines$word == 'middle'] <- "middle eastern"
tidy_V_Cuisines$word[tidy_V_Cuisines$word == 'wine'] <- "wine bar"


#arrange words from Amsterdam restaurant reviews in decending order
V_cuisine_count <- tidy_V_Cuisines %>%
  count(word) %>%
  arrange(desc(n))

non_cuisines <- c("european", "vegetarian", "friendly", "options", 
                  "gluten", "free", "fast", "vegan", "healthy", "grill", 
                  "food", "central", "south", "fusion")

#remove words that aren't cuisines
V_cuisine_count <- V_cuisine_count[-grep(paste(non_cuisines, collapse = '|'), V_cuisine_count$word),]

#plot cuisines
ggplot(V_cuisine_count[seq(1,25, by=1),], aes(x=reorder(word, n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Vienna Cuisines")+
  xlab("")+
  guides(fill=FALSE) + coord_flip()

#wordcloud of cuisines
V_cuisine_count %>%
  with(wordcloud(word, n, min.freq=20, random.order = FALSE,
                 colors = brewer.pal(6, "Spectral")))


