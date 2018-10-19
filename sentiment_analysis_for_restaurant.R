library(tidyverse)
rest = read_csv("TA_restaurants_curated.csv")

library(tidytext)
library(dplyr)


# only consider the restaurants who has been reviewed
rest_count = rest[rest$Reviews != "[[], []]",] %>%
  group_by(City) %>%
  count()



tidy_rest = rest %>%
  dplyr::select(City, Reviews) %>%
  unnest_tokens("word", Reviews)

data("stop_words")
tidy_rest<-tidy_rest %>%
  anti_join(stop_words)

tidy_rest<-tidy_rest[-grep("\\b\\d+\\b", tidy_rest$word),]

tidy_rest$word <- gsub("\\s+","",tidy_rest$word)

library(SnowballC)
tidy_rest<-tidy_rest %>%
  mutate_at("word", funs(wordStem((.), language="en")))

tidy_rest %>%
  count(word) %>%
  arrange(desc(n))

rest_sentiment <- tidy_rest %>%
  inner_join(get_sentiments("bing")) %>%
  filter(sentiment=="positive") %>%
  count(City, sentiment)

head(rest_sentiment)

library(ggplot2)

rest_sentiment_portion = data.frame(City = rest_sentiment$City, n = rest_sentiment$n / rest_count[1:31,]$n) %>%
  arrange(desc(n))
rest_sentiment_portion$City <- factor(rest_sentiment_portion$City,
                                      levels = order(rest_sentiment_portion$n))


ggplot(rest_sentiment_portion, aes(x=City, y=n))+
  geom_point(color="red")+
  theme_minimal()+
  ylab("Percentage of Positive Words in diff cities")+
  xlab("id")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rest_rating = rest %>%
  na.omit() %>%
  group_by(City) %>%
  summarise(average_rating = mean(Rating)) %>%
  arrange(desc(average_rating))
