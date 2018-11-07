setwd("~/Desktop/Graduate/duke/MIDS/semester 1/text-scraping/project/tourism")
library(tidyverse)
library(tidytext)
library(dplyr)
library(lubridate)
hotel = read_csv("Hotel_Reviews.csv")

## extract address and coordinates
# addr = hotel %>%
#   select(Hotel_Address, lat, lng) %>%
#   distinct(Hotel_Address, lat, lng)
#   
# 
# write.csv(file = "hotel_addr.csv", addr)
# 
# coordna_city = unique(hotel$Hotel_Address[is.na(hotel$lat)])
# write.csv(file = "coordna_city.csv", coordna_city)

## load cities
hotel_city = read.table(file = "hotel_city.txt", sep = ",", stringsAsFactors = F)
names(hotel_city) = c("Hotel_Address", "city")

hotel = hotel %>%
  left_join(hotel_city)
hotel$city = trimws(hotel$city)

# fix geodecode inconsistency
hotel$city[hotel$city == "BCN"] = "Barcelona"
hotel$city[hotel$city == "Bobigny"] = "Paris"
hotel$city[hotel$city == "City of London"] = "London"
hotel$city[hotel$city == "Innere Vienna"] = "Vienna"
hotel$city[hotel$city == "London Borough of Islington"] = "London"
hotel$city[hotel$city == "MI"] = "Milan"
hotel$city[hotel$city == "Milano"] = "Milan"

# check "city" is contained in address
for (i in 1:length(hotel$city)) {
  if (!grepl(hotel$city[i], hotel$Hotel_Address[i])) {
    print(paste(toString(i), "not matched"))
  } else {
    print(paste(toString(i),"matched"))
  }
}


# delete positive reviews based on its "positive word counts"
hotel$Positive_Review[hotel$Review_Total_Positive_Word_Counts == 0] = ""
# delete negative reviews based on its "negative word counts"
hotel$Negative_Review[hotel$Review_Total_Negative_Word_Counts == 0] = ""

# overall sentiment
tidy_hotel = hotel %>%
  dplyr::select(city, Positive_Review) %>%
  unnest_tokens("word", Positive_Review) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(city, sentiment) 
  
# sentiment analysis across times for each city
city_sentiments = list()
#hotel$Review_Date = as.Date(hotel$Review_Date,format='%m/%d/%Y')
hotel$Review_Date = mdy(hotel$Review_Date)
# group by year_month
hotel$Review_YearMonth = rep(0, length(hotel$Review_Date))
hotel$Review_YearMonth = paste0(year(hotel$Review_Date), month(hotel$Review_Date))

for (city in unique(hotel$city)) {
  city_hotel = hotel[hotel$city == city, ]
  tidy_city_hotel = city_hotel %>%
    dplyr::select(Review_YearMonth, Positive_Review) %>%
    unnest_tokens("word", Positive_Review) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(Review_YearMonth, sentiment) 
  city_sentiments = append(city_sentiments, list(tidy_city_hotel))
}

positive = city_sentiments[[1]] %>%
  filter(sentiment=="positive")
negative = city_sentiments[[1]] %>%
  filter(sentiment=="negative")
ggplot()+
  geom_line(group = 1, data = negative, aes(x = Review_YearMonth, y = n), colour = "grey")+
  geom_line(group = 2, data = positive, aes(x = Review_YearMonth, y = n), colour="blue")+
  theme_minimal()+
  ylab("TODO")+
  xlab("Date")

#求每个城市中不同时间段里的评论个数 TODO！
