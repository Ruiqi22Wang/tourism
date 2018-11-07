setwd("~/Desktop/Graduate/duke/MIDS/semester 1/text-scraping/project/tourism")
library(tidyverse)
library(tidytext)
library(dplyr)
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

tidy_hotel = hotel %>%
  dplyr::select(city, Positive_Review) %>%
  unnest_tokens("word", Positive_Review) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(city, sentiment) 
  
