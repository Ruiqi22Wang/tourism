library(tidyverse)
library(ggmap)
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
hotel_city = read.table(file = "hotel_city.txt", sep = ",")
names(hotel_city) = c("Hotel_Address", "city")

hotel = hotel %>%
  left_join(hotel_city)
