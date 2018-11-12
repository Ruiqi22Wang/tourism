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
# for (i in 1:length(hotel$city)) {
#   if (!grepl(hotel$city[i], hotel$Hotel_Address[i])) {
#     print(paste(toString(i), "not matched"))
#   } else {
#     print(paste(toString(i),"matched"))
#   }
# }


# delete positive reviews based on its "positive word counts"
hotel$Positive_Review[hotel$Review_Total_Positive_Word_Counts == 0] = ""
# delete negative reviews based on its "negative word counts"
hotel$Negative_Review[hotel$Review_Total_Negative_Word_Counts == 0] = ""

# overall sentiment
tidy_hotel_pos = hotel %>%
  dplyr::select(city, Positive_Review) %>%
  unnest_tokens("word", Positive_Review) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(city, sentiment) 
tidy_hotel_neg = hotel %>%
  dplyr::select(city, Negative_Review) %>%
  unnest_tokens("word", Negative_Review) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(city, sentiment) 
  
# sentiment analysis across times for each city
city_sentiments = list()
#hotel$Review_Date = as.Date(hotel$Review_Date,format='%m/%d/%Y')
hotel$Review_Date = mdy(hotel$Review_Date)
# group by year_month
hotel$Review_YearMonth = rep(0, length(hotel$Review_Date))
hotel$Review_YearMonth = year(hotel$Review_Date)*100 + month(hotel$Review_Date)

for (city in unique(hotel$city)) {
  city_hotel = hotel[hotel$city == city, ]
  tidy_city_hotel_pos = city_hotel %>%
    dplyr::select(Review_YearMonth, Positive_Review) %>%
    unnest_tokens("word", Positive_Review) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(Review_YearMonth, sentiment)
  tidy_city_hotel_neg = city_hotel %>%
    dplyr::select(Review_YearMonth, Negative_Review) %>%
    unnest_tokens("word", Negative_Review) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(Review_YearMonth, sentiment)
  num_month_review = city_hotel %>%
    dplyr::select(Review_YearMonth, Positive_Review) %>%
    group_by(Review_YearMonth) %>%
    summarise(num = n())
  pos_sent = tidy_city_hotel_pos$n[tidy_city_hotel_pos$sentiment == "positive"] - tidy_city_hotel_pos$n[tidy_city_hotel_pos$sentiment == "negative"]
  neg_sent = tidy_city_hotel_neg$n[tidy_city_hotel_neg$sentiment == "negative"] - tidy_city_hotel_neg$n[tidy_city_hotel_neg$sentiment == "positive"]
  
  tidy_city_hotel = data.frame(Review_YearMonth = unique(tidy_city_hotel_pos$Review_YearMonth)
                               , pos_sent = pos_sent, neg_sent = neg_sent)
  tidy_city_hotel = tidy_city_hotel %>% left_join(num_month_review) %>%
    mutate(Review_YearMonth = as.factor(Review_YearMonth))

  city_sentiments = append(city_sentiments, list(tidy_city_hotel))
}

plot_sentiments = function(sent_list, city_index) {
  city = sent_list[[city_index]]
  plot = ggplot(data = city)+
    geom_line(group = 1, aes(x = Review_YearMonth, y = (pos_sent / num)), colour = "green")+
    geom_line(group = 1, aes(x = Review_YearMonth, y = (neg_sent / num)), colour = "red")+
    theme_minimal()+
    ylab("TODO")+
    xlab("Date")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(plot)
}

plot1 = plot_sentiments(city_sentiments, 1)
plot2 = plot_sentiments(city_sentiments, 2)
plot3 = plot_sentiments(city_sentiments, 3)
plot4 = plot_sentiments(city_sentiments, 4)
plot5 = plot_sentiments(city_sentiments, 5)
plot6 = plot_sentiments(city_sentiments, 6)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=3, nrow = 3)

# why all sentiments at the last two months is unually?
t = hotel %>%
  group_by(city, Review_YearMonth) %>%
  summarise(num = n())
View(t)
