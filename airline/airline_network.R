setwd("~/Desktop/Graduate/duke/MIDS/semester 1/text-scraping/project/tourism/airline")
library(tidyverse)

airlines = read_csv("airlines.csv")
airports = read_csv("airports.dat.txt", col_names = F)
routes = read_csv("routes.csv")
names(airports) <- c("Airpot_ID", "Airport_Name", "City", "Country", "IATA", 
                    "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", 
                    "DST", "Tz", "Type", "Source")
countries <- read_csv("countries_of_the_world.csv")

air_country = airlines %>%
  group_by(Country) %>%
  summarise(num = n()) %>%
  arrange(desc(num))

routes_coord = routes %>%
  mutate(flight_id = 1:nrow(routes)) %>%
  left_join((airports %>% select(IATA, Latitude, Longitude))
            , by = c("source_airport" = "IATA")) %>%
  rename(source_lat = Latitude, source_lng = Longitude) %>%
  left_join((airports %>% select(IATA, Latitude, Longitude, City))
            , by = c("destination_airport" = "IATA")) %>%
  rename(destination_lat = Latitude, destination_lng = Longitude, destination_city = City)
  
routes_coord_group = rbind(routes_coord %>%
                     select(flight_id, source_lat, source_lng, destination_city) %>%
                     rename(lat = source_lat, lng = source_lng), 
                     routes_coord %>%
                     select(flight_id, destination_lat, destination_lng, destination_city) %>%
                     rename(lat = destination_lat, lng = destination_lng))

ggplot(data = routes_coord) +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") + 
  geom_point(aes(x = source_lng, y = source_lat), 
             size = .1, alpha = .5, colour = "red") + 
  geom_point(aes(x = destination_lng, y = destination_lat), 
             size = .1, alpha = .5, colour = "yellow") + 
  geom_line(data = routes_coord_group, aes(x = lng, y = lat, group = flight_id), 
             alpha = 0.05, colour = "red")

# calculate in degree for the five cities
city_routes_coord = routes_coord %>% 
  group_by(destination_city) %>%
  summarise(num = n()) %>%
  na.omit() %>%
  arrange(desc(num))

# routes whose destination are in the five cities
ggplot(data = routes_coord) +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") + 
  geom_point(aes(x = source_lng, y = source_lat), 
             size = .1, alpha = .5, colour = "red") + 
  geom_point(aes(x = destination_lng, y = destination_lat), 
             size = .1, alpha = .5, colour = "yellow") + 
  geom_line(data = (routes_coord_group %>%
                      group_by(destination_city) %>%
                      filter(any(destination_city=="London") ||
                               any(destination_city=="Barcelona") ||
                               any(destination_city=="Paris") ||
                               any(destination_city=="Vienna") ||
                               any(destination_city=="Milan"))), 
            aes(x = lng, y = lat, group = flight_id, colour = destination_city), 
            alpha = 0.1)
