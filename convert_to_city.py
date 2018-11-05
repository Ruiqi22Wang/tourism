from geopy.geocoders import Nominatim
import pandas as pd

hotel_addr = pd.read_csv("hotel_addr.csv")

geolocator = Nominatim(timeout=200)

#hotel_addr.dropna()

with open("hotel_city.txt", "a") as city:
    for index, addr in hotel_addr.dropna().iterrows():
        # print (addr["lat"], addr["lng"])
        location = geolocator.reverse("{0},{1}".format(addr["lat"], addr["lng"]))
        if "city" in location.raw["address"]:
            print(addr["Hotel_Address"]+ "@" + location.raw["address"]["city"] + "\n")
            city.write(addr["Hotel_Address"]+ ", " + location.raw["address"]["city"] + "\n")
        elif "county" in location.raw["address"]:
            print(addr["Hotel_Address"]+ "@" + location.raw["address"]["county"] + "\n")
            city.write(addr["Hotel_Address"]+ ", " + location.raw["address"]["county"] + "\n")
        else:
            print(addr["Hotel_Address"]+ "@" + location.raw["address"]["city_district"] + "\n")
            city.write(addr["Hotel_Address"]+ ", " + location.raw["address"]["city_district"] + "\n")
        pass
    pass

with open("hotel_city.txt", "a") as city:
    for index, addr in hotel_addr.iloc[1303:].dropna().iterrows():
        # print (addr["lat"], addr["lng"])
        location = geolocator.reverse("{0},{1}".format(addr["lat"], addr["lng"]))
        if "city" in location.raw["address"]:
            print(addr["Hotel_Address"]+ "@" + location.raw["address"]["city"] + "\n")
            city.write(addr["Hotel_Address"]+ ", " + location.raw["address"]["city"] + "\n")
        elif "county" in location.raw["address"]:
            print(addr["Hotel_Address"]+ "@" + location.raw["address"]["county"] + "\n")
            city.write(addr["Hotel_Address"]+ ", " + location.raw["address"]["county"] + "\n")
        else:
            print(addr["Hotel_Address"]+ "@" + location.raw["address"]["city_district"] + "\n")
            city.write(addr["Hotel_Address"]+ ", " + location.raw["address"]["city_district"] + "\n")
        pass
    pass
