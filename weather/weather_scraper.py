from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from time import sleep
import csv


def scrape_weather(month, year, with_colname=True):
    # driver.get("https://www.wunderground.com/history/monthly/EGMC/date/{0}-{1}?req_city=London&req_statename=United%20Kingdomo".format(year, month))
    driver.get("https://www.wunderground.com/history/monthly/fr/paris-le-bourget/LFPB/date/{0}-{1}".format(year, month))
    weather_table = driver.find_elements_by_xpath('/html/body/app/city-history/city-history-layout/div/div[2]/section/div[2]/div[3]/div/div[1]/div/div/city-history-observation/div/div[2]/table')

    weather_heads = weather_table[0].find_elements(By.TAG_NAME, "thead") # get all of the rows in the table
    names_pre = list()
    for row in weather_heads:
        # Get the columns (all the column 2)        
        cols = row.find_elements(By.TAG_NAME, "td")
        
        for col in cols:
            names_pre.append(col.text)
            pass
        pass

    weather_body = weather_table[0].find_elements(By.TAG_NAME, "tbody")[0]
    weathers = weather_body.find_elements_by_xpath('tr/td')
    
    names = list()
    contents = list()
    for weather in weathers:
        row = list()
        cols = weather.find_elements_by_xpath('table/tbody')[0]
        print("test")
        trs = cols.find_elements_by_xpath('tr')
        cnames = trs[0].text.split(' ')
        
        # add cnames
        names.append(cnames)
        
        content = list()
        for tr in trs[1:]:
            content.append(tr.text)
            pass
        contents.append(content)
        pass
    
    new_names = list()
    new_names.append("day")
    for i in range(1, 7):
        for name in names[i]:
            new_names.append(names_pre[i] + "_" + name)
            pass
        pass
    new_names.append("date")

    num_days = len(contents[0])
    print(num_days)
    contents = list(zip(contents[0], contents[1], contents[2], contents[3],
                        contents[4], contents[5], contents[6],
                        [str(year*100+month)]*num_days))
    new_contents = list()
    for row in contents:
        new_row = list()
        for col in row:
            new_row.append(col.split())
            pass
        new_row = [item for sublist in new_row for item in sublist]
        new_contents.append(new_row)
        pass

    # write to file
    file_name = "weather_data.csv"
    with open(file_name, mode='a') as weather_file:
        writer = csv.writer(weather_file, delimiter = ',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        if (with_colname):
            writer.writerow(new_names)
            pass
        for row in new_contents:
            writer.writerow(row)
            pass
        pass
    pass


with_cname = True
sleep_index = 0
# driver.set_page_load_timeout(3000)

driver = webdriver.Firefox()
ublock_path = "/Users/wangzhenhua/Library/Application Support/Firefox/Profiles/od7mcixl.default/extensions/uBlock0@raymondhill.net.xpi"
driver.install_addon(ublock_path, temporary=True)

for year in range(2016, 2019):
    for month in range(1, 13):
        #if (sleep_index % 3 == 0):
        #    sleep(5)
        #    driver.close()
        #    driver = webdriver.Firefox()
        scrape_weather(month, year, with_cname)
        with_cname = False
        pass
    pass
    