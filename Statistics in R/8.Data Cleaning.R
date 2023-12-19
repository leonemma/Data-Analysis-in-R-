library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

View(weather)
glimpse(weather)

head(weather)

weather <- weather[,-1]

weather <- gather(weather,key = day,value = value,c("X1":"X31"))
weather <- spread(weather,key = measure,value = value)
summary(weather)

weather$day <- str_replace(weather$day, "X", "")

weather <- unite(weather,date,1:3,sep = "-")

weather$date <- ymd(weather$date)
class(weather$date)

glimpse(weather)

# first, move the Events variable to column 2
weather <- weather %>% 
  select(date, Events, CloudCover:WindDirDegrees)

# convert the other measure columns to numeric
convert_num_weather <- mutate_each(weather,
                                   funs = list(as.numeric),
                                   CloudCover:WindDirDegrees)
glimpse(convert_num_weather)

# check if all the values in each column are numeric with
# all.is.numeric from Hmisc package
weather %>% 
  select(CloudCover:WindDirDegrees) %>% 
  mutate_all(all.is.numeric) %>% 
  map(all)
