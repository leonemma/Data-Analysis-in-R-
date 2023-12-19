library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)

View(weather)
weather <- weather[,-1]
weather <- gather(weather,key = day,value = value,c("X1":"X31"))
weather <- spread(weather,key = measure,value = value)

weather$day <- str_replace(weather$day,"X"," ")

weather <- unite(weather,date,1:3,sep = "-")

weather$date <- ymd(weather$date)
class(weather$date)

weather$PrecipitationIn <- str_replace(weather$PrecipitationIn,"T","0")
class(weather$PrecipitationIn)

str(weather)

char_vars <- names(weather[4:23])

for (var in char_vars) {
  weather[[var]] <- as.numeric(weather[[var]])
}
str(weather)

weather$CloudCover <- as.factor(weather$CloudCover)
levels(weather$CloudCover)

any(is.na(weather))
sum(is.na(weather))
summary(weather)

weather <- na.omit(weather)
summary(weather)
any(is.na(weather))


