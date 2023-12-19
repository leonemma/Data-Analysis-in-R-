library(DBI)
library(tidyverse)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(),"database.sqlite")
con
dbListTables(con)

player_weight <- dbGetQuery(con,'SELECT player_name,weight FROM Player WHERE weight > 100 AND player_name LIKE "%Messi%"')
head(player_weight,10)

dbWriteTable(con, "flights", nycflights13::flights)
dbWriteTable(con, "planes", nycflights13::planes)
dbWriteTable(con, "airlines", nycflights13::airlines)

str(dbReadTable(con,'flights'))

delays <- dbGetQuery(con, "SELECT * FROM flights WHERE arr_delay > 20")
head(delays)

flights_airlines_leftjoin<- dbGetQuery(con,"SELECT flights.*, airlines.name FROM flights LEFT JOIN airlines")

str(flights_airlines_leftjoin)

flights <- tbl(con, "flights")
head(flights)

delays <- flights %>%
  filter(arr_delay > 20)
head(delays)

planes <- tbl(con, "planes")
head(planes)

airlines <- tbl(con, "airlines")
head(airlines)

delays %>%
  dbplyr::sql_render()

# Run the dim() ncol() and nrow()
dim(flights)

ncol(flights)

nrow(flights)

# To get the number of observations we can use tall()
tally(flights)

glimpse(flights)

delays_by_airport <- flights %>%
  group_by(origin) %>%
  summarize(mean_dep_delay = mean(dep_delay),
            mean_arr_delay = mean(arr_delay)) %>%
  arrange(-mean_arr_delay)
head(delays_by_airport)

# What happens behind the scenes
delays_by_airport %>%
  dbplyr::sql_render()

# Mean delays by carrier and origin.
delays_airport_airline <- flights %>%
  group_by(origin, carrier) %>%
  summarize(mean_dep_delay = mean(dep_delay),
            mean_arr_delay = mean(arr_delay)) %>%
  arrange(-mean_arr_delay) %>%
  left_join(airlines)
head(delays_airport_airline)

delays_airport_airline %>%
  dbplyr::sql_render()

# Mean delays by manufacturer for the AA airline
aa_delays_by_manufacturer <- flights %>%
  filter(carrier == "AA") %>%
  left_join(planes, by = "tailnum") %>%
  group_by(manufacturer) %>%
  summarize(mean_dep_delay = mean(dep_delay),
            mean_arr_delay = mean(arr_delay)) %>%
  arrange(-mean_arr_delay)
head(aa_delays_by_manufacturer)


# Gathering Data Locally
df <- aa_delays_by_manufacturer %>%
  collect()
df

flights %>%
  group_by(month, origin) %>%
  summarize(mean_arr_delay = mean(arr_delay)) %>%
  collect()

# Take the outcome from the collect function and 
# pass it to a plot function
flights %>%
  group_by(month, origin) %>%
  summarize(mean_arr_delay = mean(arr_delay)) %>%
  collect() %>%
  ggplot(aes(x = month, y = mean_arr_delay)) +
  geom_line(aes(color = origin)) +
  geom_point(aes(color = origin))

dbDisconnect(con)
