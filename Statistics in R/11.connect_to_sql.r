# install.packages("RSQLite")
library(DBI) # library(RSQLite) not required

# Connect to an SQLite file
con <- DBI::dbConnect(RSQLite::SQLite(), "data/iris.sqlite")
con

# List all the Tables
dbListTables(con)

# Read the table
dbReadTable(con, "Iris")

# Disconnect from the database
dbDisconnect(con)
con

# Introduction to dplyr for Databases -----------------------------------------------------------------
# install.packages(c("tidyverse", "RSQLite", "nycflights13"))
library(tidyverse)
library(DBI)


# Connect to the database
con <- dbConnect(RSQLite::SQLite(), ":memory:")
con # Is a DBIConnection object

# If you want to keep anything the database replace the ":memory:" with an sqlite file
# con <- dbConnect(RSQLite::SQLite(), "data/local.sqlite")

# Copy data frame data to database tables
dbWriteTable(con, "flights", nycflights13::flights)
dbWriteTable(con, "planes", nycflights13::planes)
dbWriteTable(con, "airlines", nycflights13::airlines)

# Check the structure of the table flights
str(dbReadTable(con, "flights"))

# Get the flights with more than 20 minutes arrival delay using SQL
delays <- dbGetQuery(con, "SELECT * FROM flights WHERE arr_delay > 20")
head(delays)

# Join the flights table with the airlines table to get an extra column with the airline name
flights_airlines_leftjoin <- dbGetQuery(con, "SELECT flights.*, airlines.name FROM flights LEFT JOIN airlines")
head(flights_airlines_leftjoin)
str(flights_airlines_leftjoin)

# Join the flights table with the planes table to get the manufacturer name and then filter the results for a specific carrier (ex. AA)
flights_aa_planes_manufacturer <- dbGetQuery(con, "SELECT flights.*, planes.manufacturer FROM flights LEFT JOIN planes ON flights.tailnum = planes.tailnum WHERE carrier == 'AA'")
head(flights_aa_planes_manufacturer)
str(flights_aa_planes_manufacturer)

# Use tbl() to create a pointer from the database table and store it in a variable
flights <- tbl(con, "flights")
head(flights)

# Use filter() to get the previous result
delays <- flights %>%
  filter(arr_delay > 20)

head(delays)

# Add pointers to the other tables
planes <- tbl(con, "planes")
head(planes)

airlines <- tbl(con, "airlines")
head(airlines)

# Inspect the underling query
delays %>%
  dbplyr::sql_render()

# Run the dim() ncol() and nrow() functions to a remote table
dim(flights)
ncol(flights)
nrow(flights)

# To get the number of observations we can use tall()
tally(flights)

# Use the glimpse function
glimpse(flights)


# More Advanced Queries ---------------------------------------------------

# Which airport has the longest-delayed arrivals?
delays_by_airport <- flights %>%
  group_by(origin) %>%
  summarize(mean_dep_delay = mean(dep_delay), mean_arr_delay = mean(arr_delay)) %>%
  arrange(-mean_arr_delay)

head(delays_by_airport)

# What happens behind the scenes
delays_by_airport %>%
  dbplyr::sql_render()

# Mean delays by carrier and origin.
delays_airport_airline <- flights %>%
  group_by(origin, carrier) %>%
  summarize(mean_dep_delay = mean(dep_delay), mean_arr_delay = mean(arr_delay)) %>%
  arrange(-mean_arr_delay) %>%
  left_join(airlines)
head(delays_airport_airline)

# Mean delays by manufacturer for the AA airline
aa_delays_by_manufacturer <- flights %>%
  filter(carrier == "AA") %>%
  left_join(planes, by = "tailnum") %>%
  group_by(manufacturer) %>%
  summarize(mean_dep_delay = mean(dep_delay), mean_arr_delay = mean(arr_delay)) %>%
  arrange(-mean_arr_delay)

head(aa_delays_by_manufacturer)

# Can't use functions from other packages
delays_error <- flights %>%
  filter(stringr::str_detect(tailnum, "ND"))


# Final Steps -------------------------------------------------------------
# Gathering Data Locally
df <- aa_delays_by_manufacturer %>%
  collect()
df

# Plotting
# Take the outcome from the collect function and pass it to a plot function
flights %>%
  group_by(month, origin) %>%
  summarize(mean_arr_delay = mean(arr_delay)) %>%
  collect() %>%
  ggplot(aes(x = month, y = mean_arr_delay)) +
  geom_line(aes(color = origin)) +
  geom_point(aes(color = origin))

# Disconnect
dbDisconnect(con)