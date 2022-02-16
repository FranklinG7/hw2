# load libraries
library(tidyverse)
library(lubridate)

# read the data, and look at the first 20 rows
data = read_csv("rest.csv")
View(data[1:20,])

# Visualize the overall distribution of inspection scores using a histogram
ggplot(data, aes(x=SCORE)) + geom_histogram(bins = 30)

# Is there any trend in terms of how highly older vs. newer restaurants score on their inspections?
# Parse the restaurant open dates so it can be graphed
data$RESTAURANTOPENDATE = ymd_hms(data$RESTAURANTOPENDATE)

# Use floor_date to get the beginning of the month
data$opendate = floor_date(data$RESTAURANTOPENDATE, unit="month")
years_open = group_by(data,opendate)

#Create a graph to see if there is a trend
ggplot(years_open, aes(x=RESTAURANTOPENDATE, y=SCORE)) +
  geom_point()

#Do the inspection score vary by city?
#View all the city names
unique(data$CITY)
#Recode function so there is only one estimated value per city
data$citytwo = recode(data$CITY, "Raleigh" = "RALEIGH","Cary" = "CARY",
                       "FUQUAY-VARINA" = "FUQUAY VARINA", "Fuquay Varina" = "FUQUAY VARINA", "Fuquay-Varina" = "FUQUAY VARINA",
                       "Garner" = "GARNER", "Morrisville" = "MORRISVILLE", "MORRISVILE" = "MORRISVILLE", 
                       "RESEARCH TRIANGLE PARK" = "RTP", "Apex" = "APEX",
                       "HOLLY SPRING" = "HOLLY SPRINGS", "Holly Springs" = "HOLLY SPRINGS", "Wake Forest" = "WAKE FOREST", "Zebulon" = "ZEBULON")
#View the updated city names
unique(data$citytwo)
#Visualization of score vs city
ggplot(data, aes(x=SCORE, y=data$citytwo)) + geom_point()
#Do the inspection scores vary by city?
group_by(data, data$citytwo) %>%
  summarize(mean_score = mean(SCORE))

#Do inspection scores vary by inspector?
unique(data$INSPECTOR)
inspect = group_by(data, INSPECTOR) %>%
  summarize(mean_score = mean(SCORE))
view(inspect)
#Sample size of each group
count(data, citytwo)
count_inspect = count(data, INSPECTOR)
view(count_inspect)

#Do inspection scores vary by time period?
data$year = year(data$RESTAURANTOPENDATE)
unique(data$year)
score_year = group_by(data, year) %>%
  summarize(mean_score = mean(SCORE))
View(score_year)

#Are the scores for restaurants higher than other types of facility?
unique(data$FACILITYTYPE)
group_by(data, FACILITYTYPE) %>%
  summarize(mean_score = mean(SCORE))

#Visualize distribution of inspection scores for restaurants
rest_data = filter(data, FACILITYTYPE == "Restaurant")
ggplot(rest_data, aes(x=SCORE)) + geom_histogram(bins = 30)
#trend in terms of how highly older vs. newer restaurants score on their inspections?
ggplot(rest_data, aes(x=RESTAURANTOPENDATE, y=SCORE)) +
  geom_point()

#Do the inspection scores for restaurants vary by city?
group_by(rest_data, citytwo) %>%
  summarize(mean_score = mean(SCORE))

#Do inspection scores for restaurants vary by inspector?
restaurant_inspector = group_by(rest_data, INSPECTOR) %>%
  summarize(mean_score = mean(SCORE))
view(restaurant_inspector)
#Sample Size for restaurant data
count(rest_data, citytwo)
count_rest_inspector = count(rest_data, INSPECTOR)
view(count_rest_inspector)