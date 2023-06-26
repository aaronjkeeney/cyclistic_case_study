library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
getwd()
setwd("~/Desktop/Coding Projects/Capstone_Cyclistic_Raw_Data/Cyclistic_csv_files")

## Note: changed September from "trippublicdata" to "tripdata" for consistency
Jun_2022 <- read_csv("202206-divvy-tripdata.csv")
Jul_2022 <- read_csv("202207-divvy-tripdata.csv")
Aug_2022 <- read_csv("202208-divvy-tripdata.csv")
Sep_2022 <- read_csv("202209-divvy-tripdata.csv")
Oct_2022 <- read_csv("202210-divvy-tripdata.csv")
Nov_2022 <- read_csv("202211-divvy-tripdata.csv")
Dec_2022 <- read_csv("202212-divvy-tripdata.csv")
Jan_2023 <- read_csv("202301-divvy-tripdata.csv")
Feb_2023 <- read_csv("202302-divvy-tripdata.csv")
Mar_2023 <- read_csv("202303-divvy-tripdata.csv")
Apr_2023 <- read_csv("202304-divvy-tripdata.csv")
May_2023 <- read_csv("202305-divvy-tripdata.csv")

spec(Jun_2022)
colnames(Jun_2022)
colnames(May_2023)

## Note: Due to updates, this is a different function than the tutorial,
##        but it achieves the correct result. 

all_trips <- rbind(Jun_2022, Jul_2022, Aug_2022, Sep_2022, Oct_2022, 
                       Nov_2022, Dec_2022, Jan_2023, Feb_2023, Mar_2023,
                       Apr_2023, May_2023)

## Since birth year was removed, I decided to leave in the longitude and latitude.
## I am hopeful that geographic data will be useful in neighborhood traffic tracking.


str(all_trips)
nrow(all_trips)
ncol(all_trips)
dim(all_trips)
head(all_trips)
summary(all_trips)

head(all_trips)

## From Stack Overflow--How to differentiate and count objects in a column
## This code confirmed that no cleaning was necessary to differentiate members
## from casual riders

categories <- unique(all_trips$member_casual)
number_of_categories <- length(categories)
list(categories)

table(all_trips$member_casual)

## These next steps split the datetime column into more human time, day, month, etc.

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- weekdays(as.Date(all_trips$date))

##Checking to see if there are unlabeled or mislabeled rows
table(all_trips$month) ## there were 12, as expected
table(all_trips$day) ## 31, as expected
table(all_trips$year) ## 2, as expected
table(all_trips$day_of_week) ## weekday function worked! Date extraction did
                             ## not, thanks stack overflow

## Calculating the ride length
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
## This data is now numeric--good to go.

## Need to remove negative trip lengths and Quality control checks
## This took a different route with subsets, but this worked well
all_trips_v2 <- subset(all_trips, all_trips$ride_length >= 0 
                       & all_trips$start_station_name != "HQ QR")

## This is the original function according to the work guide
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR"
                            | all_trips$ride_length<0),]

## I think I may have missed this comma the first time around

##Quick check for oddities
min(all_trips_v2$ride_length)
list(unique(all_trips_v2$start_station_name))
summary(all_trips_v2$ride_length)
## The maximum ride length implies the possibility of some outliers.

## DESCRIPTIVE STATISTICS AND ANALYSIS

## Basic Overview: Mean, Median, Max, Min (In seconds)
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)
sd(all_trips_v2$ride_length)

## Comparison of members and casual riders

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = length)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = sd)

## Average rides by day of the week


## Change the list of days of week to be in week order, not alphabetical
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=
                                      c("Sunday","Monday","Tuesday",
                                        "Wednesday","Thursday","Friday",
                                        "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

## Number of rides by rider type and day of the week
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

## Same calculation, but we add a plot
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual))+
  geom_col(position = "dodge")

## Plot for ride duration instead of quantity
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual))+
  geom_col(position = "dodge")

head(all_trips$start_lat)
count(all_trips_v2$member_casual)
table(all_trips_v2$member_casual)
nrow(all_trips_v2) == length(which(all_trips_v2$member_casual =="casual"))+
  length(which(all_trips_v2$member_casual =="member"))

length(unique(all_trips_v2$start_station_name))

ride_length_dist <- data.frame("Rides" = count(all_trips_v2, ride_length),
                               "Ride_Time" = unique(all_trips_v2$ride_length))

Rides <- count(all_trips_v2, ride_length)
ggplot(data = ride_length_dist, mapping = aes(x= "Ride_Time", y="Rides"))+
  geom_bar()

all_trips_v2 %>%
  group_by(ride_length) %>%
  summarise(Rides=n()) %>%
ggplot(aes(x=ride_length, y = Rides)) +
  geom_col()
       
  
  
str(ride_length_dist)

is.numeric(ride_length_dist)
