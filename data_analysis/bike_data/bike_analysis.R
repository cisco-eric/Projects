#install packages
install.packages("tidyverse")
install.packages("hms")
install.packages("RcolorBrewer")
install.packages("scales")

#load libraries that I will be using for this analysis
library(tidyverse)
library(hms)
library(RColorBrewer)
library(scales)

#loading a 2022 bike data and storing them in data frames
jan_df <- read.csv("202201-divvy-tripdata.csv")
feb_df <- read.csv("202202-divvy-tripdata.csv")
mar_df <- read.csv("202203-divvy-tripdata.csv")
apr_df <- read.csv("202204-divvy-tripdata.csv")
may_df <- read.csv("202205-divvy-tripdata.csv")
jun_df <- read.csv("202206-divvy-tripdata.csv")
jul_df <- read.csv("202207-divvy-tripdata.csv")
aug_df <- read.csv("202208-divvy-tripdata.csv")
sep_df <- read.csv("202209-divvy-publictripdata.csv")
oct_df <- read.csv("202210-divvy-tripdata.csv")
nov_df <- read.csv("202211-divvy-tripdata.csv")
dec_df <- read.csv("202212-divvy-tripdata.csv")

#combining the data frames into one over all data frame
bike_df <- rbind(jan_df, feb_df, mar_df, apr_df, may_df, jun_df, 
                 jul_df, aug_df, sep_df, oct_df, nov_df, dec_df)

#add a column to bike_df which is the total time of each trip
bike_df<- bike_df %>% 
  mutate(trip_length = as_hms(difftime(bike_df$ended_at, bike_df$started_at)))

#add a column which is the day of the week represented as  a number where Sunday
# and Saturday is 7
bike_df<- bike_df %>% 
  mutate(weekday= wday(bike_df$started_at))

#create two new data frames one for casual and one for members
members <- bike_df %>% filter(member_casual == "member")
casuals <- bike_df %>% filter(member_casual == "casual")

#calculates the average trip time for both members and casuals riders
as_hms(mean.difftime(as_hms(difftime(members$ended_at, members$started_at))))
as_hms(mean.difftime(as_hms(difftime(casuals$ended_at, casuals$started_at))))
as_hms(mean.difftime(as_hms(difftime(bike_df$ended_at, bike_df$started_at))))

#finding out how many which days of the week have 
#the most rides based off membership status
members_rides_per_day<- members %>% count(weekday) 
casuals_rides_per_day<- casuals %>% count(weekday)

#create a variable for the total amount of rides per 
#membership status
total_member_rides<-sum(members_rides_per_day$n)
per_member_rides<- percent(members_rides_per_day$n/total_member_rides)
total_casual_rides<-sum(casuals_rides_per_day$n)

monthly_members<- bike_df %>% group_by(month(started_at))%>% 
  count(member_casual)%>%filter(member_casual=="member")

monthly_casuals<- bike_df %>% group_by(month(started_at))%>% 
  count(member_casual)%>%filter(member_casual=="casual")
##############################################################################

# We can see that there is a higher ratio of member rides during certain months.
# Jan-Apr, Nov, and Dec. These will be our months of focus.
# We will focus the rest of the analysis on these months.

# Create a new data frame with the months we will be focusing on
focus_months_df <- rbind(bike_df1, bike_df2, bike_df3,
                         bike_df4, bike_df11, bike_df12)

# add two new columns one for trip length and day of the week.
focus_months_df<- focus_months_df %>% 
  mutate(trip_length = as_hms(difftime(focus_months_df$ended_at,
                                       focus_months_df$started_at)))
focus_months_df<- focus_months_df %>% 
  mutate(weekday= wday(focus_months_df$started_at))

# separate members and casual riders
focus_members <- focus_months_df %>% filter(member_casual == "member")
focus_casuals <- focus_months_df %>% filter(member_casual == "casual")

as_hms(mean.difftime(as_hms(difftime(focus_members$ended_at, focus_members$started_at))))
as_hms(mean.difftime(as_hms(difftime(focus_casuals$ended_at, focus_casuals$started_at))))
as_hms(mean.difftime(as_hms(difftime(focus_months_df$ended_at, focus_months_df$started_at))))

focus_member_day<- focus_members %>% count(weekday) 
focus_casual_day<- focus_casuals %>% count(weekday)
