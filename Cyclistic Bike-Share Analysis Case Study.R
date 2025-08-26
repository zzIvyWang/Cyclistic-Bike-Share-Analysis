library(tidyverse) 
library(lubridate)
q1_2019_df <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020_df <- read.csv("Divvy_Trips_2020_Q1.csv")

colnames(q1_2019_df)
colnames(q1_2020_df)

(q1_2019_df <- rename(q1_2019_df
                     ,ride_id = trip_id
                     ,rideable_type = bikeid
                     ,started_at = start_time
                     ,ended_at = end_time
                     ,start_station_name = from_station_name
                     ,start_station_id = from_station_id
                     ,end_station_name = to_station_name
                     ,end_station_id = to_station_id
                     ,member_casual = usertype
                     ))
# Inspect the dataframes and look for incongruencies
str(q1_2019_df)
str(q1_2020_df)
q1_2019_df <- mutate(q1_2019_df, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type))

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q1_2019_df, q1_2020_df)

all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, gender, birthyear, 
            tripduration))
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"
                                ))
table(all_trips$member_casual)
# Add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
str(all_trips)

is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(all_trips$ride_length) 
is.numeric(all_trips$ride_length)
# The dataframe includes a few hundred entries when bikes were taken out of docks 
#and checked for quality by Divvy or ride_length was negative
all_trips_V2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length <0),]
rm(Clean_Divvy_df, Divvy_df)

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
mean(all_trips_V2$ride_length) #straight average (total ride length / rides)
median(all_trips_V2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_V2$ride_length) #longest ride
min(all_trips_V2$ride_length) #shortest ride
summary(all_trips_V2$ride_length)

aggregate(all_trips_V2$ride_length ~ all_trips_V2$member_casual, FUN = mean)
aggregate(all_trips_V2$ride_length ~ all_trips_V2$member_casual, FUN = median)
aggregate(all_trips_V2$ride_length ~ all_trips_V2$member_casual, FUN = max)
aggregate(all_trips_V2$ride_length ~ all_trips_V2$member_casual, FUN = min)
# Notice that the days of the week are out of order. Let's fix that.
all_trips_V2$day_of_week <- ordered(all_trips_V2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_V2$ride_length ~ all_trips_V2$member_casual + all_trips_V2$day_of_week, FUN = mean)

all_trips_V2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday) %>%  
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_V2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday) %>%  
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips_V2$ride_length ~ all_trips_V2$member_casual + all_trips_V2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')



