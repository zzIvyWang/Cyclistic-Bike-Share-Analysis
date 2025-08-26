library(readr)  # Use readr instead of readxl for .csv files
library(dplyr)
library(hms)

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



