---
title: "Cyclistic_Bike-Share_Analysis_Case_Study"
author: "Ivy"
date: "2025-08-22"
output:
  pdf_document: default
  html_document: default
---

``` r
library(tidyverse) 
```

```
## -- Attaching core tidyverse packages ------------------------ tidyverse 2.0.0 --
## v dplyr     1.1.4     v readr     2.1.5
## v forcats   1.0.0     v stringr   1.5.1
## v ggplot2   3.5.2     v tibble    3.3.0
## v lubridate 1.9.4     v tidyr     1.3.1
## v purrr     1.1.0     
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
## i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(lubridate)

q1_2019_df <- read_csv("Divvy_Trips_2019_Q1.csv")
```

```
## Rows: 365069 Columns: 12
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (6): start_time, end_time, from_station_name, to_station_name, usertype,...
## dbl (5): trip_id, bikeid, from_station_id, to_station_id, birthyear
## num (1): tripduration
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
q1_2020_df <- read_csv("Divvy_Trips_2020_Q1.csv")
```

```
## Rows: 426887 Columns: 13
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (7): ride_id, rideable_type, started_at, ended_at, start_station_name, e...
## dbl (6): start_station_id, end_station_id, start_lat, start_lng, end_lat, en...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
colnames(q1_2019_df)
```

```
##  [1] "trip_id"           "start_time"        "end_time"         
##  [4] "bikeid"            "tripduration"      "from_station_id"  
##  [7] "from_station_name" "to_station_id"     "to_station_name"  
## [10] "usertype"          "gender"            "birthyear"
```

``` r
colnames(q1_2020_df)
```

```
##  [1] "ride_id"            "rideable_type"      "started_at"        
##  [4] "ended_at"           "start_station_name" "start_station_id"  
##  [7] "end_station_name"   "end_station_id"     "start_lat"         
## [10] "start_lng"          "end_lat"            "end_lng"           
## [13] "member_casual"
```

``` r
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
```

```
## # A tibble: 365,069 x 12
##     ride_id started_at      ended_at rideable_type tripduration start_station_id
##       <dbl> <chr>           <chr>            <dbl>        <dbl>            <dbl>
##  1 21742443 2019-01-01 0:0~ 2019-01~          2167          390              199
##  2 21742444 2019-01-01 0:0~ 2019-01~          4386          441               44
##  3 21742445 2019-01-01 0:1~ 2019-01~          1524          829               15
##  4 21742446 2019-01-01 0:1~ 2019-01~           252         1783              123
##  5 21742447 2019-01-01 0:1~ 2019-01~          1170          364              173
##  6 21742448 2019-01-01 0:1~ 2019-01~          2437          216               98
##  7 21742449 2019-01-01 0:1~ 2019-01~          2708          177               98
##  8 21742450 2019-01-01 0:1~ 2019-01~          2796          100              211
##  9 21742451 2019-01-01 0:1~ 2019-01~          6205         1727              150
## 10 21742452 2019-01-01 0:1~ 2019-01~          3939          336              268
## # i 365,059 more rows
## # i 6 more variables: start_station_name <chr>, end_station_id <dbl>,
## #   end_station_name <chr>, member_casual <chr>, gender <chr>, birthyear <dbl>
```
# Inspect the dataframes and look for incongruencies

``` r
str(q1_2019_df)
```

```
## spc_tbl_ [365,069 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ ride_id           : num [1:365069] 21742443 21742444 21742445 21742446 21742447 ...
##  $ started_at        : chr [1:365069] "2019-01-01 0:04:37" "2019-01-01 0:08:13" "2019-01-01 0:13:23" "2019-01-01 0:13:45" ...
##  $ ended_at          : chr [1:365069] "2019-01-01 0:11:07" "2019-01-01 0:15:34" "2019-01-01 0:27:12" "2019-01-01 0:43:28" ...
##  $ rideable_type     : num [1:365069] 2167 4386 1524 252 1170 ...
##  $ tripduration      : num [1:365069] 390 441 829 1783 364 ...
##  $ start_station_id  : num [1:365069] 199 44 15 123 173 98 98 211 150 268 ...
##  $ start_station_name: chr [1:365069] "Wabash Ave & Grand Ave" "State St & Randolph St" "Racine Ave & 18th St" "California Ave & Milwaukee Ave" ...
##  $ end_station_id    : num [1:365069] 84 624 644 176 35 49 49 142 148 141 ...
##  $ end_station_name  : chr [1:365069] "Milwaukee Ave & Grand Ave" "Dearborn St & Van Buren St (*)" "Western Ave & Fillmore St (*)" "Clark St & Elm St" ...
##  $ member_casual     : chr [1:365069] "Subscriber" "Subscriber" "Subscriber" "Subscriber" ...
##  $ gender            : chr [1:365069] "Male" "Female" "Female" "Male" ...
##  $ birthyear         : num [1:365069] 1989 1990 1994 1993 1994 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   trip_id = col_double(),
##   ..   start_time = col_character(),
##   ..   end_time = col_character(),
##   ..   bikeid = col_double(),
##   ..   tripduration = col_number(),
##   ..   from_station_id = col_double(),
##   ..   from_station_name = col_character(),
##   ..   to_station_id = col_double(),
##   ..   to_station_name = col_character(),
##   ..   usertype = col_character(),
##   ..   gender = col_character(),
##   ..   birthyear = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

``` r
str(q1_2020_df)
```

```
## spc_tbl_ [426,887 x 13] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ ride_id           : chr [1:426887] "EACB19130B0CDA4A" "8FED874C809DC021" "789F3C21E472CA96" "C9A388DAC6ABF313" ...
##  $ rideable_type     : chr [1:426887] "docked_bike" "docked_bike" "docked_bike" "docked_bike" ...
##  $ started_at        : chr [1:426887] "2020-01-21 20:06:59" "2020-01-30 14:22:39" "2020-01-09 19:29:26" "2020-01-06 16:17:07" ...
##  $ ended_at          : chr [1:426887] "2020-01-21 20:14:30" "2020-01-30 14:26:22" "2020-01-09 19:32:17" "2020-01-06 16:25:56" ...
##  $ start_station_name: chr [1:426887] "Western Ave & Leland Ave" "Clark St & Montrose Ave" "Broadway & Belmont Ave" "Clark St & Randolph St" ...
##  $ start_station_id  : num [1:426887] 239 234 296 51 66 212 96 96 212 38 ...
##  $ end_station_name  : chr [1:426887] "Clark St & Leland Ave" "Southport Ave & Irving Park Rd" "Wilton Ave & Belmont Ave" "Fairbanks Ct & Grand Ave" ...
##  $ end_station_id    : num [1:426887] 326 318 117 24 212 96 212 212 96 100 ...
##  $ start_lat         : num [1:426887] 42 42 41.9 41.9 41.9 ...
##  $ start_lng         : num [1:426887] -87.7 -87.7 -87.6 -87.6 -87.6 ...
##  $ end_lat           : num [1:426887] 42 42 41.9 41.9 41.9 ...
##  $ end_lng           : num [1:426887] -87.7 -87.7 -87.7 -87.6 -87.6 ...
##  $ member_casual     : chr [1:426887] "member" "member" "member" "member" ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   ride_id = col_character(),
##   ..   rideable_type = col_character(),
##   ..   started_at = col_character(),
##   ..   ended_at = col_character(),
##   ..   start_station_name = col_character(),
##   ..   start_station_id = col_double(),
##   ..   end_station_name = col_character(),
##   ..   end_station_id = col_double(),
##   ..   start_lat = col_double(),
##   ..   start_lng = col_double(),
##   ..   end_lat = col_double(),
##   ..   end_lng = col_double(),
##   ..   member_casual = col_character()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

``` r
q1_2019_df <- mutate(q1_2019_df, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type))
```
# Stack individual quarter's data frames into one big data frame

``` r
all_trips <- bind_rows(q1_2019_df, q1_2020_df)

all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, gender, birthyear, 
            tripduration))
```
# Inspect the new table that has been created

``` r
colnames(all_trips)  #List of column names
```

```
## [1] "ride_id"            "started_at"         "ended_at"          
## [4] "rideable_type"      "start_station_id"   "start_station_name"
## [7] "end_station_id"     "end_station_name"   "member_casual"
```

``` r
nrow(all_trips)  #How many rows are in data frame?
```

```
## [1] 791956
```

``` r
dim(all_trips)  #Dimensions of the data frame?
```

```
## [1] 791956      9
```

``` r
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
```

```
## # A tibble: 6 x 9
##   ride_id  started_at ended_at rideable_type start_station_id start_station_name
##   <chr>    <chr>      <chr>    <chr>                    <dbl> <chr>             
## 1 21742443 2019-01-0~ 2019-01~ 2167                       199 Wabash Ave & Gran~
## 2 21742444 2019-01-0~ 2019-01~ 4386                        44 State St & Randol~
## 3 21742445 2019-01-0~ 2019-01~ 1524                        15 Racine Ave & 18th~
## 4 21742446 2019-01-0~ 2019-01~ 252                        123 California Ave & ~
## 5 21742447 2019-01-0~ 2019-01~ 1170                       173 Mies van der Rohe~
## 6 21742448 2019-01-0~ 2019-01~ 2437                        98 LaSalle St & Wash~
## # i 3 more variables: end_station_id <dbl>, end_station_name <chr>,
## #   member_casual <chr>
```

``` r
str(all_trips)  #See list of columns and data types (numeric, character, etc)
```

```
## tibble [791,956 x 9] (S3: tbl_df/tbl/data.frame)
##  $ ride_id           : chr [1:791956] "21742443" "21742444" "21742445" "21742446" ...
##  $ started_at        : chr [1:791956] "2019-01-01 0:04:37" "2019-01-01 0:08:13" "2019-01-01 0:13:23" "2019-01-01 0:13:45" ...
##  $ ended_at          : chr [1:791956] "2019-01-01 0:11:07" "2019-01-01 0:15:34" "2019-01-01 0:27:12" "2019-01-01 0:43:28" ...
##  $ rideable_type     : chr [1:791956] "2167" "4386" "1524" "252" ...
##  $ start_station_id  : num [1:791956] 199 44 15 123 173 98 98 211 150 268 ...
##  $ start_station_name: chr [1:791956] "Wabash Ave & Grand Ave" "State St & Randolph St" "Racine Ave & 18th St" "California Ave & Milwaukee Ave" ...
##  $ end_station_id    : num [1:791956] 84 624 644 176 35 49 49 142 148 141 ...
##  $ end_station_name  : chr [1:791956] "Milwaukee Ave & Grand Ave" "Dearborn St & Van Buren St (*)" "Western Ave & Fillmore St (*)" "Clark St & Elm St" ...
##  $ member_casual     : chr [1:791956] "Subscriber" "Subscriber" "Subscriber" "Subscriber" ...
```

``` r
summary(all_trips)  #Statistical summary of data. Mainly for numerics
```

```
##    ride_id           started_at          ended_at         rideable_type     
##  Length:791956      Length:791956      Length:791956      Length:791956     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##  start_station_id start_station_name end_station_id  end_station_name  
##  Min.   :  2.0    Length:791956      Min.   :  2.0   Length:791956     
##  1st Qu.: 77.0    Class :character   1st Qu.: 77.0   Class :character  
##  Median :174.0    Mode  :character   Median :174.0   Mode  :character  
##  Mean   :204.4                       Mean   :204.4                     
##  3rd Qu.:291.0                       3rd Qu.:291.0                     
##  Max.   :675.0                       Max.   :675.0                     
##                                      NA's   :1                         
##  member_casual     
##  Length:791956     
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

``` r
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)
```

```
## 
##     casual   Customer     member Subscriber 
##      48480      23163     378407     341906
```

``` r
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"
                                ))
table(all_trips$member_casual)
```

```
## 
## casual member 
##  71643 720313
```
# Add columns that list the date, month, day, and year of each ride

``` r
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
str(all_trips)
```

```
## tibble [791,956 x 15] (S3: tbl_df/tbl/data.frame)
##  $ ride_id           : chr [1:791956] "21742443" "21742444" "21742445" "21742446" ...
##  $ started_at        : chr [1:791956] "2019-01-01 0:04:37" "2019-01-01 0:08:13" "2019-01-01 0:13:23" "2019-01-01 0:13:45" ...
##  $ ended_at          : chr [1:791956] "2019-01-01 0:11:07" "2019-01-01 0:15:34" "2019-01-01 0:27:12" "2019-01-01 0:43:28" ...
##  $ rideable_type     : chr [1:791956] "2167" "4386" "1524" "252" ...
##  $ start_station_id  : num [1:791956] 199 44 15 123 173 98 98 211 150 268 ...
##  $ start_station_name: chr [1:791956] "Wabash Ave & Grand Ave" "State St & Randolph St" "Racine Ave & 18th St" "California Ave & Milwaukee Ave" ...
##  $ end_station_id    : num [1:791956] 84 624 644 176 35 49 49 142 148 141 ...
##  $ end_station_name  : chr [1:791956] "Milwaukee Ave & Grand Ave" "Dearborn St & Van Buren St (*)" "Western Ave & Fillmore St (*)" "Clark St & Elm St" ...
##  $ member_casual     : chr [1:791956] "member" "member" "member" "member" ...
##  $ date              : Date[1:791956], format: "2019-01-01" "2019-01-01" ...
##  $ month             : chr [1:791956] "01" "01" "01" "01" ...
##  $ day               : chr [1:791956] "01" "01" "01" "01" ...
##  $ year              : chr [1:791956] "2019" "2019" "2019" "2019" ...
##  $ day_of_week       : chr [1:791956] "Tuesday" "Tuesday" "Tuesday" "Tuesday" ...
##  $ ride_length       : 'difftime' num [1:791956] 390 441 829 1783 ...
##   ..- attr(*, "units")= chr "secs"
```

``` r
is.factor(all_trips$ride_length)
```

```
## [1] FALSE
```

``` r
all_trips$ride_length <- as.numeric(all_trips$ride_length) 
is.numeric(all_trips$ride_length)
```

```
## [1] TRUE
```
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative

``` r
all_trips_V2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length <0),]
```


``` r
all_trips_V2$day_of_week <- ordered(all_trips_V2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```
# Now, let's run the average ride time by each day for members vs casual users

``` r
aggregate(all_trips_V2$ride_length ~ all_trips_V2$member_casual + all_trips_V2$day_of_week, FUN = mean)
```

```
##    all_trips_V2$member_casual all_trips_V2$day_of_week all_trips_V2$ride_length
## 1                      casual                   Sunday                5061.3044
## 2                      member                   Sunday                 972.9383
## 3                      casual                   Monday                4752.0504
## 4                      member                   Monday                 822.3112
## 5                      casual                  Tuesday                4561.8039
## 6                      member                  Tuesday                 769.4416
## 7                      casual                Wednesday                4480.3724
## 8                      member                Wednesday                 711.9838
## 9                      casual                 Thursday                8451.6669
## 10                     member                 Thursday                 707.2093
## 11                     casual                   Friday                6090.7373
## 12                     member                   Friday                 796.7338
## 13                     casual                 Saturday                4950.7708
## 14                     member                 Saturday                 974.0730
```


``` r
all_trips_V2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday) %>%  
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```

```
## `summarise()` has grouped output by 'member_casual'. You can override using the
## `.groups` argument.
```

![](Case_Study_01_clean_files/figure-latex/unnamed-chunk-9-1.pdf)<!-- --> 

``` r
all_trips_V2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday) %>%  
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```

```
## `summarise()` has grouped output by 'member_casual'. You can override using the
## `.groups` argument.
```

![](Case_Study_01_clean_files/figure-latex/unnamed-chunk-9-2.pdf)<!-- --> 
