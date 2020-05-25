library(tidyverse)
library(vroom)
library(lubridate)
#data url
url <- "https://data.cityofchicago.org/resource/wrvz-psew.csv?$where=trip_start_timestamp%3E=%222016-01-01%22%20and%20trip_start_timestamp%3C%222017-01-01%22%20and%20payment_type=%22Cash%22&$limit=1000000000"
data <- vroom(url)

#data manipulation
taxi <- data %>% 
  select(trip_start_timestamp:trip_miles,pickup_community_area:trip_total,
         pickup_centroid_latitude:pickup_centroid_longitude, dropoff_centroid_latitude:dropoff_centroid_longitude) %>% 
  mutate(start_hour = hour(trip_start_timestamp)) %>% 
  mutate(start_day = weekdays(trip_start_timestamp)) %>% 
  na.omit()

#data save locally
saveRDS(taxi, "taxi.Rds")

  
 

