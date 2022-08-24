# This code starts from a plan file in CVS format (e.g., output_trips.csv) and
# modifies modes by switching X% of trips by car within walking distance to walk

# author: @jafshin

library(tidyverse)

trips <- read_delim("output_trips.csv.gz",delim = ";")

# Identifying home-tours so that we change the mode for the whole tour
trips_sampled <- trips %>% slice_head(n=1000) %>% 
  dplyr::select(person, trip_number, modes,start_activity_type,end_activity_type)
tour_id = 0
for(i in 1:nrow(trips_sampled) ){
  
  trips_sampled[i, "tour_id"] <- tour_id
  if(trips_sampled$end_activity_type[i] == "Home") tour_id = tour_id + 1 
  # TODO if not returned home, change the tour_id regardless
}
