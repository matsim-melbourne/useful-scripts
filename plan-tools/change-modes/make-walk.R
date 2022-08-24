# This code starts from a plan file in CVS format (e.g., output_trips.csv) and
# modifies modes by switching X% of trips by car within walking distance to walk

# author: @jafshin

library(tidyverse)

trips <- read_delim("output_trips.csv.gz",delim = ";")

# Identifying home-tours so that we change the mode for the whole tour
trips_sampled <- trips %>% slice_head(n=1000) %>%
  dplyr::select(person, trip_number, modes,start_activity_type,end_activity_type)

tour_id = 0
old_person = 0

for(i in 1:nrow(trips_sampled) ){

  this_person <- trips_sampled[i, "person"]

  if(old_tour_id == tour_id){
  # still in the same tour
    if(this_person != old_person){
      # Person did not return home, moved to the next person
      tour_id <- tour_id + 1
    }
  }

  trips_sampled[i, "tour_id"] <- tour_id

  old_tour_id <- tour_id

  if(trips_sampled$end_activity_type[i] == "Home"){
    # Person returned home, tour finished
    tour_id <- tour_id + 1
  }
}

# getting tour distances
tours_distance <- trips_sampled %>%
  group_by(tour_id) %>%
  summarise(total_distance=sum(distance)/1000)

# Finding tours within walking distance of 1.6 km
tours_walkable_id <- tours_distance %>%
  filter(total_distance < 1.6 ) %>%
  distinct(tour_id)
