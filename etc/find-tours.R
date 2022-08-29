find_vista_tours <- function(trips_vista){
  
  # trips_vista <- read_csv("~/ownCloud/Data/VISTA/2012-18/T_VISTA1218_V1.csv") 
  trips_vista <- trips_vista %>% 
      dplyr::select(person=PERSID, trip_number=TRIPID, modes=LINKMODE, 
                  traveled_distance=CUMDIST,start_activity_type= ORIGPLACE1,
                  end_activity_type=DESTPLACE1)  
  
  tour_id = 0
  old_person = 0
  old_tour_id=-1
  
  for(i in 1:nrow(trips_vista) ){
  
    this_person <- trips_vista[i, "person"] %>% unlist()
    
    paste0("this person= ",this_person)
    paste0("old_tour_id= ",old_tour_id)
    paste0("tour_id= ",tour_id)
    
    if(old_tour_id == tour_id){
      # still in the same tour
      if(this_person != old_person){
        # Person did not return home, moved to the next person
        paste0("tour_id changed ")
        tour_id <- tour_id + 1
        old_person <- this_person
      }
    }else if(this_person != old_person){
      # Person has changed with the previous ending at home, change the 
      old_person <- this_person
    }
    
    trips_vista[i, "tour_id"] <- tour_id
    
    old_tour_id <- tour_id
    
    if(trips_vista$end_activity_type[i] == "Accommodation"){
      # Person returned home, tour finished
      tour_id <- tour_id + 1
    }
  }
  
  # getting tour distances
  vista_tours_distance <- trips_vista %>%
    group_by(tour_id) %>%
    summarise(total_distance=sum(traveled_distance), modes=paste(modes, sep = ",")) %>% 
    ungroup() %>% 
    distinct(tour_id, .keep_all = T) %>% 
    filter(total_distance<150)
  
  # Finding tours within walking distance of 1.6 km
  vista_tours_walkable_id <- vista_tours_distance %>%
    filter(total_distance < 1.6 ) %>%
    distinct(tour_id)
  
  vista_tours_walkable_nonwalk <- vista_tours_distance %>% 
    filter(tour_id %in% vista_tours_walkable_id$tour_id) %>% 
    filter(modes !="Walking")
  
  vista_tours_bikeable_id <- vista_tours_distance %>%
    filter(total_distance < 5.5 ) %>%
    distinct(tour_id)
  
  vista_tours_walkable_nonactive <- vista_tours_distance %>% 
    filter(tour_id %in% vista_tours_bikeable_id$tour_id) %>% 
    filter(!modes %in% c("Walking", "Bicycle"))  

 return(trips_vista)
}



find_matsim_tours <- function(trips_simulation){
  
  
  # trips_simulation <- read_delim("output_trips.csv.gz",delim = ";")
  
  # Identifying home-tours so that we change the mode for the whole tour
  trips_simulation <- trips_simulation %>% 
    # slice_head(n=10000) %>%
    dplyr::select(person, trip_number, modes, traveled_distance,
                  start_activity_type, end_activity_type)
  
  tour_id = 0
  old_person = 0
  old_tour_id=-1
  
  
  
  for(i in 1:nrow(trips_simulation) ){
    
    this_person <- trips_simulation[i, "person"] %>% unlist()
    
    paste0("this person= ",this_person)
    paste0("old_tour_id= ",old_tour_id)
    paste0("tour_id= ",tour_id)
    
    if(old_tour_id == tour_id){
      # still in the same tour
      if(this_person != old_person){
        # Person did not return home, moved to the next person
        paste0("tour_id changed ")
        tour_id <- tour_id + 1
        old_person <- this_person
      }
    }else if(this_person != old_person){
      # Person has changed with the previous ending at home, change the 
      old_person <- this_person
    }
    
    trips_simulation[i, "tour_id"] <- tour_id
    
    old_tour_id <- tour_id
    
    if(trips_simulation$end_activity_type[i] == "Home"){
      # Person returned home, tour finished
      tour_id <- tour_id + 1
    }
  }
  
  # getting tour distances
  tours_distance <- trips_simulation %>%
    group_by(tour_id) %>%
    summarise(total_distance=sum(traveled_distance)/1000) %>% 
    filter(total_distance<150)
  
  # Finding tours within walking distance of 1.6 km
  tours_walkable_id <- tours_distance %>%
    filter(total_distance < 1.6 ) %>%
    distinct(tour_id)
  
  trips_simulation %>% 
    filter(tour_id %in% tours_walkable_id$tour_id) %>% 
    filter(!modes %in% c("walk","netwalk") )
  
  tours_bikeable_id <- tours_distance %>%
    filter(total_distance < 5.5 ) %>%
    distinct(tour_id)
 
  return(trips_simulation) 
}

