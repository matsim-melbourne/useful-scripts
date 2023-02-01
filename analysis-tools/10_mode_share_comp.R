library(dplyr)
library(readr)
library(ggplot2)

netwalk_flag=T

## Sim output -----------
simOutputTrips <- read_delim(gzfile("./sample_data/simOutputs/output_trips.csv.gz"),
                             delim=";") %>% 
  # Removing trips with zero distance
  filter(traveled_distance>0)

if(netwalk_flag==T){
  simOutputTrips <-  simOutputTrips %>% 
    filter(longest_distance_mode!="walk") %>% 
    mutate(longest_distance_mode=ifelse(longest_distance_mode=="netwalk", "walk", longest_distance_mode)) 
}
  
simOutputTripsCounted <- simOutputTrips %>% 
  count(mode=longest_distance_mode) %>% 
  # Getting the pct
  mutate(pct=100*n/sum(n)) 

## VISTA ------

vistaTrips <- read_csv("./sample_data/vista/T_VISTA1218_V1.csv")

vistaTripsCounted <- vistaTrips %>% 
  # Filtering to valid trips
  filter(CUMDIST>0) %>% 
  # Refactoring the linkmode in VISTA
  mutate(mode=case_when(LINKMODE=="Bicycle" ~ "bicycle",
                        LINKMODE=="Vehicle Driver" ~ "car", 
                        LINKMODE=="Walking" ~ "walk",
                        LINKMODE%in%c("Public Bus","Train","Tram") ~ "pt",
                        TRUE ~ "other")) %>% 
  # Filtering out modes==other
  filter(mode!="other") %>% 
  # counting trips per mode
  # count(mode) %>% 
  filter(!is.na(CW_WDTRIPWGT_SA3)) %>% 
  group_by(mode) %>% 
  summarise(n=sum(CW_WDTRIPWGT_SA3)) %>% 
  # get the percentages
  mutate(pct=100*n/sum(n))
# vistaTripsCounted

### Plotting them all 

rbind(mutate(simOutputTripsCounted,source="Simulation Output"),
      mutate(vistaTripsCounted,source="VISTA Trips")
) %>% 
  ggplot(aes(x=mode, y=pct, fill=source)) +
  geom_col(position="dodge") +
  geom_text(aes(label=paste0(round(pct, digits = 1),"%")), 
            position=position_dodge(width=0.9), vjust=-0.25)

## Journey to work

# VISTA JTW

vistaJTW <- read_csv("./sample_data/vista/JTW_VISTA1218_V1.csv") %>% 
  # Filtering to valid trips
  filter(JTWDIST>0) %>% 
  # Refactoring the linkmode in VISTA
  mutate(mode=case_when(JTWMODE=="Bicycle" ~ "bike",
                        JTWMODE=="Vehicle Driver" ~ "car", 
                        JTWMODE=="Walking" ~ "walk",
                        JTWMODE%in%c("Public Bus","Train","Tram") ~ "pt",
                        TRUE ~ "other")) %>% 
  # Filtering out modes==other
  filter(mode!="other") 

vistaJTWCounted <- vistaJTW %>% 
  # counting trips per mode
  # count(mode) %>% 
  filter(!is.na(CW_WDJTWWGT_SA3)) %>% 
  group_by(mode) %>% 
  summarise(n=sum(CW_WDJTWWGT_SA3)) %>% 
  # get the percentages
  mutate(pct=100*n/sum(n))


# Simulation Output JTW

simOutputJTW <- simOutputTrips %>% 
  filter(end_activity_type=="Work") 

simOutputJTWCounted <- simOutputJTW  %>% 
  # Counting number of trips with each mode
  count(mode=longest_distance_mode) %>% 
  # Getting the pct
  mutate(pct=100*n/sum(n)) 


### Plotting JTW trips

workTripsJoined <-rbind(mutate(simOutputJTWCounted,source="Simulation Output"),
                        mutate(vistaJTWCounted,source="VISTA 2012-18")) %>% 
  mutate(type="Mandatory trips - work")

workTripsJoined %>% 
  ggplot(aes(x=mode, y=pct, fill=source)) +
  geom_col(position="dodge") +
  geom_text(aes(label=paste0(round(pct, digits = 1),"%")), 
            position=position_dodge(width=0.9), vjust=-0.25)


## education trips Analysis

# VISTA education trips}

vistaEdu <- vistaTrips %>%
  filter(DESTPURP1%in%c("Education")) %>% 
  # Filtering to valid trips
  filter(CUMDIST>0) %>% 
  # Refactoring the linkmode in VISTA
  mutate(mode=case_when(LINKMODE=="Bicycle" ~ "bike",
                        LINKMODE=="Vehicle Driver" ~ "car", 
                        LINKMODE=="Walking" ~ "walk",
                        LINKMODE%in%c("Public Bus","Train","Tram") ~ "pt",
                        TRUE ~ "other")) %>% 
  # Filtering out modes==other
  filter(mode!="other") 

vistaEduCounted <- vistaEdu %>% 
  # counting trips per mode
  # count(mode) %>% 
  filter(!is.na(CW_WDTRIPWGT_SA3)) %>% 
  group_by(mode) %>% 
  summarise(n=sum(CW_WDTRIPWGT_SA3)) %>% 
  # get the percentages
  mutate(pct=100*n/sum(n))

# Simulation Output education trips}

simOutputEdu <- simOutputTrips %>% 
  filter(end_activity_type=="Study") 

simOutputEduCounted <- simOutputEdu  %>% 
  # Counting number of trips with each mode
  count(mode=longest_distance_mode) %>% 
  # Getting the pct
  mutate(pct=100*n/sum(n)) 

### Plotting education trips

educationTripsJoined <- rbind(mutate(simOutputEduCounted,source="Simulation Output"),
                              mutate(vistaEduCounted,source="VISTA 2012-18")
)  %>% 
  mutate(type="Mandatory trips - education")

educationTripsJoined %>% 
  ggplot(aes(x=mode, y=pct, fill=source)) +
  geom_col(position="dodge") +
  geom_text(aes(label=paste0(round(pct, digits = 1),"%")), 
            position=position_dodge(width=0.9), vjust=-0.25)

## Discretionary trips Analysis

# VISTA Discretionary trips

vistaDiscretionary <- vistaTrips %>%
  filter(!DESTPURP1%in%c("Education", "Work Related")) %>% 
  # Filtering to valid trips
  filter(CUMDIST>0) %>% 
  # Refactoring the linkmode in VISTA
  mutate(mode=case_when(LINKMODE=="Bicycle" ~ "bicycle",
                        LINKMODE=="Vehicle Driver" ~ "car", # Check with Dhirendra/Alan what happens to car passengers?
                        LINKMODE=="Walking" ~ "walk",
                        LINKMODE%in%c("Public Bus","Train","Tram") ~ "pt",
                        TRUE ~ "other")) %>% 
  # Filtering out modes==other
  filter(mode!="other") 

vistaDiscretionaryCounted <- vistaDiscretionary %>% 
  # counting trips per mode
  # count(mode) %>% 
  filter(!is.na(CW_WDTRIPWGT_SA3)) %>% 
  group_by(mode) %>% 
  summarise(n=sum(CW_WDTRIPWGT_SA3)) %>% 
  # get the percentages
  mutate(pct=100*n/sum(n))


# Simulation Output Discretionary trips

simOutputDiscretionary <- simOutputTrips %>% 
  filter(!end_activity_type%in%c("Study","Work")) 

simOutputDiscretionaryCounted <- simOutputDiscretionary  %>% 
  # Counting number of trips with each mode
  count(mode=longest_distance_mode) %>% 
  # Getting the pct
  mutate(pct=100*n/sum(n)) 

### Plotting Discretionary trips
discretionaryCountsJoined <- rbind(
  mutate(simOutputDiscretionaryCounted,source="Simulation Output"),
  mutate(vistaDiscretionaryCounted,source="VISTA 2012-18")
) %>% 
  mutate(type="Discretionary trips")

discretionaryCountsJoined %>% 
  ggplot(aes(x=mode, y=pct, fill=source)) +
  geom_col(position="dodge") +
  geom_text(aes(label=paste0(round(pct, digits = 1),"%")), 
            position=position_dodge(width=0.9), vjust=-0.25)

# library(xtable)
# 
# allModeSharesJoined <- rbind(workTripsJoined,
#                              educationTripsJoined,
#                              NonWorkCountsJoined,
#                              discretionaryCountsJoined) %>% 
#   mutate(pct2=paste0(round(pct, digits = 1),"%")) %>% 
#   pivot_wider(id_cols = c("type","mode"), names_from = "source", values_from="pct2") 
# 
# print(xtable(allModeSharesJoined), booktabs = TRUE)
