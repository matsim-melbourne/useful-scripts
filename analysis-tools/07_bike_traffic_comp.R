

## Bike traffic

# `Reading input bike traffic data}

bikeObservations <- read_sf("./data/observationsJoined/cyclingObsJoined2Network.sqlite") %>% 
  dplyr::select(link_id, hour, obs_volume = count) %>% 
  st_drop_geometry()

bikeSimulation <- read_sf("./data/simOutputJoined/networkLinksWithHourlyVol.sqlite", 
                          layer="bike")

# restructuring observation and simulation bike data}

bikeSimulationFiltered <- bikeSimulation %>% 
  st_drop_geometry() %>% 
  dplyr::select(link_id=id, starts_with("X")) %>% 
  filter(link_id %in% bikeObservations$link_id)

bikeSimulationLong <- bikeSimulationFiltered %>% 
  pivot_longer(cols = starts_with("X"), names_to="hour", values_to="sim_volume") %>% 
  mutate(hour=as.double(str_remove(hour,"X"))) %>% 
  mutate(sim_volume=sim_volume*10)  %>% # Because I used 10% sample 
  mutate(hour=case_when(hour==24 ~ 0,
                        hour==25 ~ 1,
                        hour==26 ~ 2,
                        hour==27 ~ 3,
                        hour==28 ~ 4,
                        hour==29 ~ 5,
                        hour==30 ~ 6,
                        TRUE ~ hour)) %>% 
  group_by(link_id, hour) %>% 
  summarise(sim_volume=sum(sim_volume,na.rm = T))

bikeData <- bikeObservations %>% 
  left_join(bikeSimulationLong, by = c("link_id","hour"))

### Plotting the volumes

bikeSimulationFiltered <- bikeSimulation %>% 
  filter(total_vol>0) %>% 
  mutate(volumeAdjusted=total_vol*10) %>% 
  dplyr::select(volumeAdjusted)

breakPoints <- pretty(bikeSimulationFiltered$volumeAdjusted, n = 6)

ggplot(bikeSimulationFiltered) +
  annotation_map_tile(type="osmgrayscale",zoom=9, alpha=0.6) +
  geom_sf(aes(size=volumeAdjusted, colour=volumeAdjusted, alpha=volumeAdjusted)) +
  # geom_sf(aes( size=n, colour=n ))  +
  scale_color_viridis(option="magma", trans="sqrt", breaks=breakPoints, name="Daily volume" ) +
  scale_alpha_continuous(name="Daily volume", trans="sqrt", range=c(0.1, .9), breaks=breakPoints) +
  scale_size_continuous(name="Daily volume", range=c(0.01, 2), breaks=breakPoints) +
  theme_void() + 
  guides( colour = guide_legend()) +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  )

ggsave("bikeDailyTraffic.png",
       width = 7, height = 7)

bikeData %>% 
  ggplot(aes(x=hour)) +
  geom_line(aes(y=obs_volume, color="observation")) + 
  geom_line(aes(y=sim_volume, color="simulation")) +
  facet_wrap("link_id")

ggsave(paste0(outputDir,"bike.png"), width= 50, height = 50, units="cm")

bikeSimHourlyAll <- bikeSimulationLong %>% 
  filter(!is.na(sim_volume)) %>% 
  # filter(hour<24) %>% 
  group_by(hour) %>% 
  summarise(volume=sum(sim_volume)) %>% 
  mutate(volume_ratio =100* volume/sum(volume)) %>% 
  mutate(source="Simulation")

bikeObsHourlyAll <- bikeObservations %>% 
  group_by(hour) %>% 
  summarise(volume=sum(obs_volume)) %>% 
  mutate(volume_ratio =100* volume/sum(volume)) %>% 
  mutate(source="Observation")

rbind(bikeObsHourlyAll,
      bikeSimHourlyAll) %>% 
  # filter(link_id==11169) %>% 
  ggplot(aes(x=hour,y=volume_ratio, color=source)) +
  geom_col(aes(fill=source), position = "identity" ,   alpha=0.2) +
  geom_line() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  ) +
  xlab("Hour of the day") +
  ylab("Traffic volume %")

ggsave("bikeTrafficOveral.png", width= 5, height = 3)

### Using precentage error instead of GEH

bikeDataWithGEH <- bikeData %>% 
  mutate(gehStat= sqrt((2*(sim_volume-obs_volume)^2)/(sim_volume+obs_volume))) %>% 
  drop_na(gehStat)

bikeDataWithGEH %>% 
  ggplot()+
  geom_histogram(aes(x=gehStat))

bikeDataWithApe <- bikeData %>% 
  mutate(ape= abs(obs_volume-sim_volume)/obs_volume )

bikeDataWithApe %>% 
  # filter(link_id==11169) %>% 
  ggplot(aes(x=hour)) +
  geom_line(aes(y=ape)) + 
  facet_wrap("link_id")
ggsave(paste0(outputDir,"bike_ape.png"), width= 50, height = 50, units="cm")

### Comparing hourly shares

bikeDataPct.h <- bikeData %>% 
  mutate(obs_volume=replace_na(obs_volume,0),
         sim_volume=replace_na(sim_volume,0)) %>% 
  group_by(link_id) %>% 
  mutate(obs.pct.h=obs_volume/sum(obs_volume, na.rm = T),
         sim.pct.h=sim_volume/sum(sim_volume, na.rm = T)) 

bikeDataPct.h %>% 
  ggplot(aes(x=hour))+
  geom_line(aes(y=obs.pct.h, color="observation")) + 
  geom_line(aes(y=sim.pct.h, color="simulation")) +
  facet_wrap("link_id")

ggsave(paste0(outputDir,"bikepct.png"), width= 50, height = 50, units="cm")

# Hourly aggregated WAPE

error_wape <- bikeDataPct.h %>% 
  ungroup() %>% 
  filter(!is.na(sim.pct.h)) %>% 
  # mutate(sim.pct.h=ifelse(, 0, sim.pct.h)) %>% 
  # filter(hour%in%7:18) %>% 
  group_by(hour) %>% 
  summarise(wape=100*sum(abs(obs.pct.h-sim.pct.h))/sum(obs.pct.h))

error_wape %>% 
  ggplot(aes(x=hour, y=wape)) +
  geom_line() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  ) +
  ylab("Weighted Absolute Error (%)") +
  xlab("Hour of the day")


ggsave("bike_wape.png",
       width= 5, height = 3)

# Calculating GEH Statistic for Bike traffic

bikeDataWithGEH <- bikeData %>% 
  mutate(gehStat= sqrt((2*(sim_volume-obs_volume)^2)/(sim_volume+obs_volume)))
st_write(bikeDataWithGEH, paste0(outputDir,"calibrationData.sqlite"), 
         layer="bike", delete_layer=T)

# Plotting the GEH for Bike traffic

bikeDataWithGEH %>% 
  # filter(link_id==11169) %>% 
  ggplot(aes(x=hour)) +
  geom_line(aes(y=gehStat)) + 
  facet_wrap("link_id")

ggsave(paste0(outputDir,"bikeGeh.png"), width= 50, height = 50, units="cm")
