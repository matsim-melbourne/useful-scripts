## Pedestrian Traffic

# Reading input bike traffic data}

walkObservations <- read_sf("./sample_data/observationsJoined/walkObsJoined2Network.sqlite") %>% 
  dplyr::select(link_id_1, link_id_2, hour=time, obs_volume = hourly_counts) %>% 
  st_drop_geometry()

walkSimulation <- read_sf("./sample_data/simOutputJoined/networkLinksWithHourlyVol.sqlite", 
                          layer="netwalk")

# restructuring observation and simulation walk data}

walkSimulationFiltered <- walkSimulation %>% 
  st_drop_geometry() %>% 
  dplyr::select(link_id=id, starts_with("X")) %>% 
  filter(link_id %in% na.omit(c(walkObservations$link_id_1,walkObservations$link_id_2)))

walkSimulationLong <- walkSimulationFiltered %>% 
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

walkData <- walkObservations %>% 
  left_join(walkSimulationLong, by = c("link_id_1"="link_id","hour")) %>% 
  rename(sim_volume_1=sim_volume) %>%
  left_join(walkSimulationLong, by = c("link_id_2"="link_id","hour")) %>% 
  rename(sim_volume_2=sim_volume) %>% 
  rowwise() %>% 
  mutate(sim_volume=sum(sim_volume_1,sim_volume_2, na.rm = T))

### Plotting the volumes

# Daily simulated walk traffic for all roads

walkSimulationFiltered <- walkSimulation %>% 
  filter(total_vol>0) %>% 
  mutate(volumeAdjusted=total_vol*10) %>% 
  dplyr::select(volumeAdjusted)

breakPoints <- pretty(walkSimulationFiltered$volumeAdjusted, n = 6)

ggplot(walkSimulationFiltered) +
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

ggsave("walkDailyTraffic.png", width = 7, height = 7)
walkData %>% 
  # filter(link_id==11169) %>% 
  ggplot(aes(x=hour)) +
  geom_line(aes(y=obs_volume, color="observation")) + 
  geom_line(aes(y=sim_volume, color="simulation")) +
  facet_wrap("link_id_1")

ggsave(paste0(outputDir,"walk.png"), width= 50, height = 50, units="cm")

# All bikes on the road traffice

walkSimHourlyAll <- walkSimulationLong %>% 
  filter(!is.na(sim_volume)) %>% 
  # filter(hour<24) %>% 
  group_by(hour) %>% 
  summarise(volume=sum(sim_volume)) %>% 
  mutate(volume_ratio =100* volume/sum(volume)) %>% 
  mutate(source="Simulation")

walkObsHourlyAll <- walkObservations %>% 
  group_by(hour) %>% 
  summarise(volume=sum(obs_volume)) %>% 
  mutate(volume_ratio =100* volume/sum(volume)) %>% 
  mutate(source="Observation")

rbind(walkObsHourlyAll,
      walkSimHourlyAll) %>% 
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

ggsave("walkTrafficOveral.png", width= 5, height = 3)

### Using precentage error instead of GEH

walkDataWithGEH <- walkData %>% 
  mutate(gehStat= sqrt((2*(sim_volume-obs_volume)^2)/(sim_volume+obs_volume))) %>% 
  drop_na(gehStat)

walkDataWithGEH %>% 
  ggplot()+
  geom_histogram(aes(x=gehStat))


walkDataWithApe <- walkData %>% 
  mutate(ape= abs(obs_volume-sim_volume)/obs_volume )

walkDataWithApe %>% 
  ggplot(aes(x=hour)) +
  geom_line(aes(y=ape)) + 
  facet_wrap("link_id_1")
ggsave(paste0(outputDir,"walk_ape.png"), width= 50, height = 50, units="cm")

### Comparing hourly shares

walkDataPct.h <- walkData %>% 
  mutate(obs_volume=replace_na(obs_volume,0),
         sim_volume=replace_na(sim_volume,0)) %>% 
  group_by(link_id_1,link_id_2) %>% 
  mutate(obs.pct.h=obs_volume/sum(obs_volume, na.rm = T),
         sim.pct.h=sim_volume/sum(sim_volume, na.rm = T)) 

walkDataPct.h %>% 
  ggplot(aes(x=hour))+
  geom_line(aes(y=obs.pct.h, color="observation")) + 
  geom_line(aes(y=sim.pct.h, color="simulation")) +
  facet_wrap("link_id_1")

ggsave(paste0(outputDir,"walkpct.png"), width= 50, height = 50, units="cm")

# Hourly aggregated WAPE

error_wape <- walkDataPct.h %>% 
  ungroup() %>% 
  filter(!is.na(sim.pct.h)) %>% 
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

ggsave("walk_wape.png", width= 5, height = 3)
