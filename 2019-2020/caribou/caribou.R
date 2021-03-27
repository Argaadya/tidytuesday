library(tidyverse)
library(ggmap)
library(OpenStreetMap)
library(scales)
library(gganimate)

locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

# First Animation (Only Single Caribou)

most_movement <- locations %>% 
  count(animal_id) %>% 
  arrange(desc(n)) %>% 
  slice(1) %>% 
  pull(animal_id)

animal_1 <- locations %>% 
  group_by(animal_id) %>% 
  arrange(event_id) %>% 
  mutate(next_long = lead(longitude),
         next_lat = lead(latitude),
         lag_time = lag(timestamp)) %>% 
  ungroup() %>% 
  filter(animal_id == most_movement) %>% 
  filter_all(all_vars(!(is.na(.))))

canada_map <- openmap(upperLeft = c(max(animal_1$latitude)+0.2, min(animal_1$longitude)-0.2), 
                      lowerRight = c(min(animal_1$latitude)-0.2, max(animal_1$longitude)+0.2), 
                      type = "osm", zoom = NULL)

canada_proj <- openproj(canada_map)

p <- autoplot.OpenStreetMap(canada_proj) +
  geom_segment(aes(x = next_long, y = next_lat,
                   xend = longitude, yend = latitude), 
             na.rm = T, data = animal_1, color = "dodgerblue4") +
  geom_point(aes(x = next_long, y = next_lat,
                 color = animal_id), 
             data = animal_1, color = "dodgerblue4") +
  geom_label(aes(x = max(longitude)+0.19, y = min(latitude)-0.15, 
                 label = timestamp), data = animal_1, 
             inherit.aes = F, size = 5, hjust = 1) +
  labs(x = "Longitude", y = "Latitude",
       title = paste("Caribou", most_movement, "Movement Over Time")) +
  theme_minimal() +
  theme(legend.position = "none")

p_animate <- p +
  transition_time(time = lag_time) +
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_wake(wake_length = 0.2, exclude_phase = NULL)

p_animate %>% 
  anim_save(filename = "caribou_1.gif")


# Second Animation (5 Most Actives Caribou)
  
top_movement <- locations %>% 
  count(animal_id) %>% 
  arrange(desc(n)) %>% 
  head(5) %>% 
  pull(animal_id)

animal_2 <- locations %>% 
  group_by(animal_id) %>% 
  arrange(event_id) %>% 
  mutate(next_long = lead(longitude),
         next_lat = lead(latitude),
         lag_time = lag(timestamp)) %>% 
  ungroup() %>% 
  filter(animal_id %in% top_movement) %>% 
  filter_all(all_vars(!(is.na(.))))


canada_map <- openmap(upperLeft = c(max(animal_2$latitude)+0.2, min(animal_2$longitude)-0.2), 
                      lowerRight = c(min(animal_2$latitude)-0.2, max(animal_2$longitude)+0.2), 
                      type = "osm", zoom = NULL)

canada_proj <- openproj(canada_map)

p <- autoplot.OpenStreetMap(canada_proj) +
  geom_segment(aes(x = next_long, y = next_lat,
                   xend = longitude, yend = latitude,
                   color = animal_id), 
               na.rm = T, data = animal_2) +
  geom_point(aes(x = next_long, y = next_lat,
                 color = animal_id), 
             data = animal_2) +
  geom_label(aes(x = max(longitude)+0.19, y = min(latitude)-0.15, 
                 label = timestamp), data = animal_2, 
             inherit.aes = F, size = 5, hjust = 1) +
  scale_color_viridis_d(option = "B") +
  labs(x = "Longitude", y = "Latitude",
       title = paste("Movement of 5 Most Actives Caribou Over Time")) +
  theme_minimal() +
  theme(legend.position = "top")

p_animate <- p +
  transition_time(time = lag_time) +
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_wake(wake_length = 0.2, exclude_phase = NULL)

p_animate %>% 
  anim_save(filename = "caribou_2.gif")

