library(tidyverse)
library(ggrepel)
library(lubridate)
library(gganimate)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

squirrel <- nyc_squirrels %>% 
  mutate(date = mdy(date),
         year = year(date),
         month = month(date),
         day = day(date),
         wday = wday(date, label = T, abbr = T)) %>%   
  pivot_longer(cols = c(running, chasing,eating, foraging),
               names_to = "activities", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>% 
  group_by(shift, year, month, day, wday, activities) %>% 
  summarise(sighting = sum(value)) %>% 
  ungroup() %>% 
  na.omit()
squirrel

p <- ggplot(squirrel, aes(activities, shift, fill = sighting))+
  geom_tile(color = "white")+
  geom_point(shape=21, aes(size = sighting), color = "firebrick1", stroke=3)+
  scale_fill_viridis_c(option = "B")+
  scale_size_continuous(guide = F, range = c(1,35))+
  geom_text(aes(x = 4, y =2.7, label = paste0(wday,", ","Oct ",day)), 
            size = 8, color = "khaki1")+
  scale_y_discrete(expand = c(0,0.8))+
  labs(title = "Squirrels Sighting And Their Activities",
       subtitle = "Color and ring size indicates the number of sighting",
       x = "Activities", y = "Time of the Day",
       caption = "@Argaadya1")+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_blank(),
        text = element_text(colour = "lightyellow"),
        axis.text = element_text(colour = "goldenrod",size = 12),
        plot.title = element_text(size = 20),
        panel.grid = element_blank())

p

anim <- p + 
  transition_states(states = day,
                    transition_length = 2,
                    state_length = 1)
animate(anim, 200, fps = 20,  width = 720, height = 480, 
        renderer = gifski_renderer("squirrel_anim.gif"))


squirrel_area <- nyc_squirrels %>% 
  mutate(date = mdy(date),
         year = year(date),
         month = month(date),
         day = day(date),
         wday = wday(date, label = T, abbr = F),
         wday = factor(wday, levels = c("Sunday","Monday","Wednesday","Thursday","Friday","Saturday")))
squirrel_area



p <- ggplot(squirrel_area, aes(lat,long))+
  geom_hex(color = "white")+
  geom_text(aes(40.765,-73.95, label = wday), size = 14, color = "khaki1", hjust = 0)+
  scale_fill_gradient(low = "red4", high = "greenyellow")+
  labs(title = "Weekly Movement of Squirrels",
       caption = "@Argaadya1")+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_blank(),
        text = element_text(colour = "lightyellow"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size=20),
        panel.grid = element_blank())

p

anim <- p + 
  transition_states(states = wday,
                    transition_length = 5,
                    state_length = 2)
anim
animate(anim, 200, fps = 20,  width = 720, height = 720, 
        renderer = gifski_renderer("squirrel_area.gif"))
