library(tidyverse)
library(scales)
library(ggpubr)
library(ggimage)
library(extrafont)
library(ggforce)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

astronauts %>% glimpse()

setwd(here::here())

# Gender Disparity by Decades

## Background

image_space <- "tidytuesday/astronaut/space.jpg"

## Plot

df_gender <- astronauts %>% 
  mutate(
    mission_decades = case_when(year_of_mission %in% 1960:1970 ~ "1960-1970",
                                year_of_mission %in% 1971:1980 ~ "1971-1980",
                                year_of_mission %in% 1981:1990 ~ "1981-1990",
                                year_of_mission %in% 1991:2000 ~ "1991-2000",
                                year_of_mission %in% 2001:2010 ~ "2001-2010",
                                year_of_mission %in% 2011:2020 ~ "2011-2020"
                                )
  ) %>% 
  count(mission_decades, name = "freq") 

df_tile <- data.frame(
  decade = apply(df_gender,  1, function(x) rep(x[1], ceiling(as.numeric(x[2])/25)  )) %>% unlist(),
  mark = sapply(df_gender$freq, function(x) seq(0,x,25)) %>% unlist(),
  image = "tidytuesday/astronaut/astro.png"
) 

df_group_1 <- df_tile %>% 
  filter(decade %in% c("1960-1970", "1971-1980")) %>% 
  mutate(
    group = "Space Race Era",
    description = "From the first human in space (Yuri Gagarin) to the Moon Landing by Neil Armstrong and Buzz Aldrin"
  )

df_group_2 <- df_tile %>% 
  filter(decade %in% c("1991-2000"), mark %in% 300:400) %>% 
  mutate(
    group = "Launch of ISS",
    description = "The first launch of ISS on November 20, 1998"
  )

df_group_3 <- df_tile %>% 
  filter(decade %in% c("1981-1990"),
         mark %in% 200:250) %>% 
  mutate(
    group = "First Modular Space Station",
    description = "Mir was a space station that operated in low Earth orbit from 1986 to 2001, operated by the Soviet Union and later by Russia"
  )

set.seed(123)
p <- df_tile %>% 
  ggplot(aes(x = decade, y = mark)) +
  geom_mark_ellipse(aes(label = group, group = group, description = description), data = df_group_1, 
                 label.family = "Segoe UI Semilight", label.fontsize = c(16,14), con.type = "elbow",
                 con.colour = "orange", alpha = 0.25, color = "orange", fill = "skyblue") +
  geom_mark_hull(aes(label = group, group = group, description = description), data = df_group_2, 
                 label.family = "Segoe UI Semilight", label.fontsize = c(16,14), con.type = "elbow",
                 con.colour = "orange", alpha = 0.7, color = "transparent") +
  geom_mark_hull(aes(label = group, group = group, description = description), data = df_group_3, 
                 label.family = "Segoe UI Semilight", label.fontsize = c(16,14), con.type = "elbow",
                 con.colour = "orange", alpha = 0.7, color = "transparent") +
  geom_image(aes(image = image), size = 0.05, by = "height") +
  geom_image(aes(image = image, x = 0.75, y = 400), size = 0.05) +
  geom_image(aes(x = 5.75, y = 400), image = "tidytuesday/astronaut/tie.png", size = 0.2) +
  geom_text(aes(x = 1.25, y = 400, label = "25 People"), size = 8,
            family = "Segoe UI Semilight", color = "white") +
  guides(fill = F) +
  scale_size_identity() +
  labs(title = "Space Travel",
       x = NULL, y = "Number of People",
       caption = "@Argaadya") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5, 
                                  family = "Segoe UI Semibold", color = "white"),
        axis.title = element_text(color = "white", size =16),
        axis.text = element_text(color = "lightyellow", size = 13),
        plot.caption = element_text(size = 12, color = "white")
        )

# Add Background
  ggbackground(p, background = image_space)
    
ggsave("tidytuesday/astronaut/space travel.png", width = 16, height = 9)
