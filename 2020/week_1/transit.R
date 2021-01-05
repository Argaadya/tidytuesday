# Library
library(tidyverse)
library(countrycode)
library(sf)
library(scales)
library(showtext)

# Set working directory
setwd("Documents/github/tidytuesday/2020/week_1/")

# Add new font from Google Font
font_add_google(name = "Roboto Slab", family = "roboto slab")
font_add_google(name = "Space Mono", family = "space mono")
font_add_google(name = "Work Sans", family = "work sans")

# Data
## Transit Cost Data
transit_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# Country Code list
country_code <- codelist %>% 
  select(ecb, country.name.en)

# World Border Map
worldmap <- st_read("Igismap/")

# Data Preprocessing

df_transit <- transit_cost %>% 
  filter(!is.na(e)) %>% 
  mutate(country = ifelse(country == "UK", "GB", country),
         real_cost = real_cost %>% 
           as.numeric()
         ) %>% 
  left_join(country_code, by = c("country" = "ecb"))

df_agg <- df_transit %>% 
  group_by(country) %>% 
  summarise(length = sum(length),
            cost_km_millions = mean(cost_km_millions, na.rm = T)
            )

df_map <- worldmap %>% 
  as.data.frame() %>% 
  left_join(df_agg, by = c("iso2" = "country")) %>% 
  mutate(log_length = log(length, 10)) %>% 
  st_as_sf()

df_highlight <- df_agg %>% 
  filter(length > 150) %>% 
  left_join(worldmap, by = c("country" = "iso2")) %>% 
  bind_rows(
    df_map %>% 
      arrange(-area) %>% 
      head(10) %>% 
      rename(country = iso2),
    df_map %>% 
      filter(name %in% c("Egypt", "Mexico", "Indonesia", "Iran (Islamic Republic of)"))
  ) %>% 
  distinct()

# Visualization

showtext_auto() 

df_map %>% 
  ggplot(aes(fill = log_length)) +
  geom_sf(color = "#84CDEE", lwd = 0.2) +
  geom_rect(aes(xmin = -170, xmax = -165, ymin = 95, ymax = 100), 
            fill = "#073466", color = "white", lwd = 0.3) +
  geom_text(aes(x = -152, y = 97.5), label = "No Data", 
            color = "white", size = 10,
            data = data.frame(), inherit.aes = F) +
  geom_text(aes(x = lon, y = lat, 
                label = dollar(cost_km_millions, accuracy = 1), 
                size = area
                ), 
            family = "space mono",
            data = df_highlight,
            show.legend = F,
            inherit.aes = F) +
  scale_fill_binned(low = "#F0E372", high = "#AD0614", na.value = "#073466",
                    label = function(x){number(10^x, big.mark = ",")}
                    ) +
  scale_size_continuous(range = c(5, 12)) +
  labs(fill = "Total Length (km)\n\n",
       caption = "github: Argaadya",
       title = "   Projected Transit Line Around the World",
       subtitle = "\nPresenting more than 50 countries and totals more than 11,000 km of urban rail built since the late 1990s.\n 
       The number displayed shows the average cost of building traffic-infrastructure (in millions USD/km).\n"
       ) +
  theme_void() +
  theme(legend.position = "top",
        legend.key.width = unit(20, "mm"),
        legend.key.height = unit(3, "mm"),
        legend.text = element_text(colour = "white", size = 24, vjust = 1, lineheight = 0.1),
        legend.title = element_text(colour = "white", size = 28, lineheight = 0.3),
        plot.title = element_text(colour = "white", size = 48, family = "roboto slab"),
        plot.subtitle = element_text(colour = "white", size = 24, family = "work sans", hjust = 0.5, lineheight = 0.2),
        plot.caption = element_text(colour = "white", size = 24, family = "work sans", lineheight = 0.2),
        panel.background = element_rect(fill = "#1D2024", color = "transparent"),
        plot.background = element_rect(fill = "#1D2024", color = "transparent")
        )

# Save Plot

ggsave("total length.png", width = 10, height = 7)
