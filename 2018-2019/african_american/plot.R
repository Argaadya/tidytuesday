library(tidyverse)
library(scales)
library(ggrepel)
library(magick)


# Import Data
blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')

# Get first sentence of each event
text_event <- sub("([a-z0-9][?!.])\\s.*", "\\1", blackpast$events)

# Select and process the first era
old_era <- blackpast %>% 
  rownames_to_column("id") %>% 
  mutate(year = ifelse(year == "1520s", 1520, year),
         year = as.numeric(year),
         id = as.numeric(id)*8,
         country = ifelse(country %in% c("Unied States", "The United States", "U.S."), "United States", country),
         text = wrap_format(60)(text_event))  %>% 
  filter(era == "1492-1600", !(is.na(year)))

text_nudge <- rep(70, nrow(old_era))
text_nudge[ seq(2, nrow(old_era), 2)] <- -80

p <- old_era %>% 
  ggplot(aes(year, id)) +
  geom_rect(aes(group = id, fill = year, xmin = year-1, xmax = year+1, ymin = id-3, ymax = id+3),
            color = "white") +
  geom_label_repel(aes(label = text), alpha = 0.9, force = 10, direction = "x",
                   nudge_x = text_nudge, segment.color = "skyblue") +
  scale_fill_binned(type = "viridis") +
  scale_x_continuous(labels = number_format(accuracy = 1), limits = c(1400, 1700), 
                     breaks = seq(1400,1700,50)) +
  labs(title = "Colonialism and Slavery",
       subtitle = "Era 1492-1600",
       caption = "@Argaadya") +
  theme(plot.title = element_text(size = 18, colour = "white"),
        plot.subtitle = element_text(size = 14, colour = "lightyellow"),
        plot.caption = element_text(size = 12, colour = "lightyellow"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour = "white", size = 14, margin = margin(t = 15)),
        axis.line.x = element_line(colour = "white"),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.length.x.bottom = unit(-5, "points"),
        panel.background = element_rect(fill = "#1d2024"),
        plot.background = element_rect(fill = "#1d2024"),
        legend.background = element_rect(fill = "#1d2024"),
        legend.key.width = unit(10, "mm"),
        legend.key = element_rect(size = 2, fill = "red"),
        legend.position = "none")

# convert ggplot to image
p_magick <- image_graph(width = 4950, height = 2900, res = 300)
p
dev.off()

# import ship
logo <- image_read("ship.png") %>% 
  image_scale("700x")

# attach ship to image
composite_image <- image_composite(p_magick, composite_image = logo, 
                                   offset = geometry_point(x = 4000, y = 1900))

composite_image

# save
image_write(composite_image,"colonialism.png")


# 1601-1650

second_era <- blackpast %>% 
  rownames_to_column("id") %>% 
  mutate(year = ifelse(year == "1520s", 1520, year),
         year = as.numeric(year),
         id = as.numeric(id)*8,
         country = ifelse(country %in% c("Unied States", "The United States", "U.S."), "United States", country),
         text = wrap_format(60)(text_event))  %>% 
  filter(era == "1601-1700", !(is.na(year)), year <= 1650)

text_nudge <- rep(70, nrow(second_era))
text_nudge[ seq(2, nrow(second_era), 2)] <- -80

second_era %>% 
  ggplot(aes(year, id)) +
  geom_rect(aes(group = id, fill = year, xmin = year-1, xmax = year+1, ymin = id-3, ymax = id+3),
            color = "white") +
  geom_label_repel(aes(label = text), alpha = 0.9, force = 10, direction = "x",
                   nudge_x = text_nudge, segment.color = "skyblue") +
  scale_fill_binned(type = "viridis") +
  scale_x_continuous(labels = number_format(accuracy = 1), limits = c(1500, 1750), 
                     breaks = seq(1500,1800,50)) +
  labs(title = "History of African America and Other African Ancestry Around the World",
       subtitle = "Era 1601-1650",
       caption = "@Argaadya") +
  theme(plot.title = element_text(size = 18, colour = "white"),
        plot.subtitle = element_text(size = 14, colour = "lightyellow"),
        plot.caption = element_text(size = 12, colour = "lightyellow"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour = "white", size = 14, margin = margin(t = 15)),
        axis.line.x = element_line(colour = "white"),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.length.x.bottom = unit(-5, "points"),
        panel.background = element_rect(fill = "#1d2024"),
        plot.background = element_rect(fill = "#1d2024"),
        legend.background = element_rect(fill = "#1d2024"),
        legend.key.width = unit(10, "mm"),
        legend.key = element_rect(size = 2, fill = "red"),
        legend.position = "none")

ggsave("1601-1605.png", width = 16, height = 9)
