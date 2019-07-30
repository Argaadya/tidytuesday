library(tidyverse)
library(lubridate)
library(ggridges)
setwd("D:/R/tidytuesday/video_game")
video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")
write.csv(video_games,"D:/R/datasets/video_games.csv")
video_games <- read.csv("D:/R/datasets/video_games.csv")

#Is there any pattern in release date with price? -------------------------------
video_games$release_date <- mdy(video_games$release_date)
video_games <- video_games %>% filter(is.na(release_date)==F)
video_games <- video_games %>% mutate(month = month(release_date), year=year(release_date))
release_game <- video_games %>% group_by(month,year) %>% 
  summarise(playtime = mean(average_playtime), price = median(price,na.rm = T), total = n()) %>% 
  filter(is.na(price)==F)

png("game1.png", width = 4240, height = 2160, units = 'px', res = 300)  
release_game %>% ggplot()+
  geom_tile(aes(year,month,fill=price),color="#1D2024")+
  scale_fill_viridis_c(option = "B")+
  scale_y_continuous(breaks = seq(1,12,1),labels = month.abb)+
  scale_x_continuous(breaks = seq(2004,2020,2))+
  labs(title = "Median Price of Video Game",
       x = "Year", y = "Month")+
  theme_ridges()+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"))
dev.off()

#Owner distribution ------------------------------------------------------------------
png("game2.png", width = 4240, height = 2160, units = 'px', res = 300)  
video_games %>% ggplot()+
  geom_boxplot(aes(year,metascore,group=year,fill=year),color="pink",show.legend = F)+
  scale_x_continuous(breaks = seq(2004,2018,1))+
  scale_y_continuous(breaks = seq(0,100,10))+
  scale_fill_viridis_c(option = "B")+
  coord_flip()+
  theme_ridges()+
  labs(title = "Are games getting better by year?",
       x="Year", y = "Metascore")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"))
dev.off()

