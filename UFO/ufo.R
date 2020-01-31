library(tidyverse)
library(ggrepel)
library(ggridges)
library(maps)
library(cowplot)
library(lubridate)
setwd("D:/R/datasets/")

#Data Preparation ----------------------------------------------------------------------
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")
write.csv(ufo_sightings,"ufo_sight.csv")
ufo <- read.csv("ufo_sight.csv")
world <- map_data("world")
data <- ufo %>% filter(is.na(date_time)==F & is.na(latitude)==F & is.na(longitude)==F)
data$date_documented <- mdy(as.character(data$date_documented))
data$date_time <- mdy_hm(as.character(data$date_time))
data <- data %>% dplyr::mutate(year_sighted = year(data$date_time),
                               month_sighted = month(data$date_time),
                               hour_sighted = hour(data$date_time),
                               year_documented = year(data$date_documented),
                               year_span = year_documented - year_sighted,
                               befo = case_when(year_sighted < 1947 ~ "Before 1947",
                                year_sighted > 1946 & year_sighted < 1990 ~ "1947-1990",
                                year_sighted > 1989 ~ "After 1990"))
data$befo <- factor(data$befo,levels = c("Before 1947","1947-1990","After 1990"))

setwd("D:/R/tidytuesday/UFO/")

#Sighting After Roswell and the Belgium Wave ---------------------------------------------------------
png("ufo1.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(world)+
  geom_polygon(mapping = aes(x=long,y=lat,group=group),fill="black", color="white")+
  geom_point(data = data, aes(x=longitude,y=latitude, color=befo),alpha=1/2)+
  scale_color_manual(values = c("blue","red","yellow"))+
  geom_point(data=data %>% filter(befo=="Before 1947"),
             aes(x=longitude,y=latitude),size=3,color="blue",alpha=1/2)+
  coord_fixed(xlim=c(-180,-50),
              ylim = c(10,80))+
  theme_ridges()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray",colour = "lightgray"))+
  labs(title="UFO Sighting in US",
       subtitle = "Most UFO sighting took place in US. On 1947, the most famous UFO sighting happened, the Roswell Incident.
Since then, the number of UFO sighting has skyrocketed.",
       caption = "Data: NUFORC",
       color = "Period",
       y="Lattitude",x="Longitude")
dev.off()

#Number of Sighting by Year -------------------------------------------------------------
data_year <- data %>% group_by(year_sighted) %>%
  summarise(sighting = n(), durations = mean(encounter_length),
            year_span = mean(year_span))

png("ufo2.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(data_year)+
  geom_line(aes(x = year_sighted,y=sighting),color="pink",size=2,alpha=1/2)+
  geom_point(aes(x = year_sighted,y = sighting,color=sighting),size=2)+
  scale_color_gradient(low = "orange",high = "red")+
  scale_x_continuous(breaks = c(1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2020))+
  theme_ridges()+
  geom_vline(aes(xintercept = 1947),linetype="dashed",color="lightgray")+
  geom_label(aes(label = "Roswell Incident",x=1947,y=6500),
             hjust="center",fill="lightyellow",alpha=1/2)+
  geom_vline(aes(xintercept = 2004),linetype="dashed",color="lightgray")+
  geom_label(aes(label = "Facebook
Launched",x=2004,y=6500),
             hjust="center",fill="lightyellow",alpha=1/2)+
  geom_vline(aes(xintercept = 1995),linetype="dashed",color="lightgray")+
  geom_label(aes(label = "Yahoo Launched",x=1995,y=5500),
            hjust="center",fill="lightyellow",alpha=1/2)+
  geom_vline(aes(xintercept = 1982),linetype="dashed",color="lightgray")+
  geom_label(aes(label = "E.T.",x=1982,y=6000),
             hjust="center",fill="lightyellow",alpha=1/2)+
  geom_vline(aes(xintercept = 1977),linetype="dashed",color="lightgray")+
  geom_label(aes(label = "Star Wars:
A New Hope",x=1977,y=4500),
             hjust="center",fill="lightyellow",alpha=1/2)+
  geom_vline(aes(xintercept = 1966),linetype="dashed",color="lightgray")+
  geom_label(aes(label = "Star Trek:
The Original Series",x=1966,y=5500),
             hjust="center",fill="lightyellow",alpha=1/2)+
  theme(panel.grid = element_blank())+
  labs(title = "Number of UFO Sighting by Year",
       caption = "Data: NUFORC",
       subtitle = "Comparing number of UFO sighting with space-related movie and the emergence of internet",
       x = "year",y="Number of Sighting")
dev.off()

#Any Change in Trend of UFO Shape Sighting? --------------------------------------
topshape <- data %>% filter(is.na(ufo_shape)==F) %>% filter(befo=="After 1990") %>%
  group_by(ufo_shape) %>% summarise(sighting = n())
topshape <- topshape[order(topshape$sighting),]
topshape$ufo_shape <- factor(topshape$ufo_shape,topshape$ufo_shape)

data_befo <- data %>% filter(is.na(ufo_shape)==F) %>% group_by(ufo_shape,befo) %>%
  summarise(sighting = n(), durations = mean(encounter_length),
            year_span = mean(year_span))
data_befo$ufo_shape <- factor(data_befo$ufo_shape,topshape$ufo_shape)
data_befo <- data_befo %>% filter(sighting>1)

png("ufo3.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(data_befo)+
  geom_col(aes(x=ufo_shape,y=sighting,fill=befo),position = "dodge")+
  coord_flip()+
  labs(title = "Popular UFO Shape",
       subtitle = "Disk shape was the most popular shape of UFO",
       fill = "Period", x = "UFO Shape", y = "Number of Sighting",
       caption = "Data: NUFORC")
dev.off()

#UFO Sighting by Month ---------------------------------------------------------
data_month <- data %>% group_by(year_sighted,month_sighted) %>% 
  summarise(sighting = n(), durations = mean(encounter_length),
            year_span = mean(year_span))

png("ufo4.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(data_month)+
  geom_tile(aes(year_sighted,month_sighted,fill=sighting),color = "#1D2024")+
  scale_y_continuous(breaks = 1:12,labels = month.abb[1:12],trans = "reverse")+
  scale_fill_viridis_c(option = "B")+
  theme_ridges()+
  labs(title = "UFO Sighting",
       subtitle = "Brighter color indicate higher frequency of UFO Sighting",
       caption = "Data: NUFORC",
       x= "Year", y= "Month")+
  theme(plot.background = element_rect(fill =  "#1D2024", color = "#1D2024"),
        panel.background = element_rect(fill =  "#1D2024", color = "#1D2024"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"))
dev.off()

#UFO Sighting by Hour ----------------------------------------------------------
data_hour <- data %>% group_by(hour_sighted,month_sighted) %>% 
  summarise(sighting = n(), durations = mean(encounter_length),
            year_span = mean(year_span))

png("ufo5.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(data_hour)+
  geom_tile(aes(hour_sighted,month_sighted,fill=sighting),color = "#1D2024")+
  scale_y_continuous(breaks = 1:12,labels = month.abb[1:12],trans = "reverse")+
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22))+
  scale_fill_viridis_c(option = "B")+
  theme_ridges()+
  labs(title = "UFO Sighting",
       subtitle = "Brighter color indicate higher frequency of UFO Sighting",
       caption = "Data: NUFORC",
       x= "Hour of the Day", y= "Month")+
  theme(plot.background = element_rect(fill =  "#1D2024", color = "#1D2024"),
        panel.background = element_rect(fill =  "#1D2024", color = "#1D2024"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"))
dev.off()

#World Sighting ---------------------------------------------------
data <- ufo
data$date_documented <- mdy(as.character(data$date_documented))
data$date_time <- mdy_hm(as.character(data$date_time))
data <- data %>% dplyr::mutate(year_sighted = year(data$date_time),
                               month_sighted = month(data$date_time),
                               hour_sighted = hour(data$date_time),
                               year_documented = year(data$date_documented),
                               year_span = year_documented - year_sighted,
                               befo = case_when(year_sighted < 1947 ~ "Before 1947",
                                                year_sighted > 1946 & year_sighted < 1990 ~ "1947-1990",
                                                year_sighted > 1989 ~ "After 1990"))
data$befo <- factor(data$befo,levels = c("Before 1947","1947-1990","After 1990"))

png("ufo6.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(world)+
  geom_polygon(mapping = aes(x=long,y=lat,group=group),fill="#1D2024", color="white")+
  geom_point(data = data, aes(x=longitude,y=latitude, color=befo),size=1,alpha=1/2)+
  scale_color_manual(values = c("blue","red","yellow"))+
  geom_point(data=data %>% filter(befo=="Before 1947"),
             aes(x=longitude,y=latitude),size=3,color="blue",alpha=1/2)+
  theme_ridges()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "lightgray",colour = "lightgray"))+
  labs(title="UFO Sighting in US",
       subtitle = "Most UFO sighting took place in US. On 1947, the most famous UFO sighting happened, the Roswell Incident.
Since then, the number of UFO sighting has skyrocketed.",
       caption = "Data: NUFORC",
       color = "Period",
       y="Lattitude",x="Longitude")
dev.off()

#Hexbin of US Sighting ------------------------------------------------------------------
usa <- map_data("state")
dato <- data %>% filter(country=="us" & state != "hi" & state != "ak")
dato <- dato %>% group_by(state) %>% summarise(sighting = n())
head(dato)
png("ufo7.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(usa)+
  geom_polygon(aes(x=long,y=lat,group=group),color="white",fill = "#1D2024")+
  geom_hex(data = dato, aes(x=longitude,y=latitude),bins=48,alpha=1/2)+
  scale_fill_viridis_c(option = "A")+
  theme_void()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white",color="white"))+
  labs(title="Area with the most UFO Sighting in US",
       subtitle = "Overall, East Coast has higher number of UFO Sighting",
       caption = "Data: NUFORC",
       fill = "Number of Sighting")
dev.off()


#Indonesian UFO Sighting ---------------------------------------------------------------
data <- ufo %>% filter(is.na(date_time)==F & longitude>94 & longitude<142 & latitude < 6 &
                         latitude > (-11))
data$date_documented <- mdy(as.character(data$date_documented))
data$date_time <- mdy_hm(as.character(data$date_time))
data <- data %>% dplyr::mutate(year_sighted = year(data$date_time),
                               month_sighted = month(data$date_time),
                               hour_sighted = hour(data$date_time),
                               year_documented = year(data$date_documented),
                               year_span = year_documented - year_sighted,
                               befo = case_when(year_sighted < 1947 ~ "Before 1947",
                                                year_sighted > 1946 & year_sighted < 1990 ~ "1947-1990",
                                                year_sighted > 1989 ~ "After 1990"))
data$befo <- factor(data$befo,levels = c("Before 1947","1947-1990","After 1990"))

png("ufo8.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(world)+
  geom_polygon(mapping = aes(x=long,y=lat,group=group),fill="#1D2024", color="white")+
  geom_point(data = data, aes(x=longitude,y=latitude, color=befo),size=3,alpha=1/2)+
  geom_point(data=data %>% filter(befo=="Before 1947"),
             aes(x=longitude,y=latitude),size=3,color="blue",alpha=1/2)+
  scale_color_manual(values = c("red","yellow"))+
  coord_fixed(xlim=c(95,141),
              ylim = c(6,-11))+
  theme_ridges()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray",colour = "lightgray"))+
  labs(title="UFO Sighting in INDONESIA",
       subtitle = "Sighting of UFO in Indonesia only happened after 1990",
       caption = "Data: NUFORC",
       color = "Period",
       x="Lattitude",y="Longitude")
dev.off()
