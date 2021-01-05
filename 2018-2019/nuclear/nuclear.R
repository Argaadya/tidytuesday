library(tidyverse)
library(lubridate)
library(treemap)
library(maps)
library(ggrepel)

#Data Preparation
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")
write.csv(nuclear_explosions,"D:/R/datasets/nuclear_explosion.csv")
nuclear_explosions <- read.csv("D:/R/datasets/nuclear_explosion.csv")

world <- map_data("world")

setwd("D:/R/tidytuesday/nuclear_explosion/")

#Location of Nuclear Explosion -------------------------------------------------------
Japan_WW2 <- nuclear_explosions %>% filter(region == "HIROSHIMA" | region == "NAGASAKI")

png("nuclear1.png", width = 3960, height = 2160, units = 'px', res = 300)
ggplot(world)+
  geom_polygon(mapping = aes(x=long,y=lat,group=group),fill="black", color="grey28")+
  geom_point(data = nuclear_explosions, 
             aes(x=longitude,y=latitude, 
                 color = country, size = yield_upper),alpha=1/2)+
  geom_text_repel(data = Japan_WW2, aes(x=longitude,y=latitude,label=region),
                  color="lightyellow",nudge_x = 30,size=3,segment.alpha = 0.5)+
  geom_text_repel(data = nuclear_explosions %>% filter(year>1991),
                  aes(x=longitude,y=latitude),label="AFC",segment.alpha = 0.5,
                  color="lightyellow",size=2.5)+
  scale_color_discrete()+
  labs(x="",y="",
       size = "Kilotons", colore = "Country",
       caption = "AFC : After Cold War Ended (1991)",
       title = "Nuclear Explosion Around The World",
       subtitle = "USA and USSR has more powerful nuclear warhead compared to the one dropped at Hiroshima and Nagasaki,
More countries have tested nuclear weapon since the end of cold war, while no ex-USSR nations done a nuclear testing yet")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "gold",size=14))
  
dev.off()

#Power of Nuke over time ---------------------------------------------------

nuclear_explosions$type <- str_replace(nuclear_explosions$type,"WATERSUR","WATER SURFACE")
nuclear_explosions$type <- str_replace(nuclear_explosions$type,"WATER SU","WATER SURFACE")
nuclear_explosions$type <- str_replace(nuclear_explosions$type,"ATMOSPH","ATMOSPHERIC")
nuclear_explosions$type <- str_replace(nuclear_explosions$type,"WATER SURFACERFACE",
                                       "WATER SURFACE")
top_method <- nuclear_explosions %>% group_by(type) %>% 
  summarise(total=n(),magn = mean(magnitude_surface,na.rm = T)) %>% arrange(desc(magn))

nuclear_explosions$type <- factor(nuclear_explosions$type,levels = top_method$type)

png("nuclear2.png", width = 4500, height = 3240, units = 'px', res = 300)
nuclear_explosions %>% ggplot()+
  geom_point(aes(year,country,color=yield_upper,size=magnitude_surface),
             show.legend = F,alpha=.5)+
  facet_wrap(~type,scales = "free_y",nrow = 7,ncol = 3)+
  geom_text_repel(data = Japan_WW2[1,], aes(x=year,y=country,label=region),
                  color="khaki1",segment.alpha = 0.5,size=4,nudge_y = -2)+
  scale_color_gradient(low = "orange",high = "lightyellow")+
  scale_x_continuous(breaks = seq(1945,2000,10))+
  labs(x="",y="",
       title = "Method of Nuclear Warhead Deployment and Their Power
",
       caption = "Size indicate magnitude surface")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        plot.title = element_text(size=18),
        plot.caption = element_text(size=16),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow",size=10),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "gray28"),
        strip.text = element_text(colour = "snow",size=12))
dev.off()

#Underground vs Airborne Nuclear Warhead --------------------------------------
heigh <- nuclear_explosions %>% filter(depth < - 30)
strong <- nuclear_explosions %>% filter(yield_upper > 15000)

png("nuclear3.png", width = 3960, height = 2160, units = 'px', res = 300)
nuclear_explosions %>% ggplot()+
  geom_point(aes(yield_upper,-depth,color=yield_upper,
                 size=magnitude_body),alpha=.5)+
  scale_color_gradient(low = "orange",high = "lightyellow",guide = F)+
  scale_y_continuous(breaks = seq(-100,400,50))+
  geom_text_repel(data = heigh, aes(x=yield_upper,y=-depth,
                                    label=paste(name,region,sep = ", ")),
                  color="snow1",segment.alpha = 0.5,size=3,nudge_y = 20)+
  geom_text_repel(data = strong, aes(x=yield_upper,y=-depth,
                                    label=paste(name,region,sep = ", ")),
                  color="snow1",segment.alpha = 0.5,size=3,nudge_y = 20)+
  labs(x="Yield (Kilotons of TNT)", y="Height (in Km)",
       size = "Magnitude Surface",
       title = "Is Airborne Explosion Weaker Than The Ground/Underground One?",
       subtitle = "Interesting, many of USSR strongest nuke is a secret weapon (no name)")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "snow2"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "gold",size=14))
dev.off()

#Treemap
png("nuclear4.png", width = 4500, height = 3240, units = 'px', res = 300)
treemap(nuclear_explosions %>% count(country,purpose,type),
        index = c("country","purpose","type"),
        vSize = "n",type = "index",
        fontsize.labels = c(20,14,10),
        border.lwds = c(6,4,1),
        align.labels = list(c("center","top"),c("left","bottom"),c("right","center")),
        palette = "Set1",
        title = "How and Why Every Country Use A Nuclear Warhead")
dev.off()

