library(tidyverse)
library(lubridate)
library(cowplot)
library(ggridges)
library(treemap)

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")
write.csv(emperors,"D:/R/datasets/emperor.csv")
emperors <- read.csv("D:/R/datasets/emperor.csv")

setwd("D:/R/tidytuesday/roman_emperor/")
#Relation between rise of power to death cause ------------------------------------------
png("emperor1.png", width = 3960, height = 2160, units = 'px', res = 300)
emperors %>% count(cause,rise,killer) %>% 
  ggplot()+
  geom_tile(aes(killer,cause,fill=n),color="#1D2024")+
  scale_fill_gradient(low = "maroon",high = "lightyellow")+
  facet_wrap(~rise,ncol = 3,scales = "free")+
  labs(title="How Emperor Rise to Power and Death",
       x = "Killer", y="Rise of Power",fill="count")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.text.x = element_text(angle=30,vjust=1,hjust=1),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "gold",size=14))
dev.off()

#Most common cause of death for an emperor ------------------------------------------------
cause <- emperors %>% count(cause) %>% mutate(cause = reorder(cause,n)) %>% 
  ggplot()+
  geom_col(aes(cause,n,fill=n))+
  geom_text(aes(cause,n,label=n),nudge_y = 1,color="lightyellow")+
  scale_fill_viridis_c(option = "B")+
  coord_flip()+
  labs(x="Cause of Death",y="Count",fill="count",
       title = "Emperor's Cause of Death")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "gold",size=14))

rise <- emperors %>% count(rise) %>% mutate(rise = reorder(rise,n)) %>% 
  ggplot()+
  geom_col(aes(rise,n,fill=n))+
  geom_text(aes(rise,n,label=n),nudge_y = 1,color="lightyellow")+
  scale_fill_viridis_c()+
  coord_flip()+
  labs(x="Rise of Power",y="Count",fill="count",
       title = "How Emperor Rise to Power")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "gold",size=14))

#Dynasty
dinas <- emperors %>% count(dynasty) %>% mutate(dynasty = reorder(dynasty,n)) %>% 
  ggplot()+
  geom_col(aes(dynasty,n,fill=n))+
  geom_text(aes(dynasty,n,label=n),nudge_y = 1,color="lightyellow")+
  scale_fill_viridis_c(option = "C")+
  coord_flip()+
  labs(x="Dynasty",y="Count",fill="count",
       title = "Dynasty of Emperor")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "gold",size=14))

#Birth Province
birth <- emperors %>% count(birth_prv) %>% mutate(birth_prv = reorder(birth_prv,n)) %>% 
  ggplot()+
  geom_col(aes(birth_prv,n,fill=n))+
  geom_text(aes(birth_prv,n,label=n),nudge_y = 1,color="lightyellow")+
  scale_fill_viridis_c(option = "D")+
  coord_flip()+
  labs(x="Birth Province",y="Count",fill="count",
       title = "Birth Province of Emperor")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "gold",size=14))

png("emperor2.png", width = 3960, height = 2160, units = 'px', res = 300)
plot_grid(rise,cause,dinas,birth,nrow = 2)
dev.off()

#Expected Age of Emperor from each dinasty
emperors <- emperors %>% mutate(year_birth = year(emperors$birth),
                    year_death = year(emperors$death),
                    ages = if_else(year_death - year_birth > 0,
                                   year_death - year_birth,
                                   year_death + year_birth))

png("emperor3.png", width = 3960, height = 2160, units = 'px', res = 300)
emperors %>% ggplot()+
  geom_density_ridges(aes(ages,dynasty,fill=era),alpha=.5)+
  scale_x_continuous(breaks = seq(0,100,10))+
  labs(x="Ages",y="Dynasty",fill="Era",
       title = "Age Distribution of Roman Emperors")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "gold",size=14))
dev.off()

#Tree map of Rise and Death of Emperor
png("emperor4.png", width = 3960, height = 2160, units = 'px', res = 300)
treemap(emperors %>% count(rise,cause,killer),
        index = c("rise","cause","killer"),
        vSize = "n",type = "index",
        fontsize.labels = c(20,14,10),
        border.lwds = c(6,4,1),
        align.labels = list(c("center","top"),c("left","bottom"),c("right","center")),
        palette = "Set1",
        title = "Rise and Death of Emperors")
dev.off()
