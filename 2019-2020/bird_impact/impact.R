library(tidyverse)
library(ggridges)
library(lubridate)
library(cowplot)
library(Hmisc)
library(here)
library(extrafont)
library(maps)
loadfonts(device = "win")

bird_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/bird_impacts.csv")
write.csv(bird_impacts,"D:/R/datasets/bird_impact.csv")

bird_impacts <- read.csv("D:/R/datasets/bird_impact.csv")

setwd("D:/R/tidytuesday/bird_impact")
#Data Cleaning ---------------------------------------------------------------------
bird_impacts$state[bird_impacts$state=="N/A"] <- NA
bird_impacts$airport[bird_impacts$airport=="UNKNOWN"] <- NA
bird_impacts$atype[bird_impacts$atype=="UNKNOWN"] <- NA
bird_impacts$damage <- as.character(bird_impacts$damage)
bird_impacts$damage[bird_impacts$damage=="M?"] <- "M"
bird_impacts$species[str_detect(bird_impacts$species,"Unknown")==T] <- NA
bird_impacts$phase_of_flt[bird_impacts$phase_of_flt=="Unknown"] <- NA
bird_impacts$phase_of_flt <- tolower(bird_impacts$phase_of_flt)
bird_impacts$phase_of_flt <- capitalize(bird_impacts$phase_of_flt)

#When Bird Impact is Highest? ------------------------------------------------------
bydate <- bird_impacts %>% group_by(incident_month,incident_year) %>% 
  summarise(freq = n())
bydate$incident_month <- month(bydate$incident_month,label = T)

png("bird1.png", width = 3840, height = 2160, units = 'px', res = 300)  
ggplot(bydate)+
  geom_tile(aes(incident_year,incident_month,fill=freq),color="#1D2024")+
  scale_x_continuous(breaks = seq(1990,2020,2))+
  scale_fill_viridis_c(option = "B")+
  labs(title = "When Did Bird Impact Happened Most?", fill = "Frequency",
       x = "Year", y = "Month",
       subtitle = "Number of Recorded Bird Impact are Increasing, Most Frequent in September-October")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow"),
        panel.grid = element_blank())
dev.off()

#What Time of Day Did Bird Impact Happened? ------------------------------------------
bytime <- bird_impacts %>% group_by(time_of_day,sky,damage,phase_of_flt,precip) %>% 
  summarise(Frequency = n()) %>% na.omit() %>% unite(weather, c(sky,precip),sep = " - ") %>% 
  group_by(phase_of_flt,weather) %>% summarise(Frequency = n())
bytime <- bytime[order(bytime$Frequency),]
bytime$weather <- factor(bytime$weather,unique(bytime$weather))
bytime$phase_of_flt <- factor(bytime$phase_of_flt,
                              levels = c("Parked","Taxi","Take-off run","Climb","Departure",
                                         "En route","Arrival","Descent","Approach",
                                         "Landing roll","Local"))

png("bird2.png", width = 3840, height = 2160, units = 'px', res = 300)  
ggplot(bytime)+
  geom_tile(aes(phase_of_flt,weather,fill=Frequency),color="#1D2024")+
  scale_fill_viridis_c(option = "A")+
  labs(title = "When Did Bird Impact Happened Most?", fill = "Frequency", 
       subtitle = "Most of bird impact happened in clear sky/some cloud",
       x = "Phase of Flight", y = "Weather")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow"),
        panel.grid = element_blank())
dev.off()

#How Much the Cost Incurred? -------------------------------------------------------
bytime <- bird_impacts %>% group_by(time_of_day,sky,damage,phase_of_flt) %>% 
  summarise(Frequency = n(), cost = sum(cost_repairs_infl_adj,na.rm = T)) %>% na.omit()
bytime$phase_of_flt <- factor(bytime$phase_of_flt,
                              levels = c("Parked","Taxi","Take-off run","Climb","Departure",
                                         "En route","Arrival","Descent","Approach",
                                         "Landing roll","Local"))
bytime$damage <- factor(bytime$damage,levels = c("N","M","S"))
options(scipen = 10)

png("bird3.png", width = 4240, height = 2160, units = 'px', res = 300)  
ggplot(bytime)+
  geom_tile(aes(damage,sky,fill=Frequency),color="#1D2024")+
  scale_fill_viridis_c(option = "A")+
  labs(title = "When Did Bird Impact Happened Most?", fill = "Frequency",
       x = NULL, y = NULL)+
  facet_grid(cols = vars(phase_of_flt),rows = vars(time_of_day),as.table = T)+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white",size = 12),
        panel.grid = element_blank(),
        axis.text = element_text(colour = "lightyellow",family = "mono",size = 11),
        strip.background = element_rect(fill = "#1D2024",colour = "gray"),
        strip.text.x = element_text(colour = "lightyellow",family = "mono"),
        strip.text.y = element_text(colour = "lightyellow",family = "mono",size = 12))
dev.off()

png("bird4.png", width = 4240, height = 2160, units = 'px', res = 300)  
ggplot(bytime)+
  geom_tile(aes(damage,sky,fill=cost),color="#1D2024")+
  scale_fill_gradientn(colours = c("lightgreen","yellow","orange","red", "darkred"))+
  labs(title = "How Much the Cost Incurred?", fill = "Cost",
       x = NULL, y = NULL)+
  facet_grid(cols = vars(phase_of_flt),rows = vars(time_of_day),as.table = T)+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white",size = 12),
        panel.grid = element_blank(),
        axis.text = element_text(colour = "lightyellow",family = "mono",size = 11),
        strip.background = element_rect(fill = "#1D2024",colour = "gray"),
        strip.text.x = element_text(colour = "lightyellow",family = "mono"),
        strip.text.y = element_text(colour = "lightyellow",family = "mono",size = 12))
dev.off()

#What is most frequent bird species and their damage ----------------------------------------
byspecies <- bird_impacts %>% group_by(species,damage) %>% summarise(freq = n()) %>% 
              na.omit()
byspecies2 <- byspecies %>% group_by(species) %>% summarise(freq = sum(freq))
byspecies2 <- byspecies2[order(byspecies2$freq,decreasing = T),]
byspecies2 <- byspecies2[1:30,]
byspecies2 <- byspecies2[order(byspecies2$freq),]
byspecies <- byspecies[byspecies$species %in% byspecies2$species,]
byspecies$species <- factor(byspecies$species,byspecies2$species)
byspecies$damage <- factor(byspecies$damage,levels = c("S","M","N"))

png("bird5.png", width = 3840, height = 2160, units = 'px', res = 300)  
ggplot(byspecies)+
  geom_col(aes(species,freq,fill=damage),color="lightyellow")+
  scale_fill_brewer(palette = "Spectral")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow"),
        panel.grid = element_blank())+
  labs(title = "Bird with Highest Strike Cases",
       subtitle = "The Gulls is a menace",
       y = "Frequency", x = "Bird Species",fill="Damage")
dev.off()

#Cost of Strikes for Operators -------------------------------------------------------
data <- bird_impacts %>% filter(is.na(sky)==F & is.na(damage)==F & is.na(speed)==F)
data$damage[data$damage=="S"] <- "Substantial"
data$damage[data$damage=="M"] <- "Minor"
data$damage[data$damage=="N"] <- "None"
data$damage <- factor(data$damage,levels = c("None","Minor","Substantial"))

png("bird6.png", width = 3840, height = 2160, units = 'px', res = 300)  
ggplot(data)+
  geom_density_ridges(aes(cost_repairs_infl_adj,damage,fill=operator),
                      scale=0.95,color="#1D2024")+
  scale_fill_brewer(palette = "Set3",guide=F)+
  theme_ridges()+
  facet_wrap(facets = data$operator)+
  scale_x_continuous(labels = formatC(seq(0,5500000,1000000),digits = 0,format = "f",big.mark = ","),
                     breaks = seq(0,5500000,1000000))+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"))+
  labs(title = "Cost Incurred by Bird Strikes",subtitle = " ",
       x = "Cost", y = "Damages")
dev.off()

#Where? ------------------------------------------------------------
us_state <- map_data("state")
states_df <- data.frame(state = state.abb,name = state.name)
bystate <- bird_impacts %>% group_by(state) %>% summarise(freq = n()) %>% na.omit() %>% 
  left_join(states_df,by = "state") %>% 
  left_join(us_state %>% mutate(region = str_to_title(region)) %>% select(-subregion),
            by = c("name" = "region")) %>% na.omit() %>% 
  filter(!is.na(group))
bird <- bird_impacts %>% group_by(state,species) %>% summarise(freq = n()) %>% na.omit()
bird <- bird[bird$state %in% bystate$state,]
bird2 <- aggregate(freq~state,data = bird,max)
bird <- bird %>% right_join(bird2,by = c("state","freq"))
bird <- bird[!duplicated(bird$state),]
bird3 <- bird %>% left_join(states_df,by = "state") %>% 
  left_join(us_state %>% mutate(region = str_to_title(region)) %>% select(-subregion),
            by = c("name" = "region")) %>% na.omit() %>% 
  filter(!is.na(group))
bird3 <- bird3 %>% group_by(state) %>% summarise(long = mean(long), lat = mean(lat))
bird <- bird %>% left_join(bird3,by = "state")  

png("bird7.png", width = 4240, height = 2160, units = 'px', res = 300)  
ggplot(bystate)+
  geom_polygon(aes(long,lat,fill=freq,group=group),color="white")+
  geom_text(data = bird, aes(long,lat, label = species),hjust="center",color="orange")+
  scale_fill_gradient(low = "#1D2024",high = "white")+
  labs(title = "Where is Cases of Bird Strike Mostly Happened?",
       x=NULL,y=NULL,fill="Frequency")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"))
dev.off()
