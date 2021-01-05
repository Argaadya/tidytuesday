library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)

setwd("D:/R/tidytuesday/simpsons/")
#Data 
simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")
write.csv(simpsons,"D:/R/datasets/simpsons.csv")
simpsons <- read.csv("D:/R/datasets/simpsons.csv")
simpsons$season <-as.numeric(simpsons$season)
simpsons$season <- factor(simpsons$season,levels = sort(unique(simpsons$season)))

#Number of Guest Star by Season
simpsons_season <- simpsons %>% filter(is.na(role)==F) %>% 
  mutate(role = case_when(str_detect(role,"Himself") ~ "Himself",
                          str_detect(role,"Herself") ~ "Herself",
                          role != "Himself" & role != "Herself"  ~ "Other")) %>% 
  group_by(season,role) %>% summarise(total = n()) 

#Top Guest Star in each season?
top_guest <- simpsons %>% filter(is.na(role)==F) %>% 
  group_by(season,guest_star) %>% summarise(total = n()) %>% ungroup() %>% 
  group_by(season) %>% filter(total>1) %>% top_n(1,total) 

#Number of Guest Star in each season -------------------------------------------------
number_guest <- simpsons %>% count(season)
top_guest <- top_guest %>% inner_join(number_guest) %>% arrange(desc(n))
top_guest$guest_star <- factor(top_guest$guest_star,
                               levels = unique(top_guest$guest_star))

png("simpson1.png", width = 3960, height = 2160, units = 'px', res = 300)
simpsons_season %>% ggplot()+
  geom_col(aes(season,total,fill=role),position = "stack")+
  geom_text(data=top_guest,aes(season,n,label=guest_star),
            color=if_else(top_guest$guest_star=="Marcia Wallace","gold","white"),
            angle=90,hjust="left",nudge_y = 2,check_overlap = T)+
  scale_fill_brewer(palette = "Spectral",direction = -1)+
  scale_y_continuous(limits = c(0,85),breaks = seq(1,81,5))+
  labs(x="",y="Number of Guest Stars",fill="Role as",
       title = "Guest Star's Role in Each Season",
       subtitle = "Marcia Wallace is the top guest star on many season",
       caption = "Vertical Name Shows Top Guest Star In Each Season")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "#1D2024"),
              plot.background = element_rect(fill = "#1D2024"),
              legend.background = element_rect(fill = "#1D2024"),
              text = element_text(colour = "white"),
              axis.text = element_text(colour = "snow2"),
              panel.grid = element_blank(),
              strip.background = element_rect(fill = "#1D2024"),
              strip.text = element_text(colour = "gold",size=14))
dev.off()

#Link Between Guest Star in an Episode ---------------------------------------------
cor_role <- simpsons %>% 
  pairwise_count(guest_star,episode_title,sort=T) 

png("simpson2.png", width = 4560, height = 2160, units = 'px', res = 300)
cor_role %>% filter(n > 1) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(color="orange")+
  geom_node_point(color = "lightblue",size=3) +
  geom_node_label(aes(label = name), repel = T,color="black",alpha=.6) +
  theme_void()+
  labs(title = "    Guest Stars Who Played Together in A Single Episode More Than Once
",y="")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1D2024",color=NULL),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white",size=12),
        axis.text = element_blank(),
        strip.background = element_rect(fill = "gray80"),
        strip.text = element_text(colour = "#1D2024",size=12),
        legend.background = element_blank(),
        legend.text = element_text(colour = "lightyellow"))
dev.off()

#Top 5 Guest Star in Each Season ---------------------------------------------
png("simpson3.png", width = 3960, height = 2160, units = 'px', res = 300)
simpsons %>% count(guest_star,season,sort = T,name = "total") %>% filter(total>1) %>% 
  mutate(guest_star = factor(guest_star,levels = rev(unique(guest_star)))) %>% 
  group_by(season) %>% top_n(5,total) %>% ungroup() %>% 
  ggplot()+
  geom_segment(aes(x=0,xend=total,y=guest_star,yend=guest_star),color="gray80")+
  geom_vline(aes(xintercept=5),color="red",linetype="dashed")+
  geom_point(aes(total,guest_star,color=if_else(total>5,"yes","no")),
             size=2,show.legend = F)+
  facet_wrap(~season,ncol = 5,scales="free_y")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1D2024",color=NULL),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white",size=12),
        axis.text = element_text(colour = "white"),
        strip.background = element_rect(fill = "pink"),
        strip.text = element_text(colour = "#1D2024",size=12),
        legend.background = element_blank(),
        legend.text = element_text(colour = "lightyellow"))+
  labs(title= "Top Guest Star with More Than One Appearance In A Single Season",
       subtitle = "Marcia Wallace and Phil Hartman Are The Only Guest Star to Appear More Than 5 Times In A Single Season",
       x="",y="")
dev.off()

#Heatmap Number of Top 10 Guest Star in Each Season ----------------------------------
top_guest <- simpsons %>% count(guest_star,name = "total",sort = T) %>% top_n(20,total)
guest_season <- simpsons[simpsons$guest_star %in% top_guest$guest_star,]
guest_season <- guest_season %>% count(guest_star,season,sort = T,name = "total") %>% 
  mutate(guest_star = factor(guest_star,rev(unique(guest_star))))
guest_appear <- guest_season %>% count(guest_star)

png("simpson4.png", width = 3960, height = 2160, units = 'px', res = 300)
guest_season %>% 
  ggplot()+
  geom_tile(aes(guest_star,season,fill=total),color="#1D2024")+
  scale_fill_viridis_c(option = "B")+
  coord_flip()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1D2024",color=NULL),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white",size=12),
        axis.text = element_text(colour = "white"),
        strip.background = element_rect(fill = "gray80"),
        strip.text = element_text(colour = "#1D2024",size=12),
        legend.background = element_blank(),
        legend.text = element_text(colour = "lightyellow"))+
  labs(title = "Number of Star Appearance in Each Season for Top 20 Guest Star",
       x="Season",y="",fill="Appearance",
       subtitle = paste("On average, a guest star appear at least on",
                        median(guest_appear$n),
                        "seasons for the Top 20 Guest Stars"))
dev.off()
