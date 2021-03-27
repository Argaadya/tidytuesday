library(tidyverse)
library(ggridges)
library(ggrepel)
library(treemap)
library(cowplot)

setwd("D:/R/datasets/")
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")
write.csv(media_franchises,"media_franchise.csv")
data <- read.csv("media_franchise.csv")

setwd("D:/R/tidytuesday/media_franchise/")

#Comparing Revenue from Revenue Category with the Original Media -------------------
png("franchise1.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(data)+
  geom_tile(aes(revenue_category,original_media,fill=revenue),color="gray")+
  scale_fill_viridis_c(option = "B")+
  theme_ridges()+
  labs(title = "Revenue from Various Category and Original Media",
       subtitle = "Overall, Merchandise and Licensing is the highest source of revenue",
       caption = "@Argaadya1 | Data: Wikipedia",
       x= "Revenue Category", y= "Original Media",fill="Revenue (in Billions)")+
  theme(plot.background = element_rect(fill =  "#1D2024"),
        panel.background = element_rect(fill =  "#1D2024"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle=30,hjust = 1),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"))
dev.off()

#Comic vs Manga: Which one is better? -------------------------------------------------------
manga_comic <- data %>% filter(original_media == "Manga" | original_media == "Comic book") %>% 
  group_by(franchise,original_media) %>% summarise(revenue = sum(revenue))
manga1 <- manga_comic[manga_comic$revenue %in% max(manga_comic$revenue),]
numnum <- data.frame(label = c(paste("Number of franchise:",nrow(manga_comic %>% filter(original_media == "Manga" ))),
                               paste("Number of franchise:",nrow(manga_comic %>% filter(original_media == "Comic book")))),
                     original_media=c("Manga","Comic book"),
                     revenue = c(75,30))

png("franchise2.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(manga_comic)+
  geom_boxplot(aes(y=revenue,x=original_media,fill=original_media),color="pink",
               outlier.color = "orange")+
  scale_fill_discrete(guide=F)+
  geom_text(data=manga1,aes(x=original_media,y=revenue,label=franchise),color="white",nudge_y = -5)+
  geom_text(data=numnum,aes(original_media,revenue,label=label),color="white")+
  theme_ridges()+
  theme(plot.background = element_rect(fill =  "#1D2024"),
        panel.background = element_rect(fill =  "#1D2024"),
        panel.grid = element_blank(),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"))+
  labs(title = "Is Manga better than Comic Book?",
       subtitle = "Revenue from Manga are more varied, with highest total revenue ever generated is hold by Anpanman",
       caption = "@Argaadya1 | Data: Wikipedia",
       x= "Original Media", y= "Total Revenue (in Billions)")
dev.off()

#What Original Media Gain Most Revenue -----------------------------------------------
origin <- data %>% group_by(franchise,original_media) %>% 
  summarise(revenue = sum(revenue))
origin <- origin[order(origin$revenue),]
origin$franchise <- factor(origin$franchise,origin$franchise)
origin <- origin[(nrow(origin)-24):nrow(origin),]

png("franchise3.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(origin)+
  geom_col(aes(franchise,revenue,fill=original_media))+
  geom_text(aes(franchise,revenue,label=round(revenue,2)),nudge_y = 5,color="orange")+
  coord_flip()+
  theme_ridges()+
  theme(plot.background = element_rect(fill =  "#1D2024"),
        panel.background = element_rect(fill =  "#1D2024"),
        panel.grid = element_blank(),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"))+
  labs(title = "Top Franchise by Total Revenue",
       subtitle = "Manga dominate the chart, with 7 of Top 25 franchise are from Manga",
       caption = "@Argaadya1 | Data: Wikipedia",
       x= "Franchise", y= "Total Revenue (in Billions)",fill="Original Media")
dev.off()


#Treemap of Revenue Category -----------------------------------
png("franchise4.png", width = 3840, height = 2160, units = 'px', res = 300)
treemap(data,
        index = c("revenue_category","franchise"),
        vSize = "revenue",type = "index",
        palette = "Set1",
        title = "Treemap of Revenue by Category"
        )
dev.off()

#combine Plot -----------------------------------------------------------
f1 <- ggplot(data)+
  geom_tile(aes(y=revenue_category,x=original_media,fill=revenue),color="gray")+
  scale_fill_viridis_c(option = "B")+
  theme_ridges()+
  labs(title = "Revenue from Various Category and Original Media",
       subtitle = "Overall, Merchandise and Licensing is the highest source of revenue",
       caption = "@Argaadya1 | Data: Wikipedia",
       y= "Revenue Category", x= "Original Media",fill="Revenue
(in Billions)")+
  theme(plot.background = element_rect(fill =  "#1D2024"),
        panel.background = element_rect(fill =  "#1D2024"),
        panel.grid = element_blank(),
        plot.title = element_text(colour = "lightgoldenrodyellow"),
        axis.text.x = element_text(angle=45,hjust = 1,size = 10),
        axis.text.y = element_text(hjust = 1,size = 10),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "white",size = 12))

origin <- data %>% group_by(franchise,original_media) %>% 
  summarise(revenue = sum(revenue))
origin <- origin[order(origin$revenue),]
origin$franchise <- factor(origin$franchise,origin$franchise)
origin <- origin[(nrow(origin)-9):nrow(origin),]

f2 <- ggplot(origin)+
  geom_col(aes(franchise,revenue,fill=original_media))+
  geom_text(aes(franchise,revenue,label=round(revenue,2)),nudge_y = 5,color="orange")+
  geom_text(aes(franchise,3,label=original_media),size=3,color="black",hjust="left")+
  coord_flip()+
  scale_fill_discrete(guide=F)+
  theme_ridges()+
  theme(plot.background = element_rect(fill =  "#1D2024"),
        panel.background = element_rect(fill =  "#1D2024"),
        panel.grid = element_blank(),
        plot.title = element_text(colour = "lightgoldenrodyellow"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white",size = 10),
        axis.title = element_text(colour = "white",size = 12))+
  labs(title = "Top Franchise by Total Revenue",
       subtitle = "Manga dominate the chart, with 3 of Top 10 franchise originated from Manga",
       x= "Franchise", y= "Total Revenue (in Billions)",fill="Original Media")

numnum <- data.frame(label = c(paste("Number of franchise:",nrow(manga_comic %>% filter(original_media == "Manga" ))),
                               paste("Number of franchise:",nrow(manga_comic %>% filter(original_media == "Comic book")))),
                     original_media=c("Manga","Comic book"),
                     revenue = c(75,35))

f3 <- ggplot(manga_comic)+
  geom_boxplot(aes(y=revenue,x=original_media,fill=original_media),color="pink",
               outlier.color = "orange")+
  scale_fill_discrete(guide=F)+
  geom_text(data=manga1,aes(x=original_media,y=revenue,label=franchise),color="white",nudge_y = -5)+
  geom_text(data=numnum,aes(original_media,revenue,label=label),color="white")+
  theme_ridges()+
  theme(plot.background = element_rect(fill =  "#1D2024"),
        panel.background = element_rect(fill =  "#1D2024"),
        panel.grid = element_blank(),
        plot.title = element_text(colour = "lightgoldenrodyellow"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white",size = 10),
        axis.title = element_text(colour = "white",size = 12))+
  labs(title = "Manga vs Comic Book",
       subtitle = "Revenue from Manga are more varied",
       x= "Original Media", y= "Total Revenue (in Billions)")

row1 <- plot_grid(f2,f3,rel_widths = c(2,1))
png("franchise5.png", width = 3840, height = 2160, units = 'px', res = 300)
plot_grid(row1,f1,nrow = 2)
dev.off()
