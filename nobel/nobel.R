library(tidyverse)
library(ggridges)

setwd("D:/R/datasets/")
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")
write.csv(nobel_winners,"nobel_winners.csv")
write.csv(nobel_winner_all_pubs,"nobel_winners_pubs.csv")

nob_win <- read.csv("nobel_winners.csv")
nob_pub <- read.csv("nobel_winners_pubs.csv")

setwd("D:/R/tidytuesday/nobel/")

#Filter data with no NA in relevant attribute
data <- nob_win %>% filter(is.na(organization_name)==F)

#Data Grouping
by_yc <- nob_win %>% group_by(prize_year,category) %>% 
  summarise(freq = n())

#Nobel Laureate By Year -------------------------------------------------------------
png("nobel1.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(by_yc)+
  geom_tile(aes(prize_year,category,fill=freq),color="#1D2024")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        panel.grid.major.x =  element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(colour = "lightyellow"),
        axis.text = element_text(colour = "gold"),
        legend.background = element_rect(fill = "#1D2024"))+
  scale_fill_gradient2(low = "black",mid = "navy",high = "lightyellow")+
  scale_x_continuous(breaks = seq(1900,2020,10))+
  labs(title = "Nobel Prize Category by Year",
       subtitle = "During the WW2 (1940-1945) no Nobel Prize are given",
       x = "Prize Year", y = "Category", fill = "Number of Prize",
       caption = "data: dataverse.harvard.edu")
dev.off()

#Which Organization Type Has The High Number of Individual Laureate ---------------------------
by_org <- data %>% 
  mutate(organization_type = case_when(str_detect(organization_name,"Universi")==T ~ "University",
                                       str_detect(organization_name,"Institut")==T ~ "Institute",
                                       str_detect(organization_name,"Lab")==T ~ "Laboratory",
                                       str_detect(organization_name,"Colle")==T ~ "College",
                                       TRUE ~ "Other")) %>% 
  group_by(category,organization_type) %>% 
  summarise(freq = n())
by_org2 <- by_org %>% group_by(organization_type) %>% summarise(t = sum(freq),)
by_org2 <- by_org2[order(by_org2$t),]
by_org$organization_type <- factor(by_org$organization_type,
                                   levels = unique(by_org2$organization_type))

png("nobel2.png", width = 3840, height = 2160, units = 'px', res = 300)                           
ggplot(by_org)+
  geom_col(aes(organization_type,freq,fill=category))+
  coord_flip()+
  scale_fill_brewer(palette = "Set3")+
  theme_ridges()+
  labs(title = "Number of Individual Laureate by Organization Type",
       subtitle = "University Dominate the Nobel Prize",
       y = "Number of Laureate", x = "Organization Type", fill = "Category",
       caption = "data: dataverse.harvard.edu"
       )+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        panel.grid.major.x =  element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(colour = "lightyellow"),
        axis.text = element_text(colour = "gold"),
        legend.background = element_rect(fill = "#1D2024"))
dev.off()  

#Where most of Nobel Laureate Organization is -------------------------------------------------
by_organ <- data %>% group_by(organization_name,category) %>% 
  summarise(freq = n())
by_organ2 <- by_organ %>% group_by(organization_name) %>% summarise(tot = sum(freq))
by_organ2 <- by_organ2[order(by_organ2$tot,decreasing = T),]
by_organ2 <- by_organ2[1:6,]
by_organ2 <- by_organ2[order(by_organ2$tot),]
by_organ <- by_organ[by_organ$organization_name %in% by_organ2$organization_name,]
by_organ$organization_name <- factor(by_organ$organization_name,by_organ2$organization_name)

png("nobel3.png", width = 3840, height = 2160, units = 'px', res = 300)                           
ggplot(by_organ)+
  geom_col(aes(organization_name,freq,fill = category))+
  facet_grid(facets = by_organ$category)+
  scale_fill_brewer(guide=F,palette = "Set2")+
  coord_flip()+
  labs(title = "Top 6 Organization with High Number of Nobel Laureate",
       x="Organization Name", y = "Number of Laureate",
       caption = "data: dataverse.harvard.edu")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "lightyellow"),
        panel.grid = element_blank(),
        text = element_text(colour = "white",size=14),
        axis.text = element_text(colour = "lightyellow"),
        legend.background = element_rect(fill = "#1D2024"))
dev.off()  

#Top 15 People With Nobel Prize -------------------------------------------------
win_name <- nob_pub %>% filter(is.na(title)==F) %>% 
        group_by(laureate_name,is_prize_winning_paper) %>% 
        summarise(tot = n())
win_name2 <- win_name %>% filter(is_prize_winning_paper=="YES")
win_name2 <- win_name2[order(win_name2$tot,decreasing = T),]
win_name2 <- win_name2[1:30,]
win_name2 <- win_name2[order(win_name2$tot),]
win_name <- win_name[win_name$laureate_name %in% win_name2$laureate_name,]
win_name$laureate_name <- factor(win_name$laureate_name,win_name2$laureate_name)
win_name3 <- win_name %>% filter(is_prize_winning_paper=="NO")
win_name3 <- win_name3[order(win_name3$laureate_name),]
ratio <- data.frame(name = win_name2$laureate_name, 
                    win = win_name2$tot, no = win_name3$tot,
                    ratio = round(win_name2$tot/(win_name2$tot+win_name3$tot)*100,2),
                    prop = paste(win_name2$tot,"/",win_name2$tot+win_name3$tot,sep = ""))
  
png("nobel4.png", width = 3840, height = 2160, units = 'px', res = 300)                           
ggplot(win_name)+
  geom_col(aes(laureate_name,tot,fill=tot),color="white")+
  geom_text(data = ratio, aes(name,win_name3$tot,label= prop),hjust = "left",
            nudge_y = 10,color="lightyellow")+
  scale_fill_viridis_c()+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "lightyellow"),
        panel.grid = element_blank(),
        text = element_text(colour = "white",size=14),
        axis.text = element_text(colour = "lightyellow"),
        legend.background = element_rect(fill = "#1D2024"))+
  labs(title = "Individual with High Number of Nobel Prize",
       subtitle = "Number shows proportion of nobel winning publications with his/her total number of publications",
       x= "Laureate Name", y = "Number of Publications", fill = "Publications",
       caption = "data: dataverse.harvard.edu")
dev.off()
