library(tidyverse)
library(lubridate)
library(tidytext)
library(ggrepel)
library(cowplot)

#data import
tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
tidy_anime <- write.csv(tidy_anime,"D:/R/datasets/tidy_anime.csv")
tidy_anime <- read.csv("D:/R/datasets/tidy_anime.csv")

setwd("D:/R/tidytuesday/anime/")
names(tidy_anime)

#Who is the most productive studio?
#Studio with the highest rank
top_studio <- tidy_anime %>% group_by(studio) %>% 
  summarise(total = n(), score = mean(score,na.rm = T)) %>% 
  arrange(desc(score)) %>% top_n(15,score)
studio <- tidy_anime[tidy_anime$studio %in% top_studio$studio,]
studio$studio <- factor(studio$studio,levels = rev(top_studio$studio))
low_bandai <- studio %>% filter(studio=="Bandai Namco Pictures" & score == min(score))
low_bandai <- low_bandai[1,]
low_bandai$name <- "MILPOM Pilot"
low_geno <- studio %>% filter(studio=="Geno Studio") %>% arrange(score) 
low_geno <- low_geno[1,]
low_marvy <- studio %>% filter(studio=="Marvy Jack") %>% arrange(score) 
low_marvy <- low_marvy[1,]
low_minami <- studio %>% filter(studio=="Minami Machi Bugyousho") %>% arrange(score) 
low_minami <- low_minami[1,]

png("anime1.png", width = 3960, height = 2160, units = 'px', res = 300)
studio %>% ggplot()+
  geom_boxplot(aes(studio,score,fill=studio),color="pink",show.legend = F)+
  geom_text_repel(data=low_bandai,aes(studio,score,label=paste(name,", eps.",episodes,sep = "")),
            color="lightyellow",nudge_x = 1,hjust="left")+
  geom_text_repel(data=low_geno,aes(studio,score,label=paste(name,", eps.",episodes,sep = "")),
                  color="lightyellow",nudge_x = 1,hjust="right")+
  geom_text_repel(data=low_marvy,aes(studio,score,label=paste(name,", eps.",episodes,sep = "")),
                  color="lightyellow",nudge_x = -1,hjust="right")+
  geom_text_repel(data=low_minami,aes(studio,score,label=paste(name,", eps.",episodes,sep = "")),
                  color="lightyellow",nudge_x = 1,hjust="right")+
  coord_flip()+
  labs(title = "What is studio with high number of good anime score?",
       subtitle = "Bandai Namco has a really wide spread of anime score",
       x = "Studio", y="Score")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"))
dev.off()

#What is the most lucrative genre & their rating?
top_genre <- tidy_anime %>% group_by(genre) %>% 
  summarise(score = mean(score,na.rm = T), total = n()) %>% 
  arrange(desc(score)) %>% top_n(20,score)
gen_rating <- tidy_anime %>% group_by(genre,rating) %>% 
  summarise(score = mean(score,na.rm = T), total = n())
gen_rating <- gen_rating[gen_rating$genre %in% top_genre$genre,]
gen_rating$genre <- factor(gen_rating$genre,levels = rev(top_genre$genre))

png("anime2.png", width = 3960, height = 2160, units = 'px', res = 300)
gen_rating %>% ggplot()+
  geom_tile(aes(rating,genre,fill=score),color="#1D2024")+
  scale_fill_viridis_c(option = "A")+
  labs(title = "What is the most lucrative genre & their rating?",
       subtitle = "Thriller anime has a great score especially for those with PG-13 rating",
       x = "Rating", y="Genre")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"))
dev.off()

#Top anime and their unique word plot
anime <- studio %>% group_by(name,synopsis) %>% 
  summarise(total = n(),score = mean(score,na.rm = T)) %>% arrange(desc(score))
anime <- anime[1:6,]
colnames(anime) <- c("name","text","total","score")
anime$text <- as.character(anime$text)
custom_stop <- tibble(word = c("season","final","arc"))

text_anime <- anime %>% unnest_tokens(word,text) %>% count(name,word,sort = T) %>% 
  bind_tf_idf(word,name,n) %>% arrange(desc(tf_idf)) %>% anti_join(stop_words) 

png("anime3.png", width = 3960, height = 2160, units = 'px', res = 300)
text_anime %>% 
  mutate(word = factor(word,levels = rev(unique(word)))) %>% 
  group_by(name) %>% top_n(5) %>% ungroup() %>% 
  ggplot(aes(word,tf_idf,fill=name))+
  geom_col(show.legend = F)+
  coord_flip()+
  facet_wrap(facets = ~name,nrow = 2,scales = "free_y")+
  labs(title = "What is the unique word in synopsis in Top 6 anime?",
       subtitle = "Each word in each anime reflect what their unique character and plots are compared to other anime
Both Gintama: Shirogane no Tamashii-hen only describe that they are the first and the second season of the final arc of Gintama",
       x = "Word", y = "Term Frequency - Inverse Document Frequency")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"))
dev.off()

#What is the best season to launch anime?
tidy_anime <- tidy_anime %>% separate(premiered,c("season","year"))
anime_season <- tidy_anime %>% filter (is.na(season)==F & is.na(year)==F)%>% 
  group_by(season,year) %>% 
  summarise(score = mean(score,na.rm = T), popularity = mean(popularity,na.rm = T))
anime_season$season <- factor(anime_season$season,levels = c("Spring","Summer","Fall","Winter"))

popular <- anime_season %>% ggplot()+
  geom_tile(aes(year,season,fill=popularity),color="#1D2024")+
  scale_fill_gradient(low = "navy",high = "lightyellow")+
  scale_x_discrete(breaks = seq(1960,2020,2))+
  labs(title = "Older anime is more popular than the new one",
       subtitle = "Popular: how many members/users have the respective anime in their list")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 45),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"))

score <- anime_season %>% ggplot()+
  geom_tile(aes(year,season,fill=score),color="#1D2024")+
  scale_fill_gradient(low = "red",high = "lightyellow")+
  scale_x_discrete(breaks = seq(1960,2020,2))+
  labs(title = "Although newer anime has better score, it doesn't translate into popularity")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 45),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"))

png("anime4.png", width = 3960, height = 2160, units = 'px', res = 300)
plot_grid(popular,score,nrow = 2)
dev.off()
