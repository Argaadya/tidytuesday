library(tidyverse)
library(ggrepel)
library(lubridate)
library(fishualize)
library(treemap)
library(ggalluvial)

# IMPORT DATA

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

# INSPECT
glimpse(horror_movies)


## DATA PREPROCESSING
df <- horror_movies %>% 
  mutate(language = word(str_replace(language,"[|]"," "), 1),
         genres = str_remove(word(genres, 1L),"\\|")) %>%
  mutate_at(.vars = vars(title,genres,language,filming_locations,release_country,movie_rating), 
            .funs = as.factor) %>% 
  mutate(release_date = dmy(release_date),
         month = month(release_date,label = T, abbr = T),
         year = year(release_date),
         movie_run_time = as.numeric(str_remove(movie_run_time," min")),
         budget = as.numeric(str_remove_all(str_extract(budget, "\\(?[0-9,.]+\\)?"),","))) 

glimpse(df)
unique(df$language)

## HEATMAP -----------------
png("horror1.png", width = 3960, height = 2160, units = 'px', res = 300)
df %>% 
  group_by(month,year) %>% 
  summarise(run_time = mean(movie_run_time,na.rm = T), 
            review = mean(review_rating,na.rm = T)) %>% 
  drop_na(month) %>% 
ggplot(aes(month,year, fill = review))+
  geom_tile(color = "#1D2024")+
  geom_point(aes(month,year,size=run_time,color=run_time), shape = "square")+
  scale_fill_gradient(low = "black", high = "hotpink")+
  scale_size_continuous(range = c(3,20),guide = F)+
  scale_color_gradient(low = "gray30",high = "greenyellow")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(breaks = seq(2010,2020,1),expand = c(0,0))+
  labs(title = "Horror movies rating across month and year",
       fill = "Rating", x = "Month",
       color = "Run time", y = "Year",
       caption = "@Argaadya1")+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_blank(),
        text = element_text(colour = "lightyellow"),
        axis.text = element_text(colour = "goldenrod"),
        plot.title = element_text(size = 20))
dev.off()

# VIOLIN CHART -----------------
png("horror2.png", width = 3960, height = 2160, units = 'px', res = 300)
df %>% 
  filter(!(movie_rating %in% c("E","TV-PG","X","NC-17"))) %>%
  drop_na(movie_rating) %>% 
  ggplot(aes(movie_rating, review_rating, fill = movie_rating, color = movie_rating))+
  geom_violin(show.legend = F)+
  geom_jitter(alpha=0.5, show.legend = F)+
  scale_fill_fish_d("Centropyge_loricula")+
  labs(title = "Horror Movies Reviews Across Age Rating",
       x = "Rating", y = "Reviews")+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_blank(),
        text = element_text(colour = "lightyellow"),
        axis.text = element_text(colour = "goldenrod"),
        plot.title = element_text(size = 20),
        panel.grid = element_blank())
dev.off()

# SANKEY DIAGRAM ------------------------
png("horror3.png", width = 3960, height = 2160, units = 'px', res = 300)
df %>% 
  drop_na(movie_rating,genres,budget) %>%
  mutate(budget = factor(case_when(budget <= 30000 ~ "Low Budget",
                            budget > 30000 & budget <= 85000 ~ "Mid Budget",
                            budget > 85000 ~ "High Budget"),
                         levels = c("Low Budget", "Mid Budget", "High Budget"))) %>% 
  group_by(movie_rating,genres,budget,year) %>% 
  summarise(frequency = n()) %>%
  filter(frequency>3) %>% 
  ggplot(aes(axis1 = year, axis2 = genres, axis3 = movie_rating, y = frequency))+
  geom_alluvium(aes(fill = budget),alpha=0.6)+
  geom_stratum()+
  geom_text(stat = "stratum", label.strata = T)+
  theme_minimal()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_fish_d("Centropyge_loricula",direction = -1)+
  theme(axis.text.x = element_blank(),
        panel.grid = element_blank())+
  labs(title = "Sankey Diagram",
       caption = "@Argadya1")
dev.off()  
