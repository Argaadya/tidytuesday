#Load the Data
ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")


#Load the Package
library(tidyverse)
library(ggridges)
library(fishualize)

# Inspect the Data
glimpse(ipf_lifts)

# Data Preprocessing
power_clean <- ipf_lifts %>% 
  mutate_at(.vars = c("age_class","weight_class_kg","federation","equipment"),
            .funs = as.factor) %>% 
    pivot_longer(cols = c(best3squat_kg,best3deadlift_kg,best3bench_kg),
                 names_to = "best3",
                 values_to = "value") %>% 
  drop_na(age_class) %>% 
  filter(value > 0) %>% 
  mutate(best3 = case_when(best3 == "best3bench_kg" ~"Best 3 Bench",
                           best3 == "best3deadlift_kg" ~"Best 3 Deadlift",
                           best3 == "best3squat_kg" ~"Best 3 Squat")) %>% 
  mutate(age_class = if_else(age_class == "80-999", "80+", as.character(age_class)),
         sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age_class = factor(age_class, 
                            levels = c("5-12","13-15","16-17","18-19","20-23","24-34","35-39",
                                       "40-44","45-49","50-54","55-59","60-64","65-69",
                                       "70-74","75-79","80+"))) %>% 
  filter(age_class != "5-12")

# Data for Annotation
power_mean <- ipf_lifts %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  group_by(name, date, sex,age_class) %>% 
  summarise(bench = mean(best3bench_kg),
            deadlift = mean(best3deadlift_kg),
            squat = mean(best3squat_kg)) %>% 
  ungroup() %>% na.omit() %>% 
  group_by(sex)

maxbench <- power_mean %>% filter(bench == max(bench))
maxdeadlift <- power_mean %>% filter(deadlift == max(deadlift))
maxsquat <- power_mean %>% filter(squat == max(squat))
power_max <- rbind(maxbench,maxdeadlift,maxsquat)
power_max <- power_max %>%
  group_by(sex) %>% 
  mutate(best3 = case_when(bench == max(bench) ~ "Best 3 Bench",
                           deadlift == max(deadlift) ~ "Best 3 Deadlift",
                           squat == max(squat) ~ "Best 3 Squat"))
maxcol <- apply(power_max[, 5:7], 1, max)
power_max$best <- maxcol

# Create the Plot
png(filename = "power1.png", height = 2160, width = 3960, res = 300)
ggplot(power_clean, aes(value, age_class, fill = age_class))+
  geom_density_ridges(alpha=.8, color = "gray30")+
  facet_grid(cols = vars(best3), rows = vars(sex))+
  scale_fill_fish_d(option = "Acanthurus_sohal", guide = F)+
  labs(title = "Power Lift Performance Accross Ages and Sex",
       subtitle = "Best record of each category is shown on the right side",
       x = "Value (in Kg)", y = "", caption = "@Argaadya1 | #TidyTuesday")+
  scale_y_discrete(expand = c(0,0))+
  scale_x_continuous(breaks = seq(0,600,100), expand = c(0,0))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow", size = 10),
        strip.background = element_rect(fill = "maroon"),
        strip.text = element_text(colour = "snow", size = 12),
        plot.title = element_text(size=18, colour = "lightyellow"))+
  geom_label(data = power_max, aes(500, 14, label = name), hjust=1, fill = "white", size = 4.5)+
  geom_text(data = power_max, aes(500, 10, label = maxcol), hjust = 1, size = 8, color = "gray80")+
  geom_text(data = power_max, aes(500, 12, label = date), hjust = 1, size = 6, color = "gray80")
dev.off()


# Second Plot
png(filename = "power2.png", height = 2160, width = 3960, res = 300)
ggplot(power_clean, aes(equipment, value, fill= equipment))+
  geom_boxplot(color = "lightyellow", show.legend = F)+
  scale_fill_fish_d(option = "Chromis_vanderbilti")+
  facet_grid(cols = vars(best3), rows = vars(sex), scales = "free_y")+
  labs(y = "Value", x = "Equipment",
       title = "Distribution of Lift Performance Based on Equipment Used",
       subtitle = "",
       caption = "@Argaadya1 | #TidyTuesday")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow", size = 10),
        strip.background = element_rect(fill = "maroon"),
        strip.text = element_text(colour = "snow", size = 12),
        plot.title = element_text(size=18, colour = "lightyellow"))
dev.off()

