library(tidyverse)
library(ggrepel)
library(openxlsx)
library(ggridges)
setwd(dir = "D:/R/tidytuesday/ramen")
#data preparation
ramen <- read.xlsx("D:/R/datasets/Ramen.xlsx")

##clear incomplete data
data <- na.omit(ramen)

#categorized data by brand
top_brand <- data %>% group_by(brand) %>% 
  summarise(min = min(stars, na.rm = T),max = max(stars, na.rm = T),
            mean = mean(stars, na.rm = T), sd = sd(stars, na.rm = T),
            se = sd/sqrt(n()), CI = se*1.96,
            variety = length(unique(variety)),jumlah = n())
top_brand <- top_brand %>% filter(jumlah > 30)
top_brand$brand <- factor(top_brand$brand,levels = top_brand$brand)
top_brand <- top_brand %>% mutate(lower = mean-CI, upper = mean+CI,
                                  diff = mean-mean(top_brand$mean))

#plot
##top ramen by brand with confidence interval 95%
ggplot(top_brand)+theme_light()+
  geom_errorbar(aes(ymin=lower,ymax=upper,x=brand),width= 0.2)+
  geom_hline(aes(yintercept = mean(top_brand$mean)), linetype = "dashed")+
  geom_point(aes(y=mean,x=brand,color = variety), size = 2)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"))+
  labs(title = "Top Ramen Rating by Brand",
       y = "Rating", x = "Brand",
       subtitle = "Ramen with more than 30 reviews",
       caption = "data: theramenrater.com",
       color="variety")+scale_color_viridis_c()+coord_flip()+
  geom_label_repel(aes(brand,mean, label = round(mean,2)), size = 3,
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.5, "lines"),
                   fill = "white")
png("ramen14.png", width = 3840, height = 2160, units = 'px', res = 300)
dev.off()

##distance from overall rating by brand
ggplot(top_brand)+theme_light()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"))+
  geom_segment(aes(x=brand,xend=brand,y=0,yend=diff))+
  geom_point(aes(y=diff,x=brand,color=variety), size =2)+
  geom_hline(aes(yintercept=0), linetype = "dashed")+
  labs(title = "Ramen Rating compared to mean",
       x = "Brand", y= "Distance from mean",
       caption = "mean: 3.68")+
  scale_color_viridis_c()+coord_flip()
png("ramen2.png", width = 1920, height = 1080, units = 'px', res = 300)
dev.off()

#categorized ramen by country
top_country <- ramen_ratings %>% group_by(country) %>% 
                summarise(rating = mean(stars,na.rm = T), jumlah = n(),)
top_country <- top_country[order(top_country$rating,decreasing = T),]
top_country <- top_country %>% filter(jumlah > 30)
top_country <- top_country[order(top_country$rating),]
top_country$country <- factor(top_country$country,
                    levels = top_country$country)

#plot
ggplot(top_country, aes(x= rating,y=country))+theme_light()+
  geom_segment(mapping = aes(x = 1, xend= rating, y=country,yend=country))+
  geom_point(aes(color = jumlah), size = 2)+
  theme(panel.grid.major.y = element_blank())+
  labs(title = "Ramen Rating by Country",
       subtitle = "For countries with more than 30 reviews",
       color="reviews",
       caption = "data: theramenrater.com")+
  scale_color_viridis_c()
png("ramen3.png", width = 16, height = 9, units = 'cm', res = 2040)
dev.off()

#categorized by package style
mantap <- ramen_ratings %>% group_by(style) %>% 
  summarise(rating = mean(stars, na.rm = T), jumlah = n())
mantap <- mantap[order(mantap$rating),]
mantap <- na.omit(mantap)
mantap$style <- factor(mantap$style,levels = mantap$style)

#plot
ggplot(mantap, aes(style,rating))+theme_light()+
  geom_col(aes(fill=jumlah))+
  labs(title = "Ramen Rating by Package Style", fill = "reviews",
       caption = "data: theramenrater.com")+
  theme(panel.grid.major.y = element_blank())+
  scale_fill_viridis_c()+
  scale_color_brewer(type = "seq",palette = "Spectral",guide=F)+
  coord_flip()

png("ramen4.png", width = 16, height = 9, units = 'cm', res = 2040)
dev.off()

#Distribution of rating on top country
lebih <- data %>% group_by(country,brand, variety, style) %>% 
  summarise(jumlah = n(), rating = mean(stars))
lebih <- lebih[lebih$country %in% top_country$country,]
lebih$country <- factor(lebih$country, levels = top_country$country)

#plot
ggplot(lebih, aes(country,rating))+
  geom_boxplot(fill = "pink",alpha=1/2,outlier.color = "navy")+
  coord_flip()+theme_light()+theme(panel.grid.major.x = element_blank())+
  labs(title = "Distribution of Rating in Top Country",
       y="Rating",x="Country", caption = "data: theramenrater.com")
  
png("ramen5.png", width = 2048, height = 1536, units = "px",res = 300)
dev.off()

#Distribution of rating on brand
lebih_brand <- data %>% group_by(country,brand, variety, style) %>% 
  summarise(jumlah = n(), rating = mean(stars))
lebih_brand <- lebih_brand[lebih_brand$brand %in% top_brand$brand,]
lebih_brand$brand <- factor(lebih_brand$brand, levels = top_brand$brand)
lebih_brand$style <- factor(lebih_brand$style,levels = mantap$style)

#plot distribution of rating in style
ggplot(lebih_brand, aes(style,rating))+
  geom_boxplot(fill = "violet",alpha=1/2,outlier.color = "navy")+
  coord_flip()+theme_light()+theme(panel.grid.major.x = element_blank())+
  labs(title = "Distribution of Rating in Top Style",
       y="Rating",x="Brand", caption = "data: theramenrater.com")

png("ramen6.png", width = 16, height = 9, units = 'cm', res = 2040)
dev.off()

#plot distribution of rating in brand
ggplot(lebih_brand, aes(brand,rating))+
  geom_boxplot(fill = "orange",alpha=1/2,outlier.color = "navy")+
  coord_flip()+theme_light()+theme(panel.grid.major.x = element_blank())+
  labs(title = "Distribution of Rating in Top Brand",
       y="Rating",x="Brand", caption = "data: theramenrater.com")
png("ramen7.png", width = 2048, height = 1536, units = "px",res = 300)
dev.off()

#with style as facet
ggplot(lebih_brand, aes(brand,rating))+
  geom_boxplot(fill = "blue",alpha=1/2,outlier.color = "navy")+
  coord_flip()+theme_light()+
  labs(title = "Distribution of Rating in Top Brand",
       y="Rating",x="Brand", caption = "data: theramenrater.com",
       subtitle = "grouped based on package style")+
  facet_wrap(facets = lebih_brand$style)+
  theme(panel.grid.major.x = element_blank())

png("ramen8.png", width = 3072, height = 2304, units = "px",res = 300)
dev.off()

#rating as size and color scale
brando <- data %>% group_by(brand,style) %>% 
  summarise(rating = mean(stars), jumlah = n())
brando <- brando[brando$brand %in% top_brand$brand,]
brando$brand <- factor(brando$brand,top_brand$brand)

ggplot(brando, aes(brand,style, size = rating, color = rating))+ 
  geom_point()+ scale_color_viridis_c()+theme_light()+
  coord_flip()+
  labs(title = "Rating of Top Brand by Style",
       y="Style",x="Brand", caption = "data: theramenrater.com",
       subtitle = "Bigger and brighter point indicate higher rating")
png("ramen9.png", width = 2048, height = 1536, units = "px",res = 300)
dev.off()

#Proud Indonesia
Indo <- data[which(data$country=="Indonesia"),]
Indo <- Indo %>% group_by(brand,style) %>% 
  summarise(jumlah = n(), rating = mean(stars))
Indo[which(Indo$brand=="Mi Sedaap"),1] <- "Mie Sedaap"
Indo <- Indo %>% group_by(brand,style) %>% 
  summarise(jumlah = n(), rating = mean(rating))
Indo <- Indo[order(Indo$brand),]
Indo$brand <- factor(Indo$brand,unique(Indo$brand))

##Indo rating as scale
ggplot(Indo, aes(brand,style, size = rating, color = rating))+ 
  geom_point()+ scale_color_viridis_c()+theme_light()+
  coord_flip()+
  labs(title = "Indonesian Ramen",
       y="Style",x="Brand", caption = "data: theramenrater.com",
       subtitle = "Bigger and brighter point indicate higher rating")
png("ramen10.png", width = 2048, height = 2048, units = "px",res = 300)
dev.off()

#Indo boxplot
ggplot(Indo, aes(brand, rating))+theme_light()+
  geom_hline(aes(yintercept = mean(rating)), linetype = "dashed")+
  geom_boxplot(alpha = 1/2, fill = "green")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  labs(title = "Indonesian Ramen",
       y="Rating",x="Brand", caption = "data: theramenrater.com"
       )+
  coord_flip()
png("ramen11.png", width = 2048, height = 1536, units = "px",res = 300)
dev.off()

#Indo mean rating only
Indo <- data[which(data$country=="Indonesia"),]
Indo[which(Indo$brand=="Mi Sedaap"),2] <- "Mie Sedaap"
Indo <- Indo %>% group_by(brand) %>% 
  summarise(jumlah = n(), variety = length(unique(variety)),
            rating = mean(stars), CI = sd(stars)/sqrt(n()))
Indo <- Indo %>% mutate(lower = rating-CI,upper = rating+CI)
Indo <- Indo %>% mutate(diff = rating-mean(Indo$rating))
Indo <- Indo %>% mutate(kelas = 0)
Indo[which(Indo$diff>0),"kelas"] <- "Above Average"
Indo[which(Indo$diff<0),"kelas"] <- "Below Average"
Indo <- Indo[order(Indo$rating),]
Indo$brand <- factor(Indo$brand,Indo$brand)

#check the difference
ggplot(Indo)+theme_light()+
  geom_hline(aes(yintercept = mean(Indo$rating)),linetype = "dashed")+
  geom_errorbar(aes(ymin=lower,ymax=upper,x=brand),width=0.2)+
  geom_point(aes(brand,rating,color = variety))+coord_flip()+
  labs(title = "Is Indomie the best in Indonesia?",
       subtitle = "Indomie has the most variety of ramen",
       y="Rating",x="Brand", 
       caption = "vertical line indicate overall rating | data: theramenrater.com")
png("ramen12.png", width = 3072, height = 2304, units = "px",res = 300)
dev.off()

##second plot
ggplot(Indo)+theme_light()+
  geom_col(aes(brand,y=diff,fill = kelas))+coord_flip()+
  labs(title = "Is Indomie the best in Indonesia?",
       subtitle = "Indomie has the most variety of ramen",
       y="Distance from Mean",x="Brand", 
       fill = paste("Mean:",round(mean(Indo$rating),2)),
       caption = "data: theramenrater.com")
png("ramen13.png", width = 2048, height = 1536, units = "px",res = 300)
dev.off()


#ggridges
png("ramen15.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(lebih_brand, aes(rating,brand,fill=brand))+
  geom_density_ridges(alpha=1/2)+
  scale_fill_discrete(guide=F)+
  theme_ridges()
dev.off()  
