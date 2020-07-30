
# ---------- package -----------------
library(tidyverse)
library(palmerpenguins)
library(ggridges)
library(ggthemes)
library(magick)
library(scales)
library(extrafont)
library(patchwork)

# First Plot -----------------------

df_viz <- penguins %>% 
  select(species, bill_length_mm:flipper_length_mm,sex) %>% 
  pivot_longer(bill_length_mm:flipper_length_mm) %>% 
  drop_na(sex) %>% 
  mutate(
    name = name %>% 
      str_replace_all("_", " ") %>% 
      str_remove_all(pattern = " mm") %>% 
      str_to_title() %>% 
      paste("(mm)")
  )  

legend_box <- data.frame(
  name = rep("Flipper Length (mm)", 1),
  species = unique(df_viz$species) %>% 
    sort() %>% 
    as.factor(),
  freq = table(penguins$species) %>% as.numeric(),
  x = c(185, 210, 235),
  y = c(0.725, 0.725, 0.725),
  xmin = c(170, 195, 220),
  ymin = c(0.5, 0.5, 0.5),
  ymax = c(0.6, 0.6, 0.6)
) %>% 
  mutate(
    freq_percent = percent(freq/sum(freq), accuracy = 0.1),
    freq_scaled = freq/10,
    xmax = xmin + freq_scaled
  )


## Build Plot 

p1 <- penguins %>% 
  ggplot(aes(bill_length_mm, fill = species)) +
  geom_density(alpha = 0.8, color = "transparent") +
  scale_fill_manual(values = c("#f17339", "#cfc9ba", "#181217"))+
  theme_pander() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(family = "Forte", size = 20),
        plot.subtitle = element_text(family = "Segoe UI Semibold", size = 12, hjust = 1)
        ) +
  labs(
    title = "Palmer Penguins",
    subtitle = "Bill Length (mm)")

p2 <- penguins %>% 
  ggplot(aes(bill_depth_mm, fill = species)) +
  geom_density(alpha = 0.8, color = "transparent") +
  scale_fill_manual(values = c("#f17339", "#cfc9ba", "#181217"))+
  theme_pander() +
  labs(subtitle = "Bill Depth (mm)") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(family = "Segoe UI Semibold", size = 12, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

p3 <- penguins %>% 
  ggplot(aes(flipper_length_mm, fill = species)) +
  geom_density(alpha = 0.8, color = "transparent") +
  scale_fill_manual(values = c("#f17339", "#cfc9ba", "#181217"))+
  theme_pander() +
  labs(subtitle = "Flipper Length (mm)") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(family = "Segoe UI Semibold", size = 12, hjust = 1)
  )

p4 <- legend_box %>% 
  ggplot(aes(x=x,y=y,fill = species)) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = species), 
            data = legend_box, inherit.aes = F, alpha = 0.8) +
  geom_text(aes(x = x, y = y, label = species), hjust = 1, 
            data = legend_box, family = "Segoe UI Semibold") +
  geom_text(aes(x = xmax, y = y-0.35, label = freq_percent), 
            family = "Segoe UI Semilight") +
  theme_void() +
  labs(subtitle = "Observed Species",
       caption = "@Argaadya") +
  scale_y_continuous(limits = c(0,1)) +
  scale_fill_manual(values = c("#f17339", "#cfc9ba", "#181217"))+
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.subtitle = element_text(family = "Segoe UI Semibold", size = 12, hjust = 1)
    )

## Use Patchwork 

p <- p1/p2/p3/p4

## Use Magick

p_magick <- image_graph(width = 1980, height = 1980, res = 300)
p
dev.off()

### Import Image
peng <- image_read("slide.png") 
gentoo <- image_read("gentoo_logo.png")
chinstrap <- image_read("chinstrap_logo.png")
adelie <- image_read("adelie_logo.png")

## Adjust image size
peng <- image_scale(peng, geometry = geometry_size_percent(width = 50,height = 50))
peng2 <- image_flop(peng)
gentoo <-   image_scale(gentoo, geometry = geometry_size_percent(width = 30,height = 30))
adelie <-   image_scale(adelie, geometry = geometry_size_percent(width = 13,height = 13))
chinstrap <-   image_scale(chinstrap, geometry = geometry_size_percent(width = 28,height = 28))

## Put image into plot
### Sliding Penguins
composite_image <- image_composite(p_magick, composite_image = peng, 
                                   offset = geometry_point(x = 700, y =  275))  

composite_image <- image_composite(composite_image, composite_image = peng2, 
                                   offset = geometry_point(x = 1000, y =  775))  

composite_image <- image_composite(composite_image, composite_image = peng, 
                                   offset = geometry_point(x = 1000, y =  1250))  

### Legend Image
composite_image <- image_composite(composite_image, composite_image = chinstrap, 
                                   offset = geometry_point(x = 800, y =  1675))  

composite_image <- image_composite(composite_image, composite_image = gentoo, 
                                   offset = geometry_point(x = 1450, y =  1675))  

composite_image <- image_composite(composite_image, composite_image = adelie, 
                                   offset = geometry_point(x = 150, y =  1675))  

### Save Image

image_write(composite_image, "penguin1.png")


# Second Plot --------------------------------

library(tidymodels)
library(rpart)
library(rpart.plot)
library(ggdendro)

## Split Train-Test
set.seed(123)
index <- initial_split(penguins, prop = 0.8, strata = "species")
data_train <- training(index)
data_testing <- testing(index)

## Model Decision Tree
model_tree <- rpart(species ~., 
                    data = data_train)

## Predict on Testing Set
pred_test <- predict(model_tree, newdata = data_testing, type = "class")

## Get Model Accuracy
model_accuracy <- accuracy_vec(pred_test, data_testing$species)

## Get Model Rules
model_rules <- rpart.rules(model_tree, cover = T, nn = T)

model_rules

## Create Plot
p <- penguins %>% 
  ggplot(aes(bill_length_mm, flipper_length_mm)) +
  geom_segment(aes(x = 43, xend = 43, y = 170, yend = 207),
               linetype = "dashed", size = 1, alpha = 0.7) +
  geom_hline(yintercept = 207, linetype = "dashed",
             x = "Bill Lenght (mm)", y = "Flipper Length (mm)",
             size = 1, alpha = 0.7
             ) +
  geom_point(alpha = 0.6, size = 2, aes(color = species)) +
  geom_text(aes(x = 33, y = 225, label = "Gentoo"), size = unit(8, units = "pt"),
            family = "Segoe UI Semilight", color = "#057574") +
  geom_text(aes(x = 33, y = 172, label = "Adelie"), inherit.aes = F, 
            family = "Segoe UI Semilight", size = unit(8, units = "pt"),
            color = "#f17339") +
  geom_text(aes(x = 60, y = 172, label = "Chinstrap"), inherit.aes = F, hjust = 1,
            family = "Segoe UI Semilight", size = unit(8, units = "pt"),
            color = "#9000ce") +
  geom_text(aes(x = 30, y = 210, label = "Flipper Length >= 207"), size = 4,
            family = "Segoe UI Semilight", hjust = 0) +
  geom_text(aes(x = 43.5, y = 172, label = "Bill Length >= 43"), size = 4,
            family = "Segoe UI Semilight", hjust = 0) +
  labs(title = "Decision Tree on Penguin Classification",
       subtitle = paste0("Accuracy : ", percent(model_accuracy, accuracy = 0.1)),
       caption = "@Argaadya",
       x = "Bill Length (mm)", 
       y = "Flipper Length (mm)") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(family = "Segoe UI Semibold", size = 18),
        plot.subtitle = element_text(family = "Segoe UI Semibold", size = 12),
        ) +
  scale_color_manual(values = c("#f17339", "#9000ce", "#057574"))

## Convert to Image

p_magick <- image_graph(width = 2970, height = 1980, res = 300)
p
dev.off()

## Import Logo
gentoo <- image_read("gentoo_logo.png")
chinstrap <- image_read("chinstrap_logo.png")
adelie <- image_read("adelie_logo.png")

## Adjust image size
gentoo <-   image_scale(gentoo, geometry = geometry_size_percent(width = 40,height = 40))
adelie <-   image_scale(adelie, geometry = geometry_size_percent(width = 18,height = 18))
chinstrap <-   image_scale(chinstrap, geometry = geometry_size_percent(width = 40,height = 40))

## Put image into plot
composite_image <- image_composite(p_magick, composite_image = chinstrap, 
                                   offset = geometry_point(x = 2200, y =  1600)) %>% 
  image_composite(composite_image = adelie, 
                  offset = geometry_point(x = 200, y =  1600))  %>% 
  image_composite(composite_image = gentoo, 
                  offset = geometry_point(x = 200, y =  375))  

## Save Image
image_write(composite_image, "penguin2.png")

