library(tidyverse)
library(scales)
library(extrafont)
library(cowplot)
library(ggrepel)
options(scipen = 10)

setwd("D:/R/tidytuesday/moore_law/")
#DATA
cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")
write.csv(cpu,"D:/R/datasets/moore/cpu.csv")
cpu <- read.csv("D:/R/datasets/moore/cpu.csv")

gpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv")
write.csv(gpu,"D:/R/datasets/moore/gpu.csv")
gpu <- read.csv("D:/R/datasets/moore/gpu.csv")

ram <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv")
write.csv(ram,"D:/R/datasets/moore/ram.csv")
ram <- read.csv("D:/R/datasets/moore/ram.csv")

#Moore's Law in CPU --------------------------------------------------------------------
cpu_mod <- lm(log2(transistor_count) ~ date_of_introduction,data = cpu)

png("moore1.png", width = 3960, height = 2160, units = 'px', res = 300)
cpu %>% mutate(transistor_count = log2(transistor_count)) %>% 
  filter(is.na(transistor_count)==F) %>% 
  ggplot()+
  geom_line(aes(date_of_introduction,predict(cpu_mod)),color="white")+
  geom_segment(aes(x=date_of_introduction,xend=date_of_introduction,
                   y=transistor_count,yend=predict(cpu_mod)),
               color="gray80",linetype="dashed")+
  geom_point(aes(date_of_introduction,transistor_count, size = transistor_count, color=process),
             alpha=.6)+
  geom_text(aes(1970,30,label="With the size of transistors being smaller,
the CPU could fit more and more transistors inside",family="mono"),
            hjust="left",color="orange",size=4.5)+
  scale_y_continuous(labels = number_format(big.mark = ","))+
  scale_x_continuous(breaks = seq(1970,2020,5))+
  scale_color_gradient(low = "maroon",high = "orange2")+
  scale_size_continuous(guide=F)+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        plot.title = element_text(size=18),
        plot.caption = element_text(size=16),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow",size=10),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "gray28"),
        strip.text = element_text(colour = "snow",size=12))+
  labs(title = "Moore's Law on CPU",
       color = "Size (nanometers)",
       x = "Year", y = "Log2 (Transistor Count)",
       subtitle = "Moore's Law: The observation that the number of transistors in a dense integrated circuit doubles approximately every two years.")
dev.off()

#Moore's Law in GPU --------------------------------------------------------------------
gpu_mod <- lm(log2(transistor_count) ~ date_of_introduction,data = gpu)

png("moore2.png", width = 3960, height = 2160, units = 'px', res = 300)
gpu %>% mutate(transistor_count = log2(transistor_count)) %>% 
  filter(is.na(date_of_introduction)==F) %>% 
  ggplot()+
  geom_line(aes(date_of_introduction,predict(gpu_mod)),color="white")+
  geom_segment(aes(x=date_of_introduction,xend=date_of_introduction,
                   y=transistor_count,yend=predict(gpu_mod)),
               color="gray80",linetype="dashed")+
  geom_point(aes(date_of_introduction,transistor_count, size = transistor_count,color=process),
             alpha=.6)+
  geom_text(aes(1980,30,label="With the size of transistors being smaller,
the GPU could fit more and more transistors inside",family="mono"),
            hjust="left",color="orange",size=4.5)+
  scale_y_continuous(labels = number_format(big.mark = ","))+
  scale_x_continuous(breaks = seq(1970,2020,5))+
  scale_color_gradient(low = "maroon",high = "orange2")+
  scale_size_continuous(guide=F)+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        plot.title = element_text(size=18),
        plot.caption = element_text(size=16),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow",size=10),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "gray28"),
        strip.text = element_text(colour = "snow",size=12))+
  labs(title = "Moore's Law on GPU",
       color = "Size (nanometers)",
       x = "Year", y = "Log2 (Transistor Count)",
       subtitle = "Moore's Law: The observation that the number of transistors in a dense integrated circuit doubles approximately every two years.")
dev.off()

#Moore's Law on RAM -------------------------------
ram_mod <- lm(log2(transistor_count) ~ date_of_introduction,data = ram)

png("moore3.png", width = 3960, height = 2160, units = 'px', res = 300)
ram %>% mutate(transistor_count = log2(transistor_count)) %>% 
  filter(is.na(transistor_count)==F) %>% 
  ggplot()+
  geom_line(aes(date_of_introduction,predict(ram_mod)),color="white")+
  geom_segment(aes(x=date_of_introduction,xend=date_of_introduction,
                   y=transistor_count,yend=predict(ram_mod)),
               color="gray80",linetype="dashed")+
  geom_point(aes(date_of_introduction,transistor_count, size = transistor_count,color=process),
             alpha=.6)+
  geom_text(aes(1960,30,label="With the size of transistors being smaller,
the ram could fit more and more transistors inside",family="mono"),
            hjust="left",color="orange",size=4.5)+
  scale_y_continuous(labels = number_format(big.mark = ","))+
  scale_x_continuous(breaks = seq(1960,2020,5))+
  scale_color_gradient(low = "maroon",high = "orange2")+
  scale_size_continuous(guide=F)+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        plot.title = element_text(size=18),
        plot.caption = element_text(size=16),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow",size=10),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "gray28"),
        strip.text = element_text(colour = "snow",size=12))+
  labs(title = "Moore's Law on RAM",
       color = "Size (nanometers)",
       x = "Year", y = "Log2 (Transistor Count)",
       subtitle = "Moore's Law: The observation that the number of transistors in a dense integrated circuit doubles approximately every two years.")
dev.off()

#Combine 3 plots -----------------------------------------------------
plot_cpu <- cpu %>% mutate(transistor_count = log2(transistor_count)) %>% 
  filter(is.na(transistor_count)==F) %>% 
  ggplot()+
  geom_line(aes(date_of_introduction,predict(cpu_mod)),color="white")+
  geom_text(aes(1990,5,label="CPU",family="mono"),size=36,hjust="center")+
  geom_segment(aes(x=date_of_introduction,xend=date_of_introduction,
                   y=transistor_count,yend=predict(cpu_mod)),
               color="gray80",linetype="dashed")+
  geom_point(aes(date_of_introduction,transistor_count, size = transistor_count, 
                 color=transistor_count),
             alpha=.5,show.legend = F)+
  scale_y_continuous(labels = number_format(big.mark = ","),limits = c(0,40))+
  scale_x_continuous(breaks = seq(1970,2020,10))+
  scale_color_gradient(low = "maroon",high = "orange2")+
  scale_size_continuous(guide=F)+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        plot.title = element_text(size=18),
        plot.caption = element_text(size=16),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow",size=10),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "gray28"),
        strip.text = element_text(colour = "snow",size=12))+
  labs(x = "", y = "Log2 (Transistor Count)")

plot_gpu <- gpu %>% mutate(transistor_count = log2(transistor_count)) %>% 
  filter(is.na(date_of_introduction)==F) %>% 
  ggplot()+
  geom_text(aes(2000,5,label="GPU",family="mono"),size=36,hjust="center")+
  geom_line(aes(date_of_introduction,predict(gpu_mod)),color="white")+
  geom_segment(aes(x=date_of_introduction,xend=date_of_introduction,
                   y=transistor_count,yend=predict(gpu_mod)),
               color="gray80",linetype="dashed")+
  geom_point(aes(date_of_introduction,transistor_count, size = transistor_count,
                 color=transistor_count),
             alpha=.5,show.legend = F)+
  scale_y_continuous(labels = number_format(big.mark = ","),limits = c(0,40))+
  scale_x_continuous(breaks = seq(1970,2020,10))+
  scale_color_gradient(low = "maroon",high = "orange2")+
  scale_size_continuous(guide=F)+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        plot.title = element_text(size=18),
        plot.caption = element_text(size=16),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow",size=10),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "gray28"),
        strip.text = element_text(colour = "snow",size=12))+
  labs(x = "", y = "")

plot_ram <- ram %>% mutate(transistor_count = log2(transistor_count)) %>% 
  filter(is.na(transistor_count)==F) %>% 
  ggplot()+
  geom_text(aes(1990,5,label="RAM",family="mono"),size=36,hjust="center")+
  geom_line(aes(date_of_introduction,predict(ram_mod)),color="white")+
  geom_segment(aes(x=date_of_introduction,xend=date_of_introduction,
                   y=transistor_count,yend=predict(ram_mod)),
               color="gray80",linetype="dashed")+
  geom_point(aes(date_of_introduction,transistor_count, size = transistor_count,
                 color=transistor_count),
             alpha=.5,show.legend = F)+
  scale_y_continuous(labels = number_format(big.mark = ","))+
  scale_x_continuous(breaks = seq(1970,2020,10))+
  scale_color_gradient(low = "maroon",high = "orange2")+
  scale_size_continuous(guide=F)+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        plot.title = element_text(size=18),
        plot.caption = element_text(size=16),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow",size=10),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "gray28"),
        strip.text = element_text(colour = "snow",size=12))+
  labs(x = "", y = "")

png("moore4.png", width = 3960, height = 2160, units = 'px', res = 300)
plot_grid(plot_cpu,plot_gpu,plot_ram,nrow = 1)
dev.off()

#Pattern between number of transistor with computational capacity ---------------------
ram <- ram %>% 
  mutate(capacity = case_when(bit_units == "Bits" ~ capacity_bits*1,
                              bit_units == "kb" ~ capacity_bits*8000,
                              bit_units == "Mb" ~ capacity_bits*8*10^6,
                              bit_units == "Gb" ~ capacity_bits*8*10^9))

cap_mod <- lm(log10(capacity)~log10(transistor_count),data=ram)

#labeling smallest and biggest RAM capacity
one_bit <- ram %>% filter(is.na(capacity)==F) %>% 
  filter(transistor_count == min(transistor_count))
large_bit <- ram %>% filter(is.na(capacity)==F) %>% filter(capacity == max(capacity))

png("moore5.png", width = 3960, height = 2160, units = 'px', res = 300)
ram %>% 
  mutate(transistor_count = log10(transistor_count),
         capacity = log10(capacity)) %>% 
  filter(is.na(transistor_count)==F) %>% 
  ggplot()+
  geom_line(aes(predict(cap_mod),transistor_count),color="snow3")+
  geom_point(aes(capacity,transistor_count,color=capacity),size=4,alpha=.5)+
  geom_text_repel(aes(capacity,transistor_count,
                       label=manufacturer_s),
                   size=3,min.segment.length = 2,color="snow2")+
  scale_color_gradient(low = "purple",high = "violet")+
  scale_x_continuous(breaks = seq(0,14,2))+
  scale_y_continuous(breaks = seq(0,14,2))+
  scale_size_continuous(labels = number_format(big.mark = ","))+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        plot.title = element_text(size=18),
        plot.caption = element_text(size=16),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow",size=10),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "gray28"),
        strip.text = element_text(colour = "snow",size=12))+
  labs(title = "RAM Capacity In 50 Years",
       subtitle = "From a single bit computer to gigabytes computer only in 50 years",
       y = "Log10 (Transistor Count)", x = "Log10 (RAM Capacity in bits)")
dev.off()

