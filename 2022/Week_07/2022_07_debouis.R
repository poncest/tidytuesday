
## Challenge: #TidyTuesday 2022 week 07
## Author: Steven Ponce
## Date: 2022-02-16


## 1. Load packages and data ----
library(pacman)
p_load(tidyverse, tidytuesdayR, skimr, here, showtext, ggtext)

# where does the data comes from? 
# data: challenge 05
# original photo: plate 12
# https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022/challenge05

data <- read.csv(here('2022', 'Week_07', 'challenge_05_data.csv'))  
    
## add 1863 data point - plateau at 100% 
data <- rbind(data, c(1863, 0, 100))  %>% 
    arrange(Year)

## add values for labels
data <- data %>% 
    mutate(label_value = c(0.43, 0.57, 0.57, 0.40, 0.27, 0.30, 0.23, 0.27, 1.0, 1.0)) 

## add google fonts
font_add_google('Orbitron', 'Orbitron') # title
font_add_google('Bai Jamjuree','Bai Jamjuree') # axis / labels
showtext_auto()   


## 2. area chart plot ----
plate12 <- ggplot(data, aes(Year, label_value)) +
    
    geom_area(fill = "#db3855", color = "#e0d5c8") + #indianred, 
    coord_flip(xlim = c(1870, 1790), ylim = c(1, 0), clip = "off") +
    scale_x_reverse(n.breaks = 9, expand = c(0,0)) +
    scale_y_reverse(n.breaks = 9, expand = c(0,0)) +
    
    labs(
        title = "SLAVES AND FREE NEGROES.",
        subtitle = "\n\n\n",
        caption = paste0("DATA: ANTHONY STARK • VIZUALIZATION: STEVEN PONCE • #TIDYTUESDAY • 2022 • WEEK 07")) +
    
    ## theme 
    theme_void(base_family = "Orbitron") +
    
    theme(
        
        plot.margin = margin(t = 20,  
                             r = 100,  
                             b = 20,  
                             l = 100),
        
        plot.title = element_text(size = 20,
                                  color = "#424437", #black
                                  face = 'bold',
                                  hjust = 0.5, # center
                                  margin = margin(b = 10)),
        
        plot.subtitle = element_text(margin = margin(t = 10, 
                                                     b = 10)),
        
        plot.caption = element_text(size = 10,
                                    color = "gray50",  #darkgray, #beae9d"
                                    hjust = 0.5,
                                    margin = margin(t = 25, 
                                                    b = 15)),
        
        panel.grid.major.y = element_line(color = "#e0d5c8", size = 0.25),
        
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        
        panel.background = element_rect(fill = "#262522"), #black
        plot.background = element_rect(fill = "#e5dace"), #gainsboro
        
        legend.position = "none") +  

    
    # text for axis and annotations 
    # percent
    geom_text(aes(Year, -0.07,
                  label = ifelse(Year != 1863, paste0(Free, "%"), NA)),
              family ='Bai Jamjuree',
              size = 5, color = "#908170") + #gray
    
    # years
    geom_text(aes(Year, 1.07,
                  label = ifelse(Year != 1863, paste0(Year), NA)), 
              family ='Bai Jamjuree',
              size = 5, color = "#908170") + #gray
    
    # annotation
    geom_text(aes(x = 1780, y = -0.07),
              family ='Bai Jamjuree',
              color = "#908170",
              # label = "PERCENT\nOF\nFREE NEGROES") 
              label = paste0("PERCENT\n",
                             "OF\n",
                             "FREE NEGROES"))

## 3. save final figure ----
ggsave("./2022/Week_07/2022_07_duboischallenge_plate12.png", width = 10, height = 13, units = "cm", dpi = 320)




