
## Challenge: #TidyTuesday 2022 week 17
## Author: Steven Ponce
## Date: 2022-04-26
 
## 1. Load packages ----
library(pacman)
p_load(tidyverse, ggtext, showtext, tidytext, janitor, skimr)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 17)
data <- tt$hidden_gems %>% clean_names()
# rm(tt)

# OFFLINE - In case of VPN issues
# data <- write_csv(data, '2022/Week_17/hidden_gems.csv')
# data <- read.csv('2022/Week_17/hidden_gems.csv')
 
## 3. Examine the data ----

glimpse(data)
skim(data)

range(data$date)


## 4. Tidydata ----

data_tbl <- data %>% 
    # select(vol, date, title) %>% 
    select(title) %>% 
    
    # lower case
    mutate(title = str_trim(title) %>% str_to_lower()) 
    

## tokenization 
data_tokens <- data_tbl %>% 
    unnest_tokens(input = title, output = word) %>%  
    
    # remove stop words
    anti_join(stop_words) %>%                          
    
    # remove digits
    filter(!str_detect(word, '[[:digit:]]')) %>%       
    
    # remove punctuation's
    filter(!str_detect(word, '[[:punct:]]')) %>%
    
    group_by(word) %>% 
    summarise(word_count = n()) %>% 
    arrange(desc(word_count)) %>% 
    ungroup() 


## 5. Visualization ----

# base plot 
g <- data_tokens %>% 
    
    # filter top_6 words
    filter(word_count >= 8) %>% 
    
    # canvas
    ggplot(aes(x = word, y = word_count))+
    
    # geoms
    geom_bar(stat = "identity", fill = '#2F899B', width = 0.85) +
    
    geom_text(aes(label = word_count), 
              vjust = 1.6, 
              size  = 6, 
              color = 'white') 

g

# customization

# colors
# col_1      <- '#E5F5F9'
# col_2      <- '#99D8C9'
bkg_col    <- '#A7FFE2'     
title_col  <- '#48494A'

# google fonts
font_add_google(family="Abril Fatface", "Abril Fatface")                  # title
font_add_google(family="Forum", "Forum")                  # subtitle
font_add_google(family="Pompiere", "Pompiere")                  # text
font_add_google(family='Saira Semi Condensed', 'Saira Semi Condensed')  # caption
showtext_auto(enable = TRUE) 


# final plot
g +
    
    # scales
    scale_y_continuous(breaks = seq(0, 40, by = 10),
                       limits = c(0, 40),
                       expand = c(0.05, 0.05)) +
    
    coord_cartesian(clip = 'off') +
    
    # labs
    labs(
        title = 'Kaggle Hidden Gems',
        subtitle = "Out of 300 notebooks, the most recurring words in the notebook title ",
        caption = paste0(
            "#TidyTuesday: 2022 Week 17\n",
            "Source: Kaggle.com\n",
            "Visualization: Steven Ponce (@sponce1)"), 
        x = '', 
        y = '',
    )  +
    
    # theme
    theme_minimal(base_family = 'Pompiere')+
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'none',
        
        #axis.text            = element_text(size = 20), 
        axis.text.x           = element_text(size = 26), 
        axis.text.y           = element_text(size = 18), 
        
        axis.title            = element_blank(),
        
        panel.grid.minor      = element_blank(),
        panel.grid.major.x    = element_blank(),
        panel.grid.major.y    = element_line(linetype = "dotted", size = 1, color = 'gray'),
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
     
        plot.margin           = margin(t = 30, r = 40, b = 30, l = 30),
        
        plot.title    = element_text(
            family    = 'Abril Fatface',
            color     = title_col,
            face      = "bold",
            size      = 75,  
            margin    = margin(t = 10)),
        
        plot.subtitle = element_text(
            family    = 'Forum',
            color     = title_col,  
            #face      = "bold",
            size      = 45,  
            margin    = margin(t = 5, b = 40)),
        
        plot.caption  = element_text(
            family    = 'Saira Semi Condensed',
            color     = '#808080', 
            size      = 12,
            hjust     = 0.98,
            margin    = margin(t = 20, b = 10)),
    ) 
    



# resolution
showtext_opts(dpi = 300) 
     

## 6. Save final figure ----
ggsave('2022/Week_17/2022_17_hidden_gems.png',
       width = 20, height = 11, units = 'in',  dpi = 300)


showtext_auto(FALSE)

