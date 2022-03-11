## Challenge: #TidyTuesday 2022 week 10
## Author: Steven Ponce
## Date: 2022-03-09


## 1. Load packages -----------------------------------------------
library(pacman)
p_load(tidyverse, tidytuesdayR, showtext, ggtext, janitor)


## 2. Read in the data --------------------------------------------
tt <- tidytuesdayR::tt_load('2022-03-08')
erasmus <- tt$erasmus %>% 
    clean_names()

# rm(tt)
# OFFLINE - In case of VPN issues
# erasmus <- write_csv(erasmus, '2022/Week_10/erasmus.csv')
# erasmus <- read_csv('2022/Week_10/erasmus.csv')

## 3. Examine the data --------------------------------------------

glimpse(erasmus)
skim(erasmus)
names(erasmus)

## 4. Tidydata --------------------------------------------
## Top 10 receiving countries for females participants 
## between the ages 20 - 30 years
female_receiving_countries_tbl <- erasmus %>% 
    
    select(participant_gender, participant_age, receiving_country_code) %>% 
    filter(participant_gender == "Female", participant_age > 19, participant_age < 31) %>% 
    group_by(receiving_country_code) %>% 
    summarise(female_count = n()) %>% 
    arrange(desc(female_count)) %>% 
    
    # bar labels
    mutate(bar_label = format(female_count, big.mark = ',')) %>% 
    top_n(10) %>% 
    
    # replace country code with country name
    mutate(receiving_country_code = case_when(
        receiving_country_code == 'FR' ~ 'France',
        receiving_country_code == 'DE' ~ 'Germany',
        receiving_country_code == 'ES' ~ 'Spain',
        receiving_country_code == 'PL' ~ 'Poland',
        receiving_country_code == 'TR' ~ 'Turkey',
        receiving_country_code == 'UK' ~ 'United\nKingdom',
        receiving_country_code == 'IT' ~ 'Italy',
        receiving_country_code == 'HU' ~ 'Hungary',
        receiving_country_code == 'HR' ~ 'Croatia',
        receiving_country_code == 'SI' ~ 'Slovenia',
        TRUE ~ receiving_country_code)) %>% 
    
    # reorder from high to low
    mutate(receiving_country_code = receiving_country_code %>% fct_reorder(female_count)) %>%
    ungroup()

## 5. Visualization --------------------------------------------

## add google fonts
font_add_google(family='Goldman', 'Goldman')                              # title
font_add_google(family='Meera Inimai', 'Meera Inimai')                    # text
font_add_google(family='Saira Semi Condensed', 'Saira Semi Condensed')    # caption
showtext_auto(enable = TRUE) 


female_receiving_countries_tbl %>% 
    
    ggplot(aes(x = female_count,
               y = receiving_country_code)) +
    
    geom_col(fill = '#a12965', alpha = 0.85,
             width = 0.95) + 
    
    # scales
    scale_x_continuous(expand = c(.01, .01)) + 
    scale_y_discrete() +
    coord_cartesian(clip = 'off', expand = FALSE) +
    
    # col labels
    geom_text(aes(label = bar_label, hjust= -0.25), 
              family='Meera Inimai', 
              color = '#949494', size = 5) +
    
    # labels
    labs(
        title = 'Top 10 Countries to Study Abroad in Europe',
        subtitle = "for females participants between the ages 20 - 30 years",
        caption = paste0("#TidyTuesday: Week 10 • Data: Data.Europa.eu • Visualization: Steven Ponce (@sponce1)")
    ) +
     
    
    # theme
    theme_void(base_family = "Meera Inimai") +    
    theme(
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#f4f9f5", color = "#f4f9f5"),
        panel.background = element_rect(fill = "#f4f9f5", color = "#f4f9f5"),
        
        panel.grid = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.y = element_text(margin = margin(r =10), hjust = 0.5),
        
        plot.margin = margin(t = 5, r = 60, b = 5, l = 40),
        
        plot.title = element_text(
            family = 'Goldman',
            color = "#48494a",
            face = "bold",
            size = 45,  
            margin = margin(t = 20)),
        
        plot.subtitle = element_text(
            family = 'Goldman',
            color = "#48494a",
            size = 25,  
            margin = margin(t = 5, b = 20)),
        
        plot.caption = element_text(
            color = "#8a8b91",
            family = 'Saira Semi Condensed',
            size = 12,
            hjust = .5,
            margin = margin(t = 15, b = 5)))

# resolution
showtext_opts(dpi = 300)


## 6. Save final figure --------------------------------------------
ggsave("2022/Week_10/2022_10_erasmus.png", plot = last_plot(),
       width = 16, height = 8, units = 'in',  dpi = 300)


showtext_auto(FALSE)