
## Challenge: #TidyTuesday 2022 week 15
## Author: Steven Ponce
## Date: 2022-04-12


## 1. Load packages ----
library(pacman)
p_load(tidyverse,tidytuesdayR, showtext, ggtext, skimr, janitor)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 15)
data <- tt$indoor_pollution %>%  clean_names()
# rm(tt)

# OFFLINE - In case of VPN issues
# data <- write_csv(data, '2022/Week_15/indoor_pollution.csv')
# data <- read.csv('2022/Week_15/indoor_pollution.csv')

## 3. Examine the data ----

glimpse(data)
skim(data)

range(data$year)
unique(data$entity) 
unique(data$code) 


## 4. Tidydata ----
codes = c('PRI', 'USA')

data <- data %>% 
    filter(code %in% codes) %>% 
    
    # change order of levels 
    mutate(entity = fct_relevel(entity, c('United States', 'Puerto Rico'))) %>% 

    # rename column
    rename(deaths_air_pollution = deaths_cause_all_causes_risk_household_air_pollution_from_solid_fuels_sex_both_age_age_standardized_percent)
 

# labels tibble  
labels <- data %>%
    filter(year == 1990 | year == 2019) %>%
    mutate(deaths_air_pollution = round(deaths_air_pollution, digits = 4))


## 5. Visualization ----

# base plot
g <- data %>%
    ggplot(aes(x = year, 
               y = deaths_air_pollution,
               fill = code)) +
    geom_area() + 
    theme_minimal()
    
g 


# customization 
# colors
col_1      <- '#E5F5F9'
col_2      <- '#99D8C9'
bkg_col    <- '#FBFBFB'   
title_col  <- '#48494A'

# google fonts
font_add_google(family='Candal', 'Candal')                              # title
font_add_google(family='Alatsi', 'Alatsi  ')                            # text
font_add_google(family='Saira Semi Condensed', 'Saira Semi Condensed')  # caption
showtext_auto(enable = TRUE) 


# final plot
g1 <- g +
    
    geom_area(alpha = 0.6) +
    scale_fill_brewer(palette = "OrRd") +             
    geom_line(size = 0.2, color = 'black') +

    # scales
    scale_y_continuous(limits = c(0, 0.02)) +
    scale_x_continuous(breaks = c(1990, 2019)) +
    coord_cartesian(clip = 'off') +
    
    # facet
    facet_grid(~ entity) +
        
    # chart line endpoints 
    geom_point(data = labels %>% filter(entity == 'Puerto Rico'),
              aes(x = year[1],
                  y = deaths_air_pollution[1]),
              shape = 16,
              size  = 4,
              color = '#48494A',
              alpha = 0.8,
              ) +

    geom_point(data = labels %>% filter(entity == 'Puerto Rico'),
               aes(x = year[2],
                   y = deaths_air_pollution[2]),
               shape = 16,
               size  = 4,
               color = '#48494A',
               alpha = 0.8,
    ) +

    geom_point(data = labels %>% filter(entity == 'United States'),
               aes(x = year[1],
                   y = deaths_air_pollution[1]),
               shape = 16,
               size  = 4,
               color = '#48494A',
               alpha = 0.8,
    ) +
    
    geom_point(data = labels %>% filter(entity == 'United States'), 
               aes(x = year[2],
                   y = deaths_air_pollution[2]),
               shape = 16,
               size  = 4,
               color = '#48494A',
               alpha = 0.8,
    ) +  
    
    # annotate
    geom_text(
        data = labels %>% filter(entity == 'Puerto Rico'),
        mapping = aes(x = year,
                      y = deaths_air_pollution,
                      label = paste0(deaths_air_pollution, '%')),
        color   = title_col,
        size    = 6,
        hjust   = 0.5,
        nudge_y = 0.001,
    ) +
    
    geom_text(
        data = labels %>% filter(entity == 'United States'),
        mapping = aes(x = year,
                      y = deaths_air_pollution,
                      label = paste0(deaths_air_pollution, '%')),
        color   = title_col,
        size    = 6,
        hjust   = 0.5,
        nudge_y = 0.001,
    ) +

    # labs 
    labs(
        title = "Percent of Deadths from Indoor Air Pollution",
        subtitle = "Steady Decrease, 1990 - 2019",
        caption = paste0(
            "#TidyTuesday: 2022 Week 15\n",
            "Source: OurWorldInData.org\n",
            "Visualization: Steven Ponce (@sponce1)"),      
        x = '',
        y = '' ) +
     
    # theme
    theme_minimal(base_family = 'Alatsi') +  
    
    theme(
        
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'none',
        
        axis.title            = element_blank(), 
        axis.text             = element_text(size = 18), 
        axis.text.y           = element_blank(),
        
        panel.grid            = element_blank(),
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin           = margin(t = 30, r = 30, b = 30, l = 30), 

        plot.title    = element_text(
            family    = 'Candal',
            color     = title_col,
            face      = "bold",
            size      = 50,  
            margin    = margin(t = 10)),
        
        plot.subtitle = element_text(
            family    = 'Candal',
            color     = title_col,  
            size      = 35,  
            margin    = margin(t = 5, b = 40)),
        
        plot.caption  = element_text(
            family    = 'Saira Semi Condensed',
            color     = '#808080', 
            size      = 12,
            hjust     = 0.98,
            margin    = margin(t = 20, b = 10)),
    
        strip.text    = element_text(size   = 25, 
                                     face  = "bold",
                                     color = title_col,),
        
        panel.spacing = unit(2, "lines")
    )


g1      


# resolution
showtext_opts(dpi = 300) 
     

## 6. Save final figure ----
ggsave('2022/Week_15/2022_15_indoor_pollution.png', 
       width = 20, height = 11, units = 'in',  dpi = 300)


showtext_auto(FALSE)

