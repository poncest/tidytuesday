
## Challenge: #TidyTuesday 2022 week 20
## Author:    Steven Ponce
## Date:      2022-05-20
 
## 1. Load packages ----
library(pacman)
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor)


## 2. Read in the data ----
tuesdata <- tidytuesdayR::tt_load(2022, week = 20)
eurovision <- tuesdata$eurovision

# OFFLINE - In case of VPN issues
# eurovision <- write_csv(eurovision, '2022/Week_20/eurovision.csv')
# eurovision <- read.csv('2022/Week_20/eurovision.csv')
 

## 3. Examine the data ----
glimpse(eurovision)
range(eurovision$year)
unique(eurovision$event)
unique(eurovision$host_city)
unique(eurovision$host_country)


## 4. Tidydata ----
winners_1956_2003_tbl <- eurovision %>% 
    filter(year    < 2004,
           section == 'final',
           winner  == 'TRUE') %>% 
    
    select(year, host_city, artist_country, total_points, winner) %>% 
    arrange(desc(year)) %>% 
    drop_na()

winners_2004_2022_tbl <- eurovision %>% 
    filter(section == 'grand-final',
           winner  == 'TRUE') %>% 
    
    select(year, host_city, artist_country, total_points, winner) %>% 
    arrange(desc(year))%>% 
    drop_na()

winners_combined_tbl <- 
    bind_rows(winners_1956_2003_tbl, winners_2004_2022_tbl) %>% 
    arrange(desc(year)) %>% 
    drop_na() %>% 
    
    group_by(artist_country) %>% 
    summarise(number_wins = n(),
              grand_total_points = sum(total_points)) %>% 
    arrange(desc(number_wins)) %>% 
    ungroup() %>% 
    
    mutate(artist_country = as.factor(artist_country))

years_per_country_tbl <- bind_rows(winners_1956_2003_tbl, winners_2004_2022_tbl) %>% 
    drop_na() %>% 
    select(year, artist_country) %>% 
    group_by(artist_country) %>% 
    arrange(artist_country) %>% 
    ungroup() 

# data for viz
data <- winners_combined_tbl %>% 
    full_join(years_per_country_tbl) %>% 
    
    # countries with more than 3 Eurovision wins
    filter(number_wins > 3) %>% 
    mutate(win_4_or_more = case_when(
        artist_country %>% str_to_lower() %>% str_detect('united kingdom') ~ '1',
        artist_country %>% str_to_lower() %>% str_detect('sweden') ~ '1',
        artist_country %>% str_to_lower() %>% str_detect('netherlands') ~ '1',
        artist_country %>% str_to_lower() %>% str_detect('luxembourg') ~ '1',
        artist_country %>% str_to_lower() %>% str_detect('israel') ~ '1',
        artist_country %>% str_to_lower() %>% str_detect('ireland') ~ '1',
        artist_country %>% str_to_lower() %>% str_detect('france') ~ '1',
        TRUE ~ '0'
    ))


## 5. Visualization ----
# colors
col_1      <- '#6cb7b0'  # green
bkg_col    <- '#f4f4f4'     
title_col  <- '#585858' 

# google fonts
font_add_google(family="Train One", "Train One")                        # title
font_add_google(family="Gruppo", "Gruppo")                              # subtitle
font_add_google(family="Nova Mono", "Nova Mono")                        # text
font_add_google(family='Saira Semi Condensed', 'Saira Semi Condensed')  # caption
showtext_auto(enable = TRUE) 


data %>% 
    ggplot(aes(x    = year, 
               y    = fct_infreq(artist_country) %>% fct_rev(), 
               fill = win_4_or_more)) +
    
    # geoms
    geom_tile(height = 0.4,
              width  = 0.8,
              color = "black") +
    
    annotate("text", x = 2025, y = 'Ireland',
             hjust    = 1.1,
             vjust    = 0,
             color    = title_col,
             fontface = "bold",
             size     = 6,
             label    = paste0("[7]")) +  
    
    annotate("text", x = 2025, y = 'Sweden',
             hjust    = 1.1,
             vjust    = 0,
             color    = title_col,
             fontface = "bold",
             size     = 6,
             label    = paste0("[6]")) +  
    
    annotate("text", x = 2025, y = 'France',
             hjust    = 1.1,
             vjust    = 0,
             color    = title_col,
             fontface = "bold",
             size     = 6,
             label    = paste0("[5]")) +  
    
    annotate("text", x = 2025, y = 'Luxembourg',
             hjust    = 1.1,
             vjust    = 0,
             color    = title_col,
             fontface = "bold",
             size     = 6,
             label    = paste0("[5]")) +  
    
    annotate("text", x = 2025, y = 'Netherlands',
             hjust    = 1.1,
             vjust    = 0,
             color    = title_col,
             fontface = "bold",
             size     = 6,
             label    = paste0("[5]")) +  
    
    annotate("text", x = 2025, y = 'United Kingdom',
             hjust    = 1.1,
             vjust    = 0,
             color    = title_col,
             fontface = "bold",
             size     = 6,
             label    = paste0("[5]")) +  
    
    annotate("text", x = 2025, y = 'Israel',
             hjust    = 1.1,
             vjust    = 0,
             color    = title_col,
             fontface = "bold",
             size     = 6,
             label    = paste0("[4]")) +  
    
    # change color
    scale_fill_manual(values = c(col_1)) +
    
    # scales
    scale_x_continuous(breaks       = seq(1955, 2025, 10),
                       minor_breaks = seq(1955, 2025, 5),
                       limits       = c(1950, 2025),
                       expand       = c(0.0, 0.0)) +
    scale_y_discrete() +
    
    # labs
    labs(
        title = paste0('Eurovision: Ireland Leads the Pack with Seven Wins'),
        subtitle = 'Countries with more than three wins, 1956 - 2022',
        caption  = paste0("#TidyTuesday: 2022 Week 20 • Source: Eurovision • Visualization: Steven Ponce (@sponce1)") ,
        x = '', 
        y = '' ) +  

    # theme
    theme_minimal(16) +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        axis.text             = element_text(size = 16),
        axis.title            = element_blank(),
        
        panel.grid            = element_blank(),
        panel.grid.minor      = element_blank(),
        panel.grid.major.x    =  element_line(linetype = "dotted", size = 0.3, color = 'gray'),
        panel.grid.major.y    = element_line(linetype = "dotted", size = 0.3, color = 'gray'),
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin           = margin(t = 20, r = 50, b = 20, l = 30),
        
        plot.title    = element_text(
            family    = 'Train One',
            color     = title_col,
            face      = "bold",
            size      = 35,  
            margin    = margin(t = 5)),
        
        plot.subtitle = element_text(
            family    = 'Nova Mono',
            color     = title_col,
            #face      = "italic",
            size      = 25,
            margin    = margin(t = 5, b = 10)),
        
        plot.caption  = element_text(
            family    = 'Saira Semi Condensed',
            color     = '#808080', 
            size      = 14,
            hjust     = 0.98,
            margin    = margin(t = 20, b = 10)),
    ) 
        
# resolution
showtext_opts(dpi = 300)

## 6. Save final figure ----
ggsave('2022/Week_20/2022_20_eurovision.png',
        width = 14.5, height = 6.5, units = 'in',  dpi = 300)

showtext_auto(FALSE)

