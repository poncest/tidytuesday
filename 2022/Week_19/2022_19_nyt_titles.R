
## Challenge: #TidyTuesday 2022 week 19
## Author: Steven Ponce
## Date: 2022-05-10
 
## 1. Load packages ----
library(pacman)
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor)


## 2. Read in the data ----
nyt_titles <- 
    read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv') 


## 3. Examine the data ----
glimpse(nyt_titles)
range(nyt_titles$year)
unique(nyt_titles$author)
 

## 4. Tidydata ----
authors_tbl <- unique(nyt_titles$author) %>% sort() %>% as_tibble()
favorite_author <- 'Gabriel Garcia Marquez'

# data for viz
data <- nyt_titles %>% 
    mutate(title = str_to_title(title),
           title = as_factor(title) %>% fct_reorder(year) %>% fct_rev())  %>%  
    filter(author%in%favorite_author) %>% 
    arrange(year)  
    

# labels text  
label_text <- data %>%
    select(year, title, debut_rank, best_rank, total_weeks)


## 5. Visualization ----
# base plot
g <- data %>% 
    ggplot() + 
    geom_segment(aes(x = title, xend = title, y = debut_rank, yend = best_rank), 
                 color = 'gray',
                 size = 2) 
g

# colors
col_1      <- '#fca901'  # orange
col_2      <- '#4DEAC3'  # green 
bkg_col    <- '#32414A'  # gray      
title_col  <- '#ffffff'  # white

# google fonts
font_add_google(family="Luxurious Roman", "Luxurious Roman")      # title
font_add_google(family="Farsan", "Farsan")                        # subtitle
font_add_google(family="Flamenco", "Flamenco")                    # text
font_add_google(family='Roboto Condensed', 'Roboto Condensed')    # caption
showtext_auto(enable = TRUE) 


# final plot
g +
    
    # geoms
    geom_point(aes(x = title, y = debut_rank), size = 8, color = col_1) +
    geom_point(aes(x = title, y = best_rank),  size = 8, color = col_2) +

    # debut rank labels
    geom_text(
        data = label_text,
        mapping = aes(x = title,
                      y = debut_rank, 
                      label = debut_rank),
            color   = title_col,
            size    = 5,     
            hjust   = 0.5,
            nudge_x = 0.3) + 
    
    # best rank labels
    geom_text(
        data = label_text,
        mapping = aes(x = title,
                      y = best_rank, 
                      label = best_rank),
        color   = title_col,
        size    = 5,     
        hjust   = 0.5,
        nudge_x = 0.3) + 
    
    # book titles
    geom_text(
        data = label_text %>% filter(title == 'One Hundred Years Of Solitude'),
        mapping = aes(x = title,
                      y = best_rank, 
                      label = str_glue("Title: {title}
                                       Released Year: {year}
                                       Total Weeks: {total_weeks}")),
        color   = title_col,
        size    = 5,
        hjust   = 1.1,
        nudge_x = 0) + 
    
    geom_text(
        data = label_text %>% filter(title == 'The Autumn Of The Patriarch'),
        mapping = aes(x = title,
                      y = debut_rank, 
                      label = str_glue("Title: {title}
                                       Released Year: {year}
                                       Total Weeks: {total_weeks}")),
        color   = title_col,
        size    = 5,
        hjust   = 1.1,
        nudge_x = 0) + 
    
    geom_text(
        data = label_text %>% filter(title == 'Love In The Time Of Cholera'),
        mapping = aes(x = title,
                      y = best_rank, 
                      label = str_glue("Title: {title}
                                       Released Year: {year}
                                       Total Weeks: {total_weeks}")),
        color   = title_col,
        size    = 5,
        hjust   = 1.1,
        nudge_x = 0) + 
    
    geom_text(
        data = label_text %>% filter(title == 'The General In His Labyrinth'),
        mapping = aes(x = title,
                      y = best_rank, 
                      label = str_glue("Title: {title}
                                       Released Year: {year}
                                       Total Weeks: {total_weeks}")),
        color   = title_col,
        size    = 5,
        hjust   = 1.1,
        nudge_x = 0) + 
    
    geom_text(
        data = label_text %>% filter(title == 'Of Love And Other Demons'),
        mapping = aes(x = title,
                      y = debut_rank, 
                      label = str_glue("Title: {title}
                                       Released Year: {year}
                                       Total Weeks: {total_weeks}")),
        color   = title_col,
        size    = 5,
        hjust   = 1.1,
        nudge_x = 0) + 
    
    # scales 
    scale_y_continuous(breaks = seq(0, 15, by = 5),
                       limits = c(-2, 15),
                       expand = c(0.0, 0.0)) +

    coord_cartesian(clip = 'off', expand = FALSE)   +
    
    coord_flip() +
    
# labs
     labs(
        title = "The New York Times Best Sellers <br> Author: Gabriel Garcia Marques",
        subtitle = "<span style='font-size:35pt; color:#fca901'>**Debut Rank**</span> vs. <span style='font-size:35pt; color:#4DEAC3'>**Best Rank** </span>",
        caption =paste0("#TidyTuesday: 2022 Week 19 • Source: Post45 Data • Visualization: Steven Ponce (@sponce1)") ,
        x = '', 
        y = '') +
    
    # theme
    theme_minimal(base_family = 'Flamenco') +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        axis.text             = element_blank(),
        axis.title            = element_blank(),                    
        
        panel.grid            = element_blank(), 
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
     
        plot.margin           = margin(t = 30, r = 20, b = 30, l = 20),
        
        plot.title    = element_markdown(
            family    = 'Luxurious Roman',
            color     = title_col,
            face      = "bold",
            size      = 60,  
            margin    = margin(t = 10)),
        
        plot.subtitle = element_markdown(
            family    = 'Farsan',
            color     = title_col,
            size      = 40,
            margin    = margin(t = 25, b = 40)),
            
        plot.caption  = element_text(
            family    = 'Roboto Condensed',
            color     = '#c4c4c4', 
            size      = 14,
            hjust     = 0.5,
            margin    = margin(t = 20, b = 10)),
    ) 
     

# resolution
showtext_opts(dpi = 300) 


## 6. Save final figure ----
ggsave('2022/Week_19/2022_19_nyt_titles.png',
        width = 20, height = 11, units = 'in',  dpi = 300)
 
showtext_auto(FALSE)

