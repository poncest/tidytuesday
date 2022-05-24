
## Challenge: #TidyTuesday 2022 week 21
## Author:    Steven Ponce
## Date:      2022-05-244
 
## 1. Load packages ----
library(pacman)
p_load(tidyverse, tidytuesdayR, ggtext, showtext)
# p_load(MetBrewer)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 21) 
sevens <- tt$sevens
remove(tt)

## 3. Examine the data ----
glimpse(sevens)
range(sevens$date)
unique(sevens$tournament)
unique(sevens$stage) 


## 4. Tidydata ----

# select stage finals
stage_finals <- c("plate final", "final", "shield final", "olympic final", "bowl final")

sevens_df <- sevens %>% 
    mutate(stage = str_to_lower(stage)) %>% 
    # finals only
    filter(stage %in% stage_finals) %>% 
    # select specific cols
    select(date:team_2, margin:loser) %>% 
    # filter out confusing date
    filter(!date == "2022-11-28") 
    
# data for viz
data <- sevens_df %>% 
    group_by(winner) %>% 
    count() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    
    ## Add a column indicating whether the category should be highlighted
    mutate(highlight = ifelse(winner == "New Zealand", "yes", "no")) %>% 
    
    # top-10 countries 
    slice(1:10)


## 5. Visualization ----
#  Plot aesthetics 
col_1          <- '#6C79B0'
col_2          <- '#A1A9CC'
bkg_col        <- '#EEE8F0'     
title_col      <- '#767183' 
subtitle_col   <- "#767183" 
caption_col    <- "#767183"
#palette_col    <- met.brewer("Johnson", 5, type = "discrete")[c(5,2,1,3,4)]

# fonts
font_add_google("Yeseva One", family = "title")
font_add_google("Oranienbaum", family = "subtitle")
font_add_google("Libre Caslon Display", family = "text")
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE) 

# titles and caption
title_text <- str_glue("Women Rugby Sevens: <br> Top-10 Countries with most Finals Wins")

subtitle_text <- str_glue("Powerhouse New Zealand leading the way with 15 more wins than runner-up Australia.")

caption_text <- paste0("#TidyTuesday: 2022 Week 21\n",
                       "Source: Women's Rugby - ScrumQueens\n",
                       "Visualization: Steven Ponce (@sponce1)")

# plot
data %>% 
    ggplot(aes(x = reorder(winner, n), y = n,
               fill = highlight)) +
    
    # geoms
    geom_bar(stat="identity", show.legend = FALSE) +
   
    geom_hline(yintercept = 38, linetype = "dotted", size = 0.3, color = 'gray') +
    
    geom_richtext(aes(x = 'Canada', y = 38), 
                 label  = 38, 
                 fill   = 'transparent',
                 color  = col_1,
                 hjust  = 0.5,
                 vjust  = 0.5,
                 size   = 5) +
    
    coord_flip() +
    
    # scales
    scale_fill_manual(values = c("yes" = col_1, "no" = col_2)) +
    scale_y_continuous(limits = c(0, 38)) +
    scale_x_discrete() +
   
    # labels
    labs(x = "",
         y = "",
         title = title_text,
         subtitle = subtitle_text,
         caption = caption_text) +
    
    # theme
    theme_minimal(18, base_family = 'text') +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        axis.text             = element_text(size = 18),
        axis.title            = element_blank(),
        
        panel.grid            = element_blank(),
        panel.grid.minor.x    = element_line(linetype = "dotted", size = 0.
                                             , color = 'gray'),
        panel.grid.major.x    = element_line(linetype = "dotted", size = 0.
                                             , color = 'gray'),
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin           = margin(t = 10, r = 35, b = 10, l = 25),
        
        plot.title    = element_markdown(
            family    = 'title',
            color     = title_col,
            face      = "bold",
            size      = 40,  
            margin    = margin(t = 5)),
        
        plot.subtitle = element_text(
            family    = 'subtitle',
            face      = "bold",
            color     = subtitle_col,
            size      = 24,
            margin    = margin(t = 10, b = 10)),
        
        plot.caption  = element_text(
            family    = 'caption',
            color     = caption_col, 
            size      = 14,
            hjust     = 0.98,
            margin    = margin(t = 10, b = 10)),
    ) 


# resolution
showtext_opts(dpi = 300)

## 6. Save final figure ----
ggsave('2022/Week_21/2022_21_rugby.png',
        width = 14.5, height = 6.5, units = 'in',  dpi = 300)

showtext_auto(FALSE)

