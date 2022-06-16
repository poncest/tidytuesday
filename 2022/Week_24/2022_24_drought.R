
## Challenge: #TidyTuesday 2022 week 24
## Author:    Steven Ponce
## Date:      2022-06-14 


## 1. Load packages ----
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, lubridate, patchwork)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 24) 
drought <- tt$drought
rm(tt)


## 3. Examine the data ----
dim(drought)
glimpse(drought)
skim(drought)
colnames(drought)
unique(drought$'state') %>% sort()


## 4. Tidydata ----
drought_tbl <- drought %>% 
    select(-'0', -'-9') %>% 
    
    # fix the date
    mutate(DATE = str_remove(string = DATE, pattern = 'd_'),
           DATE = ymd(DATE),
           year = year(DATE)) %>% 
    
    # pivot longer
    pivot_longer(cols = -c(DATE, state, year), names_to = 'code', values_to = 'value') %>% 
    
    group_by(year, code) %>% 
    summarise(mean_probability = mean(value)) %>%  
    ungroup() %>% 
    
    # recode / condition col
    mutate(
        class = recode(code,
                       `W0`   = "abnormally wet",
                       `W1`   = "moderate wet",
                       `W2`   = "severe wet",
                       `W3`   = "extreme wet",
                       `W4`   = "exceptional wet",
                       
                       `D0`   = "abnormally dry",
                       `D1`   = "moderate drought",
                       `D2`   = "severe drought",
                       `D3`   = "extreme drought",
                       `D4`   = "exceptional drought",
        )
    ) %>% 
    
    # legent text
    mutate(legend_text = str_c(code, class, sep = ' - ') %>% str_to_title()) %>% 
    
    # reorg
    select(year, mean_probability, code:legend_text)


# wet conditions only - top panel
wet_df <- drought_tbl %>% 
    filter(str_starts(code, 'W')) %>% 
    # change direction
    mutate(mean_probability = mean_probability * -1)


# drought conditions only - bottom panel
drought_df <- drought_tbl %>% 
    filter(str_starts(code, 'D')) 


## 5. Visualization ----

# Plot aesthetics 
# colors
bkg_col        <- '#ecebec'   
title_col      <- 'gray16'
palette_col    <- met.brewer("Troy", n = 10, type = "continuous")[c(5,4,3,2,1,6,7,8,9,10)]

# fonts
font_add_google("Anton", family = "title")
font_add_google("Oranienbaum", family = "subtitle")
font_add_google("Barlow Condensed", family = "text")
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE) 

# titles, caption, and body text
title_text <- "Historical Drought Conditions"

subtitle_text <- "Contiguous U.S. (1895 - 2022)"

body_text_1 <- "The **Standard Precipitation Index (SPI)** is a relatively new drought index based only on precipitation. It's an index based on the probability of precipitation for any time scale."

body_text_2 <- "The purpose of SPI is to assign a single numeric value to the precipitation that can be compared across regions with markedly different climates. The standardization of the SPI allows the index to determine the rarity of a current drought."

caption_text <- str_glue("**#TidyTuesday:** 2022 Week 24 • ",
                         "**Source:** Drought.gov• ",
                         "**Visualization:** Steven Ponce (@sponce1) • **Tools:** #rstats, #ggplot")


# main plot
p1 <- drought_df %>% 
    ggplot(aes(x = year, y = mean_probability, fill = legend_text)) +  
    
    # geoms 
    geom_area(alpha = 0.95) +
    geom_line(position = "stack", size = 0.1, color = 'gray40') +
    
    geom_area(data = wet_df, alpha = 0.95) +
    geom_line(data = wet_df, position = "stack", size = 0.1, color = 'gray40') +
    
    # scales
    scale_x_continuous(expand   = c(0,0), 
                       breaks   = seq(1895, 2022, 25), 
                       limits   =  c(1895, 2022),
                       position = 'top') +
    
    scale_y_continuous(expand   = c(0.05, 0.05), 
                       breaks   = seq(0, 100, 25)) +
    
    scale_fill_manual(values = palette_col) +
    
    # format legend
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))+ 
    
    # labs
    labs(x = '', y = '') +
    
    # theme
    theme_minimal(16, base_family = 'text') +
    theme(
        
        plot.margin           = margin(t = 50, r = 50, b = 0, l = 50),
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        legend.position       = 'bottom',
        legend.title          = element_blank(),
        legend.text           = element_text(size = 16),
        legend.background     = element_rect(fill = bkg_col, color = bkg_col),
        
        panel.grid            = element_blank(),
        axis.line.x           = element_line(color = "gray40", size = 0.2),
        axis.ticks.x          = element_line(color = "gray40"),
        axis.text.y           = element_blank(),
    ) 

# text plot
p2 <- ggplot() +
    theme_void() +
    
    # title
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'title',
                 label       = title_text,
                 color       = title_col,
                 size        = 17, 
                 fill        = NA,     
                 box.color   = NA,
                 width       = unit(10, "in"),
                 vjust       = -2.2,
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 fontface    ="bold", 
                 lineheight  = 1) +
    
    # subtitle
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'title',
                 label       = subtitle_text,
                 color       = title_col,
                 size        = 14, 
                 fill        = NA,     
                 box.color   = NA,
                 width       = unit(10, "in"),
                 vjust       = -1.8, 
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 fontface    ="bold", 
                 lineheight  = 1) +
    
    # text 1
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'text',
                 label       = body_text_1,
                 color       = title_col,
                 size        = 8, 
                 fill        = NA, 
                 box.color   = NA,
                 width       = unit(6.5, "in"),
                 vjust       = 1, 
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0,   
                 fontface    = 'plain',
    ) +
    
    # text 2
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'text',
                 label       = body_text_2,
                 color       = title_col,
                 size        = 8, 
                 fill        = NA, 
                 box.color   = NA,
                 width       = unit(6.5, "in"),
                 vjust       = 2.0,  
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0,   
                 fontface    = 'plain',
    ) +
    
    # caption
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'caption',
                 label       = caption_text,
                 color       = title_col,
                 size        = 6, 
                 fill        = NA, 
                 box.color   = NA,
                 vjust       = 8, 
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0,
                 fontface    = 'plain',
                 width       = unit(6, "in"),
    ) +
    
    coord_cartesian(clip = "off") +
    scale_y_continuous(limits = c(0, 0), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 0), expand = c(0, 0)) +
    
    # theme
    theme(
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin = margin(t = 0, r = 20, b = 30, l = 20),
        
        plot.title    = element_markdown(
            family    = 'title',
            color     = title_col,
            face      = "bold",
            size      = 5,
            lineheight= 0.5,
            margin    = margin(t = 5))
    )


# putting all plots together
p2 + p1 +
    plot_layout(widths = c(1.5, 2)) &
    theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 10))


# resolution 
showtext_opts(dpi = 300)


## 6. Save final figure ----
ggsave('2022/Week_24/2022_24_drought.png',
       width = 20, height = 11, units = 'in', dpi = 300)

showtext_auto(FALSE) 
print(sessionInfo())
