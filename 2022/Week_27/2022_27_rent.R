
## Challenge: #TidyTuesday 2022 week 27
## Data:      San Francisco Rentals
## Author:    Steven Ponce
## Date:      2022-07-05


## 1. Load packages ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, lubridate)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 27) 
rent <- tt$rent
rm(tt)

#|- offline ----
# write_csv(rent, '2022/Week_27/rent.csv')
# rent <- read_csv(file = '2022/Week_27/rent.csv')


## 3. Examine the data ----
dim(rent)
glimpse(rent)
skim(paygap)
colnames(rent)
unique(rent$'county') %>% sort()
unique(rent$'city') %>% sort()


## 4. Tidydata ----
rent_tbl <- rent %>% 
    # date format
    mutate(date = ymd(date)) %>% 
    # select specific cols
    select(date:sqft) %>% 
    # filter out '0 beds'
    filter(beds != 0) %>% 
    # drop NA
    drop_na() %>% 
    # add new column
    mutate(BA_BR = str_c(baths, " BA", ' - ', beds, " BR")) %>% 
    # group by and summarize
    group_by(year, county, BA_BR) %>% 
    summarise(
        median_price = median(price),
        median_sqft  = median(sqft),
    ) %>%
    # precision
    mutate_if(is.numeric, round, 1) %>% 
    ungroup() 


# |- data for plot ----
commom_combinations_BA_BR <- c("1 BA - 1 BR", "1 BA - 2 BR", 
                               "1 BA - 3 BR", "2 BA - 3 BR", 
                               "3 BA - 4 BR")

rent_data_plot <- rent_tbl %>% 
    # select most common BA and BR combinations
    filter(BA_BR %in% commom_combinations_BA_BR)


## 5. Visualization ----
# |- Plot aesthetics ----
bkg_col        <- '#F9F9F9' 
title_col      <- 'black'
subtitle_col   <- "black"
caption_col    <- "black"
#palette_col    <- met.brewer("Johnson", n = 5, type = "discrete")
palette_col    <- met.brewer("Juarez", n = 5, type = "discrete")


# |-  titles and caption ----
title_text    <- 'Can you afford rent in San Francisco?'

subtitle_text <- "Median rent in San Francisco, 2001-2018"

caption_text <- paste0("**#TidyTuesday:** 2022 Week 27 • **Source:** Kate Pennington<br>",
                       "**Visualization:** Steven Ponce (@sponce1) • **Tools:** #rstats, #ggplot")


# |-  fonts ----
font_add_google("M PLUS 1 Code", family = "title")
font_add_google("M PLUS 1 Code", family = "subtitle")
font_add_google("Jura", family = "text")
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE) 


# |- main plot ----
# scatter plot sqft vs. price 
p1 <- rent_data_plot %>% 
    ggplot(aes(x = median_sqft, y = median_price, color = BA_BR )) + 
    
    # geoms
    geom_point(size = 2.5, alpha = 0.85) + 

    # scales
    scale_x_continuous(breaks = seq(500, 45000, by = 500),
                       limits = c(400, 4500),
                       expand = c(0, 0)) +
    
    scale_y_continuous(breaks = seq(0, 9000, by = 1000),
                       limits = c(0, 9500),
                       expand = c(0, 0),
                       labels = scales::dollar_format(scale = 1e-3, suffix = 'K')) + 
    
    scale_color_manual(values = palette_col) +
    coord_cartesian(clip = 'off', expand = FALSE) +
    
    # labs
    labs(
        x = '**Median Square Footage**', y = '**Median<br>Rent**',
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text) 
    

# |- Annotated Plot ----
p1 +
    # color legend
    ggtext::geom_richtext(
        data = tibble(
            x = c(700, 950, 1200, 1500, 2500), 
            y = c(400, 4200, 650, 6000, 1500),
            
            BA_BR = c("1 BA - 1 BR", "1 BA - 2 BR", "1 BA - 3 BR", 
                      "2 BA - 3 BR", "3 BA - 4 BR"),
            
            lab = c("<b style='font-family:text;font-size:18pt;'>1 BA - 1 BR</b>",
                    "<b style='font-family:text;font-size:18pt;'>1 BA - 2 BR</b>",
                    "<b style='font-family:text;font-size:18pt;'>1 BA - 3 BR</b>",
                    "<b style='font-family:text;font-size:18pt;'>2 BA - 3 BR</b>",
                    "<b style='font-family:text;font-size:18pt;'>3 BA - 4 BR</b>"),
            
            angle = c(0, 0, 0, 0, 0)
        ),
        
        aes(x, y, label = lab, color = BA_BR, angle = angle),
        size = 7, fill = NA,  face = 'bold',
        label.padding = grid::unit(rep(2, 1), "pt")
    ) +
    
    # annotations + arrows
    # Alameda county
    annotate("curve", x = 3870, y = 3430, xend = 4000, yend = 1610, 
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"), 
             curvature = -0.3, color = palette_col[4], size = 0.2) +
    
    annotate("richtext", x = 3700, y = 3700, fontface = 'bold', fill = NA, 
             label.colour = NA, size = 5, color = palette_col[4], family = 'text',
             label = str_glue("Alamenda Country<br>",
                              "Rent: $1,500<br>",
                              "Area: 4,000 ft2"),
             lineheight = 0.9) + 
    
    # San Mateo county
    annotate("curve", x = 2040, y = 8100, xend = 1735, yend = 9000, 
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"), 
             curvature = -0.3, color = palette_col[3], size = 0.2) +
    
    annotate("richtext", x = 2200, y = 8400, fontface = 'bold', fill = NA, 
             label.colour = NA, size = 5, color = palette_col[3], family = 'text',
             label = str_glue("San Mateo Country<br>",
                              "Rent: $9,000<br>",
                              "Area: 1,721 ft2"),
             lineheight = 0.9) +

    # San Francisco county
    annotate("curve", x = 800, y = 6200, xend = 510, yend = 5100, 
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"), 
             curvature = 0.3, color = palette_col[2], size = 0.2) +
    
    annotate("richtext", x = 950, y = 6500, fontface = 'bold', fill = NA, 
             label.colour = NA, size = 5, color = palette_col[2], family = 'text',
             label = str_glue("San Francisco Country<br>",
                              "Rent: $5,000<br>",
                              "Area: 500 ft2"),
             lineheight = 0.9) +
    
    # theme
    theme_classic(base_family = "text", 16) +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title.x   = element_markdown(size = 16,
                                          margin = margin (t = 10), 
                                          hjust = 0.5),
        
        axis.title.y   = element_markdown(size = 16,
                                          margin = margin (r = 10), 
                                          hjust = 0.5,
                                          vjust = 0.5,
                                          angle = 0),
        
        axis.line     = element_line(color = "gray60"),   
        
        plot.margin   = margin(t = 25, r = 25, b = 10, l = 25),
        
        plot.title    = element_text(
            family    = 'title',
            color     = title_col,
            face      = "bold",
            size      = 40,  
            margin    = margin(t = 10)),
        
        plot.subtitle = element_text(
            family    = 'subtitle',
            color     = title_col,
            lineheight= 0.6, 
            size      = 30,
            margin    = margin(t = 10, b = 10)),
        
        plot.caption  = element_markdown(
            family    = 'caption',
            color     = caption_col, 
            size      = 13,
            hjust     = 1.0,
            margin    = margin(t = 10, b = 10)),
    )

 
# |-  resolution ----
showtext_opts(dpi = 400)
 
 
# ## 6. Save final figure ----
ggsave('2022/Week_27/2022_27_rent.png',
       width = 20, height = 11, units = 'in', dpi = 400)

showtext_auto(FALSE) 

