
## Challenge: #TidyTuesday 2022 week 22
## Author:    Steven Ponce
## Date:      2022-05-31  
 

## 1. Load packages ----
library(pacman)
p_load(tidyverse, tidytuesdayR, ggtext, showtext)
p_load(janitor, MetBrewer, patchwork)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 22) 
reputation_raw <- tt$reputation %>% clean_names()
poll_raw <- tt$poll%>% clean_names()
remove(tt)

## 3. Examine the data ----
glimpse(reputation_raw)
unique(reputation_raw$company) %>% sort()
unique(reputation_raw$industry) %>% sort()
unique(reputation_raw$name) %>% sort()

unique(poll_raw$industry) %>% sort()
glimpse(poll_raw)
unique(poll_raw$year)


## 4. Tidydata ----
selected_dimensions <- c('GROWTH', 'P&S')

# reputation score levels
label_text = c('Critical', 'Very poor', 'Poor', 'Fair', 'Good', 'Very good', 'Excellent')

reputation <- reputation_raw %>% 
    filter(name %in% selected_dimensions) %>% 
    select(-rank) %>% 
    mutate(name = str_to_lower(name)) %>% 
    
    # wide table
    pivot_wider(names_from = name, values_from = score) %>% 
    
    # column indicating whether the category should be highlighted
    mutate(highlight = ifelse(industry == "Retail", "yes", "no")) 
     

## 5. Visualization ----
# Plot aesthetics 
col_1          <- '#E16723'     
col_2          <- 'gray80'
bkg_col        <- '#FAF6EF'     
title_col      <- '#767183' 
subtitle_col   <- "#767183" 
caption_col    <- "#767183"

# fonts
font_add_google("Anton", family = "title")
font_add_google("Oranienbaum", family = "subtitle")
font_add_google("Barlow Condensed", family = "text")
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE) 

# titles and caption
title_text <- "America's Top 100 <br>Most Visible Companies"

subtitle_text <- "Growth and P&S Dimensions"

body_text <- "The 2022 Axios Harris Poll 100 is based on a survey of **33,096** Americans in a nationally representative sample conducted **March 11- April 3**. The two-step process starts fresh each year by surveying the public’s top-of-mind awareness of companies that either excel or falter in society."

caption_text <- paste0("#TidyTuesday: 2022 Week 22 • ",
                       "**Source:** Axios and Harris Poll • ",
                       "**Visualization:** Steven Ponce (@sponce1) • **Tools:** #rstats, #ggplot")


# main plot
main_plot <- reputation %>% 
    ggplot(aes(x = `p&s`, y = growth, color = highlight)) +
    
    # geoms
    geom_point(size = 3, alpha = 0.9) +
    
    geom_hline(yintercept = 65, linetype = 2, size = 0.5, color = 'gray65')+
    geom_vline(xintercept = 65, linetype = 2, size = 0.5, color = 'gray65')+
    
    # annotation
    annotate("curve", x = 65, y = 55, xend = 55.8, yend = 55.4, arrow = arrow(length = unit(0.02, "npc"), type = "closed"), curvature = 0.3, color = 'gray20', size = 0.2) +
    annotate("richtext", x = 65, y = 54, label = "The Trump Organization", fontface = 'bold', fill = NA, label.colour = NA, size = 6, color = 'gray20') +
    
    annotate("curve", x = 78, y = 57, xend = 66.9, yend = 58, arrow = arrow(length = unit(0.02, "npc"), type = "closed"), curvature = 0.3, color = 'gray20', size = 0.2) +
    annotate("richtext", x = 78, y = 56, label = "Sears Holdings Corporation", fontface = 'bold', fill = NA, label.colour = NA, size = 6, color = 'gray20') +
    
    annotate("curve", x = 79, y = 61, xend = 70, yend = 62.5, arrow = arrow(length = unit(0.02, "npc"), type = "closed"), curvature = 0.3, color = 'gray20', size = 0.2) +
    annotate("richtext", x = 79, y = 60, label = "JCPenney", fontface = 'bold', fill = NA, label.colour = NA, size = 6, color = 'gray20') +
    
    annotate("curve", x = 70, y = 85, xend = 83.4, yend = 84.4, arrow = arrow(length = unit(0.02, "npc"), type = "closed"), curvature = -0.3, color = 'gray20', size = 0.2) +
    annotate("richtext", x = 70, y = 84, label = "Trader Joe's", fontface = 'bold', fill = NA, label.colour = NA, size = 6, color = 'gray20') +
    
    # scales
    scale_color_manual(name="",
                       values = c("yes" = col_1, "no" = col_2),
                       labels = c("<span style='font-size:28pt; color:#E16723'>Retail</span>",
                                  "<span style='font-size:28pt; color:gray80'>Others</span>"),
                       guide = guide_legend(override.aes = list(size = 5))) +
    
    scale_x_continuous(
        breaks = seq(50, 80, by = 5),
        labels = label_text,
        limits = c(50, 85)) +
    
    scale_y_continuous(
        breaks = seq(50, 80, by = 5),
        labels = label_text,
        limits = c(50, 85)) +
    
    coord_cartesian(clip = 'off') +
    
    # labels
    labs(x = "Product & Services",
         y = "Growth",
         title    = '',
         subtitle = '',
         caption  = '') +
    
    # theme
    theme_void(18, base_family = 'text') +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        
        axis.title.x          = element_text(size = 22, 
                                             face = 'bold',
                                             hjust = 0.43,
                                             margin = margin(t = 10)), 
        axis.title.y          = element_text(size = 22, 
                                             face = 'bold',
                                             vjust = 0.43,
                                             margin = margin(r = 10)), 
        
        axis.text             = element_text(size = 18),
        
        panel.grid            = element_blank(),
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin           = margin(t = 10, r = 35, b = 10, l = 25),
        
        legend.background     = element_blank(),
        legend.position       = c(0, 0.95),
        legend.direction      = "horizontal",
        legend.title          = element_text(size = 16, lineheight = 1.3),
        legend.justification  = "left",
        legend.key            = element_blank(),
        legend.key.width      = unit(3, "pt"),
        legend.text           = element_markdown(margin = margin(r = 10))
    ) +
    guides(fill = "none")


# title plot
title_plot <- ggplot() +
    theme_void() +
    
    # title
    geom_textbox(aes(x = 0, y = 1),  family = 'title',
                  label       = title_text,
                  color       = title_col,
                  size        = 20, 
                  fill        = NA,     
                  box.color   = NA,
                  width       = unit(8, "in"),
                  vjust       = 0.5,
                  valign      = 0.5,
                  hjust       = 0.5, 
                  halign      = 0.5,
                  fontface    ="bold", 
                  lineheight  = 1) +
    
    # subtitle
    geom_textbox(aes(x = 0, y = 1),  family = 'title',
                 label       = subtitle_text,
                 color       = title_col,
                 size        = 14, 
                 fill        = NA,     
                 box.color   = NA,
                 width       = unit(8, "in"),
                 vjust       = 2.3,
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 fontface    ="bold", 
                 lineheight  = 1) +
    
    coord_cartesian(clip = "off") +
    
    theme(
    plot.background   = element_rect(fill = bkg_col, color = bkg_col),
    panel.background  = element_rect(fill = bkg_col, color = bkg_col),
        )


# text plot
text_plot <- ggplot() +
    theme_void() +
    
    # text
    geom_textbox(aes(x = 0, y = 1),  family = 'text',
                 label       = body_text,
                 color       = title_col,
                 size        = 7, 
                 fill        = NA, 
                 box.color   = NA,
                 width       = unit(5, "in"),
                 vjust       = 1.1,
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0,   
                 fontface    = 'plain',
                 ) +
    
     # caption
    geom_textbox(aes(x = 0, y = 0),  family = 'caption',
                 label       = caption_text,
                 color       = title_col,
                 size        = 6, 
                 fill        = NA, 
                 box.color   = NA,
                 vjust       = 0.3,
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0,
                 fontface    = 'plain',
                 width       = unit(5, "in"),
    ) +
    
    coord_cartesian(clip = "off") +
    
    theme(
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.title    = element_markdown(
            family    = 'title',
            color     = title_col,
            face      = "bold",
            size      = 5,
            lineheight= 0.5,
            margin    = margin(t = 5))
    )


# putting all plots together
(title_plot / text_plot) | main_plot +
    
    theme(plot.margin      = margin(10, 10, 10, 10))

# resolution
showtext_opts(dpi = 300)


## 6. Save final figure ----
ggsave('2022/Week_22/2022_22_reputation.png',
        width = 18, height = 10, units = 'in', dpi = 300)

showtext_auto(FALSE)

