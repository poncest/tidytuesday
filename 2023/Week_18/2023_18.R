 
## Challenge: #TidyTuesday 2023 week 18 
## Data:      The Portal Project
## Author:    Steven Ponce
## Date:      2023-05-01


## 1. LOAD PACKAGES & SETUP ----  
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, glue, camcorder, scales, lubridate, MetBrewer)


# |- figure size ---- 
gg_record(
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 6,
    height = 4,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 18) 

# plots   <- tt$plots %>% clean_names()
species <- tt$species %>% clean_names()
surveys <- tt$surveys %>% clean_names()

readme(tt)  
rm(tt)   
 

## 3. EXAMINING THE DATA ----
glimpse(species)  
glimpse(surveys)  


## 4. TIDYDATA ---- 
data_raw <- surveys %>% 
    inner_join(y = species, by = "species") 
    

data_plot <- data_raw %>% 
    group_by(year, treatment, commonname) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(
        treatment  = str_to_title(treatment),
        commonname = str_to_title(commonname)
        ) 

selected_rodents <- c("Desert Pocket Mouse")

# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- "#fcffff"
title_col    <- "gray10"              
subtitle_col <- "gray10" 
caption_col  <- "gray10" 
col_palette  <- met.brewer("Troy", n = 15, type = "continuous")[c(14,11,9)]


### |-  titles and caption ----

tt <- str_glue("#TidyTuesday: 2023 Week 18 &bull; Source: Portal Project Data 1978-2022<br>")  
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("The Portal Project, 1978-2022") 

subtitle_text <- str_glue("Long-term ecological research studying the dynamics of desert rodents, plants, ants,<br>and weather in Arizona. The plot below shows the count of **Desert Pocket Mouse** surveyed.") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Anton", family = "title") 
font_add_google("Titillium Web", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     


### |-  main plot ----
data_plot %>% 
    filter(commonname %in% selected_rodents) %>% 
    
    ggplot(aes(year, count, fill = treatment)) +
    
    # geoms
    geom_area()+
    
    # facets
    facet_wrap(~ treatment, ncol = 6)+
    
    # scale
    scale_fill_manual(values = col_palette, guide = "none") +

    scale_x_continuous(breaks = seq(1978, 2022, by = 40), 
                       limits = c(1978, 2022))+
    
    scale_y_continuous(breaks = seq(0, 500, by = 250), 
                       limits = c(0, 500))+
    
    coord_cartesian(clip = "off") +
    
    # labs
    labs(
        x       = "", 
        y       = "Count",
        title   = title_text,
        subtitle   = subtitle_text,
        caption = caption_text) +
    
    # theme 
    theme_minimal() +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background     = element_rect(fill = bkg_col, color = bkg_col),
        panel.background    = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin         = margin(t = 10, r = 10, b = 10, l = 10),
        
        axis.title          = element_text(size = 10, face = 'bold', margin = margin(t = 10)), 
        axis.text           = element_text(size = 10),
        
        axis.line.x         = element_line(color = "black"),
        panel.grid.major.y  = element_line(linetype = "dotted", size = 0.2, color = 'gray'),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        
        strip.text          = element_textbox(size    = 8,
                                             face     = 'bold',
                                             color    = "#0C0C0F",
                                             hjust    = 0.5,
                                             halign   = 0.5,
                                             r        = unit(5, "pt"),
                                             width    = unit(5.5, "npc"),
                                             padding  = margin(3, 0, 3, 0),
                                             margin   = margin(3, 3, 3, 3),
                                             fill     = "transparent"),
        
        panel.spacing       = unit(2, 'lines'),
        
        plot.title          = element_text(
            family          = 'title',
            color           = title_col,
            face            = "bold",
            size            = 20,  
            margin          = margin(t = 10, b = 5)),
        
        plot.subtitle       = element_markdown(
            family          = 'text',
            color           = title_col,
            #face            = "bold",
            size            = 10,  
            margin          = margin(t = 5, b = 10)),
        
        plot.caption        = element_markdown(
            family          = 'caption',
            color           = caption_col,
            lineheight      = 0.6,
            size            = 10,
            hjust           = 0.5,
            halign          = 0.5,
            margin          = margin(t = 10, b = 10)),
    )


sessionInfo()
## 6. SESSION INFO ---- 

# R version 4.3.0 (2023-04-21 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    

# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     

# ttached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     

# other attached packages:
# [1] MetBrewer_0.2.0    scales_1.2.1       camcorder_0.1.0    glue_1.6.2        
# [5] here_1.0.1         janitor_2.2.0      showtext_0.9-5     showtextdb_3.0    
# [9] sysfonts_0.8.8     ggtext_0.1.2       tidytuesdayR_1.0.2 lubridate_1.9.2   
# [13] forcats_1.0.0      stringr_1.5.0      dplyr_1.1.2        purrr_1.0.1       
# [17] readr_2.1.4        tidyr_1.3.0        tibble_3.2.1       ggplot2_3.4.2     
# [21] tidyverse_2.0.0    pacman_0.5.1      
# 
# loaded via a namespace (and not attached):
# [1] gtable_0.3.3      xfun_0.39         tzdb_0.3.0        vctrs_0.6.2      
# [5] tools_4.3.0       generics_0.1.3    parallel_4.3.0    curl_5.0.0       
# [9] gifski_1.6.6-1    fansi_1.0.4       pkgconfig_2.0.3   readxl_1.4.2     
# [13] lifecycle_1.0.3   farver_2.1.1      compiler_4.3.0    textshaping_0.3.6
# [17] munsell_0.5.0     snakecase_0.11.0  usethis_2.1.6     pillar_1.9.0     
# [21] crayon_1.5.2      magick_2.7.4      commonmark_1.9.0  tidyselect_1.2.0 
# [25] rvest_1.0.3       stringi_1.7.12    labeling_0.4.2    rsvg_2.4.0       
# [29] rprojroot_2.0.3   grid_4.3.0        colorspace_2.1-0  cli_3.6.1        
# [33] magrittr_2.0.3    utf8_1.2.3        withr_2.5.0       bit64_4.0.5      
# [37] timechange_0.2.0  httr_1.4.5        bit_4.0.5         cellranger_1.1.0 
# [41] ragg_1.2.5        hms_1.1.3         markdown_1.6      rlang_1.1.1      
# [45] gridtext_0.1.5    Rcpp_1.0.10       selectr_0.4-2     xml2_1.3.4       
# [49] renv_0.15.5       svglite_2.1.1     rstudioapi_0.14   vroom_1.6.3      
# [53] jsonlite_1.8.4    R6_2.5.1          

# RStudio 2023.03.0+386 "Cherry Blossom" Release (3c53477afb13ab959aeb5b34df1f10c237b256c3, 2023-03-09) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.03.0+386 Chrome/108.0.5359.179 Electron/22.0.3 Safari/537.36

