
## Challenge: #TidyTuesday 2022 week 30
## Data:      Bring Your Own Data (BYOD)
## Author:    Steven Ponce
## Date:      2022-07-26 

# Decided to use #TidyTuesday 2021 week 15 - Deforestation (soybean)

## 1. LOAD PACKAGES & SETUP ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, camcorder, ggrepel)

gg_record(
    dir    = here("temp_plots"),
    device = "png", 
    width  = 10, 
    height = 8, 
    units  = "in", 
    dpi    = 600)

# |- resolution ----
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2021, week = 15) 
soybean_use <- tt$soybean_use %>% clean_names()
rm(tt)


#|- offline ----
# write_csv(soybean_use, '2022/Week_30/soybean_use.csv')
# soybean_use <- read_csv(file = '2022/Week_30/soybean_use.csv')


### 3. EXAMINING THE DATA ----
dim(soybean_use) 
glimpse(soybean_use)
colnames(soybean_use)
range(soybean_use$year)
unique(soybean_use$entity) %>% sort()


## 4. TIDYDATA ----
soybean_use_world <- soybean_use %>% 
    filter(entity == "World") %>% 
    
    pivot_longer(
        cols      = c("human_food", "animal_feed", "processed"),
        names_to  = 'soybean_use',
        values_to = 'production_Kg'
                ) %>% 
    
    mutate(soybean_use = recode(soybean_use, 
                                "human_food"  = "Direct human food",
                                "animal_feed" = "Direct animal feed",
                                "processed"   = "Processed",
                                )) %>% 
    
    mutate(production_Mt = production_Kg / 1E6) 

 
# 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 
bkg_col        <- '#F6F6F6'   
title_col      <- 'black'
subtitle_col   <- "black"
caption_col    <- "black"
palette_col    <- met.brewer("Veronese", n = 7, type = "discrete")[c(6,1,3)]
# monochromeR::generate_palette("brown", modification = "go_lighter", n_colours = 10, view_palette = T)


# |-  titles and caption ----
title_text    <- 'The golden bean uses: food, feed or fuel?'

subtitle_text <- "Soybean, World data, 1961-2013"

caption_text  <- str_glue("**#TidyTuesday:** 2022 Week 30 • **Source:** Our World in Data<br>",
                          "**Visualization:** Steven Ponce (@sponce1) • **Tools:** #rstats, #ggplot")


# |-  fonts ----
font_add_google("Roboto Condensed", family = "title")
font_add_google("Roboto Condensed", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE) 
 

# |-  main plot ----
soybean_use_world %>% 
    ggplot(aes(x = year, y = production_Mt, color = soybean_use)) +
    
    # geoms
    geom_line(size = 1.25) +
    
    geom_label_repel(aes(label = soybean_use),
               data = soybean_use_world %>% filter(year == max(year)),
               position         = "identity",
               family           = "text",
               fontface         = "bold",
               size             = 4,
               xlim             = c(2013.8, NA),
               segment.size     = 0.5,
               segment.alpha    = 0.5,
               segment.linetype = "dotted",
               box.padding      = 0.1,
               ) +
    
    # scales
    scale_x_continuous(breaks = seq(1961, 2013, by = 10),
                       limits = c(1961, 2020),
                       expand = c(0.03, 0.03)) +

    scale_y_continuous(breaks = seq(0, 250, by = 50),
                       limits = c(0, 250),
                       expand = c(0.03, 0.03),
                       labels = c(0,50,100,150,200,'250 Metric\nTons')) +
    
    coord_cartesian(clip = "off") +
    
    # color
    scale_color_manual(values = palette_col) +
    
    # labs
    labs(
        x = '', y = '',
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text) +

    # theme
    theme_minimal(
        base_size   = 16,
        base_family = 'text') +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title.y     = element_text(size = 16,
                                        face = 'bold',
                                        margin = margin (r = 10)),
        
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black"),
        
        
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.4, color = 'gray'),
        
        plot.margin        = margin(t = 25, r = 25, b = 25, l = 25),

        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 32,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_text(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.6, 
            face           = "plain",
            size           = 18,
            margin         = margin(t = 10, b = 10)),
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 12,
            hjust          = 1.0,
            margin         = margin(t = 10, b = 10)),
    )
 

## 6. SESSION INFO ----
# sessionInfo()

# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)

# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    

# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     

# other attached packages:
# [1] ggrepel_0.9.1        camcorder_0.0.2.9000 skimr_2.1.4          here_1.0.1           janitor_2.1.0       
# [6] MetBrewer_0.2.0      showtext_0.9-5       showtextdb_3.0       sysfonts_0.8.8       ggtext_0.1.1        
# [11] tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0        dplyr_1.0.9          purrr_0.3.4         
# [16] readr_2.1.2          tidyr_1.2.0          tibble_3.1.7         ggplot2_3.3.6        tidyverse_1.3.1     
# [21] pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] fs_1.5.2         usethis_2.1.6    lubridate_1.8.0  bit64_4.0.5      httr_1.4.3       rprojroot_2.0.3 
# [7] repr_1.1.4       tools_4.2.1      backports_1.4.1  utf8_1.2.2       R6_2.5.1         DBI_1.1.3       
# [13] colorspace_2.0-3 withr_2.5.0      tidyselect_1.1.2 bit_4.0.4        curl_4.3.2       compiler_4.2.1  
# [19] cli_3.3.0        rvest_1.0.2      xml2_1.3.3       scales_1.2.0     askpass_1.1      digest_0.6.29   
# [25] base64enc_0.1-3  pkgconfig_2.0.3  htmltools_0.5.2  dbplyr_2.2.1     fastmap_1.1.0    rlang_1.0.3     
# [31] readxl_1.4.0     rstudioapi_0.13  generics_0.1.3   farver_2.1.1     jsonlite_1.8.0   vroom_1.5.7     
# [37] magrittr_2.0.3   Rcpp_1.0.9       munsell_0.5.0    fansi_1.0.3      lifecycle_1.0.1  stringi_1.7.8   
# [43] snakecase_0.11.0 grid_4.2.1       parallel_4.2.1   crayon_1.5.1     haven_2.5.0      gridtext_0.1.4  
# [49] hms_1.1.1        magick_2.7.3     knitr_1.39       pillar_1.7.0     markdown_1.1     reprex_2.0.1    
# [55] glue_1.6.2       pdftools_3.3.0   qpdf_1.2.0       gifski_1.6.6-1   renv_0.15.5      modelr_0.1.8    
# [61] vctrs_0.4.1      tzdb_0.3.0       selectr_0.4-2    cellranger_1.1.0 gtable_0.3.0     assertthat_0.2.1
# [67] xfun_0.31        broom_1.0.0      rsvg_2.3.1       ellipsis_0.3.2  

# RStudio 2021.09.0+351 "Ghost Orchid" Release (077589bcad3467ae79f318afe8641a1899a51606, 2021-09-20) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36


