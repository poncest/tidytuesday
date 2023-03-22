 
## Challenge: #TidyTuesday 2023 week 12
## Data:      Programming Languages
## Author:    Steven Ponce
## Date:      2023-03-20


## 1. LOAD PACKAGES & SETUP ----  
library(pacman)   
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, glue, camcorder, scales, lubridate)
p_load(ggdist, gghalves)


# figure size
gg_record(
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 8,
    height = 6,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 12) 

languages <- tt$languages %>% clean_names()

readme(tt) 
rm(tt)   
 

## 3. EXAMINING THE DATA ----
glimpse(languages)  
colnames(languages)

languages$type %>% unique()



## 4. TIDYDATA ----  

selected_languages <- c("pl", "queryLanguage")

languages_tbl <- languages %>% 
    filter(type %in% selected_languages) %>% 
    mutate(type = case_when(
        type == "queryLanguage" ~ "Query\nLanguage",
        type == "pl"            ~ "Programming\nLanguage",
    ))




# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- "#fcffff"
title_col    <- "gray10"              
subtitle_col <- "gray10" 
caption_col  <- "gray10" 

col_palette <- c("Programming\nLanguage" = "#8F2D56", "Query\nLanguage" = "#218380")


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 12 &bull; Source: Programming Language DataBase<br>")  
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text <- str_glue("Distribution of Selected Programming Languages Type, 1974 - 2018") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Anton", family = "title") 
font_add_google("Titillium Web", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     
                

### |-  main plot ----
languages_tbl %>% 
    ggplot(aes(x = as_factor(type), y = appeared, 
               fill =  type, color = type)) +
    
    geom_boxplot(
        width = 0.2, 
        fill  = "white",
        size  = 0.6, 
        outlier.shape = NA
    ) +
    
    stat_halfeye(
        adjust = 0.33, 
        width  = 0.67,
        color  = NA, 
        position = position_nudge(x = 0.15)
    ) +
    
    geom_half_point(
        side  = "l",
        alpha = 0.3, 
        size  = 0.8,
        range_scale = 0.3,
    ) +
    
    # scale
    scale_fill_manual(values = col_palette, guide = "none") +
    scale_color_manual(values = col_palette, guide = "none") +
    
    scale_y_continuous(breaks = seq(1940, 2025, by = 20), 
                       limits = c(1940, 2023)) +
    
    scale_x_discrete(expand = c(0.07, 0.07)) +
    
    coord_flip(clip = 'off')+
    
    # labs
    labs(
        x        = "", 
        y        = "",
        title    = title_text,
        caption  = caption_text) +
    
    
    # theme 
    theme_minimal() +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background     = element_rect(fill = bkg_col, color = bkg_col),
        panel.background    = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin         = margin(t = 10, r = 10, b = 10, l = 10),
        
        axis.title          = element_text(size = 14, face = 'bold', margin = margin(t = 10)), 
        axis.text           = element_text(size = 12),
        
        axis.line.x         = element_line(color = "black"),
        panel.grid.major.x  = element_line(linetype = "dotted", size = 0.2, color = 'gray'),
        panel.grid.major.y  = element_blank(),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 20,  
            margin         = margin(t = 10, b = 10)),
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col,
            lineheight     = 0.6,
            size           = 10,
            hjust          = 0.5,
            halign         = 0.5,
            margin         = margin(t = 10, b = 10)),
    )



 
sessionInfo()
## 6. SESSION INFO ---- 

# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)

# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    

# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     

# other attached packages:
# [1] gghalves_0.1.4     ggdist_3.1.1       lubridate_1.8.0    scales_1.2.1       camcorder_0.1.0    glue_1.6.2        
# [7] here_1.0.1         janitor_2.1.0      showtext_0.9-5     showtextdb_3.0     sysfonts_0.8.8     ggtext_0.1.2      
# [13] tidytuesdayR_1.0.2 forcats_0.5.1      stringr_1.5.0      dplyr_1.1.0        purrr_1.0.1        readr_2.1.2      
# [19] tidyr_1.3.0        tibble_3.1.8       ggplot2_3.4.1      tidyverse_1.3.2    pacman_0.5.1      

# loaded via a namespace (and not attached):
# [1] httr_1.4.3           bit64_4.0.5          vroom_1.6.1          jsonlite_1.8.0       modelr_0.1.8        
# [6] assertthat_0.2.1     distributional_0.3.0 selectr_0.4-2        renv_0.15.5          googlesheets4_1.0.0 
# [11] cellranger_1.1.0     pillar_1.8.1         backports_1.4.1      gridtext_0.1.4       rvest_1.0.2         
# [16] snakecase_0.11.0     colorspace_2.1-0     pkgconfig_2.0.3      broom_1.0.0          gifski_1.6.6-1      
# [21] haven_2.5.0          magick_2.7.3         svglite_2.1.1        tzdb_0.3.0           googledrive_2.0.0   
# [26] generics_0.1.3       farver_2.1.1         usethis_2.1.6        ellipsis_0.3.2       withr_2.5.0         
# [31] cli_3.6.0            magrittr_2.0.3       crayon_1.5.2         readxl_1.4.0         fs_1.6.0            
# [36] fansi_1.0.4          xml2_1.3.3           textshaping_0.3.6    tools_4.2.2          hms_1.1.2           
# [41] gargle_1.2.0         lifecycle_1.0.3      munsell_0.5.0        reprex_2.0.1         compiler_4.2.2      
# [46] systemfonts_1.0.4    rlang_1.0.6          grid_4.2.2           rstudioapi_0.13      rsvg_2.3.1          
# [51] labeling_0.4.2       gtable_0.3.1         DBI_1.1.3            curl_5.0.0           markdown_1.1        
# [56] R6_2.5.1             bit_4.0.5            utf8_1.2.3           rprojroot_2.0.3      ragg_1.2.2          
# [61] stringi_1.7.12       parallel_4.2.2       Rcpp_1.0.10          vctrs_0.5.2          dbplyr_2.2.1        
# [66] tidyselect_1.2.0      

# RStudio 2022.12.0+353 "Elsbeth Geranium" Release (7d165dcfc1b6d300eb247738db2c7076234f6ef0, 2022-12-03) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2022.12.0+353 Chrome/102.0.5005.167 Electron/19.1.3 Safari/537.36

