 
## Challenge: #TidyTuesday 2023 week 05
## Data:      Pet Cats UK
## Author:    Steven Ponce
## Date:      2023-01-30
 

## 1. LOAD PACKAGES & SETUP ----  
library(pacman)   
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, glue, camcorder, scales)
p_load(lubridate, treemapify, MetBrewer)   



# figure size
gg_record(
    dir    = here("temp_plots"), 
    device = "png",
    width  = 8,
    height = 6,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 05) 

cats_uk <- tt$cats_uk %>% clean_names()
cats_uk_reference <- tt$cats_uk_reference %>% clean_names()

readme(tt) 
rm(tt)   


## 3. EXAMINING THE DATA ----
glimpse(cats_uk ) 
glimpse(cats_uk_reference) 

cats_uk$tag_id %>% unique() %>% sort()
cats_uk_reference$tag_id %>% unique() %>% sort()

# left join
cats_uk_tbl <- cats_uk %>% 
    left_join(y = cats_uk_reference, by = "tag_id") 

# save memory
rm(cats_uk, cats_uk_reference)


## 4. TIDYDATA ---- 

data_tbl <- cats_uk_tbl %>% 
    # filter out outliers
    filter(
        algorithm_marked_outlier != TRUE,
        manually_marked_outlier != TRUE) %>% 
    # date
    mutate(date = as.Date(timestamp)) %>% 
    # remove NAs
    drop_na() %>% 
    
    group_by(animal_id, animal_reproductive_condition, animal_sex) %>% 
    summarise(
        avg_hrs_indoor = mean(hrs_indoors),
        avg_age_years  = mean(age_years),
        avg_prey_p_month = mean(prey_p_month),
        avg_ground_speed = mean(ground_speed),
        ) %>% 
    ungroup()

glimpse(data_tbl)
summary(data_tbl)


# 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 

bkg_col      <- "#F1F1F1"
title_col    <-  "gray10"              
subtitle_col <-  "gray10" 
caption_col  <-  "gray10" 
palette_col  <- met.brewer("VanGogh1", n = 5, type = "continuous")[c(5:1)]


# |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 05 &bull; Source: Movebank for Animal Tracking Data<br>")
tw <- str_glue("<span style='font-family:fa-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")

title_text <- str_glue("Pet Cats in the United Kingdom")

subtitle_text <- str_glue("Average Indoor Time by Cat's Gender") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 | {gh} poncest | Tools: #rstats #ggplot")


# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Playfair Display", family = "title")                          
font_add_google("Playfair Display", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)      
                

# |-  main plot ----
data_tbl %>%    
    ggplot(aes(area     = avg_hrs_indoor, 
               fill     = factor(avg_hrs_indoor),               
               subgroup = animal_sex,
               label    = paste(animal_id, avg_hrs_indoor, sep = "\n"))) +
    
    # geoms
    geom_treemap() +
    
    ## subgroups
    geom_treemap_subgroup_border(colour = "white", size = 5) +
    
    geom_treemap_subgroup_text(place    = "centre", 
                               reflow   = TRUE,
                               layout   = 'scol',
                               grow     = TRUE,
                               alpha    = 0.25, 
                               colour   = "black",
                               fontface = "italic") +
    
    ## group
    geom_treemap_text(colour = "#e5e1e1e1",
                      place  = "centre",
                      size   = 15,
                      grow   = TRUE) +
    
    # labs
    labs(x = "", y = "",
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text
    ) + 
    
    # scale
    scale_fill_manual(values = palette_col,
                      name = "Average Time\nIndoors (Hr)")+ 
    
    #theme
    theme_void() +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'top',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_blank(),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 36,  
            margin         = margin(t = 5, b = 5)),
        
        plot.subtitle      = element_text(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.8, 
            face           = "bold",
            size           = 24,
            margin         = margin(b = 10)),
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 10,
            hjust          = 0.5, 
            halign         = 0.5,
            margin         = margin(t = 10, b = 5)),
    )



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
# [1] MetBrewer_0.2.0    treemapify_2.5.5   ggbeeswarm_0.7.1   lubridate_1.8.0    scales_1.2.1       camcorder_0.1.0   
# [7] glue_1.6.2         here_1.0.1         janitor_2.1.0      showtext_0.9-5     showtextdb_3.0     sysfonts_0.8.8    
# [13] ggtext_0.1.2       tidytuesdayR_1.0.2 forcats_0.5.1      stringr_1.4.0      dplyr_1.0.9        purrr_0.3.4       
# [19] readr_2.1.2        tidyr_1.2.0        tibble_3.1.7       ggplot2_3.3.6      tidyverse_1.3.2    pacman_0.5.1      

# loaded via a namespace (and not attached):
# [1] httr_1.4.3          viridisLite_0.4.0   bit64_4.0.5         vroom_1.6.1         jsonlite_1.8.0      modelr_0.1.8       
# [7] assertthat_0.2.1    selectr_0.4-2       renv_0.15.5         vipor_0.4.5         googlesheets4_1.0.0 cellranger_1.1.0   
# [13] pillar_1.8.1        backports_1.4.1     digest_0.6.29       gridtext_0.1.4      rvest_1.0.2         snakecase_0.11.0   
# [19] colorspace_2.0-3    pkgconfig_2.0.3     broom_1.0.0         gifski_1.6.6-1      haven_2.5.0         magick_2.7.3       
# [25] svglite_2.1.1       tzdb_0.3.0          googledrive_2.0.0   farver_2.1.1        generics_0.1.3      usethis_2.1.6      
# [31] ellipsis_0.3.2      withr_2.5.0         cli_3.6.0           magrittr_2.0.3      crayon_1.5.2        readxl_1.4.0       
# [37] fs_1.6.0            fansi_1.0.4         xml2_1.3.3          beeswarm_0.4.0      textshaping_0.3.6   tools_4.2.2        
# [43] hms_1.1.2           gargle_1.2.0        lifecycle_1.0.3     munsell_0.5.0       reprex_2.0.1        compiler_4.2.2     
# [49] systemfonts_1.0.4   rlang_1.0.6         grid_4.2.2          rstudioapi_0.13     rsvg_2.3.1          labeling_0.4.2     
# [55] gtable_0.3.0        DBI_1.1.3           curl_4.3.2          markdown_1.1        R6_2.5.1            bit_4.0.5          
# [61] utf8_1.2.2          rprojroot_2.0.3     ragg_1.2.2          stringi_1.7.12      parallel_4.2.2      Rcpp_1.0.10        
# [67] ggfittext_0.9.1     vctrs_0.5.2         dbplyr_2.2.1        tidyselect_1.2.0      

# RStudio 2022.12.0+353 "Elsbeth Geranium" Release (7d165dcfc1b6d300eb247738db2c7076234f6ef0, 2022-12-03) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2022.12.0+353 Chrome/102.0.5005.167 Electron/19.1.3 Safari/537.36

