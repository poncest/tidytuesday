 
## Challenge: #TidyTuesday 2023 week 10
## Data:      Numbats in Australia
## Author:    Steven Ponce
## Date:      2023-03-06


## 1. LOAD PACKAGES & SETUP ----  
library(pacman)   
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, glue, camcorder, scales)
p_load(ozmaps, sf, paletteer) 


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
tt <- tidytuesdayR::tt_load(2023, week = 10) 

numbats <- tt$numbats %>% clean_names()

readme(tt) 
rm(tt)   
 

## 3. EXAMINING THE DATA ----
glimpse(numbats)  
colnames(numbats)
numbats$data_resource_name %>% unique()
numbats$scientific_name %>% unique()
numbats$dryandra %>% unique()



## 4. TIDYDATA ----  

# data for viz
data_plot <- numbats %>% 
    select(scientific_name, decimal_latitude:decimal_longitude, year:tmin) %>% 
    group_by(year, decimal_longitude, decimal_latitude) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    arrange(desc(year)) %>%
    drop_na(year)



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- "#435e55"
map_col      <- "#EBBB7FFF"
yrs_col      <- "#b4b4b4"
title_col    <- "#d1bc1c"              
subtitle_col <- "#fcffff" 
caption_col  <- "#fcffff" 

# col_palette <- paletteer_d(`"palettetown::blastoise"`, 9, type = "continuous")


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 10 &bull; Source: Atlas of Living Australia<br>")  
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text <- str_glue("Numbats Observations in Australia, 1856 - 2023") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Roboto", family = "title") 
font_add_google("Roboto", family = "subtitle")                    
font_add_google("Roboto", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     
                

### |-  AUS States ----
sf_oz <- ozmap("states")

### |-  Main Plot ----
sf_oz %>% 
    ggplot() +
    
    # geoms
    geom_sf(fill = map_col, color = 'gray10', alpha = 0.2)+
    
    geom_point(data = data_plot,
               aes(x = decimal_longitude, y = decimal_latitude, size = count),
               shape = 21,
               alpha = 0.5,
               fill  = "#b55300",
               color = "gray90" ,
               na.rm = TRUE, 
               show.legend = FALSE) + 
    
    # scales
    # scale_fill_manual(values = col_palette) + 
    scale_size_continuous(range = c(1, 3)) +
    coord_sf(clip = "off") +
    
    # facet
    facet_wrap(~ year) +
    
    # labs
    labs(
        x        = "", 
        y        = "",
        title    = title_text,
        caption  = caption_text)+
    
    # theme
    theme_void() +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background     = element_rect(fill = bkg_col, color = bkg_col),
        panel.background    = element_rect(fill = bkg_col, color = bkg_col),
        
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        
        plot.margin         = margin(t = 10, r = 10, b = 10, l = 10),
        
        strip.text          = element_textbox(size     = 11, 
                                              face     = 'bold', 
                                              color    = yrs_col, 
                                              hjust    = 0.5,
                                              halign   = 0.5,
                                              r        = unit(5, "pt"), 
                                              width    = unit(0.5, "npc"),
                                              padding  = margin(3, 0, 3, 0), 
                                              margin   = margin(3, 3, 3, 3),
                                              fill     = "transparent"),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 18,  
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
# [1] paletteer_1.5.0    sf_1.0-9           ozmaps_0.4.5       scales_1.2.1       camcorder_0.1.0   
# [6] glue_1.6.2         here_1.0.1         janitor_2.1.0      showtext_0.9-5     showtextdb_3.0    
# [11] sysfonts_0.8.8     ggtext_0.1.2       tidytuesdayR_1.0.2 forcats_0.5.1      stringr_1.5.0     
# [16] dplyr_1.1.0        purrr_1.0.1        readr_2.1.2        tidyr_1.3.0        tibble_3.1.8      
# [21] ggplot2_3.4.1      tidyverse_1.3.2    pacman_0.5.1      

# loaded via a namespace (and not attached):
# [1] httr_1.4.3          bit64_4.0.5         vroom_1.6.1         jsonlite_1.8.0      modelr_0.1.8       
# [6] assertthat_0.2.1    selectr_0.4-2       renv_0.15.5         googlesheets4_1.0.0 cellranger_1.1.0   
# [11] pillar_1.8.1        backports_1.4.1     gridtext_0.1.4      rvest_1.0.2         snakecase_0.11.0   
# [16] colorspace_2.1-0    pkgconfig_2.0.3     broom_1.0.0         gifski_1.6.6-1      haven_2.5.0        
# [21] magick_2.7.3        svglite_2.1.1       tzdb_0.3.0          oz_1.0-22           proxy_0.4-27       
# [26] googledrive_2.0.0   generics_0.1.3      usethis_2.1.6       ellipsis_0.3.2      withr_2.5.0        
# [31] cli_3.6.0           magrittr_2.0.3      crayon_1.5.2        readxl_1.4.0        fs_1.6.0           
# [36] fansi_1.0.4         xml2_1.3.3          class_7.3-21        tools_4.2.2         hms_1.1.2          
# [41] gargle_1.2.0        lifecycle_1.0.3     munsell_0.5.0       reprex_2.0.1        compiler_4.2.2     
# [46] e1071_1.7-12        systemfonts_1.0.4   rlang_1.0.6         units_0.8-1         classInt_0.4-8     
# [51] grid_4.2.2          rstudioapi_0.13     rsvg_2.3.1          gtable_0.3.1        DBI_1.1.3          
# [56] curl_5.0.0          rematch2_2.1.2      R6_2.5.1            lubridate_1.8.0     bit_4.0.5          
# [61] utf8_1.2.3          rprojroot_2.0.3     KernSmooth_2.23-20  stringi_1.7.12      parallel_4.2.2     
# [66] Rcpp_1.0.10         vctrs_0.5.2         dbplyr_2.2.1        tidyselect_1.2.0      

# RStudio 2022.12.0+353 "Elsbeth Geranium" Release (7d165dcfc1b6d300eb247738db2c7076234f6ef0, 2022-12-03) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2022.12.0+353 Chrome/102.0.5005.167 Electron/19.1.3 Safari/537.36

