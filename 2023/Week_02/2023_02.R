 
## Challenge: #TidyTuesday 2023 week 02
## Data:      Bird FeederWatch data
## Author:    Steven Ponce
## Date:      2023-01-10 
 

## 1. LOAD PACKAGES & SETUP ----  
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, skimr, glue, camcorder, scales)


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
tt <- tidytuesdayR::tt_load(2023, week = 02) 


PFW_2021_public <- tt$PFW_2021_public %>% clean_names()
PFW_count_site_data_public_2021 <- tt$PFW_count_site_data_public_2021 %>% clean_names()

readme(tt) 
rm(tt)  


#|- offline ----   
# write_csv(PFW_2021_public, '2023/Week_00/PFW_2021_public.csv')
# write_csv(PFW_count_site_data_public_2021, '2023/Week_00/PFW_count_site_data_public_2021.csv')

# PFW_2021_public <- read_csv(file = '2023/Week_00/PFW_2021_public.csv') %>% clean_names()
# PFW_count_site_data_public_2021 <- read_csv(file = '2023/Week_00/PFW_count_site_data_public_2021.csv') %>% clean_names()

## 3. EXAMINING THE DATA ----
glimpse(PFW_2021_public) 
glimpse(PFW_count_site_data_public_2021) 

PFW_2021_public$species_code %>% unique()
PFW_2021_public$subnational1_code %>% unique() %>% sort()


## 4. TIDYDATA ---- 
birds <- PFW_2021_public %>% 
    select(loc_id:subnational1_code, month, day, year, species_code, how_many, valid) %>% 
    
    # filter for all blue jay species 
    filter(species_code == "blujay") %>% 
    
    # filter for US data only
    separate(col = subnational1_code, into = c("country", "state")) %>% 
    filter(country == "US") %>%     
    
    # filter for the state of NJ
    filter(state == "NJ") %>% 
    
    # add labels (for clarification)
    mutate(common_name = case_when(
        species_code == "blujay" ~ "blue jay")
    ) %>% 
    
    # blue jays count
    group_by(loc_id, latitude, longitude, state, year) %>% 
    summarise(total_birds = sum(how_many)) %>% 
    ungroup() 
 

# |-  NJ map ----

# base map
base_map <- map_data("state") %>% 
    filter(region == "new jersey")

# counties
nj_counties <- map_data("county")  %>% 
    filter(region == "new jersey")



# 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 
map_col      <- "#8c78d0"
bkg_col      <- "#1e193c"
        
title_col    <-  "white"              
subtitle_col <-  "white" 
caption_col  <-  "white" 
    
                    
# |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 02 &bull; Source: FeederWatch<br>")
tw <- str_glue("<span style='font-family:fa-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")

title_text <- str_glue("Project FeederWatch")

subtitle_text    <- str_glue("Blue Jay Sightings in New Jersey")  

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 | {gh} poncest | Tools: #rstats #ggplot")


# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Jost", family = "title")                          
font_add_google("Jost", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  



# |- main plot ---- 
ggplot() + 
    
    # NJ base map
    geom_polygon(data = base_map, aes(x = long, y = lat, group = region),
                  color = "gray90", fill = map_col) +
    
    # NJ counties
    geom_polygon(data = nj_counties, aes(x = long, y = lat, group = group), 
                 fill = NA, color = "gray70", size = 0.3) +
    
    # blue jays 
    geom_point(data = birds, aes(x = longitude, y = latitude, size = total_birds), 
               shape = 21,  alpha = 0.85,
               fill = "#00A6FB",
               color="gray90") +
    
    # scales
    coord_sf(clip = "off") +
   
    # facets
    facet_wrap(~ year) +
        
    # labs 
    labs(
        x = '', 
        y = '',
        size     = "Count",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text
    ) +
    
    # theme
    theme_void() +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        
        legend.position       = 'top',
        legend.title          = element_text(color = "white", size = 14, ),
        legend.text           = element_text(color = "white", size = 14),
    
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
        
        strip.text            = element_textbox(size     = 18, 
                                               face     = 'bold', 
                                               color = "white",
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
            size           = 35,  
            margin         = margin(t = 5, b = 5)),
        
        plot.subtitle      = element_text(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.8, 
            face           = "bold",
            size           = 20,
            margin         = margin(b = 10)),
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 12,
            hjust          = 0.5, 
            halign         = 0.5,
            margin         = margin(t = 10, b = 5)),
    ) 


## 6. SESSION INFO ---- 

# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] maps_3.4.0           scales_1.2.1         camcorder_0.0.2.9000 glue_1.6.2           skimr_2.1.4         
# [6] here_1.0.1           janitor_2.1.0        showtext_0.9-5       showtextdb_3.0       sysfonts_0.8.8      
# [11] ggtext_0.1.1         tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0        dplyr_1.0.9         
# [16] purrr_0.3.4          readr_2.1.2          tidyr_1.2.0          tibble_3.1.7         ggplot2_3.3.6       
# [21] tidyverse_1.3.1      pacman_0.5.1        
# 
# loaded via a namespace (and not attached):
# [1] fs_1.5.2           sf_1.0-9           usethis_2.1.6      lubridate_1.8.0    bit64_4.0.5        httr_1.4.3        
# [7] rprojroot_2.0.3    repr_1.1.4         tools_4.2.2        backports_1.4.1    utf8_1.2.2         R6_2.5.1          
# [13] KernSmooth_2.23-20 DBI_1.1.3          colorspace_2.0-3   withr_2.5.0        tidyselect_1.1.2   bit_4.0.4         
# [19] curl_4.3.2         compiler_4.2.2     textshaping_0.3.6  cli_3.4.1          rvest_1.0.2        xml2_1.3.3        
# [25] labeling_0.4.2     classInt_0.4-8     proxy_0.4-27       askpass_1.1        systemfonts_1.0.4  digest_0.6.29     
# [31] base64enc_0.1-3    pkgconfig_2.0.3    htmltools_0.5.2    dbplyr_2.2.1       fastmap_1.1.0      rlang_1.0.4       
# [37] readxl_1.4.0       rstudioapi_0.13    farver_2.1.1       generics_0.1.3     jsonlite_1.8.0     vroom_1.5.7       
# [43] magrittr_2.0.3     Rcpp_1.0.9         munsell_0.5.0      fansi_1.0.3        lifecycle_1.0.1    stringi_1.7.8     
# [49] snakecase_0.11.0   grid_4.2.2         crayon_1.5.1       haven_2.5.0        gridtext_0.1.4     hms_1.1.1         
# [55] magick_2.7.3       knitr_1.39         pillar_1.8.1       markdown_1.1       reprex_2.0.1       pdftools_3.3.0    
# [61] qpdf_1.2.0         gifski_1.6.6-1     renv_0.15.5        modelr_0.1.8       vctrs_0.4.1        tzdb_0.3.0        
# [67] selectr_0.4-2      cellranger_1.1.0   gtable_0.3.0       assertthat_0.2.1   xfun_0.31          broom_1.0.0       
# [73] e1071_1.7-12       rsvg_2.3.1         class_7.3-20       ragg_1.2.2         units_0.8-1        ellipsis_0.3.2    


# RStudio 2022.12.0+353 "Elsbeth Geranium" Release (7d165dcfc1b6d300eb247738db2c7076234f6ef0, 2022-12-03) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2022.12.0+353 Chrome/102.0.5005.167 Electron/19.1.3 Safari/537.36

