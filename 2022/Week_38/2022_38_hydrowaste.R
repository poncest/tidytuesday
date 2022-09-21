
## Challenge: #TidyTuesday 2022 week 38
## Data:      Hydro Waterwaste plants
## Author:    Steven Ponce
## Date:      2022-09-20   
 

## 1. LOAD PACKAGES & SETUP ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, glue, camcorder, scales)

gg_record(
    dir    = here("temp_plots"),
    device = "png",
    width  = 16,
    height = 10,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 38) 
hydrowaste <- tt$HydroWASTE_v10 %>% clean_names()
readme(tt) 
rm(tt)  


#|- offline ----   
# write_csv(hydrowaste, '2022/Week_38/hydrowaste.csv')
# hydrowaste <- read_csv(file = '2022/Week_38/hydrowaste.csv') %>% clean_names()


## 3. EXAMINING THE DATA ----
glimpse(hydrowaste) 
skim(hydrowaste)

unique(hydrowaste$country) %>% sort()
unique(hydrowaste$qual_loc) %>% sort()

summary_quality_tbl <- hydrowaste %>%
    group_by(qual_loc) %>% 
    summarise(total_quality = n()) %>% 
    ungroup()


## 4. TIDYDATA ---- 

# |- wastewater treatment plants ----
hydrowaste <- hydrowaste %>% 
    mutate(
        class = case_when(
            qual_loc == 1  ~ "normal",
            qual_loc == 2  ~ "normal",
            qual_loc == 3  ~ "problematic",
            TRUE           ~ "not analyzed"
        )
    )

# |-  world map ----
map <- map_data("world") %>% 
    filter(region != "Antarctica")



# 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 
map_col      <- "gray5"      
bkg_col      <- "gray30"  
title_col    <- "white"
subtitle_col <- "white"
caption_col  <- "white"
col_palette  <- c("normal" = "#75E4B3", "problematic" = "#BA1200", "not analyzed" = "#848C8E")


# |-  titles and caption ----
title_text    <- "Distribution of wastewater treatment plants"
                         
subtitle_text <- str_glue("Each dot represents a plant. The color corresponds to the accuracy of their\n reported testing.<br><br>",
                          "<span style='font-size:22pt; color:#75E4B3'>**Medium-High**</span>",
                          "<span style='font-size:22pt; color:gray30'>**&#8212; _**</span>",           # space between text
                          "<span style='font-size:22pt; color:#C42847'>**Low**</span>",
                          "<span style='font-size:22pt; color:gray30'>**&#8212; _**</span>",           # space between text
                          "<span style='font-size:22pt; color:#848C8E'>**Not Analyzed**</span>",
                          )

caption_text  <- str_glue("#TidyTuesday: 2022 Week 38 &bull; Source: Marcedo et al, 2022<br>",
                           "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                           "<span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                           "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                           "<span style='font-family:fa-brands'>&#x23;</span> ggplot")

# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Roboto", family = "title")                          
font_add_google("Roboto", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |- main plot ---- 
ggplot() +
    
    # world map
    geom_polygon(data = map, aes(x = long, y = lat, group = group),
                 color = map_col, fill = map_col) +
    
    # wastewater treatment plants 
    geom_point(data = hydrowaste, aes(x = lon_wwtp, y = lat_wwtp, color = class), 
               shape = 20, size = 0.04, alpha = 0.85) +                 
    
    # scales
    scale_color_manual(values = col_palette) +
    coord_fixed(1.3) +
    
    # labs
    labs(title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text) +
    
    # theme
    theme_void() +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 50,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_markdown(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.8, 
            face           = "bold",
            size           = 25,
            margin         = margin(t = 10, b = 30)),
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 12,
            hjust          = 0.5,
            margin         = margin(t = 10, b = 10)),
    )



## 6. SESSION INFO ---- 

# version.string R version 4.2.1 (2022-06-23 ucrt)
# nickname: Funny-Looking Kid 
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)

# Matrix products: default
 
# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    

# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     

# other attached packages:
# [1] scales_1.2.1         camcorder_0.0.2.9000 glue_1.6.2           skimr_2.1.4          here_1.0.1          
# [6] janitor_2.1.0        MetBrewer_0.2.0      showtext_0.9-5       showtextdb_3.0       sysfonts_0.8.8      
# [11] ggtext_0.1.1         tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0        dplyr_1.0.9         
# [16] purrr_0.3.4          readr_2.1.2          tidyr_1.2.0          tibble_3.1.7         ggplot2_3.3.6       
# [21] tidyverse_1.3.1      pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] fs_1.5.2          usethis_2.1.6     lubridate_1.8.0   bit64_4.0.5       httr_1.4.3        rprojroot_2.0.3  
# [7] repr_1.1.4        tools_4.2.1       backports_1.4.1   utf8_1.2.2        R6_2.5.1          DBI_1.1.3        
# [13] colorspace_2.0-3  withr_2.5.0       tidyselect_1.1.2  bit_4.0.4         curl_4.3.2        compiler_4.2.1   
# [19] textshaping_0.3.6 cli_3.3.0         rvest_1.0.2       xml2_1.3.3        labeling_0.4.2    askpass_1.1      
# [25] systemfonts_1.0.4 digest_0.6.29     base64enc_0.1-3   pkgconfig_2.0.3   htmltools_0.5.2   dbplyr_2.2.1     
# [31] fastmap_1.1.0     maps_3.4.0        rlang_1.0.4       readxl_1.4.0      rstudioapi_0.13   farver_2.1.1     
# [37] generics_0.1.3    jsonlite_1.8.0    vroom_1.5.7       magrittr_2.0.3    Rcpp_1.0.9        munsell_0.5.0    
# [43] fansi_1.0.3       lifecycle_1.0.1   stringi_1.7.8     snakecase_0.11.0  grid_4.2.1        parallel_4.2.1   
# [49] crayon_1.5.1      haven_2.5.0       gridtext_0.1.4    hms_1.1.1         magick_2.7.3      knitr_1.39       
# [55] pillar_1.8.1      markdown_1.1      reprex_2.0.1      pdftools_3.3.0    qpdf_1.2.0        gifski_1.6.6-1   
# [61] renv_0.15.5       modelr_0.1.8      vctrs_0.4.1       tzdb_0.3.0        selectr_0.4-2     cellranger_1.1.0 
# [67] gtable_0.3.0      assertthat_0.2.1  xfun_0.31         broom_1.0.0       rsvg_2.3.1        ragg_1.2.2       
# [73] ellipsis_0.3.2   


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36 


