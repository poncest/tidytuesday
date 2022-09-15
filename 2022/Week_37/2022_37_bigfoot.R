
## Challenge: #TidyTuesday 2022 week 37
## Data:      Bigfoot
## Author:    Steven Ponce
## Date:      2022-09-13   
 

## 1. LOAD PACKAGES & SETUP ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, glue, camcorder, scales, ggbeeswarm)

gg_record(
    dir    = here("temp_plots"),
    device = "png",
    width  = 10,
    height = 10,
    units  = "in",
    dpi    = 600)

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 37) 
bigfoot <- tt$bigfoot %>% clean_names()
rm(tt) 


#|- offline ----   
# write_csv(bigfoot, '2022/Week_37/bigfoot.csv')
# bigfoot <- read_csv(file = '2022/Week_37/bigfoot.csv') %>% clean_names()


## 3. EXAMINING THE DATA ----
glimpse(bigfoot) 
skim(bigfoot)


## 4. TIDYDATA ---- 

# |-  main df ----
state_season_tbl <- bigfoot %>% 
    filter(season != "Unknown") %>% 
    group_by(state, season) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    arrange(desc(count)) %>% 
    mutate(
        highlight = if_else(condition = count > 150, true = "yes", false = "no"),
        season    = as_factor(season) %>% factor(levels = (c("Spring", "Summer", "Fall", "Winter")))
        )


# |-  labels df ----
label_df <- state_season_tbl %>% 
    slice(1:3) %>% 
    mutate(label_text = str_glue("{state}\n{count}"))

    
# 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 
col_1        <- "white"
bkg_col      <- "gray20"  
title_col    <- "white"
subtitle_col <- "white"
caption_col  <- "white"
col_palette  <- c("no" = "gray50", "yes" = "#F4CDA5")

# |-  titles and caption ----
title_text    <- "Bigfoot Reported Sightings in the United States"

subtitle_text <- str_wrap("Your best chance to “see” Bigfoot are in the states of Washington and California during Summer and Fall.", width = 70)

caption_text  <- str_glue("#TidyTuesday: 2022 Week 37 &bull; Source: Data.World<br>",
                           "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                           " <span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                           "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                           "<span style='font-family:fa-brands'>&#x23;</span> ggplot")

# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Ranchers", family = "title")                          
font_add_google("Roboto", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  

# |-  main plot ----
state_season_tbl %>% 
    ggplot(aes(season, count, color = highlight)) +
    
    # geometries
    geom_beeswarm(cex = 1.3, size = 2) +                       
    geom_beeswarm(data = label_df, aes(cex = 1.3, size = 2)) +
    
    # text labels - state and number of observations
    geom_text(data = label_df, aes(x = season, y = count + 10, label = state), 
              size = 6, fontface = "bold", hjust = -0.5, vjust = 0) +
    
    geom_text(data = label_df, aes(x = season, y = count + 0, label = count), 
              size = 6, fontface = "bold", hjust = -2.5, vjust = 0) +
    
    # scales
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(0, 250, by = 50),
                       limits = c(0, 250)) +
    scale_color_manual(values = col_palette) +
    
    # labs
    labs(x        = "", 
         y        = "Number of\nObservations",
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text) +
    
    # theme
    theme_minimal(
        base_size   = 16,
        base_family = 'text'
        ) +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.text.x        = element_text(colour = "white"),
        axis.text.y        = element_blank(),
        
        axis.title.x       = element_text(colour = "white"),
        axis.title.y       = element_text(colour = "white", angle = 0, vjust = 0.85, hjust = 0.5),
        
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_blank(),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 35,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_text(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.8, 
            face           = "bold",
            size           = 20,
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
# [1] ggbeeswarm_0.6.0     scales_1.2.1         camcorder_0.0.2.9000 glue_1.6.2           skimr_2.1.4         
# [6] here_1.0.1           janitor_2.1.0        MetBrewer_0.2.0      showtext_0.9-5       showtextdb_3.0      
# [11] sysfonts_0.8.8       ggtext_0.1.1         tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0       
# [16] dplyr_1.0.9          purrr_0.3.4          readr_2.1.2          tidyr_1.2.0          tibble_3.1.7        
# [21] ggplot2_3.3.6        tidyverse_1.3.1      pacman_0.5.1      

# loaded via a namespace (and not attached):
# [1] httr_1.4.3       jsonlite_1.8.0   modelr_0.1.8     assertthat_0.2.1 pdftools_3.3.0   askpass_1.1      vipor_0.4.5     
# [8] renv_0.15.5      cellranger_1.1.0 qpdf_1.2.0       pillar_1.8.1     backports_1.4.1  digest_0.6.29    gridtext_0.1.4  
# [15] rvest_1.0.2      snakecase_0.11.0 colorspace_2.0-3 htmltools_0.5.2  pkgconfig_2.0.3  broom_1.0.0      gifski_1.6.6-1  
# [22] haven_2.5.0      magick_2.7.3     tzdb_0.3.0       generics_0.1.3   usethis_2.1.6    ellipsis_0.3.2   withr_2.5.0     
# [29] repr_1.1.4       cli_3.3.0        magrittr_2.0.3   crayon_1.5.1     readxl_1.4.0     fs_1.5.2         fansi_1.0.3     
# [36] xml2_1.3.3       beeswarm_0.4.0   tools_4.2.1      hms_1.1.1        lifecycle_1.0.1  munsell_0.5.0    reprex_2.0.1    
# [43] compiler_4.2.1   rlang_1.0.4      grid_4.2.1       rstudioapi_0.13  rsvg_2.3.1       base64enc_0.1-3  gtable_0.3.0    
# [50] DBI_1.1.3        curl_4.3.2       R6_2.5.1         lubridate_1.8.0  knitr_1.39       fastmap_1.1.0    utf8_1.2.2      
# [57] rprojroot_2.0.3  stringi_1.7.8    Rcpp_1.0.9       vctrs_0.4.1      dbplyr_2.2.1     tidyselect_1.1.2 xfun_0.31


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36


