
## Challenge: #TidyTuesday 2022 week 41
## Data:      Ravelry Yarn
## Author:    Steven Ponce
## Date:      2022-10-11   
 

## 1. LOAD PACKAGES & SETUP ----  
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext)
p_load(janitor, here, skimr, glue, camcorder, scales, ggbeeswarm)

gg_record(
    dir    = here("temp_plots"),
    device = "png",
    width  = 12,
    height = 10,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 41) 
yarn <- tt$yarn %>% clean_names()
readme(tt) 
rm(tt)  


#|- offline ----   
# write_csv(yarn, '2022/Week_41/yarn.csv')
# yarn <- read_csv(file = '2022/Week_41/yarn.csv') %>% clean_names()


## 3. EXAMINING THE DATA ----
glimpse(yarn) 
skim(yarn)


## 4. TIDYDATA ---- 

# specify avg ratings
selected_avg_rating <- c(1, 2, 3, 4, 5)

# |-  main df
yarn_df <- yarn %>% 
    filter(rating_average %in% selected_avg_rating) %>% 
    
    filter(
        rating_average != is.na(rating_average),
        yardage        != is.na(yardage),
        discontinued == "TRUE" | 
            discontinued == "FALSE"
        ) %>% 
    
    mutate(rating_average = as_factor(rating_average)) %>% 
    
    select(discontinued, rating_average, yardage)
  


# 5. VISUALIZATION ---- 
# |- plot aesthetics ----  
bkg_col      <- "#F3E0DB"   
title_col    <- "black"
subtitle_col <- "black"
caption_col  <- "black"
col_palette  <- list("TRUE"  = "#6D435A",                    
                     "FALSE" = "#76828B")                 

# |-  titles and caption ----
title_text    <- "Ravelry Yarn"
                         
subtitle_text <- "<span style='font-size:40pt; color:#6D435A'>**Available**</span> - <span style='font-size:40pt; color:#76828B'>**Discontinued** </span>"

caption_text  <- str_glue("#TidyTuesday: 2022 Week 41 &bull; Source: ravelry.com<br>",
                           "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                           "<span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                           "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                           "<span style='font-family:fa-brands'>&#x23;</span> ggplot")

# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Poller One", family = "title")                          
font_add_google("Lily Script One", family = "subtitle")                     
font_add_google("Flamenco", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |- plot ----  
yarn_df %>% 
    ggplot(aes(x = rating_average, y = yardage)) +
    
    # geoms
    geom_quasirandom(aes(color = discontinued), 
                     na.rm = TRUE, size = 1, method = "minout") + 
   
    # scales
    scale_x_discrete() +
    scale_y_continuous(trans = "log2", expand = c(0.1, 0.1)) +
    scale_color_manual(values = col_palette) +
    coord_flip(clip = "off") +
    
    # labs
    labs(x = "Average Rating",
         y = "log2 Yardage",
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text) +
    
    # theme
    theme_minimal(base_size = 16) +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        panel.grid         = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.4, color = 'gray'),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        axis.title.x       = element_text(size = 20, 
                                          face = 'bold',
                                          hjust = 0.5,
                                          margin = margin(t = 10)), 
        
        axis.title.y       = element_text(size = 20, 
                                          face = 'bold',
                                          vjust = 0.5,
                                          margin = margin(r = 10)), 
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 60,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_markdown(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.8, 
            face           = "bold",
            size           = 40,
            margin         = margin(t = 10, b = 30)),
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col, 
            size           = 13,
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
# [1] ggbeeswarm_0.6.0     scales_1.2.1         camcorder_0.0.2.9000 glue_1.6.2           skimr_2.1.4          here_1.0.1          
# [7] janitor_2.1.0        showtext_0.9-5       showtextdb_3.0       sysfonts_0.8.8       ggtext_0.1.1         tidytuesdayR_1.0.2  
# [13] forcats_0.5.1        stringr_1.4.0        dplyr_1.0.9          purrr_0.3.4          readr_2.1.2          tidyr_1.2.0         
# [19] tibble_3.1.7         ggplot2_3.3.6        tidyverse_1.3.1      pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] fs_1.5.2          usethis_2.1.6     lubridate_1.8.0   bit64_4.0.5       httr_1.4.3        rprojroot_2.0.3   repr_1.1.4       
# [8] tools_4.2.1       backports_1.4.1   utf8_1.2.2        R6_2.5.1          vipor_0.4.5       DBI_1.1.3         colorspace_2.0-3 
# [15] withr_2.5.0       tidyselect_1.1.2  bit_4.0.4         curl_4.3.2        compiler_4.2.1    textshaping_0.3.6 cli_3.3.0        
# [22] rvest_1.0.2       xml2_1.3.3        askpass_1.1       systemfonts_1.0.4 digest_0.6.29     base64enc_0.1-3   pkgconfig_2.0.3  
# [29] htmltools_0.5.2   dbplyr_2.2.1      fastmap_1.1.0     rlang_1.0.4       readxl_1.4.0      rstudioapi_0.13   farver_2.1.1     
# [36] generics_0.1.3    jsonlite_1.8.0    vroom_1.5.7       magrittr_2.0.3    Rcpp_1.0.9        munsell_0.5.0     fansi_1.0.3      
# [43] lifecycle_1.0.1   stringi_1.7.8     snakecase_0.11.0  grid_4.2.1        parallel_4.2.1    crayon_1.5.1      haven_2.5.0      
# [50] gridtext_0.1.4    hms_1.1.1         magick_2.7.3      knitr_1.39        pillar_1.8.1      markdown_1.1      reprex_2.0.1     
# [57] pdftools_3.3.0    qpdf_1.2.0        gifski_1.6.6-1    renv_0.15.5       modelr_0.1.8      vctrs_0.4.1       tzdb_0.3.0       
# [64] selectr_0.4-2     cellranger_1.1.0  gtable_0.3.0      assertthat_0.2.1  xfun_0.31         broom_1.0.0       rsvg_2.3.1       
# [71] ragg_1.2.2        beeswarm_0.4.0    ellipsis_0.3.2 


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36 


