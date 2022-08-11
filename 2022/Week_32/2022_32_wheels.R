
## Challenge: #TidyTuesday 2022 week 32
## Data:      Ferris Wheels
## Author:    Steven Ponce
## Date:      2022-08-09  


## 1. LOAD PACKAGES & SETUP ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, lubridate, glue, camcorder)

gg_record(
    dir    = here("temp_plots"), 
    device = "png", 
    width  = 10, 
    height = 08, 
    units  = "in", 
    dpi    = 600)

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 32) 
wheels <- tt$wheels %>% clean_names() 
rm(tt)


#|- offline ----
# write_csv(wheels, '2022/Week_32/wheels.csv')
# wheels <- read_csv(file = '2022/Week_32/wheels.csv') %>% clean_names()


### 3. EXAMINING THE DATA ----
glimpse(wheels)
skim(wheels)
colnames(wheels)
unique(wheels$country) 


## 4. TIDYDATA ----
# |- data for plot ----
wheels_tbl <- wheels %>% 
    select(name:country, number_of_cabins:status) %>% 
    mutate(
        open_year  = year(opened),
        cost       = as.numeric(str_extract(construction_cost, pattern = '[:digit:]+')),
        highlight  = ifelse(seating_capacity > 1000, "yes", "no")
        ) %>% 
    drop_na(cost, status, highlight)
              
# |- data for labels ----
labels_tbl <- wheels_tbl %>% 
    select(name, cost, seating_capacity, status) %>% 
    arrange(desc(seating_capacity)) %>% 
    slice_head(n = 3)


# 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 
col_1          <- 'black'         
col_2          <- '#819FCC'
bkg_col        <- '#A5EEE9'   
title_col      <- 'black'
subtitle_col   <- "black"
caption_col    <- "black"
palette_col    <- met.brewer("Veronese", n = 7, type = "discrete")[c(6,1)]
# monochromeR::generate_palette("brown", modification = "go_lighter", n_colours = 10, view_palette = T)


# |-  titles and caption ----
title_text    <- 'Ferris Wheels'

subtitle_text <- str_wrap("The Top-3 Ferris wheels in seating capacity are not in operation.", width = 80)

caption_text   <- str_glue("
                         #TidyTuesday: 2022 Week 32 &bull; Source: ferriswheels<br>",
                           "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                           " <span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                           "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                           "<span style='font-family:fa-brands'>&#x23;</span> ggplot")


# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Monoton", family = "title")
font_add_google("Share", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |-  main plot ----
p1 <- wheels_tbl %>% 
    ggplot(aes(y = cost, x = seating_capacity )) +
    
    # geoms
    geom_point(aes(size  = 2.5, 
                   alpha = 0.85, 
                   color = highlight, 
                   fill  = highlight)) +
    
    geom_smooth(method   = 'lm', 
                se       = FALSE,
                linetype = 2,
                size     = 0.3,
                color    = 'gray50')+
    
    # scales  
    scale_color_manual(name   = "",
                       values = c("yes" = col_1, "no" = col_2)) +
    
    scale_x_continuous(
         breaks = seq(0, 2000, by = 500),
         limits = c(0, 2000),
         expand = c(0.03, 0.03),) +
    
    scale_y_continuous(
        breaks = seq(0, 700, by = 100),
        limits = c(0, 700),
        expand = c(0.03, 0.03),
        labels = c(0,100,200,300,400,500,600,'$700M\nUSD')) +
    
    coord_cartesian(clip = 'off') +
    
    # labs
    labs(
        x        = 'Seating Capacity', 
        y        = 'Construction Cost',
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
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 65,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_text(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.6, 
            face           = "plain",
            size           = 20,
            margin         = margin(t = 10, b = 30)),
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 12,
            hjust          = 1.0,
            margin         = margin(t = 10, b = 10)),
    )

# |- annotated plot ----
p1 + 
    geom_text(data = labels_tbl, 
              aes(x      = seating_capacity - 40, 
                  y      = cost - 10,
                  size   = 2.5, 
                  vjust  = 1,
                  hjust  = 1,
                  lineheight = 0.8,
                  label  = glue('{name}\n',
                                'Cost: ${cost}, ',
                                'Status: {status}'))) 
                  
 
## 6. SESSION INFO ---- 

# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)

# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     

# other attached packages:
# [1] camcorder_0.0.2.9000 glue_1.6.2           lubridate_1.8.0      skimr_2.1.4          here_1.0.1          
# [6] janitor_2.1.0        MetBrewer_0.2.0      showtext_0.9-5       showtextdb_3.0       sysfonts_0.8.8      
# [11] ggtext_0.1.1         tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0        dplyr_1.0.9         
# [16] purrr_0.3.4          readr_2.1.2          tidyr_1.2.0          tibble_3.1.7         ggplot2_3.3.6       
# [21] tidyverse_1.3.1      pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] nlme_3.1-158      fs_1.5.2          usethis_2.1.6     bit64_4.0.5       httr_1.4.3        rprojroot_2.0.3  
# [7] repr_1.1.4        tools_4.2.1       backports_1.4.1   utf8_1.2.2        R6_2.5.1          mgcv_1.8-40      
# [13] DBI_1.1.3         colorspace_2.0-3  withr_2.5.0       tidyselect_1.1.2  bit_4.0.4         curl_4.3.2       
# [19] compiler_4.2.1    textshaping_0.3.6 cli_3.3.0         rvest_1.0.2       xml2_1.3.3        scales_1.2.0     
# [25] askpass_1.1       systemfonts_1.0.4 digest_0.6.29     base64enc_0.1-3   pkgconfig_2.0.3   htmltools_0.5.2  
# [31] dbplyr_2.2.1      fastmap_1.1.0     rlang_1.0.4       readxl_1.4.0      rstudioapi_0.13   farver_2.1.1     
# [37] generics_0.1.3    jsonlite_1.8.0    vroom_1.5.7       magrittr_2.0.3    Matrix_1.4-1      Rcpp_1.0.9       
# [43] munsell_0.5.0     fansi_1.0.3       lifecycle_1.0.1   stringi_1.7.8     snakecase_0.11.0  grid_4.2.1       
# [49] parallel_4.2.1    crayon_1.5.1      lattice_0.20-45   splines_4.2.1     haven_2.5.0       gridtext_0.1.4   
# [55] hms_1.1.1         magick_2.7.3      knitr_1.39        pillar_1.8.0      markdown_1.1      reprex_2.0.1     
# [61] pdftools_3.3.0    qpdf_1.2.0        gifski_1.6.6-1    renv_0.15.5       modelr_0.1.8      vctrs_0.4.1      
# [67] tzdb_0.3.0        selectr_0.4-2     cellranger_1.1.0  gtable_0.3.0      assertthat_0.2.1  xfun_0.31        
# [73] broom_1.0.0       rsvg_2.3.1        ragg_1.2.2        ellipsis_0.3.2


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36


