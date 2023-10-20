

## Challenge: #TidyTuesday 2023 week 42
## Data:      Taylor Swift
## Author:    Steven Ponce
## Date:      2023-10-16


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
pacman::p_load(ggragged, ggpcp, tayloRswift)
theme_set(theme_light(base_size = 14))


# |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 8,
    height = 10,
    units  = "in",
    dpi    = 320) 

# |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 42) 


taylor_album_songs <- tt$taylor_album_songs %>% clean_names() %>% glimpse()

tidytuesdayR::readme(tt) 
rm(tt)   



## 3. EXAMINING THE DATA ----
skim(taylor_album_songs)
taylor_album_songs %>% count(album_name, sort = T) 



## 4. TIDYDATA ----

# |- Tidy ----

taylor_tbl <- taylor_album_songs %>% 
    select(album_name, album_release, track_number, track_name, danceability, energy, loudness, 
           speechiness, acousticness, liveness, valence, tempo) %>% 
    
    mutate(
        album_name = str_replace(album_name, " \\(Taylor's Version\\)", ""),
        album_name = str_to_title(album_name),
        album_name = fct_reorder(album_name, album_release)
    )%>%
    
    # str_to_title (Specific columns)
    rename_with(
        .data = .,
        .cols = danceability:tempo,
        .fn = ~ str_to_title(.x)
    ) %>% 
    # add a blank column (will be used in facet_ragged_rows())
    mutate(artist = "") %>% 
    
    # str_wrap album name
    mutate(album_name = str_wrap(album_name, width = 5))
    

# |- Mean Summary ----
taylor_mean_tbl <- taylor_tbl %>% 
    group_by(album_name, album_release) %>%
    summarize(
        across(c(Danceability:Tempo),
               ~ mean(., na.rm = TRUE),
               .names = "Mean_{.col}"
        )
    ) %>%
    ungroup()

    

# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- "#2d2b2e"
title_col    <- "gray"          
subtitle_col <- "gray"  
caption_col  <- "gray50" 
text_col     <- "gray50"   


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 42 &bull; Source: Taylor R package <br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Taylor Swift") 

subtitle_text <- str_glue("Analyzing Taylor's music through various Spotify indexes. ") 

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Sacramento", family = "title")                            
font_add_google("Sarabun", family = "subtitle")   
font_add_google("Pragati Narrow", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     


#' Note
#' I would like to examine or glance multiple responses (y-variable) at once.
#' I don't want to use a radar chart or polar bar plot at this time

# TODO use parallel coordinate plot from the {ggpcp}


### |-  final plot ---- 

taylor_tbl %>% 
    
    ggplot(aes(color = album_name)) +
    
    # Geoms
    ggpcp::geom_pcp(aes(vars = vars(Danceability:Tempo)))+
    
    # Scales
    scale_x_discrete()+
    scale_y_continuous(breaks = c(0, 0.5, 1))+
    scale_color_taylor(palette = "taylorSwift", guide = "none")+
    
    # Labels
    labs(x = "", y = "",
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text) +       
    
    # Facet
    facet_ragged_cols(
        vars(as_factor(album_name)),
        vars(as_factor(artist)),
        labeller = "label_value",
        scales = "free_y")+
    
    # Theme
    theme_minimal()+
    theme( 
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
       
        plot.background     = element_rect(fill = bkg_col, color = bkg_col),
        panel.background    = element_rect(fill = bkg_col, color = bkg_col),
       
        plot.margin         = margin(t = 10, r = 0, b = 10, l = 20),
        
        axis.title          = element_text(size = 10, face = 'bold', family   = 'text', color = text_col, margin = margin(t = 10)), 
        axis.text           = element_text(size = 10, family   = 'text', color = text_col),
        
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
       
        strip.text          = element_textbox(size     = 11,
                                              color    = text_col,
                                              family   = 'text',
                                              r        = unit(5, "pt"),
                                              width    = unit(5.5, "npc"),
                                              padding  = margin(0, 0, 0, 0),
                                              margin   = margin(0, 20, 0, -1),
                                              fill     = "transparent"),
       
       panel.spacing        = unit(1.5 , 'lines'),
             
       plot.title           = element_text(
           family           = 'title',
           color            = title_col,
           face             = "bold",
           size             = 42,  
           margin           = margin(t = 5, b = 10)),
         
       plot.subtitle         = element_text(
           family            = 'subtitle',
           color             = title_col,
           lineheight        = 0.8, 
           size              = 18,  
           margin            = margin(t = 0, b = 20)),
         
       plot.caption         = element_markdown(
           family           = 'caption',
           color            = caption_col,
           lineheight       = 0.6,
           size             = 10,
           hjust            = 0.5,
           halign           = 0.5,
           margin           = margin(t = 10, b = 5)),
    )


sessioninfo::session_info(include_base = TRUE) 
# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-10-20
# rstudio  2023.09.0+463 Desert Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────
# package      * version  date (UTC) lib source
# assertthat     0.2.1    2019-03-21 [1] CRAN (R 4.3.1)
# base         * 4.3.1    2023-06-16 [2] local
# base64enc      0.1-3    2015-07-28 [1] CRAN (R 4.3.0)
# bit            4.0.5    2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5    2020-08-30 [1] CRAN (R 4.3.0)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1    2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0    2023-03-17 [1] CRAN (R 4.3.0)
# compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.2    2022-09-29 [1] CRAN (R 4.3.0)
# curl           5.0.2    2023-08-14 [1] CRAN (R 4.3.1)
# datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.33   2023-07-07 [1] CRAN (R 4.3.1)
# dplyr        * 1.1.3    2023-09-03 [1] CRAN (R 4.3.1)
# fansi          1.0.4    2023-01-22 [1] CRAN (R 4.3.0)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.3.0)
# fastmap        1.1.1    2023-02-24 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.3    2023-07-20 [1] CRAN (R 4.3.1)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggpcp        * 0.1.0    2023-10-20 [1] Github (yaweige/ggpcp@acf7efd)
# ggplot2      * 3.4.3    2023-08-14 [1] CRAN (R 4.3.1)
# ggragged     * 0.1.0    2023-04-20 [1] CRAN (R 4.3.0)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue           1.6.2    2022-02-24 [1] CRAN (R 4.3.0)
# graphics     * 4.3.1    2023-06-16 [2] local
# grDevices    * 4.3.1    2023-06-16 [2] local
# grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# here           1.0.1    2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# htmltools      0.5.6    2023-08-10 [1] CRAN (R 4.3.1)
# httr           1.4.7    2023-08-15 [1] CRAN (R 4.3.1)
# janitor      * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.7    2023-06-29 [1] CRAN (R 4.3.1)
# knitr          1.44     2023-09-11 [1] CRAN (R 4.3.1)
# lifecycle      1.0.3    2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2    2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.5    2023-08-07 [1] CRAN (R 4.3.1)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.8      2023-08-23 [1] CRAN (R 4.3.1)
# methods      * 4.3.1    2023-06-16 [2] local
# munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# parallel       4.3.1    2023-06-16 [2] local
# pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# purrr        * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5    2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr        * 2.1.4    2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.3    2023-07-06 [1] CRAN (R 4.3.1)
# remotes        2.4.2.1  2023-07-18 [1] CRAN (R 4.3.1)
# repr           1.1.6    2023-01-26 [1] CRAN (R 4.3.1)
# rlang          1.1.1    2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot      2.0.3    2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg           2.4.0    2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.3    2022-08-19 [1] CRAN (R 4.3.0)
# scales       * 1.2.1    2022-08-20 [1] CRAN (R 4.3.1)
# selectr        0.4-2    2019-11-20 [1] CRAN (R 4.3.0)
# sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-6    2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.0)
# skimr        * 2.1.5    2022-12-23 [1] CRAN (R 4.3.1)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.1)
# stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.7.12   2023-01-11 [1] CRAN (R 4.3.0)
# stringr      * 1.5.0    2022-12-02 [1] CRAN (R 4.3.0)
# svglite        2.1.1    2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8    2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4    2022-02-11 [1] CRAN (R 4.3.0)
# tayloRswift  * 0.2.0    2023-10-20 [1] Github (asteves/tayloRswift@445a16a)
# textshaping    0.3.6    2021-10-13 [1] CRAN (R 4.3.0)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.0)
# tidyr        * 1.3.0    2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.0)
# tidytuesdayR   1.0.2    2022-02-01 [1] CRAN (R 4.3.0)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tools          4.3.1    2023-06-16 [2] local
# tzdb           0.4.0    2023-05-12 [1] CRAN (R 4.3.0)
# usethis        2.2.2    2023-07-06 [1] CRAN (R 4.3.1)
# utf8           1.2.3    2023-01-31 [1] CRAN (R 4.3.0)
# utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.6.3    2023-06-14 [1] CRAN (R 4.3.1)
# vroom          1.6.3    2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.1    2023-09-26 [1] CRAN (R 4.3.1)
# xfun           0.40     2023-08-09 [1] CRAN (R 4.3.1)
# xml2           1.3.5    2023-07-06 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ────────────────────────────────────────────────────────────────────────────────────────────────────────
# > 
