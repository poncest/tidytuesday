

## Challenge: #TidyTuesday 2023 week 41
## Data:      Haunted Places in the United States
## Author:    Steven Ponce
## Date:      2023-10-09


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
pacman::p_load(tidytext, textdata, ggwordcloud, png)
# pacman::p_load(ggmap, usdata, ggthemes)
theme_set(theme_light(base_size = 16))


# |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 8,
    height = 8,
    units  = "in",
    dpi    = 320) 

# |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 41) 


haunted_places <- tt$haunted_places %>% 
    clean_names() %>% 
    glimpse()


tidytuesdayR::readme(tt) 
rm(tt)   



## 3. EXAMINING THE DATA ----
skim(haunted_places)
haunted_places %>% count(country, sort = T) 
haunted_places %>% count(state, sort = T) # focus on CA
head(haunted_places$description)



## Lets do sentiment analysis (wordcloud) and map with California data only

## 4. TIDYDATA ----

# |- Sentiment Analysis - wordcloud data ----

#  NRC lexicon (If needed)
# library(remotes)
# install_github("EmilHvitfeldt/textdata")
# install_github("juliasilge/tidytext")

# tidy 
haunted_places_tbl <- haunted_places %>% 
    select(state, description) %>% 
    mutate(description = str_to_lower(description))
   

# word proportions
haunted_places_prop <- haunted_places_tbl %>% 
    group_by(state) %>% 
    mutate(line_number = row_number()) %>% 
    ungroup() %>% 
    
    # unnest tokens & remove stop words
    unnest_tokens(word, description) %>% 
    anti_join(stop_words) %>% 
    
    # summary
    count(word, sort = TRUE) %>% 
    mutate(proportion = n / sum(n))


# sentiments
description_sentiments <- haunted_places_prop %>% 
    
    # NRC Word-Emotion Association Lexicon
    inner_join(get_sentiments("nrc")) %>% 
    
    # summary
    count(word, sentiment) %>%
    
    # pivot and calculate sentiment
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
    mutate(sentiment = positive - negative) 


# tidy episodes
description_tidy <- haunted_places_tbl %>% 
    group_by(state) %>% 
    mutate(line_number = row_number()) %>% 
    ungroup() %>% 
    
    # unnest tokens & remove stop words
    unnest_tokens(word, description) %>% 
    anti_join(stop_words) 


# worldcloud tbl
wordcloud_tbl <- description_tidy %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) 



    


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <-  "#1a1a13"
title_col    <-  "#6e1722"              
caption_col  <-  "#6e1722"       



### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 41 &bull; Source: Tim Renner on data.world<br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Sentiment of Haunted Places\nacross the Unites States") 

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")



### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Creepster", family = "title")                            
font_add_google("Sarabun", family = "text")                  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     



### |-  final plot ----  
wordcloud_tbl %>%
    
    ggplot(aes(label = word, size = n, color = n)) +
    
    # geoms
    geom_text_wordcloud_area(mask = png::readPNG("2023/Week_41/pumpkin.png"),
                             rm_outside = TRUE)+
    
    # scales
    scale_size_area(max_size = 40) +                    
    scale_color_gradient(low = "#ffae17", high = "#f25607")+
    
    # labs
    labs(x = "", y = "",
         title    = title_text,
         caption  = caption_text) +
    
    # theme
    theme_minimal() +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_blank(),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 42,  
            margin         = margin(t = 5, b = 5)),

        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col,
            lineheight     = 0.6,
            size           = 10,
            hjust          = 0.5,
            halign         = 0.5,
            margin         = margin(t = 10, b = 5)),
    )



sessioninfo::session_info(include_base = TRUE) 
# ─ Session info ────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-10-11
# rstudio  2023.09.0+463 Desert Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────
# package      * version  date (UTC) lib source
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
# ggplot2      * 3.4.3    2023-08-14 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# ggwordcloud  * 0.5.0    2019-06-02 [1] CRAN (R 4.3.1)
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
# janeaustenr    1.0.0    2022-08-26 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.7    2023-06-29 [1] CRAN (R 4.3.1)
# knitr          1.43     2023-05-25 [1] CRAN (R 4.3.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lattice        0.21-8   2023-04-05 [2] CRAN (R 4.3.1)
# lifecycle      1.0.3    2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2    2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.5    2023-08-07 [1] CRAN (R 4.3.1)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.8      2023-08-23 [1] CRAN (R 4.3.1)
# Matrix         1.6-1    2023-08-14 [2] CRAN (R 4.3.1)
# methods      * 4.3.1    2023-06-16 [2] local
# munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# parallel       4.3.1    2023-06-16 [2] local
# pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# png          * 0.1-8    2022-11-29 [1] CRAN (R 4.3.0)
# purrr        * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5    2023-01-12 [1] CRAN (R 4.3.0)
# rappdirs       0.3.3    2021-01-31 [1] CRAN (R 4.3.0)
# Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr        * 2.1.4    2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.3    2023-07-06 [1] CRAN (R 4.3.1)
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
# SnowballC      0.7.1    2023-04-25 [1] CRAN (R 4.3.0)
# stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.7.12   2023-01-11 [1] CRAN (R 4.3.0)
# stringr      * 1.5.0    2022-12-02 [1] CRAN (R 4.3.0)
# svglite        2.1.1    2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8    2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4    2022-02-11 [1] CRAN (R 4.3.0)
# textdata     * 0.4.4    2022-09-02 [1] CRAN (R 4.3.0)
# textshaping    0.3.6    2021-10-13 [1] CRAN (R 4.3.0)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.0)
# tidyr        * 1.3.0    2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.0)
# tidytext     * 0.4.1    2023-01-07 [1] CRAN (R 4.3.0)
# tidytuesdayR   1.0.2    2022-02-01 [1] CRAN (R 4.3.0)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tokenizers     0.3.0    2022-12-22 [1] CRAN (R 4.3.0)
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
# ───────────────────────────────────────────────────────────────────────────────────────────────
# > 
