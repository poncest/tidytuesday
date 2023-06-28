 
## Challenge: #TidyTuesday 2023 week 27
## Data:      Historical Markers
## Author:    Steven Ponce
## Date:      2023-07-02


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, camcorder, scales, lubridate)
pacman::p_load(geojsonio, sf, rgdal, rgeos, broom, MetBrewer)              
 

# |- figure size ---- 
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
tt <- tidytuesdayR::tt_load(2023, week = 27) 

historical_markers <- tt$historical_markers %>% clean_names()

readme(tt)   
rm(tt)   


## 3. EXAMINING THE DATA ----
glimpse(historical_markers)  

historical_markers$title %>% unique() %>% sort()
historical_markers$subtitle %>% unique() %>% sort()
historical_markers$addl_subtitle %>% unique() %>% sort()

historical_markers %>% 
    count(erected_by, sort = T)

historical_markers %>%                      
    count(state_or_prov, sort = T)

historical_markers %>% 
    count(county_or_parish, sort = T)

historical_markers %>% 
    count(city_or_town, sort = T)

historical_markers %>% 
    count(title, sort = T)


## 4. TIDYDATA ---- 

#' Reference:
#' Download the Hexagones boundaries at geojson format here
#' https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.


# |-  US hexabin map ----
# load hex bin shapes file
hex_map <- geojson_read(x = "2023/Week_27/us_states_hexgrid.geojson", what = "sp")

# format
hex_map@data <- hex_map@data %>%
    mutate(google_name = gsub(" \\(United States\\)", "", google_name)) 

# basic hexbin map of the US
plot(hex_map)

# tidy to produce a tibble
hex_map_tidy <- tidy(hex_map, region = "google_name")

# calculate the centroid of each hexagon for state abbreviations label
centers <- cbind.data.frame(
    data.frame(gCentroid(hex_map, byid = TRUE), id = hex_map@data$iso3166_2) 
    ) 


# |- plot data ----

data <- historical_markers %>%    
    count(state_or_prov, sort = T, name = "count") %>%
    filter(state_or_prov != "Puerto Rico") %>% 
    mutate(bin = cut(count, c(0, 100, 250, 750, 1000, 11000),
                     labels = c("< 100", "100-250", "250-750", "750-1000", "1000 >" ),
                     include.lowest = TRUE)) %>% 
    rename(state = state_or_prov)

data %>% count(bin, sort = T)


# left join 
data_plot <- hex_map_tidy %>% 
    left_join(. ,data, by = c('id' = 'state')) 



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- '#D6D5C9'     
title_col    <- "#702632"              
subtitle_col <- "#702632"    
caption_col  <- "#702632"  
text_col     <- "#702632"  

col_palette <- met.brewer(name = "Morgenstern", n = 5, type ="discrete")


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 27 &bull; Source: Historical Marker Database USA Index<br>")  
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Historical Markers in the United States") 
                               
subtitle_text <- str_glue("The Historical Marker Database: A catalog of public history cast in metal,<br>carved on stone, or embedded in resin") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Archivo Black", family = "title")                     
font_add_google("Big Shoulders Display", family = "subtitle") 
font_add_google("Share", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     


### |-  final map ----
ggplot() +
    # geoms
    geom_polygon(data = data_plot, 
                 aes(x = long, y = lat, group = id, fill = bin), size = 0, alpha = 0.9) +
    
    geom_polygon(data = data_plot, 
                 aes(x = long, y = lat, group = id, fill = bin), color = "#1f1f29") +
    
    geom_text(data = centers, 
              aes(x = x, y = y, label = id), color="gray20", size = 3, alpha = 0.9) +
    
    # scale
    scale_fill_manual( 
        values = col_palette, 
        name   = "Historical markers bin size (count)",                       
        guide  = guide_legend(keyheight = unit(3, units = "mm"),
                              keywidth  = unit(12, units = "mm"),
                              label.position = "bottom",
                              title.position = 'top',
                              nrow = 1)) +
    
    # coordinates
    coord_sf() +
    
    # labs
    labs(
        x        = "Longitude", 
        y        = "Latitude",
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
        
        plot.background     = element_rect(fill = bkg_col, color = bkg_col),
        panel.background    = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin         = margin(t = 10, r = 10, b = 10, l = 10),
        
        plot.title          = element_markdown(
            family          = 'title',
            color           = title_col,
            face            = "bold",
            size            = 23,                               
            margin          = margin(t = 10, b = 5)),
        
        plot.subtitle       = element_markdown(
            family          = 'text',
            color           = title_col,
            lineheight      = 0.8, 
            size            = 17,  
            margin          = margin(t = 5, b = 15)),
        
        plot.caption        = element_markdown(
            family          = 'caption',
            color           = caption_col,
            lineheight      = 0.6,
            size            = 11,
            hjust           = 0.5,
            halign          = 0.5,
            margin          = margin(t = 10, b = 10)), 
    )


sessioninfo::session_info(include_base = TRUE) 
## 6. SESSION INFO ---- 
# ─ Session info ─────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.0 (2023-04-21 ucrt)
# os       Windows 10 x64 (build 19044)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-06-28
# rstudio  2023.06.0+421 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# backports      1.4.1   2021-12-13 [1] CRAN (R 4.3.0)
# base         * 4.3.0   2023-04-21 [?] local
# bit            4.0.5   2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5   2020-08-30 [1] CRAN (R 4.3.0)
# broom        * 1.0.4   2023-03-11 [1] CRAN (R 4.3.0)
# camcorder    * 0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.3.0)
# class          7.3-21  2023-01-23 [2] CRAN (R 4.3.0)
# classInt       0.4-9   2023-02-28 [1] CRAN (R 4.3.1)
# cli            3.6.1   2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# P compiler       4.3.0   2023-04-21 [2] local
# crayon         1.5.2   2022-09-29 [1] CRAN (R 4.3.0)
# crul           1.4.0   2023-05-17 [1] CRAN (R 4.3.0)
# curl           5.0.0   2023-01-12 [1] CRAN (R 4.3.0)
# P datasets     * 4.3.0   2023-04-21 [2] local
# DBI            1.1.3   2022-06-18 [1] CRAN (R 4.3.0)
# dplyr        * 1.1.2   2023-04-20 [1] CRAN (R 4.3.0)
# e1071          1.7-13  2023-02-01 [1] CRAN (R 4.3.1)
# fansi          1.0.4   2023-01-22 [1] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# foreign        0.8-84  2022-12-06 [2] CRAN (R 4.3.0)
# fs             1.6.2   2023-04-25 [1] CRAN (R 4.3.0)
# generics       0.1.3   2022-07-05 [1] CRAN (R 4.3.0)
# geojson        0.3.4   2020-06-23 [1] CRAN (R 4.3.1)
# geojsonio    * 0.11.1  2023-05-16 [1] CRAN (R 4.3.1)
# geojsonsf      2.0.3   2022-05-30 [1] CRAN (R 4.3.1)
# ggplot2      * 3.4.2   2023-04-03 [1] CRAN (R 4.3.0)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0  2023-05-19 [1] CRAN (R 4.3.0)
# glue           1.6.2   2022-02-24 [1] CRAN (R 4.3.0)
# P graphics     * 4.3.0   2023-04-21 [2] local
# P grDevices    * 4.3.0   2023-04-21 [2] local
# P grid           4.3.0   2023-04-21 [2] local
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.3   2023-03-21 [1] CRAN (R 4.3.0)
# here           1.0.1   2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3   2023-03-21 [1] CRAN (R 4.3.0)
# httpcode       0.3.0   2020-04-10 [1] CRAN (R 4.3.0)
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# jqr            1.2.3   2022-03-10 [1] CRAN (R 4.3.1)
# jsonlite       1.8.5   2023-06-05 [1] CRAN (R 4.3.0)
# KernSmooth     2.23-20 2021-05-03 [2] CRAN (R 4.3.0)
# lattice        0.21-8  2023-04-05 [2] CRAN (R 4.3.0)
# lazyeval       0.2.2   2019-03-15 [1] CRAN (R 4.3.1)
# lifecycle      1.0.3   2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2   2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.3.0)
# maptools       1.1-7   2023-05-29 [1] CRAN (R 4.3.1)
# markdown       1.7     2023-05-16 [1] CRAN (R 4.3.0)
# MetBrewer    * 0.2.0   2022-03-21 [1] CRAN (R 4.3.0)
# P methods      * 4.3.0   2023-04-21 [2] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1   2019-03-11 [1] CRAN (R 4.3.0)
# P parallel       4.3.0   2023-04-21 [2] local
# pillar         1.9.0   2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.3.0)
# proxy          0.4-27  2022-06-09 [1] CRAN (R 4.3.1)
# purrr        * 1.0.1   2023-01-10 [1] CRAN (R 4.3.0)
# R6             2.5.1   2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5   2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# readr        * 2.1.4   2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.2   2023-02-09 [1] CRAN (R 4.3.0)
# rgdal        * 1.6-7   2023-05-31 [1] CRAN (R 4.3.1)
# rgeos        * 0.6-3   2023-05-24 [1] CRAN (R 4.3.1)
# rlang          1.1.1   2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot      2.0.3   2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.14    2022-08-22 [1] CRAN (R 4.3.0)
# rsvg           2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.3   2022-08-19 [1] CRAN (R 4.3.0)
# scales       * 1.2.1   2022-08-20 [1] CRAN (R 4.3.0)
# selectr        0.4-2   2019-11-20 [1] CRAN (R 4.3.0)
# sessioninfo    1.2.2   2021-12-06 [1] CRAN (R 4.3.0)
# sf           * 1.0-13  2023-05-24 [1] CRAN (R 4.3.1)
# showtext     * 0.9-6   2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.0)
# snakecase      0.11.0  2019-05-25 [1] CRAN (R 4.3.0)
# sp           * 1.6-1   2023-05-31 [1] CRAN (R 4.3.0)
# P stats        * 4.3.0   2023-04-21 [2] local
# stringi        1.7.12  2023-01-11 [1] CRAN (R 4.3.0)
# stringr      * 1.5.0   2022-12-02 [1] CRAN (R 4.3.0)
# svglite        2.1.1   2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8   2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4   2022-02-11 [1] CRAN (R 4.3.0)
# textshaping    0.3.6   2021-10-13 [1] CRAN (R 4.3.0)
# tibble       * 3.2.1   2023-03-20 [1] CRAN (R 4.3.0)
# tidyr        * 1.3.0   2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect     1.2.0   2022-10-10 [1] CRAN (R 4.3.0)
# tidytuesdayR * 1.0.2   2022-02-01 [1] CRAN (R 4.3.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0   2023-01-11 [1] CRAN (R 4.3.0)
# P tools          4.3.0   2023-04-21 [2] local
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# units          0.8-2   2023-04-27 [1] CRAN (R 4.3.1)
# usethis        2.2.0   2023-06-06 [1] CRAN (R 4.3.0)
# utf8           1.2.3   2023-01-31 [1] CRAN (R 4.3.0)
# P utils        * 4.3.0   2023-04-21 [2] local
# V8             4.3.0   2023-04-08 [1] CRAN (R 4.3.1)
# vctrs          0.6.2   2023-04-19 [1] CRAN (R 4.3.0)
# vroom          1.6.3   2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.0   2022-03-03 [1] CRAN (R 4.3.0)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# xml2           1.3.4   2023-04-27 [1] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.0/library
# 
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────────
# > 

  