 
## Challenge: #TidyTuesday 2023 week 26
## Data:      US Populated Places
## Author:    Steven Ponce
## Date:      2023-06-25


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, camcorder, scales, lubridate)
pacman::p_load(osmdata, sf, tigris, ggspatial)


# |- figure size ---- 
gg_record(
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 6,
    height = 8,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 26) 

us_place_names   <- tt$us_place_names %>% clean_names()
us_place_history <- tt$us_place_history %>% clean_names()

readme(tt)   
rm(tt)   


## 3. EXAMINING THE DATA ----
glimpse(us_place_names)  
glimpse(us_place_history)  



## 4. TIDYDATA ---- 

# |- plot data

mayaguez <- us_place_names %>% 
    left_join(y = us_place_history, by = 'feature_id') %>% 
    rename(lat = prim_lat_dec,
           lon = prim_long_dec) %>% 
    filter(state_name  == "Puerto Rico",
           county_name == "Mayagüez") 



# start building the map -- Mayaguez, PR
bbx <- getbb("Mayaguez, PR")

min_lon       <- -67.35000; max_lon <- -67.01737
min_lat       <-  18.05297; max_lat <-  18.38989
bbx           <- rbind(x = c(min_lon, max_lon), y = c(min_lat, max_lat))
colnames(bbx) <- c("min","max")


# highways in Mayaguez, PR
highways <- bbx %>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value=c("motorway", "trunk",
                            "primary","secondary", 
                            "tertiary","motorway_link",
                            "trunk_link","primary_link",
                            "secondary_link",
                            "tertiary_link")) %>%
    osmdata_sf()


# visualizing the highways 
ggplot() +
    geom_sf(data = highways$osm_lines,
            aes(color=highway),
            size  = 0.4,
            alpha = 0.65)+
    theme_void()


# streets in Mayaguez, PR
streets <- bbx %>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c("residential", "living_street",
                              "service","unclassified",
                              "pedestrian", "footway",
                              "track","path")) %>%
    osmdata_sf()


# visualizing the streets
ggplot() +
    geom_sf(data = streets$osm_lines,
            aes(color=highway),
            size  = 0.4,
            alpha = 0.65)+
    theme_minimal()


# rivers in Mayaguez, PR
river <- bbx %>%
    opq() %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    osmdata_sf()


# combining streets highways, and rivers 
ggplot() +
    geom_sf(data  = streets$osm_lines,
            col   = "#6B727C",
            size  = 0.4,
            alpha = 0.65) +
    
    geom_sf(data  = highways$osm_lines,
            col   = "#6B727C",
            size  = 0.6,
            alpha = 0.8)+
    
    geom_sf(data = river$osm_lines,
            color = "#6A98D9",
            size  = 0.2,
            alpha = 1) +
    
    coord_sf(xlim = c(min_lon,max_lon),
             ylim = c(min_lat,max_lat),
             expand = FALSE)+
    
    theme_void()


# adding geography
counties_PR <- counties(state="PR", cb = TRUE, class = "sf")

counties_PR <- st_crop(counties_PR,
                       xmin = min_lon, xmax = max_lon,
                       ymin = min_lat, ymax = max_lat)



# adding water shape
get_water <- function(county_GEOID){
    area_water("PR", county_GEOID, class = "sf")
}

water <- do.call(rbind, 
                 lapply(counties_PR$COUNTYFP,get_water))

water <- st_crop(water,
                 xmin = min_lon, xmax = max_lon,
                 ymin = min_lat, ymax = max_lat)

st_erase <- function(x, y) {
    st_difference(x, st_union(y))
}

counties_PR <- st_erase(st_union(counties_PR), water)


# the map so far...(in-process)
ggplot() + 
    geom_sf(data = counties_PR,
            inherit.aes= FALSE,
            lwd  = 0.0, fill = "#343C47")+
    
    geom_sf(data = highways$osm_lines,
            inherit.aes = FALSE,
            color = "#000000",
            size  = 0.3,
            alpha = 0.8) +
    
    geom_sf(data = streets$osm_lines,
            inherit.aes = FALSE,
            color = "#000000",
            size = 0.1,
            alpha = 0.6) +
   
     geom_sf(data = river$osm_lines,
            inherit.aes = FALSE,
            color = "#000000",
            size  = 0.2,
            alpha = 05) +
    
    coord_sf(xlim = c(min_lon,max_lon),
             ylim = c(min_lat,max_lat),
             expand = FALSE)+

    theme_void()+
    theme(panel.background = element_rect(fill = "#334c6f"))
   


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- '#334c6f'     
title_col    <- "#F4F4F4"              
subtitle_col <- "#F4F4F4"  
caption_col  <- "#F4F4F4" 
text_col     <- "#F4F4F4" 


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 26 &bull; Source: National Map Staged Products Directory<br>")  
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Places to Visit in Mayagüez, Puerto Rico") 
                               
subtitle_text <- str_glue("As per USGS National Map") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Big Shoulders Text", family = "title")      
font_add_google("Share", family = "subtitle")
font_add_google("Share", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     


### |-  final map ----
ggplot() + 
    
    # mayaguez, PR shape
    geom_sf(data = counties_PR,
            inherit.aes = FALSE,
            lwd         = 0.0,
            fill        ="#343C47") +
    
    # major roads
    geom_sf(data = highways$osm_lines,
            inherit.aes = FALSE,
            color       = "#D9AC55",
            size        = 0.6,
            alpha       = 0.45) +
    
    # smaller roads
    geom_sf(data = streets$osm_lines,
            inherit.aes = FALSE,
            color       = "#6B727C",
            size        = 0.4,
            alpha       = 0.65) +
    
    # rivers
    geom_sf(data = river$osm_lines,
            inherit.aes = FALSE,
            color       = "#6A98D9",
            size        = 0.2,
            alpha       = 0.5) +
    
    # places in mayaguez
    geom_point(data = mayaguez,
               aes(lon, lat), 
               color = "#6AD95F",  
               size  = 0.8,
               alpha = 1)+
    
    # coordinates
    coord_sf(xlim   = c(min_lon,max_lon),
             ylim   = c(min_lat,max_lat),
             expand = FALSE) +
    
    # scale bars and north arrow
    annotation_scale(location = "bl", width_hint = 0.4, bar_cols = c("#1A1313", "#F4F4F4")) +
    
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering)+
    
    # labs
    labs(
        x        = "Longitude", 
        y        = "Latitude",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text
    ) +
    
    theme_void() +
    theme(
        plot.background     = element_rect(fill = '#1A1313', color = '#1A1313'),
        panel.background    = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin         = margin(t = 10, r = 10, b = 10, l = 10),
        
        plot.title          = element_markdown(
            family          = 'title',
            color           = title_col,
            face            = "bold",
            size            = 24, 
            margin          = margin(t = 10, b = 5)),
        
        plot.subtitle       = element_markdown(
            family          = 'text',
            color           = title_col,
            lineheight      = 0.8, 
            size            = 18,  
            margin          = margin(t = 5, b = 10)),
        
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
# ─ Session info ───────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.0 (2023-04-21 ucrt)
# os       Windows 10 x64 (build 19044)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-06-25
# rstudio  2023.06.0+421 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.3.0   2023-04-21 [?] local
# bit            4.0.5   2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5   2020-08-30 [1] CRAN (R 4.3.0)
# camcorder    * 0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.3.0)
# class          7.3-21  2023-01-23 [2] CRAN (R 4.3.0)
# classInt       0.4-9   2023-02-28 [1] CRAN (R 4.3.1)
# cli            3.6.1   2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# P compiler       4.3.0   2023-04-21 [2] local
# crayon         1.5.2   2022-09-29 [1] CRAN (R 4.3.0)
# curl           5.0.0   2023-01-12 [1] CRAN (R 4.3.0)
# P datasets     * 4.3.0   2023-04-21 [2] local
# DBI            1.1.3   2022-06-18 [1] CRAN (R 4.3.0)
# dplyr        * 1.1.2   2023-04-20 [1] CRAN (R 4.3.0)
# e1071          1.7-13  2023-02-01 [1] CRAN (R 4.3.1)
# fansi          1.0.4   2023-01-22 [1] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.2   2023-04-25 [1] CRAN (R 4.3.0)
# generics       0.1.3   2022-07-05 [1] CRAN (R 4.3.0)
# ggplot2      * 3.4.2   2023-04-03 [1] CRAN (R 4.3.0)
# ggspatial    * 1.1.8   2023-04-13 [1] CRAN (R 4.3.1)
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
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# httr2          0.2.3   2023-05-08 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.5   2023-06-05 [1] CRAN (R 4.3.0)
# KernSmooth     2.23-20 2021-05-03 [2] CRAN (R 4.3.0)
# lifecycle      1.0.3   2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2   2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.7     2023-05-16 [1] CRAN (R 4.3.0)
# P methods      * 4.3.0   2023-04-21 [2] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# osmdata      * 0.2.3   2023-06-01 [1] CRAN (R 4.3.1)
# pacman         0.5.1   2019-03-11 [1] CRAN (R 4.3.0)
# P parallel       4.3.0   2023-04-21 [2] local
# pillar         1.9.0   2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.3.0)
# proxy          0.4-27  2022-06-09 [1] CRAN (R 4.3.1)
# purrr        * 1.0.1   2023-01-10 [1] CRAN (R 4.3.0)
# R6             2.5.1   2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5   2023-01-12 [1] CRAN (R 4.3.0)
# rappdirs       0.3.3   2021-01-31 [1] CRAN (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# readr        * 2.1.4   2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.2   2023-02-09 [1] CRAN (R 4.3.0)
# rlang          1.1.1   2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot      2.0.3   2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.14    2022-08-22 [1] CRAN (R 4.3.0)
# rsvg           2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.3   2022-08-19 [1] CRAN (R 4.3.0)
# s2             1.1.4   2023-05-17 [1] CRAN (R 4.3.1)
# scales       * 1.2.1   2022-08-20 [1] CRAN (R 4.3.0)
# selectr        0.4-2   2019-11-20 [1] CRAN (R 4.3.0)
# sessioninfo    1.2.2   2021-12-06 [1] CRAN (R 4.3.0)
# sf           * 1.0-13  2023-05-24 [1] CRAN (R 4.3.1)
# showtext     * 0.9-6   2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.0)
# snakecase      0.11.0  2019-05-25 [1] CRAN (R 4.3.0)
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
# tigris       * 2.0.3   2023-05-19 [1] CRAN (R 4.3.1)
# timechange     0.2.0   2023-01-11 [1] CRAN (R 4.3.0)
# P tools          4.3.0   2023-04-21 [2] local
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# units          0.8-2   2023-04-27 [1] CRAN (R 4.3.1)
# usethis        2.2.0   2023-06-06 [1] CRAN (R 4.3.0)
# utf8           1.2.3   2023-01-31 [1] CRAN (R 4.3.0)
# P utils        * 4.3.0   2023-04-21 [2] local
# uuid           1.1-0   2022-04-19 [1] CRAN (R 4.3.0)
# vctrs          0.6.2   2023-04-19 [1] CRAN (R 4.3.0)
# vroom          1.6.3   2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.0   2022-03-03 [1] CRAN (R 4.3.0)
# wk             0.7.3   2023-05-06 [1] CRAN (R 4.3.1)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# xml2           1.3.4   2023-04-27 [1] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.0/library
# 
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────────────
# > 


  