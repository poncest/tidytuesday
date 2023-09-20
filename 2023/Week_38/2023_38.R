

## Challenge: #TidyTuesday 2023 week 38 
## Data:      CRAN Package Authors
## Author:    Steven Ponce
## Date:      2023-09-18


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, scales, lubridate)
pacman::p_load(ggraph, igraph)
theme_set(theme_light())


# |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 10,
    height = 8,
    units  = "in",
    dpi    = 320) 

# |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 38) 

cran_20230905    <- tt$cran_20230905 %>% clean_names()
package_authors  <- tt$package_authors %>% clean_names()
cran_graph_nodes <- tt$cran_graph_nodes %>% clean_names()
cran_graph_edges <- tt$cran_graph_edges %>% clean_names()

readme(tt) 
rm(tt)   


## 3. EXAMINING THE DATA ----
cran_20230905 %>% count(package, sort = T) 
cran_20230905 %>% count(author, sort = T) 

package_authors %>% count(authors_r, sort = T)



## 4. TIDYDATA ----

# |- plot data ----

authors_graph <- package_authors %>% 
    count( authors_r, package,sort = T) %>% 
    head(250) %>% 
    igraph::graph_from_data_frame() 



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- '#2C3950'        
title_col    <- "#F7D5B1"          
subtitle_col <- "#F7D5B1"  
caption_col  <- "#F7D5B1" 
text_col     <- "#F7D5B1"  



### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 38 &bull; Source: CRAN Package Authors<br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("CRAN Package Authors Network Diagram") 

subtitle_text <- str_glue("Force-Directed Graph Layout Algorithm: Fruchterman-Reingold<br><br>", 
                          "Only showing the first 250 packages") 

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")



### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Titillium Web", family = "title")                           
font_add_google("Dosis", family = "subtitle")              
font_add_google("Barlow Condensed", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
font_add_google("Shadows Into Light", family = "annote")  
showtext_auto(enable = TRUE)     


### |-  final plot ---- 

set.seed(4321)  

ggraph(graph = authors_graph, layout = 'fr') +     
    
    # geoms
    geom_edge_arc(strength = 0.2, width = 0.5, alpha = 0.15, color = "#fafafa") +
    
    geom_edge_fan(aes(alpha = 0.2, width = n), 
                  edge_colour = "#fafafa", show.legend = FALSE, check_overlap = T) +
    
    geom_node_point(color = '#F7D5B1', alpha = 0.95, size = 1.5) +  
    
    geom_node_text(aes(label = name), repel = TRUE, check_overlap = T, 
                   size = 1.8, color = "#fafafa") +
    # scales
    scale_edge_width(range = c(0.5, 2.5)) +
    
    # labs
    labs(title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text)+
    
    # theme
    theme_void()+
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
          
        plot.margin           = margin(t = 10, r = 15, b = 10, l = 15),
          
        axis.text             = element_blank(),
        axis.title            = element_blank(),
        panel.grid            = element_blank(),
          
        plot.title            = element_text(
            family            = 'title',
            color             = title_col,
            face              = "bold",
            size              = 25,  
            margin            = margin(t = 10, b = 5)),
          
        plot.subtitle         = element_markdown(
            family            = 'subtitle',
            color             = title_col,
            lineheight        = 0.8, 
            size              = 18,  
            margin            = margin(t = 5, b = 10)),
        
        plot.caption          = element_markdown(
            family            = 'caption',
            color             = caption_col,
            lineheight        = 0.6,
            size              = 10,
            hjust             = 0.5,
            halign            = 0.5,
            margin            = margin(t = 10, b = 10)),
        
    )


  

sessioninfo::session_info(include_base = TRUE) 
# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-09-20
# rstudio  2023.06.2+561 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────────
# package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [2] local
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
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.3    2023-07-20 [1] CRAN (R 4.3.1)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggforce        0.4.1    2022-10-04 [1] CRAN (R 4.3.1)
# ggplot2      * 3.4.3    2023-08-14 [1] CRAN (R 4.3.1)
# ggraph       * 2.1.0    2022-10-09 [1] CRAN (R 4.3.1)
# ggrepel        0.9.3    2023-02-03 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue           1.6.2    2022-02-24 [1] CRAN (R 4.3.0)
# graphics     * 4.3.1    2023-06-16 [2] local
# graphlayouts   1.0.0    2023-05-01 [1] CRAN (R 4.3.1)
# grDevices    * 4.3.1    2023-06-16 [2] local
# grid           4.3.1    2023-06-16 [2] local
# gridExtra      2.3      2017-09-09 [1] CRAN (R 4.3.0)
# gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# here           1.0.1    2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# httr           1.4.7    2023-08-15 [1] CRAN (R 4.3.1)
# igraph       * 1.5.1    2023-08-10 [1] CRAN (R 4.3.1)
# janitor      * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.7    2023-06-29 [1] CRAN (R 4.3.1)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.3    2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2    2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.5    2023-08-07 [1] CRAN (R 4.3.1)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.8      2023-08-23 [1] CRAN (R 4.3.1)
# MASS           7.3-60   2023-05-04 [2] CRAN (R 4.3.1)
# methods      * 4.3.1    2023-06-16 [2] local
# munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# parallel       4.3.1    2023-06-16 [2] local
# pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# polyclip       1.10-4   2022-10-20 [1] CRAN (R 4.3.0)
# purrr        * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5    2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr        * 2.1.4    2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.3    2023-07-06 [1] CRAN (R 4.3.1)
# rlang          1.1.1    2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot      2.0.3    2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg           2.4.0    2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.3    2022-08-19 [1] CRAN (R 4.3.0)
# scales       * 1.2.1    2022-08-20 [1] CRAN (R 4.3.0)
# selectr        0.4-2    2019-11-20 [1] CRAN (R 4.3.0)
# sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-6    2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.0)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.1)
# stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.7.12   2023-01-11 [1] CRAN (R 4.3.0)
# stringr      * 1.5.0    2022-12-02 [1] CRAN (R 4.3.0)
# svglite        2.1.1    2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8    2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4    2022-02-11 [1] CRAN (R 4.3.0)
# textshaping    0.3.6    2021-10-13 [1] CRAN (R 4.3.0)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.0)
# tidygraph      1.2.3    2023-02-01 [1] CRAN (R 4.3.1)
# tidyr        * 1.3.0    2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.0)
# tidytuesdayR * 1.0.2    2022-02-01 [1] CRAN (R 4.3.0)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tools          4.3.1    2023-06-16 [2] local
# tweenr         2.0.2    2022-09-06 [1] CRAN (R 4.3.1)
# tzdb           0.4.0    2023-05-12 [1] CRAN (R 4.3.0)
# usethis        2.2.2    2023-07-06 [1] CRAN (R 4.3.1)
# utf8           1.2.3    2023-01-31 [1] CRAN (R 4.3.0)
# utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.6.3    2023-06-14 [1] CRAN (R 4.3.1)
# viridis        0.6.4    2023-07-22 [1] CRAN (R 4.3.1)
# viridisLite    0.4.2    2023-05-02 [1] CRAN (R 4.3.0)
# vroom          1.6.3    2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.0    2022-03-03 [1] CRAN (R 4.3.0)
# xfun           0.40     2023-08-09 [1] CRAN (R 4.3.1)
# xml2           1.3.5    2023-07-06 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# > 