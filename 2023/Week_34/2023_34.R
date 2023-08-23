

## Challenge: #TidyTuesday 2023 week 34 
## Data:      Refugees
## Author:    Steven Ponce
## Date:      2023-08-21


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, scales, lubridate)


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
tt <- tidytuesdayR::tt_load(2023, week = 34) 

population <- tt$population %>% clean_names()

readme(tt)   
rm(tt)   


## 3. EXAMINING THE DATA ----
colnames(population)
glimpse(population)  

population$year %>% range()
population %>% count(year, sort = T)
population %>% count(coo_name, sort = T)   
population %>% count(coa_name, sort = T)  
population %>% count(refugees, sort = T)
population %>% count(asylum_seekers, sort = T)
population %>% count(returned_refugees, sort = T)
population %>% count(idps, sort = T)



## 4. TIDYDATA ----

# flexible summary function - change in `asylum seeker` 20221 - 2022

flexible_summary_function <- function(tbl, category){
    
    tbl %>% 
        filter(year %in% c(2021, 2022)) %>% 
        select(year:asylum_seekers) %>% 
        arrange(desc(coa_name)) %>% 
        
        group_by(coo_name, coa_name) %>%  
        summarise(
            ratio   = last({{ category }}) / first({{ category }}),
            change  = ratio - 1,                                          
            `2021`  = first({{ category }}),
            `2022`  = last({{ category }}),
            .groups = "drop",
        ) %>% 
        ungroup()
}


asylum_seekers_2021_2022 <- population %>% 
    filter(coo_name == "United States of America") %>% 
    flexible_summary_function(category = asylum_seekers) %>% 
    
    # remove NaN and Inf
    filter_all(all_vars(!is.infinite(.))) %>% 
    filter_all(all_vars(!is.nan(.))) %>% 
    
    # format country names
    mutate(
        coa_name = case_when(
            coa_name == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
            coa_name == "Netherlands (Kingdom of the)"                         ~ "Netherlands",
            TRUE ~ coa_name
        )
    ) %>% 
    
    # highlight column for color
    mutate(
        highlight = case_when(
            change > 0   ~ "increase",
            change < 0   ~ "decrease",
            change == 0  ~ "neutral"
        )) %>% 
    
    # reorder
    mutate(coa_name = fct_reorder(coa_name, change)) 



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- "#323238" 
title_col    <- "#d7d7d8"             
subtitle_col <- "#d7d7d8"   
caption_col  <- "#d7d7d8"   
text_col     <- "#d7d7d8"      

col_palette  <- c("increase" = "#5cbcc6", "decrease"  = "#ed93bd", "neutral" = "#d7d7d8" )



### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 34 &bull; Source: Refugees R package<br>")  
X <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Which countries do Americans\nseek asylum in 2022?") 

subtitle_text <- str_glue("The size of the point represents # asylum seekers per country in 2021") 

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")



### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Quicksand", family = "title")                           
font_add_google("Barlow Condensed", family = "subtitle")              
font_add_google("Barlow Condensed", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
font_add_google("Shadows Into Light", family = "annote")  
showtext_auto(enable = TRUE)     



### |-  final plot ----
asylum_seekers_2021_2022 %>% 
    ggplot(aes(x = change, y = coa_name, color = highlight))+
    
    # geoms
    geom_vline(xintercept = 0, linetype = 1, linewidth = 0.3, color = "#d7d7d8")+
    
    geom_errorbarh(aes(xmin = 0, xmax = change),
                   height = 0,
                   position = position_dodge(width = 0.7),
                   size  = 0.25,
                   na.rm = TRUE) +
    
    geom_point(aes(size =  `2021`),
               position = position_dodge(width = 0.7),
               na.rm = TRUE) +
    
    # scales
    scale_x_continuous(breaks = seq(-1, 1, by = 0.25),
                       limits = c(-1, 1),
                       labels = percent) +
    scale_y_discrete()+
    scale_color_manual(values = col_palette)+ 
    scale_size_continuous(range = c(1, 7), guide = "none") +
    coord_cartesian(clip = 'off')+
    
    # labs
    labs(x        = "Shift in # asylum seekers from 2021 to 2022",
         y        = "",
         color    = "",
         size     = "",
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text) +
    
    # theme
    theme_minimal(12, base_family = 'text')+ 
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        axis.title.x          = element_text(size = 14, face = 'bold', 
                                             margin = margin(t = 10), color = text_col ), 
        axis.text             = element_text(size = 12, color = text_col),
        
        axis.line.x           = element_line(color = "gray"),
        panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.3, color = '#d7d7d8'),
        panel.grid            = element_blank(),
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin           = margin(t = 10, r = 25, b = 10, l = 25),
        
        plot.title            = element_text(
            family            = 'title',
            color             = title_col,
            face              = "bold",
            size              = 32,  
            lineheight        = 0.9, 
            margin            = margin(t = 10)),
          
        plot.subtitle         = element_text(
            family            = 'subtitle',
            color             = title_col,
            #face              = "bold",
            size              = 14 ,  
            margin            = margin(t = 5, b = 20)),
          
        plot.caption          = element_markdown(
            family            = 'caption',
            color             = caption_col,
            lineheight        = 0.6,
            size              = 11,
            hjust             = 0.5,
            halign            = 0.5,
            margin            = margin(t = 10, b = 10)),
    )

  

sessioninfo::session_info(include_base = TRUE) 
# ─ Session info ───────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-08-23
# rstudio  2023.06.1+524 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────
# package      * version date (UTC) lib source
# base         * 4.3.1   2023-06-16 [2] local
# bit            4.0.5   2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5   2020-08-30 [1] CRAN (R 4.3.0)
# camcorder      0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1   2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# compiler       4.3.1   2023-06-16 [2] local
# crayon         1.5.2   2022-09-29 [1] CRAN (R 4.3.0)
# curl           5.0.0   2023-01-12 [1] CRAN (R 4.3.0)
# datasets     * 4.3.1   2023-06-16 [2] local
# dplyr        * 1.1.2   2023-04-20 [1] CRAN (R 4.3.0)
# fansi          1.0.4   2023-01-22 [1] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.2   2023-04-25 [1] CRAN (R 4.3.0)
# generics       0.1.3   2022-07-05 [1] CRAN (R 4.3.0)
# ggplot2      * 3.4.2   2023-04-03 [1] CRAN (R 4.3.0)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0  2023-05-19 [1] CRAN (R 4.3.0)
# glue           1.6.2   2022-02-24 [1] CRAN (R 4.3.0)
# graphics     * 4.3.1   2023-06-16 [2] local
# grDevices    * 4.3.1   2023-06-16 [2] local
# grid           4.3.1   2023-06-16 [2] local
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.3   2023-03-21 [1] CRAN (R 4.3.0)
# here           1.0.1   2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3   2023-03-21 [1] CRAN (R 4.3.0)
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.5   2023-06-05 [1] CRAN (R 4.3.0)
# lifecycle      1.0.3   2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2   2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.7     2023-05-16 [1] CRAN (R 4.3.0)
# methods      * 4.3.1   2023-06-16 [2] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1   2019-03-11 [1] CRAN (R 4.3.0)
# parallel       4.3.1   2023-06-16 [2] local
# pillar         1.9.0   2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.3.0)
# purrr        * 1.0.1   2023-01-10 [1] CRAN (R 4.3.0)
# R6             2.5.1   2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5   2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# readr        * 2.1.4   2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.2   2023-02-09 [1] CRAN (R 4.3.0)
# rlang          1.1.1   2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot      2.0.3   2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.14    2022-08-22 [1] CRAN (R 4.3.0)
# rsvg           2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.3   2022-08-19 [1] CRAN (R 4.3.0)
# scales       * 1.2.1   2022-08-20 [1] CRAN (R 4.3.0)
# selectr        0.4-2   2019-11-20 [1] CRAN (R 4.3.0)
# sessioninfo    1.2.2   2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-6   2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.0)
# snakecase      0.11.0  2019-05-25 [1] CRAN (R 4.3.0)
# stats        * 4.3.1   2023-06-16 [2] local
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
# tools          4.3.1   2023-06-16 [2] local
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# usethis        2.2.0   2023-06-06 [1] CRAN (R 4.3.0)
# utf8           1.2.3   2023-01-31 [1] CRAN (R 4.3.0)
# utils        * 4.3.1   2023-06-16 [2] local
# vctrs          0.6.2   2023-04-19 [1] CRAN (R 4.3.0)
# vroom          1.6.3   2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.0   2022-03-03 [1] CRAN (R 4.3.0)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# xml2           1.3.4   2023-04-27 [1] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ──────────────────────────────────────────────────────────────────────────────────────────────
# > 


  