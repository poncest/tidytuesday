 
## Challenge: #TidyTuesday 2023 week 23
## Data:      Energy
## Author:    Steven Ponce
## Date:      2023-06-05


## 1. LOAD PACKAGES & SETUP ----  
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, camcorder, scales, lubridate)


# |- figure size ---- 
gg_record(
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 6,
    height = 6,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
#tt <- tidytuesdayR::tt_load(2023, week = 23) 

owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv') %>%
    clean_names()

# owid_energy <- tt$owid_energy %>% clean_names()

# readme(tt)  
# rm(tt)   
 

## 3. EXAMINING THE DATA ----
glimpse(owid_energy)  
owid_energy$iso_code %>% unique() %>% sort()
range(owid_energy$year)



# |- plot data
energy_tbl <- owid_energy %>% 
    filter(iso_code == "USA") %>% 
    
    # select Per capita electricity generation from 'x', 
    # measured in kilowatt-hours
    select(country:gdp, ends_with("elec_per_capita")) %>% 
    
    # pivot longer
    pivot_longer(cols      = !c(country, year, iso_code, population, gdp),
                 names_to  =  "energy",       # ernergy per capita
                 values_to = "output") %>%    # kilowatt_hours
    
    # rename variables
    mutate(
        type = case_when(
            energy == 'hydro_elec_per_capita'      ~ 'Hydroelectric',
            energy == 'nuclear_elec_per_capita'    ~ 'Nuclear',
            energy == 'solar_elec_per_capita'      ~ 'Solar',
            energy == 'wind_elec_per_capita'       ~ 'Wind',
            energy == 'low_carbon_elec_per_capita' ~ 'Low Carbon',
            energy == 'biofuel_elec_per_capita'    ~ 'Biofuel',
            energy == 'oil_elec_per_capita'        ~ 'Oil',
            energy == 'gas_elec_per_capita'        ~ 'Gas',
            energy =='renewables_elec_per_capita'  ~ 'Renewables',
            TRUE                                   ~ 'Others'
        )
    ) %>% 
    mutate(type = as_factor(type)) %>% 
    
    # # color
    mutate(
        highlight = if_else(type %in% c("Oil", "Hydroelectric", "Nuclear", 'Biofuel'), "down", "up")
        ) %>% 
    
    # manual reorder
    mutate(type =  fct_relevel(type, c('Gas', 'Low Carbon', 'Renewables','Solar', 'Wind',
                                       'Biofuel', 'Nuclear', 'Hydroelectric', 'Oil')
                               )
           ) %>% 
    
    # drop NA
    drop_na()
    


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- '#222D4B'
title_col    <- "#F9E3D1"              
subtitle_col <- "#F9E3D1"  
caption_col  <- "#F9E3D1" 
text_col     <- "#F9E3D1" 
col_palette <- list("up" = "#A3F1A4", "down" ="#F04D25")



### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 23 &bull; Source: Energy Data Explorer<br>")  
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("United States Energy Per Capita, 1965 - 2018") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")

up   <- str_glue("<span style='font-size:13pt; color: #A3F1A4'>**upper**</span>")
down <- str_glue("<span style='font-size:13pt; color: #F04D25'>**downward**</span>")

subtitle_text <- str_glue("The United States energy per capita {up} and {down} trend") 



### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Anton", family = "title") 
font_add_google("Titillium Web", family = "subtitle")
font_add_google("Titillium Web", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     



### |-   plot ----
energy_tbl %>% 
    filter(type != "Others") %>% 
    ggplot(aes(x = year, y = output, fill = highlight)) +
    
    # geoms
    geom_area(na.rm = TRUE, show.legend = FALSE)+

    # scales
    scale_x_continuous(breaks = seq(1965, 2018, by = 50),
                       limits = c(1965, 2020),
                       expand = c(0.05, 0.05)) +
    
    scale_y_continuous()+
    
    scale_fill_manual(values = col_palette) +
    
    coord_cartesian(clip = "off")+
    
    # labs
    labs(
        x        = "", 
        y        = "kilowatt-hours",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text) +
    
    # facet
    facet_wrap(vars(type), scales = "free_y")+
    
    # theme
    theme_minimal() +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'top',
        
        plot.background     = element_rect(fill = bkg_col, color = bkg_col),
        panel.background    = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin         = margin(t = 10, r = 10, b = 10, l = 10),
        
        axis.title          = element_text(size = 10, face = 'bold',color = text_col, margin = margin(t = 10)), 
        axis.text           = element_text(size = 10, color = text_col),
        axis.line.x         = element_line(color = text_col),
        
        panel.grid.major.y  = element_line(linetype = "dotted", linewidth = 0.3, color = 'gray'),
        panel.grid.minor.y  = element_blank(),
        
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        
        strip.text          = element_textbox(size     = 10,
                                              face     = 'bold',
                                              color    = text_col,
                                              hjust    = 0.5,
                                              halign   = 0.5,
                                              r        = unit(5, "pt"),
                                              width    = unit(5.5, "npc"),
                                              padding  = margin(3, 0, 3, 0),
                                              margin   = margin(3, 3, 3, 3),
                                              fill     = "transparent"),
        
        panel.spacing       = unit(1, 'lines'),
        
        plot.title          = element_markdown(
            family          = 'title',
            color           = title_col,
            face            = "bold",
            size            = 22,  
            margin          = margin(t = 10, b = 5)),
        
        plot.subtitle       = element_markdown(
            family          = 'text',
            color           = title_col,
            size            = 13,  
            margin          = margin(t = 5, b = 10)),
        
        plot.caption        = element_markdown(
            family          = 'caption',
            color           = caption_col,
            lineheight      = 0.6,
            size            = 10,
            hjust           = 0.5,
            halign          = 0.5,
            margin          = margin(t = 10, b = 10)),
    )



sessioninfo::session_info(include_base = TRUE)
## 6. SESSION INFO ---- 
# ─ Session info ─────────────────────────────────────
# setting  value
# version  R version 4.3.0 (2023-04-21 ucrt)
# os       Windows 10 x64 (build 19044)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/La_Paz
# date     2023-06-06
# rstudio  2023.03.0+386 Cherry Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.3.0   2023-04-21 [?] local
# bit            4.0.5   2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5   2020-08-30 [1] CRAN (R 4.3.0)
# camcorder    * 0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1   2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# P compiler       4.3.0   2023-04-21 [2] local
# crayon         1.5.2   2022-09-29 [1] CRAN (R 4.3.0)
# curl           5.0.0   2023-01-12 [1] CRAN (R 4.3.0)
# P datasets     * 4.3.0   2023-04-21 [2] local
# dplyr        * 1.1.2   2023-04-20 [1] CRAN (R 4.3.0)
# fansi          1.0.4   2023-01-22 [1] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.2   2023-04-25 [1] CRAN (R 4.3.0)
# generics       0.1.3   2022-07-05 [1] CRAN (R 4.3.0)
# ggplot2      * 3.4.2   2023-04-03 [1] CRAN (R 4.3.0)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.6.6-1 2022-04-05 [1] CRAN (R 4.3.0)
# glue           1.6.2   2022-02-24 [1] CRAN (R 4.3.0)
# P graphics     * 4.3.0   2023-04-21 [2] local
# P grDevices    * 4.3.0   2023-04-21 [2] local
# P grid           4.3.0   2023-04-21 [2] local
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.3   2023-03-21 [1] CRAN (R 4.3.0)
# here           1.0.1   2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3   2023-03-21 [1] CRAN (R 4.3.0)
# httr           1.4.5   2023-02-24 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.4   2022-12-06 [1] CRAN (R 4.3.0)
# labeling       0.4.2   2020-10-20 [1] CRAN (R 4.3.0)
# lifecycle      1.0.3   2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2   2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.6     2023-04-07 [1] CRAN (R 4.3.0)
# P methods      * 4.3.0   2023-04-21 [2] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.3.0)
# P parallel       4.3.0   2023-04-21 [2] local
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
# sessioninfo    1.2.2   2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-5   2022-02-09 [1] CRAN (R 4.3.0)
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
# timechange     0.2.0   2023-01-11 [1] CRAN (R 4.3.0)
# P tools          4.3.0   2023-04-21 [2] local
# tzdb           0.3.0   2022-03-28 [1] CRAN (R 4.3.0)
# usethis        2.1.6   2022-05-25 [1] CRAN (R 4.3.0)
# utf8           1.2.3   2023-01-31 [1] CRAN (R 4.3.0)
# P utils        * 4.3.0   2023-04-21 [2] local
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
# ────────────────────────────────────────────────────
# >
    
  