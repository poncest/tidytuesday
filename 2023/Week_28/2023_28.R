

## Challenge: #TidyTuesday 2023 week 28
## Data:      Global Surface Temperatures
## Author:    Steven Ponce
## Date:      2023-07-10


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, camcorder, scales, lubridate)
 

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
tt <- tidytuesdayR::tt_load(2023, week = 28) 

global_temps <- tt$global_temps %>% clean_names()
nh_temps     <- tt$nh_temps %>% clean_names()
sh_temps     <- tt$sh_temps %>% clean_names()

readme(tt)   
rm(tt)   


## 3. EXAMINING THE DATA ----
glimpse(global_temps)  

global_temps$year %>% range()


## 4. TIDYDATA ---- 

# |- plot data ----
combined_temps <- bind_rows(
    list("Global" = global_temps, "Northern Hemisphere" = nh_temps, "Southern Hemisphere" = sh_temps),
    .id = "id") %>% 
    select(id:dec) %>%
    filter(year != 2023) %>% 
    pivot_longer(cols = -c(id, year), names_to = "month", values_to = "temp") %>% 
    mutate(month = str_to_title(month)) %>% 
    group_by(id, year, month) %>%
    summarise(mean_temp = mean(temp)) %>%             # deviation from the mean temp 1951-1980
    arrange(desc(year)) %>% 
    ungroup() %>% 
    mutate(mean_temp = replace_na(mean_temp, 0)) %>% 
    mutate(highlight = if_else(mean_temp <= 0, "down", "up"))


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- "#42B5B7"                  #'#30AEB0'   
title_col    <- "#FAFAFA"              
subtitle_col <- "#FAFAFA"    
caption_col  <- "#FAFAFA"  
text_col     <- "#FAFAFA"  

col_palette <- list("down" = "#2F195B" , "up" = "#BD3250") 


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 28 &bull; Source: NASA GISS Surface Temperature Analysis (GISTEMP v4)<br>")  
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Global Surface Temperatures, 1880 - 2022") 

subtitle_text <- str_glue("The temperatures are deviations from the corresponding 1951-1980 means") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Anton", family = "title")                  #Archivo Black        
font_add_google("Roboto Condensed", family = "subtitle")            #Big Shoulders Display
font_add_google("Share", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     


### |-  initial plot ----
p1 <- combined_temps %>% 
    ggplot(aes(x = year, y = mean_temp, grop = id, fill = highlight)) +
    
    # geoms
    geom_col(width = 0.95, show.legend = FALSE)+
    geom_hline(yintercept = 0, linewidth = 0.3, color = "#FAFAFA")+
    
    # scales
    scale_y_continuous(expand = expansion(mult = 0.01)) +
    scale_x_continuous(expand = expansion(mult = 0.01), position = 'top') +
    scale_fill_manual(values = col_palette)+
    
    # labs
    labs(
        y        = "", 
        x        = "",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text) +
    
    # facet
    facet_wrap(~ factor(id, levels = c("Northern Hemisphere", "Southern Hemisphere", "Global")),
               ncol = 1 ) +
    
    # themes
    theme_minimal()+
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
        
        axis.text.y         = element_blank(),
        axis.title.y        = element_blank(),

        panel.grid          = element_blank(),
    
        strip.text          = element_textbox(size     = 12,
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
            size            = 24,  
            margin          = margin(t = 10, b = 5)),
        
        plot.subtitle       = element_markdown(
            family          = 'subtitle',
            color           = title_col,
            lineheight      = 0.8, 
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


### |-  Annotated Plot -----
# DF for annotations
arrow_up_df   <- tibble(id = "Northern Hemisphere", label_text = "ABOVE AVERAGE", highlight = "")
arrow_down_df <- tibble(id = "Northern Hemisphere", label_text = "BELOW AVERAGE", highlight = "")

p1 +
    # arrow up - above average
    geom_segment(data = arrow_up_df, 
                 aes(x = 1878, xend = 1878, y = 0, yend = 0+7), 
                 color     = "#BD3250", 
                 linewidth = 0.5, 
                 arrow     = arrow(length = unit(0.02, "npc"), type = "closed"),
    ) +
    
    # text up - above average
    geom_text(data = arrow_up_df, 
              aes(x = 1878, y = 10, label = str_wrap(label_text, width = 20)),
              family    = 'text',
              fontface  = "bold",
              hjust     = 0,
              size      = 4,
              color     = "#BD3250") +
    
    # arrow down - below average
    geom_segment(data = arrow_down_df,
                 aes(x = 1878, xend = 1878, y = 0, yend = 0-7),
                 color     = "#2F195B",
                 linewidth = 0.5,
                 arrow     = arrow(length = unit(0.02, "npc"), type = "closed"),
    ) + 
    
    # text down - below average
    geom_text(data = arrow_down_df, 
              aes(x = 1878, y = -10, label = str_wrap(label_text, width = 20)),
              family    = 'text',
              fontface  = "bold",
              hjust     = 0,
              vjust = -0.05,
              size      = 4,
              color     = "#2F195B") 



sessioninfo::session_info(include_base = TRUE) 
## 6. SESSION INFO ---- 
# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19044)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-07-11
# rstudio  2023.06.1+524 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────
# package      * version date (UTC) lib source
# base         * 4.3.1   2023-06-16 [2] local
# bit            4.0.5   2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5   2020-08-30 [1] CRAN (R 4.3.0)
# camcorder    * 0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
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
# labeling       0.4.2   2020-10-20 [1] CRAN (R 4.3.0)
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
# ────────────────────────────────────────────────────────────────────────────────────────────────────────────
# > 

  