

## Challenge: #TidyTuesday 2023 week 49
## Data:      Life Expectancy
## Author:    Steven Ponce
## Date:      2023-12-04



#' REFERENCES:
#' Automating exploratory plots with ggplot2 and purrr
#' https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/
#' 
#' Efficiency and Consistency: Automate Subset Graphics with ggplot2 and purrr
#' https://www.cedricscherer.com/2023/07/05/efficiency-and-consistency-automate-subset-graphics-with-ggplot2-and-purrr/



## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
pacman::p_load(here, purrr, cowplot)



# |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 10,
    height = 5,
    units  = "in",
    dpi    = 320) 

# |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 49) 

life_expectancy <- tt$life_expectancy %>% clean_names() %>% glimpse()

tidytuesdayR::readme(tt) 
rm(tt)   



## 3. EXAMINING THE DATA ----
skim(life_expectancy)
glimpse(life_expectancy)
colnames(life_expectancy) %>% sort()



# 4. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- "#2E3440"
title_col    <- "#fafafa"          
subtitle_col <- "#fafafa"  
text_col     <- "#fafafa"  
caption_col  <- "white" 


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 49 &bull; Source: Our World in Data life expectancy <br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Life Expectancy in the Greater Antilles Islands") 

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Crimson Pro", family = "title")                            
font_add_google("Nunito Sans", family = "subtitle")   
font_add_google("Nunito Sans", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     


### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
    axis.title.x     = element_text(margin = margin(10, 0, 0, 0), size = rel(.8), color = "#E5E9F0"),
    axis.title.y     = element_text(margin = margin(0, 10, 0, 0), size = rel(.8), color = "#E5E9F0"),
    axis.text        = element_text(size = rel(.65), color = text_col),
 
    panel.grid.major = element_line(linetype = "dotdash", linewidth = 0.04, color = '#d7d7d8'),
    panel.grid       = element_blank(),
    panel.border     = element_rect(color = "#cccccc", fill = NA, linewidth = .6),
    panel.spacing    = unit(.9, "lines"),
 
    strip.text       = element_text(size = rel(1)),
 
    plot.margin      = margin(t = 10, r = 10, b = 10, l = 10),
    plot.title       = element_text(size = rel(1.4), face = "bold", hjust = .5, color = text_col),
    plot.background  = element_rect(fill = bkg_col, color = bkg_col),
    panel.background = element_rect(fill = bkg_col, color = bkg_col),
    
)


### |-  plot function ----
plot_figures <- function(island_name, save = FALSE) {
    
    ## check if input is valid
    if (!island_name %in% life_expectancy$entity) stop("Island name not listed in the data set.")
    if (!is.logical(save)) stop("save should be either TRUE or FALSE.")
    
    p <- 
        
        ggplot(data = life_expectancy,
               aes(x = year, y = life_expectancy, color = code, group = code))+
        
        # Geoms
        #  other/background data
        geom_step(data      = filter(life_expectancy, !entity %in% island_name),
                  color     = "grey65", 
                  alpha     = .25, 
                  linewidth = .1,
                  na.rm     = TRUE) + 
        
        # data of interest
        geom_step(data      = filter(life_expectancy, entity %in% island_name), 
                  color     = "#ACDB4B",     
                  linewidth = .5,
                  na.rm     = TRUE) +
        
        # Scales
        scale_x_continuous(breaks = seq(1900, 2021, by = 25),
                           limits = c(1900, 2021))+
        
        scale_y_continuous(limits = c(0, 100))+
        
        coord_cartesian(clip = 'off')+
        
        # Labs
        labs(x = "Year", 
             y = "Life Expectancy", 
             title = island_name, 
             color = NULL)
    
    # save plot
    if (isTRUE(save)) {
        ggsave(path = here("2023/Week_49/"),
               p, filename = paste0(island_name, ".png"), 
               width = 6, height = 6, units = 'in', dpi = 320)
    }
    
    return(p)
}


# islands of interest
greater_antilles <- c("Puerto Rico", "Cuba", "Cayman Islands", "Dominican Republic", "Jamaica", "Haiti")


# PLOT ALL USING {purr}
all_plots <- map(greater_antilles, ~ plot_figures(island_name = .x))


# viewing one plot
# all_plots[5]


### |-  main plot ----   
main <- cowplot::plot_grid(plotlist = all_plots)              
main


### |-  title plot ----                        
title <- ggplot() + 
    ggtitle(title_text) + 
    theme_void() +
    theme(
        plot.background = element_rect(fill = bkg_col, color = bkg_col),
        plot.margin     = margin(t = 0.5, r = 0, b = 0.5, l = 0, unit = "cm"),
        
        plot.title      = element_text(size   = rel(3), 
                                       family = 'title', 
                                       face   ="bold", 
                                       hjust  = 0.5, 
                                       color  = title_col),
        )
title

### |-  caption plot ----
caption <- ggplot() + 
    ggtitle(caption_text) + 
    theme_void() +
    theme(
        plot.background = element_rect(fill = bkg_col, color = bkg_col),
        plot.margin     = margin(t = 0, r = 0, b = 0.4, l = 0, unit = "cm"),
        
        plot.title      = element_markdown(size       = rel(.5), 
                                           family     = 'caption', 
                                           lineheight = 0.6,
                                           hjust      = 0.5, 
                                           halign     = 0.5, 
                                           color      = caption_col))
caption


# avoids text-spacing issues when using cowplot/ggtext/showtext
cowplot::set_null_device("agg") 

### |-  final plot ---- 
final_plot <- cowplot::plot_grid(
    title, main, caption, 
    rel_heights = c(0.15, 1, 0.03),
    ncol  = 1, 
    align = "v",
    byrow =  FALSE
    )

final_plot



# 5. SESSION INFO ---- 

sessioninfo::session_info(include_base = TRUE) 
# ─ Session info ──────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-12-06
# rstudio  2023.09.0+463 Desert Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────
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
# cowplot      * 1.1.1    2020-12-30 [1] CRAN (R 4.3.1)
# crayon         1.5.2    2022-09-29 [1] CRAN (R 4.3.0)
# curl           5.0.2    2023-08-14 [1] CRAN (R 4.3.1)
# datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.33   2023-07-07 [1] CRAN (R 4.3.1)
# dplyr        * 1.1.3    2023-09-03 [1] CRAN (R 4.3.1)
# fansi          1.0.5    2023-10-08 [1] CRAN (R 4.3.2)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.3.0)
# fastmap        1.1.1    2023-02-24 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.3    2023-07-20 [1] CRAN (R 4.3.1)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggplot2      * 3.4.4    2023-10-12 [1] CRAN (R 4.3.2)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue           1.6.2    2022-02-24 [1] CRAN (R 4.3.0)
# graphics     * 4.3.1    2023-06-16 [2] local
# grDevices    * 4.3.1    2023-06-16 [2] local
# grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# here         * 1.0.1    2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# htmltools      0.5.6    2023-08-10 [1] CRAN (R 4.3.1)
# httr           1.4.7    2023-08-15 [1] CRAN (R 4.3.1)
# janitor      * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.7    2023-06-29 [1] CRAN (R 4.3.1)
# knitr          1.44     2023-09-11 [1] CRAN (R 4.3.1)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.4    2023-11-07 [1] CRAN (R 4.3.2)
# lubridate    * 1.9.2    2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.5    2023-08-07 [1] CRAN (R 4.3.1)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.11     2023-10-19 [1] CRAN (R 4.3.2)
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
# repr           1.1.6    2023-01-26 [1] CRAN (R 4.3.1)
# rlang          1.1.2    2023-11-04 [1] CRAN (R 4.3.2)
# rprojroot      2.0.3    2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg           2.4.0    2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.3    2022-08-19 [1] CRAN (R 4.3.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.3.2)
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
# utf8           1.2.4    2023-10-22 [1] CRAN (R 4.3.2)
# utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.6.5    2023-12-01 [1] CRAN (R 4.3.2)
# vroom          1.6.3    2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.2    2023-10-30 [1] CRAN (R 4.3.2)
# xfun           0.41     2023-11-01 [1] CRAN (R 4.3.2)
# xml2           1.3.5    2023-07-06 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ─────────────────────────────────────────────────────────────────────────────────────────────
# > 
