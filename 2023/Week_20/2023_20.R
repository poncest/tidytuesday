 
## Challenge: #TidyTuesday 2023 week 20
## Data:      Tornados
## Author:    Steven Ponce
## Date:      2023-05-14


## 1. LOAD PACKAGES & SETUP ----  
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, glue, camcorder, scales, lubridate, MetBrewer)
p_load(patchwork)


# |- figure size ---- 
gg_record(
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 6,
    height = 6,
    units  = "in",
    dpi    = 300) 

# |- resolution ---- 
showtext_opts(dpi = 300)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 20) 

tornados <- tt$tornados %>% clean_names()

readme(tt)  
rm(tt)   
 

## 3. EXAMINING THE DATA ----
glimpse(tornados)  



tornados_pr <- tornados %>% 
    select(om:date, st, mag, inj, fat, loss, sn) %>% 
    filter(st == "PR")


# # |- how many tornados? ----
## 29 total tornados

tornados_pr_tbl <- tornados_pr %>% 
    group_by(yr) %>% 
    summarise(count = n()) %>%
    ungroup() %>% 
    mutate(total = sum(count)) %>% 
    arrange(desc(count))
    
# how many injuries?
## only one reported injury (1969)

tornados_pr %>% 
    group_by(yr, inj) %>% 
    summarise(injuries = sum(inj)) %>%
    ungroup() %>% 
    mutate(total_injuries = sum(injuries)) %>% 
    arrange(desc(injuries))

# how many fatalities?
## zero fatality reported

tornados_pr %>% 
    group_by(yr, fat) %>% 
    summarise(fatalities = sum(fat)) %>%
    ungroup() %>% 
    mutate(total_fatalities = sum(fatalities)) %>% 
    arrange(desc(fatalities))

# |-  Estimated property damages? ---
## total property loss $1,431,000

tornados_pr_loss_tbl <- tornados_pr %>% 
    drop_na(loss) %>% 
    group_by(yr, loss) %>% 
    summarise(property_loss = sum(loss)) %>%
    ungroup() %>% 
    mutate(total_loss = sum(property_loss)) %>% 
    #mutate_all(~ replace_na(., 0)) %>% 
    mutate(bar_label = format(property_loss, big.mark = ","))


# |-  PR map ----
map <- map_data("world") %>% 
    filter(region == "Puerto Rico") %>% 
    mutate(label_text = "Total Tornados:<br>29")



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
map_col      <- "#1C6E8C"   
bkg_col      <- "#fcffff"
title_col    <- "gray10"              
subtitle_col <- "gray10" 
caption_col  <- "gray10" 



### |-  titles and caption ----

tt <- str_glue("#TidyTuesday: 2023 Week 20 &bull; Source: NOAA's National Weather Service Storm Prediction Center Severe Weather Maps, Graphics, and Data Page<br>")  
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Tornados in Puerto Rico?") 

subtitle_text <- str_wrap(str_glue("According to the NOAA's National Weather Service Storm Prediction Center Severe Weather Maps, Graphics, and Data Page, Puerto Rico has seen 29 recorded tornados. Most of them had a magnitude of EF-0, except four reported a magnitude of EF-1."), width = 80) 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Anton", family = "title") 
font_add_google("Titillium Web", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     


# |- map ----  
map_pr <- ggplot(data = map) +
    
    # geom
    # geom_polygon(aes(x = long, y = lat, group = group), 
    #              color = map_col, fill = map_col) +
    
    # geom_textbox(aes(label = label_text,
    #                  x = -66.5,
    #                  y = 18.2,
    #                  halign = 0.5,
    #                  vjust  = 0.5),
    #              family     = "text",
    #              color      = "red",
    #              fontface   = "bold",
    #              size       = 5,    #8
    #              width      = unit(50, "line"),
    #              box.color  = NA,
    #              alpha      = 0.9,
    #              fill       = NA,
    # ) +
    # 
    # # scales
    # scale_color_manual(values = col_palette) +
    # coord_map(projection = "mercator", clip = "on")+
    
    # labs
    labs(title    = title_text,
         subtitle = subtitle_text) +
    
    # theme
    theme_void() +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin        = margin(t = 5, r = 10, b = 5, l = 10),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 20,  #20
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_text(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.8, 
            #face           = "bold",
            size           = 10,   #10
            margin         = margin(t = 10, b = 30)),
    )

map_pr


# |- Estimated property loss plot ----

title_text_2    <- str_glue("Estimated Total Damage $1.4M") 


damage <- tornados_pr_loss_tbl %>% 
    ggplot(aes(x = yr, y = loss)) +
    
    # geoms
    
    geom_line()+
    geom_point()+
    geom_area(alpha = 0.1)+
    
    
    # scales
    scale_x_continuous(breaks = seq(1959, 2022, by = 5.0)) +
    
    scale_y_continuous(
        labels = dollar_format(suffix = " K", scale = 1e-3),
        breaks = seq(0, 60e5, by = 1e5),
        limits = c(0, 6e5),
        expand = c(0.03, 0.03),
    ) +
    
    # labs
    labs(
        x = "", y = "",
        title    = title_text_2,
        caption  = caption_text) +
    
    # theme
    theme_minimal(base_size   = 12, base_family = 'text') +   #12
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title.x       = element_text(size = 6, face = 'bold', margin = margin (t = 10)),  #10
        axis.title.y       = element_text(size = 6, face = 'bold', margin = margin (r = 10)),  #10
        
        axis.line.x        = element_line(color = "black"),
        axis.ticks.x       = element_line(color = "black"),
        
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.4, color = 'gray'),
        
        plot.margin        = margin(t = 5, r = 10, b = 5, l = 10),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 14,  ## 30
            margin         = margin(t = 10, b = 10)),
        
        plot.caption       = element_markdown( 
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 8,  #8
            hjust          = 0.5,
            margin         = margin(t = 5, b = 10)),
    )

damage



# |- Tornado count ----

title_text_3    <- str_glue("Reported Tornados 29") 


count <- tornados_pr_tbl %>% 
    ggplot(aes(x = yr, y = count)) +
    
    # geoms
    geom_line()+
    geom_point()+

    # scales
    scale_x_continuous(breaks = seq(1959, 2022, by = 5)) +
    
    scale_y_continuous(
        breaks = seq(0, 4, by = 1),
        limits = c(0, 4),
        expand = c(0.03, 0.03),
    ) +
    
    # labs
    labs(
        x = "", y = "",
        title    = title_text_3,
    ) +
    
    # theme
    theme_minimal(base_size   = 12, base_family = 'text') +  #12
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title.x       = element_text(size = 6, face = 'bold', margin = margin (t = 10)),  #10
        axis.title.y       = element_text(size = 6, face = 'bold', margin = margin (r = 10)),  #10
        
        axis.line.x        = element_line(color = "black"),
        axis.ticks.x       = element_line(color = "black"),
        
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.4, color = 'gray'),
        
        plot.margin        = margin(t = 5, r = 10, b = 5, l = 10),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 14,   #20  
            margin         = margin(t = 10, b = 10)),
        
        plot.caption       = element_markdown( 
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 8,  #8
            hjust          = 0.5,
            margin         = margin(t = 5, b = 10)),
    )

count



# |- Putting all plots together ---

#left  <- map_pr 
right <- count / damage

right + 
    plot_annotation(
        title = "Tornados in Puerto Rico?",
        subtitle = str_wrap(str_glue("According to the NOAA's National Weather Service Storm Prediction Center Severe Weather Maps, Graphics, and Data Page, Puerto Rico has seen 29 recorded tornados. Most of them had a magnitude of EF-0, except four reported a magnitude of EF-1."), width = 100),
    theme = theme(
        plot.margin   = margin(t = 5, r = 10, b = 5, l = 10),
        
        plot.title    = element_text(size = 22, face = "bold", family = 'title'),
        plot.subtitle = element_text(size = 9, family = 'subtitle')
    )
) 



sessioninfo::session_info(include_base = TRUE)
## 6. SESSION INFO ---- 

# sessioninfo::session_info(include_base = TRUE)
# ─ Session info ──────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.0 (2023-04-21 ucrt)
# os       Windows 10 x64 (build 19044)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-05-17
# rstudio  2023.03.0+386 Cherry Blossom (desktop)
# pandoc   NA
#
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.3.0   2023-04-21 [2] local
# P bit            4.0.5   2022-11-15 [?] CRAN (R 4.3.0)
# P bit64          4.0.5   2020-08-30 [?] CRAN (R 4.3.0)
# camcorder    * 0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# P cellranger     1.1.0   2016-07-27 [?] CRAN (R 4.3.0)
# P cli            3.6.1   2023-03-23 [?] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# P compiler       4.3.0   2023-04-21 [?] local
# P crayon         1.5.2   2022-09-29 [?] CRAN (R 4.3.0)
# P curl           5.0.0   2023-01-12 [?] CRAN (R 4.3.0)
# P datasets     * 4.3.0   2023-04-21 [?] local
# dplyr        * 1.1.2   2023-04-20 [1] CRAN (R 4.3.0)
# P fansi          1.0.4   2023-01-22 [?] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# P fs             1.6.2   2023-04-25 [?] CRAN (R 4.3.0)
# P generics       0.1.3   2022-07-05 [?] CRAN (R 4.3.0)
# ggplot2      * 3.4.2   2023-04-03 [1] CRAN (R 4.3.0)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.6.6-1 2022-04-05 [1] CRAN (R 4.3.0)
# P glue         * 1.6.2   2022-02-24 [?] CRAN (R 4.3.0)
# P graphics     * 4.3.0   2023-04-21 [?] local
# P grDevices    * 4.3.0   2023-04-21 [?] local
# P grid           4.3.0   2023-04-21 [?] local
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.3   2023-03-21 [1] CRAN (R 4.3.0)
# P here         * 1.0.1   2020-12-13 [?] CRAN (R 4.3.0)
# P hms            1.1.3   2023-03-21 [?] CRAN (R 4.3.0)
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# P jsonlite       1.8.4   2022-12-06 [?] CRAN (R 4.3.0)
# labeling       0.4.2   2020-10-20 [1] CRAN (R 4.3.0)
# P lifecycle      1.0.3   2022-10-07 [?] CRAN (R 4.3.0)
# P lubridate    * 1.9.2   2023-02-10 [?] CRAN (R 4.3.0)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# P magrittr       2.0.3   2022-03-30 [?] CRAN (R 4.3.0)
# mapproj        1.2.11  2023-01-12 [1] CRAN (R 4.3.0)
# maps           3.4.1   2022-10-30 [1] CRAN (R 4.3.0)
# markdown       1.6     2023-04-07 [1] CRAN (R 4.3.0)
# MetBrewer    * 0.2.0   2022-03-21 [1] CRAN (R 4.3.0)
# P methods      * 4.3.0   2023-04-21 [?] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# P pacman       * 0.5.1   2019-03-11 [?] CRAN (R 4.3.0)
# P parallel       4.3.0   2023-04-21 [?] local
# patchwork    * 1.1.2   2022-08-19 [1] CRAN (R 4.3.0)
# P pillar         1.9.0   2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig      2.0.3   2019-09-22 [?] CRAN (R 4.3.0)
# P purrr        * 1.0.1   2023-01-10 [?] CRAN (R 4.3.0)
# P R6             2.5.1   2021-08-19 [?] CRAN (R 4.3.0)
# ragg           1.2.5   2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# P readr        * 2.1.4   2023-02-10 [?] CRAN (R 4.3.0)
# P readxl         1.4.2   2023-02-09 [?] CRAN (R 4.3.0)
# renv           0.17.3  2023-04-06 [1] CRAN (R 4.3.0)
# P rlang          1.1.1   2023-04-28 [?] CRAN (R 4.3.0)
# P rprojroot      2.0.3   2022-04-02 [?] CRAN (R 4.3.0)
# P rstudioapi     0.14    2022-08-22 [?] CRAN (R 4.3.0)
# rsvg           2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# P rvest          1.0.3   2022-08-19 [?] CRAN (R 4.3.0)
# scales       * 1.2.1   2022-08-20 [1] CRAN (R 4.3.0)
# P selectr        0.4-2   2019-11-20 [?] CRAN (R 4.3.0)
# P sessioninfo    1.2.2   2021-12-06 [?] CRAN (R 4.3.0)
# showtext     * 0.9-6   2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.0)
# snakecase      0.11.0  2019-05-25 [1] CRAN (R 4.3.0)
# P stats        * 4.3.0   2023-04-21 [?] local
# P stringi        1.7.12  2023-01-11 [?] CRAN (R 4.3.0)
# P stringr      * 1.5.0   2022-12-02 [?] CRAN (R 4.3.0)
# svglite        2.1.1   2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8   2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4   2022-02-11 [1] CRAN (R 4.3.0)
# textshaping    0.3.6   2021-10-13 [1] CRAN (R 4.3.0)
# P tibble       * 3.2.1   2023-03-20 [?] CRAN (R 4.3.0)
# tidyr        * 1.3.0   2023-01-24 [1] CRAN (R 4.3.0)
# P tidyselect     1.2.0   2022-10-10 [?] CRAN (R 4.3.0)
# P tidytuesdayR * 1.0.2   2022-02-01 [?] CRAN (R 4.3.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.3.0)
# P timechange     0.2.0   2023-01-11 [?] CRAN (R 4.3.0)
# P tools          4.3.0   2023-04-21 [?] local
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# P usethis        2.1.6   2022-05-25 [?] CRAN (R 4.3.0)
# P utf8           1.2.3   2023-01-31 [?] CRAN (R 4.3.0)
# P utils        * 4.3.0   2023-04-21 [?] local
# P vctrs          0.6.2   2023-04-19 [?] CRAN (R 4.3.0)
# P vroom          1.6.3   2023-04-28 [?] CRAN (R 4.3.0)
# P withr          2.5.0   2022-03-03 [?] CRAN (R 4.3.0)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# P xml2           1.3.4   2023-04-27 [?] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/Desktop/tidytuesday/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/830ce55b
# 
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────
# > 
    
  