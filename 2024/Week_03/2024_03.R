
## Challenge: #TidyTuesday 2024 week 03 
## Data:      US Polling Places 2012-2020
## Author:    Steven Ponce
## Date:      2024-01-15


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
theme_set(theme_minimal(base_size = 12, base_family = "text"))  


### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 5,
  units  = "in",
  dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(x = base::as.double("2024"), 
                            week = base::as.double("03")) 

polling_place <- tt$polling_places |> clean_names() |> glimpse()

tidytuesdayR::readme(tt) 
rm(tt)  


## 3. EXAMINING THE DATA ----
skim(polling_place)
glimpse(polling_place)
colnames(polling_place) |> sort()

range(polling_place$election_date)
unique(polling_place$state)
polling_place  |>  count(state, sort = T) 
polling_place  |>  count(location_type, sort = T) 
polling_place  |>  count(election_date, sort = T) 


## 4. TIDYDATA ----

### |- Tidy ----
data_plot <- polling_place |> 
    
    # election year
    mutate(election_year = year(election_date)) |> 
    
    # select last two elections (from the dataset)
    filter(election_year == 2018 |
               election_year == 2020) |> 
    
    # summary
    group_by(election_year, state) |> 
    summarise(polling_sites = n()) |> 
    ungroup() |> 
    
    # pivot wider to inspect and remove state with missing data
    pivot_wider(id_cols = state,
                names_from =  election_year, 
                values_from = polling_sites ) |> 
    drop_na() |> 
    
    # calculate ratio and shift in polling site number
    mutate(
        ratio = map2_dbl(
            .x = data_plot$"2018",
            .y = data_plot$"2020",
            .f = \(first_yr, last_yr){
                last_yr / first_yr
            }
        ),
        polling_sites_shift = 1 - ratio) |> 
    
    select(state, polling_sites_shift) |> 
    
    # color
    mutate(color = if_else(polling_sites_shift < 0, "neg", "pos")) |> 
    
    # lump and reorder
    mutate(state = fct_lump(state, 10)) |> 
    filter(state != "Other") |> 
    mutate(state = fct_reorder(state, polling_sites_shift))



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- '#222D4B'
title_col    <- "#F9E3D1"              
subtitle_col <- "#F9E3D1"  
caption_col  <- "#F9E3D1" 
text_col     <- "#F9E3D1" 

col_palette <- list("pos" = "#A3F1A4", "neg" ="#F04D25")    

### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2024 Week 03 &bull; Source: Center for Public Integrity <br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Which are the US States with the most significant shift in<br>polling site numbers?") 

pos <- str_glue("<span style='color:{ col_palette['pos'] }'>**Positive**</span>")
neg <- str_glue("<span style='color:{ col_palette['neg'] }'>**Negative**</span>")

subtitle_text <- str_glue("{ pos} and { neg } change, 2018-2020.<br>")

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Crimson Pro", family = "title")                            
font_add_google("Crimson Pro", family = "subtitle")   
font_add_google("Crimson Pro", family = "text")  
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = 'plot',
  
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(.8), color = text_col, family   = 'text', face = 'bold'),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(.8), color = text_col, family   = 'text', face = 'bold'),
  
  axis.text             = element_text(size = rel(.65), color = text_col, family   = 'text'),
  axis.line.x           = element_line(color = "gray"),
  
  panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = '#d7d7d8'),
  panel.grid            = element_blank(),
  
  
  plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),

  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  
)


### |-  final plot ----  
data_plot |> 
    
    ggplot(aes(x = polling_sites_shift, y = state, group = state, color = color)) +
    
    # Geoms
    geom_vline(xintercept = 0, linewidth = 0.2, color = 'lightgray') +
    
    geom_errorbarh(aes(xmin = 0, xmax = polling_sites_shift),
                   height = 0, linewidth  = 1) +
    
    # Scales
    scale_x_continuous(breaks = seq(-.5, 1, by = 0.25),
                       limits = c(-.5, 1),
                       labels = percent_format(suffix = " %"), 
                       position = 'top') +
    scale_y_discrete() +
    scale_color_manual(values = col_palette) +
    coord_cartesian(clip = "off") +

    
    # Labs
    labs(x = "Shift in polling sites number from 2018 to 2020",
         y = "",
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text) +
    
    # Theme
    theme( 
        panel.grid.minor     = element_blank(),
        panel.grid.major     = element_blank(),
        
        plot.title           = element_markdown(
            size               = rel(1.2), 
            family             = 'title',
            color              = title_col,
            face               = 'bold',
            lineheight         = 1.1,
            margin             = margin(t = 5, b = 5)),
        
        plot.subtitle        = element_markdown(
            size               = rel(.85),
            family             = 'subtitle',
            color              = title_col,
            lineheight         = 0.85,
            margin             = margin(t = 5, b = 10)),
        
        plot.caption         = element_markdown(
            size               = rel(.5),
            family             = 'caption',
            color              = caption_col,
            lineheight         = 0.6,
            hjust              = 0.5,
            halign             = 0.5,
            margin             = margin(t = 10, b = 5)),
    )



# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ───────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-01-15
# rstudio  2023.12.0+369 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.3.2   2023-10-31 [2] local
# base64enc      0.1-3   2015-07-28 [1] CRAN (R 4.3.0)
# P bit            4.0.5   2022-11-15 [?] CRAN (R 4.3.0)
# P bit64          4.0.5   2020-08-30 [?] CRAN (R 4.3.0)
# camcorder      0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# P cellranger     1.1.0   2016-07-27 [?] CRAN (R 4.3.0)
# P cli            3.6.1   2023-03-23 [?] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# P compiler       4.3.2   2023-10-31 [?] local
# P crayon         1.5.2   2022-09-29 [?] CRAN (R 4.3.0)
# P curl           5.0.0   2023-01-12 [?] CRAN (R 4.3.0)
# P datasets     * 4.3.2   2023-10-31 [?] local
# digest         0.6.31  2022-12-11 [1] CRAN (R 4.3.0)
# dplyr        * 1.1.2   2023-04-20 [1] CRAN (R 4.3.0)
# P fansi          1.0.4   2023-01-22 [?] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# fastmap        1.1.1   2023-02-24 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# P fs             1.6.2   2023-04-25 [?] CRAN (R 4.3.0)
# P generics       0.1.3   2022-07-05 [?] CRAN (R 4.3.0)
# ggplot2      * 3.4.4   2023-10-12 [1] CRAN (R 4.3.2)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.6.6-1 2022-04-05 [1] CRAN (R 4.3.0)
# P glue           1.6.2   2022-02-24 [?] CRAN (R 4.3.0)
# P graphics     * 4.3.2   2023-10-31 [?] local
# P grDevices    * 4.3.2   2023-10-31 [?] local
# P grid           4.3.2   2023-10-31 [?] local
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4   2023-08-21 [1] CRAN (R 4.3.2)
# P here           1.0.1   2020-12-13 [?] CRAN (R 4.3.0)
# P hms            1.1.3   2023-03-21 [?] CRAN (R 4.3.0)
# htmltools      0.5.5   2023-03-23 [1] CRAN (R 4.3.0)
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# P jsonlite       1.8.4   2022-12-06 [?] CRAN (R 4.3.0)
# knitr          1.42    2023-01-25 [1] CRAN (R 4.3.0)
# P lifecycle      1.0.3   2022-10-07 [?] CRAN (R 4.3.0)
# P lubridate    * 1.9.2   2023-02-10 [?] CRAN (R 4.3.0)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# P magrittr       2.0.3   2022-03-30 [?] CRAN (R 4.3.0)
# markdown       1.6     2023-04-07 [1] CRAN (R 4.3.0)
# P methods      * 4.3.2   2023-10-31 [?] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# P pacman         0.5.1   2019-03-11 [?] CRAN (R 4.3.0)
# P parallel       4.3.2   2023-10-31 [?] local
# P pillar         1.9.0   2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig      2.0.3   2019-09-22 [?] CRAN (R 4.3.0)
# P purrr        * 1.0.1   2023-01-10 [?] CRAN (R 4.3.0)
# P R6             2.5.1   2021-08-19 [?] CRAN (R 4.3.0)
# ragg           1.2.5   2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# P readr        * 2.1.4   2023-02-10 [?] CRAN (R 4.3.0)
# P readxl         1.4.2   2023-02-09 [?] CRAN (R 4.3.0)
# renv           1.0.3   2023-09-19 [1] CRAN (R 4.3.2)
# repr           1.1.6   2023-01-26 [1] CRAN (R 4.3.0)
# P rlang          1.1.1   2023-04-28 [?] CRAN (R 4.3.0)
# P rprojroot      2.0.3   2022-04-02 [?] CRAN (R 4.3.0)
# P rstudioapi     0.14    2022-08-22 [?] CRAN (R 4.3.0)
# rsvg           2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# P rvest          1.0.3   2022-08-19 [?] CRAN (R 4.3.0)
# scales       * 1.3.0   2023-11-28 [1] CRAN (R 4.3.2)
# P selectr        0.4-2   2019-11-20 [?] CRAN (R 4.3.0)
# P sessioninfo    1.2.2   2021-12-06 [?] CRAN (R 4.3.0)
# showtext     * 0.9-6   2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.0)
# skimr        * 2.1.5   2022-12-23 [1] CRAN (R 4.3.0)
# snakecase      0.11.0  2019-05-25 [1] CRAN (R 4.3.0)
# P stats        * 4.3.2   2023-10-31 [?] local
# P stringi        1.7.12  2023-01-11 [?] CRAN (R 4.3.0)
# P stringr      * 1.5.0   2022-12-02 [?] CRAN (R 4.3.0)
# svglite        2.1.1   2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8   2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4   2022-02-11 [1] CRAN (R 4.3.0)
# textshaping    0.3.6   2021-10-13 [1] CRAN (R 4.3.0)
# P tibble       * 3.2.1   2023-03-20 [?] CRAN (R 4.3.0)
# tidyr        * 1.3.0   2023-01-24 [1] CRAN (R 4.3.0)
# P tidyselect     1.2.0   2022-10-10 [?] CRAN (R 4.3.0)
# P tidytuesdayR   1.0.2   2022-02-01 [?] CRAN (R 4.3.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.3.0)
# P timechange     0.2.0   2023-01-11 [?] CRAN (R 4.3.0)
# P tools          4.3.2   2023-10-31 [?] local
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# P usethis        2.1.6   2022-05-25 [?] CRAN (R 4.3.0)
# P utf8           1.2.3   2023-01-31 [?] CRAN (R 4.3.0)
# P utils        * 4.3.2   2023-10-31 [?] local
# P vctrs          0.6.2   2023-04-19 [?] CRAN (R 4.3.0)
# P vroom          1.6.3   2023-04-28 [?] CRAN (R 4.3.0)
# withr          2.5.2   2023-10-30 [1] CRAN (R 4.3.2)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# P xml2           1.3.4   2023-04-27 [?] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/d6d4a2bb
# 
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────
# > 
    
