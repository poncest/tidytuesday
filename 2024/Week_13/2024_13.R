
## Challenge: #TidyTuesday 2024 week 13
## Data:      NCAA Men's March Madness
## Author:    Steven Ponce
## Date:      2024-03-25


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)

### |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 6,
    height = 10,
    units  = "in",
    dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
# tt <- tidytuesdayR::tt_load(x = base::as.double("2024"), 
#                            week = base::as.double("13")) 

# team_results <- tt$team_results |> clean_names() |> glimpse()
# public_picks <- tt$public_picks |> clean_names() |> glimpse()

# Option 2: Read directly from GitHub
team_results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/team-results.csv')|> clean_names() |> glimpse()
public_picks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/public-picks.csv')|> clean_names() |> glimpse()

# tidytuesdayR::readme(tt) 
# rm(tt)  


## 3. EXAMINING THE DATA ----
glimpse(public_picks)
skim(public_picks)
colnames(public_picks)


## 4. TIDYDATA ----

### |- Tidy ----
data_plot <- public_picks |> 
    # Drop colums
    select(-teamno, -year) |> 
    # Pivot longer
    pivot_longer(cols = -team,
                 names_to  = "name",
                 values_to = "pct") |> 
    
    mutate(
        # Convert pct to digits
        pct = parse_number(pct) / 100,
        # Add round column based on name
        round = case_when(
            name == "r64"    ~ "Round of 64",
            name == "r32"    ~ "Round of 32",
            name == "s16"    ~ "Sweet 16",
            name == "e8"     ~ "Elite 8",
            name == "f4"     ~ "Final 4",
            name == "finals" ~ "Finals"
        ),
        # Specify levels
        round = factor(round,
                       levels = c("Round of 64", "Round of 32", "Sweet 16", 
                                  "Elite 8", "Final 4", "Finals")
                       ),
    # Winning pct bins
        winning_pct_bin = case_when(
        pct <= .20  ~ "20 or less",
        pct <  .40  ~ "20 - 40",
        pct <  .60  ~ "40 - 60",
        pct <  .80  ~ "60 - 80",
        pct >= .80  ~ "80 or more"
        ),
        winning_pct_bin = factor(winning_pct_bin,
                                 levels = c("20 or less", "20 - 40", "40 - 60","60 - 80", "80 or more")),
    # Reorder
    team = fct_reorder(team, pct, .desc = FALSE)
    )



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('gray', 0.8)    
title_col    <- "#373131"              
subtitle_col <- "gray30"    
caption_col  <- "gray30"   
text_col     <- "gray30"     
col_palette  <- colorspace::lighten("#A787AA",  seq(0.9, 0, length.out = 5))


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: { 2024 } Week { 13 } &bull; Source: Men's March Madness Data<br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Public Predictions: 2024 March Madness") 

subtitle_text <- str_glue("NCAA Men's Basketball")

fill_text     <- " Winning Percentage Bins (%)"

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Squada One", regular.wt = 400, family = "title")       
font_add_google("Abel", family = "subtitle")  
font_add_google("Saira Extra Condensed", family = "text")        
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'top',
    
    plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
    
    axis.text             = element_text(size = rel(1), color = text_col, family = 'text'),
    
    axis.line.x           = element_line(color = "gray90", linewidth = .2),

    panel.grid.minor.y    = element_blank(),
    panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.05, color = 'gray'),

    panel.grid.minor.x    = element_blank(),
    panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray'),
)


### |-  final plot ----  
data_plot |> 
    ggplot(aes(x = round, y = team, fill = winning_pct_bin)) +  
    
    # Geoms
    geom_tile(col = 'gray50') +
    
    # Scales
    scale_x_discrete(labels = c("Round<br>of 64", "Round<br>of 32", "Sweet<br>16",
                                "Elite 8", "Final 4", "Finals"),
                     position = 'top') +
    
    scale_y_discrete(expand = c(0,0)) +
    
    scale_fill_manual(values = col_palette,
                      expand = c(0,0)) +
    
    guides(fill = guide_legend(title.position = 'top', nrow = 1)) +
    
    coord_fixed(clip = 'off', ratio = .25) +
    
    # Labs
    labs(
        x        = element_blank(),
        y        = element_blank(),
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text,
        fill     = fill_text) +
    
    # Theme
    theme(
        legend.title    = element_markdown(size = rel(.95), hjust = 0.5),
        legend.text     = element_text(size = rel(0.8)), 
        
        axis.text.x.top = element_markdown(face = 'bold', vjust = 0.5),
        axis.text.y     = element_text(size = rel(.85)),
        axis.ticks      = element_blank(),
        
        plot.title      = element_text(
            size        = rel(1.85), 
            family      = 'title',
            face        = 'bold',
            color       = title_col,
            margin      = margin(t = 5, b = 5)), 
        
        plot.subtitle   = element_markdown(
            size        = rel(.92), 
            family      = 'subtitle',
            color       = title_col,
            lineheight  = 1.1, 
            margin      = margin(t = 5, b = 10)),  
        
        plot.caption    = element_markdown(
            size        = rel(.65), 
            family      = 'caption',
            color       = caption_col,
            lineheight  = 0.65,
            hjust       = 0.5,
            halign      = 0.5,
            margin      = margin(t = 10, b = 5)),
    ) 



# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ───────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.3 (2024-02-29 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-03-25
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.3.3   2024-02-29 [2] local
# base64enc      0.1-3   2015-07-28 [1] CRAN (R 4.3.0)
# P bit            4.0.5   2022-11-15 [?] CRAN (R 4.3.0)
# P bit64          4.0.5   2020-08-30 [?] CRAN (R 4.3.0)
# camcorder      0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# P cellranger     1.1.0   2016-07-27 [?] CRAN (R 4.3.0)
# P cli            3.6.1   2023-03-23 [?] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# P compiler       4.3.3   2024-02-29 [?] local
# P crayon         1.5.2   2022-09-29 [?] CRAN (R 4.3.0)
# P curl           5.0.0   2023-01-12 [?] CRAN (R 4.3.0)
# P datasets     * 4.3.3   2024-02-29 [?] local
# P digest         0.6.34  2024-01-11 [?] CRAN (R 4.3.2)
# P dplyr        * 1.1.4   2023-11-17 [?] RSPM (R 4.3.0)
# P fansi          1.0.4   2023-01-22 [?] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# fastmap        1.1.1   2023-02-24 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.3   2023-07-20 [1] CRAN (R 4.3.3)
# P generics       0.1.3   2022-07-05 [?] CRAN (R 4.3.0)
# ggplot2      * 3.5.0   2024-02-23 [1] CRAN (R 4.3.2)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.6.6-1 2022-04-05 [1] CRAN (R 4.3.0)
# glue           1.7.0   2024-01-09 [1] CRAN (R 4.3.2)
# P graphics     * 4.3.3   2024-02-29 [?] local
# P grDevices    * 4.3.3   2024-02-29 [?] local
# P grid           4.3.3   2024-02-29 [?] local
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4   2023-08-21 [1] CRAN (R 4.3.2)
# P here           1.0.1   2020-12-13 [?] CRAN (R 4.3.0)
# P hms            1.1.3   2023-03-21 [?] CRAN (R 4.3.0)
# htmltools      0.5.7   2023-11-03 [1] CRAN (R 4.3.2)
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# P jsonlite       1.8.8   2023-12-04 [?] RSPM (R 4.3.0)
# knitr          1.45    2023-10-30 [1] CRAN (R 4.3.2)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.3.1)
# P lifecycle      1.0.3   2022-10-07 [?] CRAN (R 4.3.0)
# lubridate    * 1.9.3   2023-09-27 [1] CRAN (R 4.3.2)
# magick         2.8.3   2024-02-18 [1] CRAN (R 4.3.3)
# P magrittr       2.0.3   2022-03-30 [?] CRAN (R 4.3.0)
# markdown       1.12    2023-12-06 [1] CRAN (R 4.3.3)
# P methods      * 4.3.3   2024-02-29 [?] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# P pacman         0.5.1   2019-03-11 [?] CRAN (R 4.3.0)
# P parallel       4.3.3   2024-02-29 [?] local
# P pillar         1.9.0   2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig      2.0.3   2019-09-22 [?] CRAN (R 4.3.0)
# P pkgload        1.3.3   2023-09-22 [?] CRAN (R 4.3.2)
# P purrr        * 1.0.2   2023-08-10 [?] CRAN (R 4.3.1)
# P R6             2.5.1   2021-08-19 [?] CRAN (R 4.3.0)
# P ragg           1.2.7   2023-12-11 [?] RSPM (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# readr        * 2.1.5   2024-01-10 [1] CRAN (R 4.3.2)
# readxl         1.4.3   2023-07-06 [1] CRAN (R 4.3.2)
# renv           1.0.5   2024-02-29 [1] CRAN (R 4.3.3)
# repr           1.1.6   2023-01-26 [1] CRAN (R 4.3.0)
# rlang          1.1.3   2024-01-10 [1] CRAN (R 4.3.2)
# P rprojroot      2.0.3   2022-04-02 [?] CRAN (R 4.3.0)
# P rstudioapi     0.14    2022-08-22 [?] CRAN (R 4.3.0)
# rsvg           2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.4   2024-02-12 [1] CRAN (R 4.3.2)
# scales       * 1.3.0   2023-11-28 [1] CRAN (R 4.3.2)
# P sessioninfo    1.2.2   2021-12-06 [?] CRAN (R 4.3.0)
# showtext     * 0.9-6   2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.0)
# skimr        * 2.1.5   2022-12-23 [1] CRAN (R 4.3.0)
# snakecase      0.11.0  2019-05-25 [1] CRAN (R 4.3.0)
# P stats        * 4.3.3   2024-02-29 [?] local
# stringi        1.8.3   2023-12-11 [1] CRAN (R 4.3.2)
# stringr      * 1.5.1   2023-11-14 [1] CRAN (R 4.3.3)
# svglite        2.1.1   2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts    1.0.5   2023-10-09 [1] CRAN (R 4.3.2)
# textshaping    0.3.6   2021-10-13 [1] CRAN (R 4.3.0)
# P tibble       * 3.2.1   2023-03-20 [?] CRAN (R 4.3.0)
# tidyr        * 1.3.1   2024-01-24 [1] CRAN (R 4.3.2)
# P tidyselect     1.2.0   2022-10-10 [?] CRAN (R 4.3.0)
# tidytuesdayR   1.0.3   2023-12-13 [1] CRAN (R 4.3.2)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.3.0)
# P timechange     0.2.0   2023-01-11 [?] CRAN (R 4.3.0)
# P tools          4.3.3   2024-02-29 [?] local
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# usethis        2.2.3   2024-02-19 [1] CRAN (R 4.3.3)
# P utf8           1.2.3   2023-01-31 [?] CRAN (R 4.3.0)
# P utils        * 4.3.3   2024-02-29 [?] local
# P vctrs          0.6.5   2023-12-01 [?] CRAN (R 4.3.2)
# vroom          1.6.5   2023-12-05 [1] CRAN (R 4.3.2)
# withr          2.5.2   2023-10-30 [1] CRAN (R 4.3.2)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# P xml2           1.3.4   2023-04-27 [?] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/07866f9d
# 
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────
# > 