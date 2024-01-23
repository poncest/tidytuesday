
## Challenge: #TidyTuesday 2024 week 04 
## Data:      Educational attainment of young people in English towns
## Author:    Steven Ponce
## Date:      2024-01-23


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
pacman::p_load(ggstatsplot, patchwork, paletteer)

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)

    
## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(x = base::as.double("2024"), 
                            week = base::as.double("04")) 

english_education <- tt$english_education |> clean_names() |> glimpse()

tidytuesdayR::readme(tt) 
rm(tt)  


## 3. EXAMINING THE DATA ----
skim(english_education)
glimpse(english_education)
colnames(english_education) |> sort()

english_education |> count(town11nm, sort = T)
english_education |> count(size_flag, sort = T)
english_education |> count(rgn11nm, sort = T)
english_education |> count(income_flag, sort = T)
english_education |> count(university_flag, sort = T)


    
# 4. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- '#fbf7f0'
title_col    <- "#4f2217"              
subtitle_col <- "#4f2217"   
caption_col  <- "#4f2217"    
text_col     <- "#4f2217"   
col_palette  <- c("#6d2f20",  "#e09351", "#94b594", "#224b5e")

### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2024 Week 04 | Source: The UK Office for National Statistics\n")  

title_text    <- str_glue("Educational achievements of young individuals\nresiding in towns across England.") 
subtitle_text <- str_glue("Distribution of education score across English towns")
caption_text  <- str_glue("{tt} Visualization: @sponce1 | @sponce1(graphic.social) Code: poncest | Tools: #rstats #ggplot2")


### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Jura", family = "title")                            
font_add_google("Jura", family = "subtitle")   
font_add_google("Jura", family = "text")  
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  


### |-  final plot ----  
theme_set(theme_minimal(base_size = 12))

ggstatsplot::grouped_ggbetweenstats(
    data                  = english_education,
    x                     = size_flag,
    y                     = education_score,
    grouping.var          = university_flag,
    
    pairwise.display      = "significant",
    ggsignif.args         = list(textsize = 3, tip_length = 0.05, na.rm = TRUE),
    p.adjust.method       = "holm",
    
    plotgrid.args         = list(nrow = 1),                                     # layout

    centrality.point.args = list(size = 4, color = "#4d1c2b", alpha = 0.9),
    point.args            = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6, seed = 4321), 
                                 alpha  = 1, 
                                 size   = 1.5, 
                                 shape  = 16, 
                                 stroke = 0, 
                                 na.rm  = TRUE),
    
    boxplot.args          = list(width = 0.3, alpha = 0.2, na.rm = TRUE, color = 'gray20', linewidth = .4),
    violin.args           = list(width = 0.5, alpha = 0.2, na.rm = TRUE, color = 'gray20', linewidth = .4),
    
    ggplot.component      = list(
        ggplot2::scale_y_continuous(                                            # adjust y-axis scale
            breaks = seq(-15, 15, by = 5),
            limits = (c(-12, 15)),
        ),
        scale_color_manual(values = col_palette),                               # colors
        ggplot2::theme(title =  element_text(size = rel(.55),                   # adjust subplot title
                                             family = 'text',
                                             ))
        ),
    
    xlab = "Resident Size Area", 
    ylab = "Education Score",

    annotation.args  = list(
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text
        ),

   ggtheme = ggthemes::theme_pander(),                                          # theme

) &                                                                             # Notice the & from`{patchwork}
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
    
        axis.title.x          = element_text(margin = margin(10, 0, 0, 0), 
                                             size   = rel(2), 
                                             color  = text_col, 
                                             family = 'text', 
                                             face   = 'bold'),
        
        axis.title.y          = element_text(margin = margin(0, 10, 0, 0), 
                                             size   = rel(2), 
                                             color  = text_col, 
                                             family = 'text', 
                                             face   = 'bold'),
        
        axis.text             = element_text(size = rel(.8), 
                                             color = text_col, 
                                             family = 'text'),
        
        plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
    
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        text                  = element_text(family = 'text', color = text_col),
    
        plot.title            = element_text(
            size              = rel(2.3), 
            family            = 'title',
            color             = title_col,
            face              = 'bold',
            lineheight        = 1.1,
            margin            = margin(t = 5, b = 5)),
    
        plot.subtitle         = element_text(
            size              = rel(1.3),
            family            = 'subtitle',
            face              = 'plain',
            color             = title_col,
            lineheight        = 1.0,
            margin            = margin(t = 10, b = 5)),
        
        plot.caption          = element_text(                                   # element_markdown messes up the last equation
            size              = rel(.7),
            family            = 'caption',  
            color             = caption_col,
            lineheight        = 0.9,
            hjust             = 0.5,
            margin            = margin(t = 10, b = 5)),
    )



# 5. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ────────────────────────────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-01-23
# rstudio  2023.12.0+369 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────
# !  package          * version    date (UTC) lib source
# base             * 4.3.2      2023-10-31 [2] local
# base64enc          0.1-3      2015-07-28 [1] CRAN (R 4.3.0)
# P  BayesFactor        0.9.12-4.6 2023-12-01 [?] CRAN (R 4.3.2)
# P  bayestestR         0.13.1     2023-04-07 [?] CRAN (R 4.3.0)
# P  bit                4.0.5      2022-11-15 [?] CRAN (R 4.3.0)
# P  bit64              4.0.5      2020-08-30 [?] CRAN (R 4.3.0)
# P  BWStest            0.2.3      2023-10-10 [?] CRAN (R 4.3.2)
# cachem             1.0.8      2023-05-01 [1] CRAN (R 4.3.0)
# camcorder          0.1.0      2022-10-03 [1] CRAN (R 4.3.0)
# P  cellranger         1.1.0      2016-07-27 [?] CRAN (R 4.3.0)
# P  cli                3.6.1      2023-03-23 [?] CRAN (R 4.3.0)
# P  coda               0.19-4     2020-09-30 [?] CRAN (R 4.3.2)
# colorspace         2.1-0      2023-01-23 [1] CRAN (R 4.3.0)
# P  compiler           4.3.2      2023-10-31 [?] local
# P  correlation        0.8.4      2023-04-06 [?] CRAN (R 4.3.0)
# P  crayon             1.5.2      2022-09-29 [?] CRAN (R 4.3.0)
# P  curl               5.0.0      2023-01-12 [?] CRAN (R 4.3.0)
# P  datasets         * 4.3.2      2023-10-31 [?] local
# P  datawizard         0.9.1      2023-12-21 [?] CRAN (R 4.3.2)
# P  digest             0.6.34     2024-01-11 [?] CRAN (R 4.3.2)
# P  dplyr            * 1.1.4      2023-11-17 [?] RSPM (R 4.3.0)
# P  effectsize         0.8.6      2023-09-14 [?] CRAN (R 4.3.2)
# P  fansi              1.0.4      2023-01-22 [?] CRAN (R 4.3.0)
# farver             2.1.1      2022-07-06 [1] CRAN (R 4.3.0)
# fastmap            1.1.1      2023-02-24 [1] CRAN (R 4.3.0)
# forcats          * 1.0.0      2023-01-29 [1] CRAN (R 4.3.0)
# P  fs                 1.6.2      2023-04-25 [?] CRAN (R 4.3.0)
# P  generics           0.1.3      2022-07-05 [?] CRAN (R 4.3.0)
# ggplot2          * 3.4.4      2023-10-12 [1] CRAN (R 4.3.2)
# P  ggrepel            0.9.5      2024-01-10 [?] CRAN (R 4.3.2)
# P  ggsignif           0.6.4      2022-10-13 [?] CRAN (R 4.3.2)
# P  ggstatsplot      * 0.12.2     2024-01-14 [?] CRAN (R 4.3.2)
# ggtext           * 0.1.2      2022-09-16 [1] CRAN (R 4.3.0)
# P  ggthemes           5.0.0      2023-11-21 [?] CRAN (R 4.3.2)
# gifski             1.6.6-1    2022-04-05 [1] CRAN (R 4.3.0)
# P  glue               1.6.2      2022-02-24 [?] CRAN (R 4.3.0)
# P  gmp                0.7-4      2024-01-15 [?] CRAN (R 4.3.2)
# P  graphics         * 4.3.2      2023-10-31 [?] local
# P  grDevices        * 4.3.2      2023-10-31 [?] local
# P  grid               4.3.2      2023-10-31 [?] local
# gridtext           0.1.5      2022-09-16 [1] CRAN (R 4.3.0)
# gtable             0.3.4      2023-08-21 [1] CRAN (R 4.3.2)
# P  here               1.0.1      2020-12-13 [?] CRAN (R 4.3.0)
# P  hms                1.1.3      2023-03-21 [?] CRAN (R 4.3.0)
# htmltools          0.5.5      2023-03-23 [1] CRAN (R 4.3.0)
# httr               1.4.6      2023-05-08 [1] CRAN (R 4.3.0)
# P  insight            0.19.7     2023-11-26 [?] CRAN (R 4.3.2)
# janitor          * 2.2.0      2023-02-02 [1] CRAN (R 4.3.0)
# P  jsonlite           1.8.8      2023-12-04 [?] RSPM (R 4.3.0)
# knitr              1.42       2023-01-25 [1] CRAN (R 4.3.0)
# P  kSamples           1.2-10     2023-10-07 [?] CRAN (R 4.3.2)
# P  lattice            0.21-9     2023-10-01 [?] CRAN (R 4.3.2)
# P  lifecycle          1.0.3      2022-10-07 [?] CRAN (R 4.3.0)
# P  lubridate        * 1.9.2      2023-02-10 [?] CRAN (R 4.3.0)
# magick             2.7.4      2023-03-09 [1] CRAN (R 4.3.0)
# P  magrittr           2.0.3      2022-03-30 [?] CRAN (R 4.3.0)
# P  MASS               7.3-60     2023-05-04 [?] CRAN (R 4.3.2)
# P  Matrix             1.6-1.1    2023-09-18 [?] CRAN (R 4.3.2)
# P  MatrixModels       0.5-3      2023-11-06 [?] CRAN (R 4.3.2)
# memoise            2.0.1      2021-11-26 [1] CRAN (R 4.3.0)
# P  methods          * 4.3.2      2023-10-31 [?] local
# P  multcompView       0.1-9      2023-04-09 [?] CRAN (R 4.3.2)
# munsell            0.5.0      2018-06-12 [1] CRAN (R 4.3.0)
# P  mvtnorm            1.2-4      2023-11-27 [?] CRAN (R 4.3.2)
# P  pacman             0.5.1      2019-03-11 [?] CRAN (R 4.3.0)
# paletteer        * 1.5.0      2022-10-19 [1] CRAN (R 4.3.0)
# P  parallel           4.3.2      2023-10-31 [?] local
# P  parameters         0.21.3     2023-11-02 [?] CRAN (R 4.3.2)
# P  patchwork        * 1.2.0      2024-01-08 [?] CRAN (R 4.3.2)
# P  pbapply            1.7-2      2023-06-27 [?] CRAN (R 4.3.2)
# P  performance        0.10.8     2023-10-30 [?] CRAN (R 4.3.2)
# P  pillar             1.9.0      2023-03-22 [?] CRAN (R 4.3.0)
# P  pkgconfig          2.0.3      2019-09-22 [?] CRAN (R 4.3.0)
# P  PMCMRplus          1.9.10     2023-12-10 [?] CRAN (R 4.3.2)
# P  purrr            * 1.0.2      2023-08-10 [?] CRAN (R 4.3.1)
# P  R6                 2.5.1      2021-08-19 [?] CRAN (R 4.3.0)
# ragg               1.2.5      2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp               1.0.10     2023-01-22 [1] CRAN (R 4.3.0)
# PD RcppParallel       5.1.7      2023-02-27 [?] CRAN (R 4.3.2)
# P  readr            * 2.1.4      2023-02-10 [?] CRAN (R 4.3.0)
# P  readxl             1.4.2      2023-02-09 [?] CRAN (R 4.3.0)
# rematch2           2.1.2      2020-05-01 [1] CRAN (R 4.3.0)
# renv               1.0.3      2023-09-19 [1] CRAN (R 4.3.2)
# repr               1.1.6      2023-01-26 [1] CRAN (R 4.3.0)
# P  rlang              1.1.1      2023-04-28 [?] CRAN (R 4.3.0)
# P  Rmpfr              0.9-5      2024-01-21 [?] CRAN (R 4.3.2)
# P  rprojroot          2.0.3      2022-04-02 [?] CRAN (R 4.3.0)
# P  rstantools         2.3.1.1    2023-07-18 [?] CRAN (R 4.3.2)
# P  rstudioapi         0.14       2022-08-22 [?] CRAN (R 4.3.0)
# rsvg               2.4.0      2022-11-21 [1] CRAN (R 4.3.0)
# P  rvest              1.0.3      2022-08-19 [?] CRAN (R 4.3.0)
# scales           * 1.3.0      2023-11-28 [1] CRAN (R 4.3.2)
# P  selectr            0.4-2      2019-11-20 [?] CRAN (R 4.3.0)
# P  sessioninfo        1.2.2      2021-12-06 [?] CRAN (R 4.3.0)
# showtext         * 0.9-6      2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb       * 3.0        2020-06-04 [1] CRAN (R 4.3.0)
# skimr            * 2.1.5      2022-12-23 [1] CRAN (R 4.3.0)
# snakecase          0.11.0     2019-05-25 [1] CRAN (R 4.3.0)
# P  stats            * 4.3.2      2023-10-31 [?] local
# P  statsExpressions   1.5.3      2024-01-13 [?] CRAN (R 4.3.2)
# P  stringi            1.7.12     2023-01-11 [?] CRAN (R 4.3.0)
# P  stringr          * 1.5.0      2022-12-02 [?] CRAN (R 4.3.0)
# P  SuppDists          1.1-9.7    2022-01-03 [?] CRAN (R 4.3.2)
# svglite            2.1.1      2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts         * 0.8.8      2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts        1.0.4      2022-02-11 [1] CRAN (R 4.3.0)
# textshaping        0.3.6      2021-10-13 [1] CRAN (R 4.3.0)
# P  tibble           * 3.2.1      2023-03-20 [?] CRAN (R 4.3.0)
# tidyr            * 1.3.0      2023-01-24 [1] CRAN (R 4.3.0)
# P  tidyselect         1.2.0      2022-10-10 [?] CRAN (R 4.3.0)
# P  tidytuesdayR       1.0.2      2022-02-01 [?] CRAN (R 4.3.0)
# tidyverse        * 2.0.0      2023-02-22 [1] CRAN (R 4.3.0)
# P  timechange         0.2.0      2023-01-11 [?] CRAN (R 4.3.0)
# P  tools              4.3.2      2023-10-31 [?] local
# tzdb               0.4.0      2023-05-12 [1] CRAN (R 4.3.0)
# P  usethis            2.1.6      2022-05-25 [?] CRAN (R 4.3.0)
# P  utf8               1.2.3      2023-01-31 [?] CRAN (R 4.3.0)
# P  utils            * 4.3.2      2023-10-31 [?] local
# P  vctrs              0.6.5      2023-12-01 [?] CRAN (R 4.3.2)
# P  vroom              1.6.3      2023-04-28 [?] CRAN (R 4.3.0)
# withr              2.5.2      2023-10-30 [1] CRAN (R 4.3.2)
# xfun               0.39       2023-04-20 [1] CRAN (R 4.3.0)
# P  xml2               1.3.4      2023-04-27 [?] CRAN (R 4.3.0)
# P  zeallot            0.1.0      2018-01-28 [?] CRAN (R 4.3.2)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/d6d4a2bb
# 
# P ── Loaded and on-disk path mismatch.
# D ── DLL MD5 mismatch, broken installation.
# 
# ───────────────────────────────────────────────────────────────

