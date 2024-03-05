
## Challenge: #TidyTuesday 2024 week 10
## Data:      Trash Wheel Collection Data
## Author:    Steven Ponce
## Date:      2024-03-04


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
pacman::p_load(ggdist)                                        


### |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 8,
    height = 8,
    units  = "in",
    dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(x = base::as.double("2024"), 
                            week = base::as.double("10")) 

trashwheel <- tt$trashwheel |> clean_names() |> glimpse()

tidytuesdayR::readme(tt) 
rm(tt)  


## 3. EXAMINING THE DATA ----
glimpse(trashwheel)
skim(trashwheel)
colnames(trashwheel)

trashwheel |> count(id, sort = T)
trashwheel |> count(name, sort = T)      
trashwheel |> count(dumpster, sort = T)  
trashwheel |> count(month, sort = T)
trashwheel |> count(year, sort = T)


## 4. TIDYDATA ----

### |- Tidy ----
trashwheel_df <- trashwheel |> 
    # Drop NAs
    drop_na(year) |> 
    # Format date
    mutate(date = mdy(date),
           year = year(date),
           month = month(date, abbr = TRUE)
           ) |>
    # Sum by month
    group_by(id, name, year, month) |> 
    summarise(across(weight:homes_powered, ~ sum(.x, na.rm = TRUE)
                     # .names = "mean_{.col}"
                     )) |> 
    ungroup() |> 
    # Date
    mutate(date = make_date(year, month)) |> 
    # Arrange columns
    select(id:month, date, everything()) |> 
    # Drop columns
    select(-c(weight, volume, homes_powered)) |> 
    # Pivot longer
    pivot_longer(cols = plastic_bottles:sports_balls, 
                 values_to = "count", 
                 names_to  = "trash_type") |> 
    # Format and convert to factors 
    mutate(
        trash_type = snakecase::to_title_case(trash_type),
        trash_type = fct_reorder(trash_type, count, .desc = TRUE, .na_rm = TRUE),
        name = fct_reorder(name, count, .desc = FALSE, .na_rm = TRUE),
    )
    
# Trash type count
trashwheel_df |> 
    group_by(trash_type) |> 
    summarise(total_count = sum(count)) |> 
    ungroup() |> 
    arrange(desc(total_count))

# trash_type      total_count
# <fct>                 <dbl>
# 1 Cigarette Butts    13618298
# 2 Plastic Bottles     2201576
# 3 Wrappers            1900711
# 4 Polystyrene         1425374
# 5 Plastic Bags         976127


# Trash type count by wheel
trashwheel_df |> 
    group_by(trash_type, name) |> 
    summarise(total_count = sum(count)) |> 
    ungroup() |> 
    arrange(desc(total_count))

# trash_type      name                  total_count
# <fct>           <fct>                       <dbl>
# 1 Cigarette Butts Mister Trash Wheel       11735100
# 2 Cigarette Butts Professor Trash Wheel     1254278
# 3 Plastic Bottles Mister Trash Wheel        1246155
# 4 Polystyrene     Mister Trash Wheel         920011
# 5 Wrappers        Mister Trash Wheel         898129


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#fbf7f0', 0.75) 
title_col    <- "gray10"              
subtitle_col <- "gray20"   
caption_col  <- "gray20"  
text_col     <- "gray20" 

col_palette <- MetBrewer::met.brewer("Tam", n = 4)

### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: { 2024 } Week { 10 } &bull; Source: Healthy Harbor Trash Wheel Collection Data<br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text <- str_glue("Mr. Trash Wheel") 

cigarette  <- str_glue("<span style='font-size:14pt'>**Cigarette Butts**</span>")

plastic    <- str_glue("<span style='font-size:14pt'>**Plastic Bottles**</span>")

subtitle_text <- str_glue("A trash interceptor is a device placed in a river to collect floating debris, preventing it from reaching the harbor.<br><br>",
                          "{ cigarette } and { plastic } were the most collected items.",
                          ) 

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Montserrat Subrayada", regular.wt = 700, family = "title")  
font_add_google("Abel", family = "subtitle")  
font_add_google("Barlow Condensed", family = "text")  
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    
    plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
    
    axis.text             = element_text(size = rel(1), color = text_col, family   = 'text'),
    
    axis.line.x           = element_line(color = "black", linewidth = .2),
    
    panel.grid.minor.y    = element_blank(),
    panel.grid.major.y    = element_blank(),
    
    panel.grid.minor.x    = element_blank(),
    panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.3, color = 'gray'),
    
    strip.text            = element_textbox(size     = rel(1.1),
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
)

### |-  final plot ----  
trashwheel_df |>
    ggplot(aes(x = count, y = name, fill = name)) +
    
    # Geoms
    ggdist::stat_slab() +
    
    geom_boxplot(
        width = 0.1,
        outlier.shape = NA
    ) +
    
    geom_point(
        aes(color = name),
        position = position_nudge(y = -0.16),
        shape = '|',
        size  = 2.5,
        alpha = 0.15
    ) +
    
    # Scales
    scale_x_continuous(trans = "log2", expand = expansion(mult = c(0.05, 0.0))) +
    scale_y_discrete() +
    scale_fill_manual(values = col_palette) +
    scale_color_manual(values = col_palette) +
    
    # Labs
    labs(x        = "Count (Log2 Scale)",
         y        = NULL,       
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text
    ) +
    
    # Facets
    facet_wrap(vars(trash_type)) +
    
    # Theme
    theme(
        plot.title      = element_text(
            size        = rel(2.8), 
            family      = 'title',
            face        = 'bold',
            color       = title_col,
            margin      = margin(t = 5, b = 5)), 
        
        plot.subtitle   = element_markdown(
            size        = rel(1.1), 
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

# ─ Session info ────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-03-05
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────
# ! package        * version date (UTC) lib source
# base           * 4.3.2   2023-10-31 [2] local
# base64enc        0.1-3   2015-07-28 [1] CRAN (R 4.3.0)
# P bit              4.0.5   2022-11-15 [?] CRAN (R 4.3.0)
# P bit64            4.0.5   2020-08-30 [?] CRAN (R 4.3.0)
# camcorder        0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# P cellranger       1.1.0   2016-07-27 [?] CRAN (R 4.3.0)
# P cli              3.6.1   2023-03-23 [?] CRAN (R 4.3.0)
# colorspace       2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark       1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# P compiler         4.3.2   2023-10-31 [?] local
# P crayon           1.5.2   2022-09-29 [?] CRAN (R 4.3.0)
# P curl             5.0.0   2023-01-12 [?] CRAN (R 4.3.0)
# P datasets       * 4.3.2   2023-10-31 [?] local
# P digest           0.6.34  2024-01-11 [?] CRAN (R 4.3.2)
# P distributional   0.3.2   2023-03-22 [?] CRAN (R 4.3.0)
# P dplyr          * 1.1.4   2023-11-17 [?] RSPM (R 4.3.0)
# P fansi            1.0.4   2023-01-22 [?] CRAN (R 4.3.0)
# farver           2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# fastmap          1.1.1   2023-02-24 [1] CRAN (R 4.3.0)
# forcats        * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# P fs               1.6.2   2023-04-25 [?] CRAN (R 4.3.0)
# P generics         0.1.3   2022-07-05 [?] CRAN (R 4.3.0)
# ggdist         * 3.3.1   2023-11-27 [1] CRAN (R 4.3.2)
# ggplot2        * 3.5.0   2024-02-23 [1] CRAN (R 4.3.2)
# ggtext         * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski           1.6.6-1 2022-04-05 [1] CRAN (R 4.3.0)
# glue             1.7.0   2024-01-09 [1] CRAN (R 4.3.2)
# P graphics       * 4.3.2   2023-10-31 [?] local
# P grDevices      * 4.3.2   2023-10-31 [?] local
# P grid             4.3.2   2023-10-31 [?] local
# gridtext         0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable           0.3.4   2023-08-21 [1] CRAN (R 4.3.2)
# P here             1.0.1   2020-12-13 [?] CRAN (R 4.3.0)
# P hms              1.1.3   2023-03-21 [?] CRAN (R 4.3.0)
# htmltools        0.5.7   2023-11-03 [1] CRAN (R 4.3.2)
# httr             1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# janitor        * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# P jsonlite         1.8.8   2023-12-04 [?] RSPM (R 4.3.0)
# knitr            1.45    2023-10-30 [1] CRAN (R 4.3.2)
# P lifecycle        1.0.3   2022-10-07 [?] CRAN (R 4.3.0)
# lubridate      * 1.9.3   2023-09-27 [1] CRAN (R 4.3.2)
# magick           2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# P magrittr         2.0.3   2022-03-30 [?] CRAN (R 4.3.0)
# markdown         1.6     2023-04-07 [1] CRAN (R 4.3.0)
# MetBrewer        0.2.0   2022-03-21 [1] CRAN (R 4.3.0)
# P methods        * 4.3.2   2023-10-31 [?] local
# munsell          0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# P pacman           0.5.1   2019-03-11 [?] CRAN (R 4.3.0)
# P parallel         4.3.2   2023-10-31 [?] local
# P pillar           1.9.0   2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig        2.0.3   2019-09-22 [?] CRAN (R 4.3.0)
# P purrr          * 1.0.2   2023-08-10 [?] CRAN (R 4.3.1)
# P R6               2.5.1   2021-08-19 [?] CRAN (R 4.3.0)
# P ragg             1.2.7   2023-12-11 [?] RSPM (R 4.3.0)
# Rcpp             1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# readr          * 2.1.5   2024-01-10 [1] CRAN (R 4.3.2)
# readxl           1.4.3   2023-07-06 [1] CRAN (R 4.3.2)
# renv             1.0.4   2024-02-21 [1] CRAN (R 4.3.2)
# repr             1.1.6   2023-01-26 [1] CRAN (R 4.3.0)
# rlang            1.1.3   2024-01-10 [1] CRAN (R 4.3.2)
# P rprojroot        2.0.3   2022-04-02 [?] CRAN (R 4.3.0)
# P rstudioapi       0.14    2022-08-22 [?] CRAN (R 4.3.0)
# rsvg             2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# rvest            1.0.4   2024-02-12 [1] CRAN (R 4.3.2)
# scales         * 1.3.0   2023-11-28 [1] CRAN (R 4.3.2)
# P selectr          0.4-2   2019-11-20 [?] CRAN (R 4.3.0)
# P sessioninfo      1.2.2   2021-12-06 [?] CRAN (R 4.3.0)
# showtext       * 0.9-6   2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb     * 3.0     2020-06-04 [1] CRAN (R 4.3.0)
# skimr          * 2.1.5   2022-12-23 [1] CRAN (R 4.3.0)
# snakecase        0.11.0  2019-05-25 [1] CRAN (R 4.3.0)
# P stats          * 4.3.2   2023-10-31 [?] local
# P stringi          1.7.12  2023-01-11 [?] CRAN (R 4.3.0)
# P stringr        * 1.5.0   2022-12-02 [?] CRAN (R 4.3.0)
# svglite          2.1.1   2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts       * 0.8.8   2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts      1.0.5   2023-10-09 [1] CRAN (R 4.3.2)
# textshaping      0.3.6   2021-10-13 [1] CRAN (R 4.3.0)
# P tibble         * 3.2.1   2023-03-20 [?] CRAN (R 4.3.0)
# tidyr          * 1.3.1   2024-01-24 [1] CRAN (R 4.3.2)
# P tidyselect       1.2.0   2022-10-10 [?] CRAN (R 4.3.0)
# tidytuesdayR     1.0.3   2023-12-13 [1] CRAN (R 4.3.2)
# tidyverse      * 2.0.0   2023-02-22 [1] CRAN (R 4.3.0)
# P timechange       0.2.0   2023-01-11 [?] CRAN (R 4.3.0)
# P tools            4.3.2   2023-10-31 [?] local
# tzdb             0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# P usethis          2.1.6   2022-05-25 [?] CRAN (R 4.3.0)
# P utf8             1.2.3   2023-01-31 [?] CRAN (R 4.3.0)
# P utils          * 4.3.2   2023-10-31 [?] local
# P vctrs            0.6.5   2023-12-01 [?] CRAN (R 4.3.2)
# vroom            1.6.5   2023-12-05 [1] CRAN (R 4.3.2)
# withr            2.5.2   2023-10-30 [1] CRAN (R 4.3.2)
# xfun             0.39    2023-04-20 [1] CRAN (R 4.3.0)
# P xml2             1.3.4   2023-04-27 [?] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/d6d4a2bb
# 
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────────────
# > 
