
## Challenge: #TidyTuesday 2024 week 34
## Data:      English Monarchs and Marriages
## Author:    Steven Ponce
## Date:      2024-08-19


## 1. LOAD PACKAGES & SETUP ----
pacman::p_load(
  tidyverse,     # Easily Install and Load the 'Tidyverse'
  ggtext,        # Improved Text Rendering Support for 'ggplot2'
  showtext,      # Using Fonts More Easily in R Graphs
  janitor,       # Simple Tools for Examining and Cleaning Dirty Data
  skimr,         # Compact and Flexible Summaries of Data
  scales,        # Scale Functions for Visualization
  lubridate,     # Make Dealing with Dates a Little Easier
  MetBrewer,     # Color Palettes Inspired by Works at the Metropolitan Museum of Art
  ggalt          # Extra Coordinate Systems, 'Geoms', Statistical Transformations, Scales and Fonts for 'ggplot2'
 )  


### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  8,
  height =  12,
  units  = "in",
  dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <-tidytuesdayR::tt_load(2024, week = 34) 

monarchs_marriages <- tt$english_monarchs_marriages_df |> clean_names() |> glimpse()

tidytuesdayR::readme(tt)
rm(tt)



## 3. EXAMINING THE DATA ----
glimpse(monarchs_marriages)
skim(monarchs_marriages)



## 4. TIDYDATA ----

# Tidy
cleaned_monarchs_marriages <- monarchs_marriages |>
    mutate(
        across(everything(), ~na_if(., "-")),
        across(everything(), ~na_if(., "NA")),
        across(everything(), ~na_if(., "?")),
        across(everything(), ~na_if(., "–")),
        across(where(is.character), str_trim),
        across(c(king_age, consort_age, year_of_marriage), ~str_replace_all(., "\\(\\?\\)", "")),
        across(c(king_age, consort_age, year_of_marriage), as.numeric),
        pair_name = paste(king_name, "-", consort_name)
    )

# Filter out rows with NA values in king_age or consort_age, and calculate the age gap
filtered_monarchs_marriages <- cleaned_monarchs_marriages %>%
    filter(!is.na(king_age) & !is.na(consort_age)) %>%
    mutate(
        age_gap = abs(king_age - consort_age),
        pair_name = paste(king_name, "-", consort_name),
        pair_name = fct_reorder(pair_name, -consort_age, .desc = TRUE)
    ) 

# Subset for kings labels
kings_data <- filtered_monarchs_marriages %>%
    select(pair_name, king_name,king_age, consort_age) %>%
    mutate(hjust = case_when(
        king_age < consort_age ~ 1,  
        king_age > consort_age ~ -0.5,  
        TRUE ~ -1                  
    ))

# Subset for consorts labels
consorts_data <- filtered_monarchs_marriages%>%
    select(pair_name, consort_name, consort_age, king_age) %>%
    mutate(hjust = case_when(
        consort_age < king_age ~ 1,
        consort_age > king_age ~ -0.4,
        TRUE ~ 1
    ))



# 5. VISUALIZATION ----

### |- plot aesthetics ----
bkg_col      <- colorspace::lighten('#f7f5e9', 0.05)    
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <- col_palette  <- MetBrewer::met.brewer("Degas")[c(6, 2)]  

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 34 } &bull; Source: List of Monarchs by marriage (.ianvisits.co.uk)<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Age at Marriage of Kings and Consorts")

subtitle_text <- str_glue("A comparison of ages at marriage among monarchs and their consorts across history.")

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Merriweather Sans", regular.wt = 400, family = "subtitle")
font_add_google("Merriweather Sans", regular.wt = 400, family = "text")
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
showtext_auto(enable = TRUE)

### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = "top",
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    plot.margin           = margin(t = 20, r = 25, b = 20, l = 25),
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.2), color = text_col,
                                         family = "text", face = "bold", hjust = 0.5),
    axis.title.y          = element_blank(),                                    # Hide y-axis title
    axis.text.y           = element_blank(),                                    # Hide y-axis labels
    axis.text.x           = element_text(size = rel(0.95), color = text_col, family = "text"),
    axis.line.x           = element_line(color = "gray40", linewidth = 0.12),
    panel.grid.minor.x    = element_blank(),
    panel.grid.major.x    = element_blank(),
    panel.grid.major.y    = element_blank(),  
    panel.grid.minor.y    = element_blank(),
    
    axis.text.x.top       = ggtext::element_markdown(),
)   


### |-  final plot ----  
ggplot() +
    geom_dumbbell(
        data = filtered_monarchs_marriages,
        aes(x = king_age, xend = consort_age, y = pair_name),
        size = 0.25, 
        color = 'gray50', 
        size_x = 3, 
        size_xend = 3, 
        dot_guide = FALSE
    ) +
    
    # Plot kings' data
    geom_point(
        data = kings_data,
        aes(x = king_age, y = pair_name, color = "King's Age"), 
        size = 3
    ) +
    geom_text(
        data = kings_data,
        aes(x = king_age, y = pair_name, label = king_name, hjust = hjust),
        size = 2.8,
        color = col_palette[2],                
        fontface = 'bold',
        nudge_x = -2  
    ) +
    
    # Plot consorts' data
    geom_point(
        data = consorts_data,
        aes(x = consort_age, y = pair_name, color = "Consort's Age"), 
        size = 3
    ) +
    geom_text(
        data = consorts_data,
        aes(x = consort_age, y = pair_name, label = consort_name, hjust = hjust),
        size = 2.8,
        color = col_palette[1],               
        fontface = 'bold',
        nudge_x = -2  
    ) +
    
    # Scales
    scale_x_continuous(
        limits = c(-15,80),
        labels = scales::label_number(suffix = " yrs"),
        position = "top"
    ) +
    scale_y_discrete() +
    scale_color_manual(
        name = "Legend:",
        values = col_palette                
    ) +
    coord_cartesian(clip = 'off') +
    
    # Labs
    labs(
        x = element_blank(),
        y = element_blank(),
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text
    ) +
    
    # Theme
    theme(
        plot.title = element_markdown(
            size = rel(2),
            family = "title",
            color = title_col,
            face = "bold",
            lineheight = 0.85,
            margin = margin(t = 5, b = 5)
        ),
        plot.subtitle = element_markdown(
            size = rel(1.1),
            family = "subtitle",
            color = title_col,
            lineheight = 1,
            margin = margin(t = 5, b = 15)
        ),
        plot.caption = element_markdown(
            size = rel(.65),
            family = "caption",
            color = caption_col,
            lineheight = 0.6,
            hjust = 0,
            halign = 0,
            margin = margin(t = 10, b = 0)
        )
    )



# 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-08-19
# rstudio  2024.04.2+764 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# P ash            1.0-15   2015-09-01 [?] CRAN (R 4.4.0)
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger     1.1.0    2016-07-27 [?] CRAN (R 4.4.0)
# P cli            3.6.2    2023-12-11 [?] CRAN (R 4.4.0)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# P digest         0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P extrafont      0.19     2023-01-18 [?] CRAN (R 4.4.0)
# P extrafontdb    1.0      2012-06-11 [?] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# P fs             1.6.4    2024-04-25 [?] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# P GGally         2.2.1    2024-02-14 [?] CRAN (R 4.4.0)
# P ggalt        * 0.4.0    2017-02-15 [?] CRAN (R 4.4.1)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggstats        0.6.0    2024-04-05 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue           1.7.0    2024-01-09 [?] CRAN (R 4.4.0)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here           1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P httr           1.4.7    2023-08-15 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P KernSmooth     2.23-22  2023-07-10 [?] CRAN (R 4.4.0)
# P knitr          1.46     2024-04-06 [?] CRAN (R 4.4.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P maps           3.4.2    2023-12-15 [?] CRAN (R 4.4.0)
# P markdown       1.12     2023-12-06 [?] CRAN (R 4.4.0)
# P MASS           7.3-60.2 2024-04-24 [?] local
# P MetBrewer    * 0.2.0    2022-03-21 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman         0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P pkgload        1.3.4    2024-01-16 [?] CRAN (R 4.4.0)
# P plyr           1.8.9    2023-10-02 [?] CRAN (R 4.4.0)
# P proj4          1.0-14   2024-01-14 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P readxl         1.4.3    2023-07-06 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# P rlang          1.1.3    2024-01-10 [?] CRAN (R 4.4.0)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# P Rttf2pt1       1.3.12   2023-01-22 [?] CRAN (R 4.4.0)
# P rvest          1.0.4    2024-02-12 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P selectr        0.4-2    2019-11-20 [?] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats        * 4.4.0    2024-04-24 [?] local
# P stringi        1.8.3    2023-12-11 [?] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite        2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts    1.1.0    2024-05-15 [1] CRAN (R 4.4.0)
# textshaping    0.4.0    2024-05-24 [1] CRAN (R 4.4.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidytuesdayR   1.0.3    2023-12-13 [?] CRAN (R 4.4.0)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P usethis        2.2.3    2024-02-19 [?] CRAN (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr          3.0.0    2024-01-16 [?] CRAN (R 4.4.0)
# P xfun           0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/windows/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/d6ee0ff8
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────────
# > 