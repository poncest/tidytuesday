
## Challenge: #TidyTuesday 2025 week 16
## Data:      Fatal Car Crashes on 4/20
## Author:    Steven Ponce
## Date:      2025-04-20  


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,      # Easily Install and Load the 'Tidyverse'
    ggtext,         # Improved Text Rendering Support for 'ggplot2'
    showtext,       # Using Fonts More Easily in R Graphs
    janitor,        # Simple Tools for Examining and Cleaning Dirty Data
    skimr,          # Compact and Flexible Summaries of Data
    scales,         # Scale Functions for Visualization
    glue,           # Interpreted String Literals
    here,           # A Simpler Way to Find Your Files
    camcorder       # Record Your Plot History 
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  8,
    height =  8,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 16) 

daily_accidents <- tt$daily_accidents |> clean_names()
# daily_accidents_420 <- tt$daily_accidents_420 |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(daily_accidents)
skim(daily_accidents)


## 4. TIDYDATA ----

### |-  tidy data ----      
# Add date components to daily accidents data  
daily_accidents_tidy <- daily_accidents |>
    mutate(
        year = year(date),
        month = month(date),
        day = day(date),
        is_april_20 = (month == 4 & day == 20)
    )

# Calculate yearly averages
yearly_averages <- daily_accidents_tidy |>
    group_by(year) |>
    summarize(
        yearly_avg = mean(fatalities_count),
        .groups = "drop"
    )

# Extract April 20th data for each year
april_20_yearly <- daily_accidents_tidy |>
    filter(is_april_20) |>
    select(year, fatalities_count) |>
    arrange(year) |>
    # Join with yearly averages
    left_join(yearly_averages, by = "year") |>
    # Calculate difference from yearly average
    mutate(
        diff_from_avg = fatalities_count - yearly_avg,
        above_avg = diff_from_avg > 0
    )

# Find key statistics for annotations
max_year_data <- april_20_yearly |> 
    filter(diff_from_avg == max(diff_from_avg))
max_year <- max_year_data$year
max_diff <- max_year_data$diff_from_avg

min_year_data <- april_20_yearly |> 
    filter(diff_from_avg == min(diff_from_avg))
min_year <- min_year_data$year
min_diff <- min_year_data$diff_from_avg

# Count years above and below average
n_above <- sum(april_20_yearly$above_avg)
n_below <- nrow(april_20_yearly) - n_above


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = c(
        "FALSE" = "#3A6CA4", "TRUE" = "#F05E23"
    )
)

### |-  titles and caption ----
title_text <- str_glue("April 20th Fatal Crashes Compared to Yearly Average (1992-2016)")
subtitle_text <- str_glue("Difference between April 20th fatalities and the yearly average for each year")

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 16,
    source_text =  "420 raw-data via OSF" 
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

# Start with base theme
base_theme <- create_base_theme(colors)

# Add weekly-specific theme elements
weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        # Text styling 
        plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.14), margin = margin(b = 10)),
        plot.subtitle = element_text(family = fonts$subtitle, color = colors$text, size = rel(0.78), margin = margin(b = 20)),
        
        # Axis elements
        axis.title = element_text(color = colors$text, face = "bold", size = rel(0.8)),
        axis.text = element_text(color = colors$text, size = rel(0.7)),
        
        # Grid elements
        panel.grid.minor = element_line(color = "gray50", linewidth = 0.05),
        panel.grid.major = element_line(color = "gray50", linewidth = 0.02),
        
        # Legend elements
        legend.position = "plot",
        legend.direction = "horizontal",
        legend.title = element_text(family = fonts$text, size = rel(0.8), face = "bold"),
        legend.text = element_text(family = fonts$text, size = rel(0.7)),
        
        # two-row legend
        legend.box.spacing = unit(0.4, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.spacing.x = unit(0.2, "cm"),
 
        legend.box = "horizontal",
        legend.box.just = "left",
        
        # Plot margins 
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    )
)

# Set theme
theme_set(weekly_theme)

### |-  plot  ----
ggplot(april_20_yearly, aes(x = year, y = diff_from_avg, fill = above_avg)) +
    # Geoms
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    # Annotate
    annotate(
        "text", x = max_year, y = max_diff + 7, 
        label = paste0("Peak: ", max_year, " (+", round(max_diff), ")"),
        color = colors$palette[2], fontface = "bold", size = 3.5
        ) +
    annotate(
        "text", x = min_year, y = min_diff - 7, 
        label = paste0("Low: ", min_year, " (", round(min_diff), ")"),
        color = colors$palette[1], fontface = "bold", size = 3.5
        ) +
    annotate(
        "text", x = max(april_20_yearly$year) - 3, y = 43, 
        label = paste0(n_above, " years above avg"),
        color = colors$palette[2], size = 3.5, hjust = 1
        ) +
    annotate(
        "text", x = max(april_20_yearly$year) - 3, y = -43, 
        label = paste0(n_below, " years below avg"),
        color = colors$palette[1], size = 3.5, hjust = 1
        ) +
    # Scales
    scale_fill_manual(
        values = colors$palette,
        labels = c("Below Average", "Above Average")
    ) +
    scale_x_continuous(
        breaks = seq(
            min(april_20_yearly$year),
            max(april_20_yearly$year), 
            by = 5)
        ) +
    # Labs
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = NULL,
        y = "Difference from Yearly Average",
    ) +
    # Theme
    theme(
        plot.title = element_text(
            size = rel(1.4),
            family = fonts$title,
            face = "bold",
            color = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.subtitle = element_text(
            size = rel(0.85),
            family = fonts$subtitle,
            color = alpha(colors$subtitle, 0.9),
            lineheight = 1.2,
            margin = margin(t = 5, b = 10)
        ),
        plot.caption = element_markdown(
            size = rel(0.65),
            family = fonts$caption,
            color = colors$caption,
            hjust = 0.5,
            margin = margin(t = 10)
        )
    ) 


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-20
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder    * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# P digest         0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gh             1.4.1    2024-03-28 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P gitcreds       0.1.2    2022-09-08 [?] CRAN (R 4.4.0)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.4.2)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here         * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P httr2          1.0.1    2024-04-01 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.46     2024-04-06 [?] CRAN (R 4.4.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.3    2024-09-11 [?] CRAN (R 4.4.2)
# P rappdirs       0.3.3    2021-01-31 [?] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats        * 4.4.0    2024-04-24 [?] local
# stringi        1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite        2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts    1.1.0    2024-05-15 [1] CRAN (R 4.4.0)
# textshaping    0.4.0    2024-05-24 [1] CRAN (R 4.4.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidytuesdayR   1.1.2    2024-09-09 [?] CRAN (R 4.4.2)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr          3.0.2    2024-10-28 [?] CRAN (R 4.4.2)
# P xfun           0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────
# > 