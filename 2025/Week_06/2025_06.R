
## Challenge: #TidyTuesday 2025 week 06
## Data:      CDC Datasets
## Author:    Steven Ponce
## Date:      2025-01-08            


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
    patchwork,      # The Composer of Plots
    camcorder       # Record Your Plot History 
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  10,
    height =  10,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 06) 

cdc <- tt$cdc_datasets |> clean_names()
# fpi <- tt$fpi_codes |> clean_names()
# omb <- tt$omb_codes |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(cdc)
skim(cdc)
# glimpse(fpi)
# glimpse(omb)



## 4. TIDYDATA ----

### |-  tidy data ----

# P1. Distribution plot  
distribution <- cdc |>
  mutate(
    access_category = case_when(
      public_access_level %in% c("public", "public domain") ~ "Public Access",
      public_access_level == "restricted public" ~ "Restricted Access",
      public_access_level == "non-public" ~ "No Public Access",
      TRUE ~ "Unspecified"
    )
  ) |>
  count(access_category) |>
  mutate(
    pct = n / sum(n) * 100,
    label = sprintf("%.1f%%\n(n=%d)", pct, n),
    # Conditional hjust and color based on count
    hjust = ifelse(n > 200, 1.2, -0.2),
    label_color = ifelse(n > 200, "white", "gray30")
  )

# P2. Timeline plot
timeline_data <- cdc |>
  mutate(
    access_category = case_when(
      public_access_level %in% c("public", "public domain") ~ "Public Access",
      public_access_level == "restricted public" ~ "Restricted Access",
      public_access_level == "non-public" ~ "No Public Access",
      TRUE ~ "Unspecified"
    ),
    issued = as.Date(issued)
  ) |>
  filter(!is.na(issued)) |>
  arrange(issued) |>
  group_by(access_category) |>
  mutate(cumulative = row_number()) |>
  ungroup()

# Find the inflection point around 2020
inflection_date <- as.Date("2020-04-01")



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
    palette = c(
        "Public Access"     = "#3498DB",      
        "Restricted Access" = "#E67E22",   
        "No Public Access"  = "#C0392B",    
        "Unspecified"       = "#7F8C8D" 
    )
)

### |-  titles and caption ----
title_text <- str_glue("CDC Dataset Purge: A Timeline of Public Health Data Removal")
subtitle_text <- str_glue("Analysis of CDC datasets backed up on archive.org during the Trump administration's data purge")

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 06,
    source_text = "archive.org/details/20250128-cdc-datasets"
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
        # Axis elements
        axis.title = element_text(color = colors$text, size = rel(0.8)),
        axis.text = element_text(color = colors$text, size = rel(0.7)),
        
        # Grid elements
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey60", linewidth = 0.1),
        
        # Legend elements
        legend.position = "right",
        legend.title = element_text(family = fonts$text, size = rel(0.8)),
        legend.text = element_text(family = fonts$text, size = rel(0.7)),
        
        # Plot margins 
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),

    )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----

# P1. Distribution plot 
p1 <- ggplot(distribution,
             aes(x = reorder(access_category, n), 
                 y = n, 
                 fill = access_category)
) +
    
    # Geoms
    geom_col() +
    geom_text(
        aes(label = label,
            hjust = hjust,
            color = label_color),
        size = 3.5,
        fontface = "bold",
        family = fonts$text
    ) +
    coord_flip() +
    
    # Scales
    scale_fill_manual(values = colors$palette) +
    scale_color_identity() +  
    scale_x_discrete() +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.25)) 
    ) +
    
    # Labs
    labs(
        title = "Distribution of Dataset Access Levels",
        x = NULL,
        y = "Number of Datasets"
    ) +
    
    # Theme
    theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        
        plot.title = element_text(
            size   = rel(1.3),
            family = fonts$title,
            face   = "bold",
            color  = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        )
    )

# P2. Timeline plot with enhancements
p2 <- ggplot(timeline_data,
             aes(x = issued, 
                 y = cumulative, 
                 fill = access_category)
) +
    # Rectangle
    annotate(
        "rect",
        xmin = inflection_date,
        xmax = max(timeline_data$issued),
        ymin = 0,
        ymax = max(timeline_data$cumulative),
        fill = "gray85",
        alpha = 0.3
    ) +
    
    # Geoms
    geom_area(alpha = 0.8) +
    geom_line(color = "white", linewidth = 0.5) +
    geom_vline(
        xintercept = inflection_date, 
        linetype = "dashed", 
        color = colors$caption,
        alpha = 0.7
    ) +
    # Note
    annotate(
        "text", 
        x = inflection_date,
        y = max(timeline_data$cumulative) * 0.9,
        label = "Notable increase in\narchival activity",
        hjust = -0.1,
        size = 3.5,
        color = colors$caption,
        family = fonts$caption
    ) +
    
    # Scales
    scale_fill_manual(values = colors$palette) +
    scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
    ) +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.1)),
        breaks = function(x) seq(0, max(x), by = 50)
    ) +
    
    # Labs
    labs(
        title = "Cumulative Growth of Archived Datasets",
        subtitle = sprintf("Note: Only %d out of %d datasets (%.1f%%) have valid dates",
                           sum(!is.na(cdc$issued)),
                           nrow(cdc),
                           100 * sum(!is.na(cdc$issued)) / nrow(cdc)),
        x = "Year",
        y = "Number of Datasets"
    ) +
    
    # Theme
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        
        plot.title = element_text(
            size = rel(1.3),
            family = fonts$title,
            face = "bold",
            color = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.subtitle = element_text(
            size = rel(0.9),
            family = fonts$caption,
            color = colors$caption,
            margin = margin(b = 10)
        ),
        axis.title = element_text(
            size = rel(0.9),
            family = fonts$caption,
            color = colors$caption
        ),
    )

# Combine plots 
combined_plot <- (p1 / p2) +
    plot_layout(heights = c(0.8, 1.2)) 

combined_plot <- combined_plot +
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text, 
        theme = theme(
            plot.title = element_text(
                size   = rel(1.75),
                family = fonts$title,
                face   = "bold",
                color  = colors$title,
                lineheight = 1.1,
                margin = margin(t = 5, b = 5)
            ),   
            plot.subtitle = element_text(
                size   = rel(0.9),
                family = fonts$subtitle,
                color  = colors$subtitle,
                lineheight = 1.2,
                margin = margin(t = 5, b = 5)
            ), 
            plot.caption = element_markdown(
                size   = rel(0.6),
                family = fonts$caption,
                color  = colors$caption,
                hjust  = 0.5,
                margin = margin(t = 10)
            ),
            plot.margin = margin(t = 20, r = 10, b = 20, l = 10),
        )
    )

combined_plot  



# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-02-08
# rstudio  2024.12.0+467 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────
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
# P patchwork    * 1.3.0    2024-09-16 [?] CRAN (R 4.4.1)
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
# ────────────────────────────────────────────────────────────────────────────────────────
# > 