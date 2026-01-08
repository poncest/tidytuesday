## Challenge: #TidyTuesday 2026 week 01
## Data:      BYOD - Google "Diet" Trends
## Author:    Steven Ponce
## Date:      2026-01-08

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,     # Easily Install and Load the 'Tidyverse'
    ggtext,        # Improved Text Rendering Support for 'ggplot2'
    showtext,      # Using Fonts More Easily in R Graphs
    ggrepel,       # Non-overlapping Text Labels
    janitor,       # Simple Tools for Examining and Cleaning Dirty Data
    scales,        # Scale Functions for Visualization
    glue           # Interpreted String Literals
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 8,
    height = 6,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
# pak::pkg_install('gtrendsR')
# library(gtrendsR)
# library(tidyverse)

# Pull first batch
# terms_diet <- c("diet", "keto", "intermittent fasting", "calorie counting", "Ozempic")
# trends_diet <- gtrends(
#     keyword = terms_diet,
#     geo = "US",
#     time = "2016-01-01 2025-01-07"
# )

# Pull second batch
# terms_expanded <- c("Wegovy", "Mounjaro", "Paleo")
# trends_expanded <- gtrends(
#     keyword = terms_expanded,
#     geo = "US",
#     time = "2016-01-01 2025-01-07"
# )

# Combine and clean
# diet_combined <- bind_rows(
#     trends_diet$interest_over_time,
#     trends_expanded$interest_over_time
# ) |>
#     mutate(
#         hits = case_when(
#             hits == "<1" ~ "0.5",
#             TRUE ~ hits
#         ),
#         hits = as.numeric(hits)
#     )

# Save combined data
# write_csv(diet_combined, "2026/Week_01/diet_trends_combined.csv")

raw_data <- read_csv("2026/Week_01/diet_trends_combined.csv") |> 
    clean_names()


## 3. EXAMINING THE DATA ----
glimpse(raw_data)


## 4. TIDY DATA ----
### |-  clean individual terms ----
diet_clean <- raw_data |>
    mutate(
        hits = case_when(
            hits == "<1" ~ 0.5,
            TRUE ~ as.numeric(hits)
        ),
        date = as.Date(date),
        year = year(date),
        month = month(date)
    ) |>
    select(date, year, month, keyword, hits)

### |-  create grouped categories ----
diet_grouped <- diet_clean |>
    mutate(
        category = case_when(
            keyword %in% c("keto", "Paleo") ~ "Fad Diets",
            keyword %in% c("Ozempic", "Wegovy", "Mounjaro") ~ "GLP-1 Drugs",
            keyword == "diet" ~ "diet",
            TRUE ~ "Other"
        )
    ) |>
    # Exclude minor terms
    filter(category != "Other") |>
    group_by(date, year, month, category) |>
    summarise(
        hits = sum(hits, na.rm = TRUE),
        .groups = "drop"
    )

### |-  find the crossover point ----
crossover_data <- diet_grouped |>
    filter(category %in% c("Fad Diets", "GLP-1 Drugs")) |>
    pivot_wider(names_from = category, values_from = hits) |>
    filter(`GLP-1 Drugs` > `Fad Diets`) |>
    slice_min(date, n = 1)

crossover_date <- crossover_data$date
crossover_date

# Get the intersection y-value for annotation
crossover_y <- crossover_data$`GLP-1 Drugs`

# Create label data for end of lines 
label_data <- diet_grouped |>
    filter(date == max(date)) |>
    mutate(
        label = case_when(
            category == "GLP-1 Drugs" ~ "GLP-1 Drugs\n(Ozempic, Wegovy, Mounjaro)",
            category == "Fad Diets" ~ "Fad Diets\n(keto, Paleo)",
            TRUE ~ category
        )
    )

# Define prescription era start
prescription_era_start <- as.Date("2023-10-01")


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
      "diet" = "gray50",
      "Fad Diets" = "#3498db",
      "GLP-1 Drugs" = "#e74c3c"
  )
)

### |- titles and caption ----
title_text <- str_glue("The Prescription Takeover")

subtitle_text <- str_glue(
    "U.S. Google searches show a dramatic shift: combined interest in <span style='color:#e74c3c;'>**GLP-1 drugs**</span><br>",
    "(Ozempic, Wegovy, Mounjaro) has overtaken <span style='color:#3498db;'>**fad diets**</span> (keto, Paleo) since late 2023."
)

caption_text <- create_social_caption(
    tt_year = 2026,
    tt_week = 01,
    source_text = "Google Trends (US), 2016-2025 via { gtrendsR }"
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
    plot.title = element_text(
      face = "bold", family = fonts$title, size = rel(1.4),
      color = colors$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_markdown(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.9), margin = margin(b = 20), hjust = 0
    ),

    # Grid
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.25),

    # Axes
    axis.title = element_text(size = rel(0.8), color = "gray30"),
    axis.text = element_text(color = "gray30"),
    axis.text.y = element_text(size = rel(0.85)),
    axis.ticks = element_blank(),

    # Facets
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(
      face = "bold",
      color = "gray20",
      size = rel(0.9),
      margin = margin(t = 6, b = 4)
    ),
    panel.spacing = unit(1.5, "lines"),

    # Legend elements
    legend.position = "plot",
    legend.title = element_text(
      family = fonts$subtitle,
      color = colors$text, size = rel(0.8), face = "bold"
    ),
    legend.text = element_text(
      family = fonts$tsubtitle,
      color = colors$text, size = rel(0.7)
    ),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(20, 20, 20, 20),
    
  )
)

# Set theme
theme_set(weekly_theme)

### |- Plot ----
# Create the plot
diet_grouped |>
    ggplot(aes(x = date, y = hits, color = category)) +
    
    # Geoms
    geom_line(linewidth = 0.4, alpha = 0.25) +
    geom_smooth(
        method = "loess",
        span = 0.2,
        se = FALSE,
        linewidth = 1.8
    ) +
    geom_text_repel(
        data = label_data |> 
            filter(category == "GLP-1 Drugs"),
        aes(label = label),
        hjust = 0,
        direction = "y",
        nudge_x = 50,
        nudge_y = 5,
        segment.color = NA,
        family = fonts$text,
        fontface = "bold",
        size = 3,
        lineheight = 0.85,
        box.padding = 0.5
    ) +
    geom_text_repel(
        data = label_data |> 
            filter(category != "GLP-1 Drugs"),
        aes(label = label),
        hjust = 0,
        direction = "y",
        nudge_x = 50,
        # nudge_y = 5,
        segment.color = NA,
        family = fonts$text,
        fontface = "bold",
        size = 3,
        lineheight = 0.85,
        box.padding = 0.5
    ) +
    
    # Annotate
    annotate(
        "rect",
        xmin = prescription_era_start,
        xmax = max(diet_grouped$date) + 60,
        ymin = -Inf,
        ymax = Inf,
        fill = "#e74c3c",
        alpha = 0.05
    ) +
    annotate(
        "text",
        x = prescription_era_start + 200,
        y = 130,
        label = "Prescription Era",
        family = fonts$text,
        size = 3,
        color = "gray30",
        fontface = "bold.italic"
    ) +
    annotate(
        "point",
        x = crossover_date,
        y = crossover_y,
        size = 3,
        color = "gray20"
    ) +
    annotate(
        "text",
        x = crossover_date - 90,
        y = crossover_y + 15,
        label = "GLP-1 surpasses\nfad diets",
        family = fonts$text,
        size = 2.8,
        color = "gray30",
        hjust = 1,
        lineheight = 0.9
    ) +
    annotate(
        "curve",
        x = crossover_date - 80,
        y = crossover_y + 10,
        xend = crossover_date - 10,
        yend = crossover_y + 2,
        curvature = 0.2,
        arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
        color = "gray40",
        linewidth = 0.4
    ) +
    
    # Scales
    scale_color_manual(
        values = c(
            "diet" = "gray50",
            "Fad Diets" = "#3498db",
            "GLP-1 Drugs" = "#e74c3c"
        )
    ) +
    scale_x_date(
        date_breaks = "2 years",
        date_labels = "%Y",
        expand = expansion(mult = c(0.02, 0.18))
    ) +
    scale_y_continuous(
        breaks = seq(0, 100, 25),
        limits = c(0, 130),
        expand = expansion(mult = c(0, 0.02))
    ) +
    coord_cartesian(clip = "off") +
    
    # Labs
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        y = "Combined Search Interest"
    ) +
    
    # Theme
    theme(
    plot.title = element_markdown(
        size = rel(2.3),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.15,
        margin = margin(t = 8, b = 5)
    ),
    plot.subtitle = element_markdown(
        size = rel(0.9),
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.88),
        lineheight = 1.5,
        margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
        size = rel(0.65),
        family = fonts$subtitle,
        color = colors$caption,
        hjust = 0,
        lineheight = 1.4,
        margin = margin(t = 20, b = 5)
    )
)
    

# 6. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts,
# and formatting across all my #TidyTuesday projects. The core analysis logic
# (data tidying and visualization) uses only standard tidyverse packages.
#
# -----------------------------------------------------------------------------
# FUNCTIONS USED IN THIS SCRIPT:
# -----------------------------------------------------------------------------
#
# 📂 R/utils/fonts.R
#    • setup_fonts()       - Initialize Google Fonts with showtext
#    • get_font_families() - Return standardized font family names
#
# 📂 R/utils/social_icons.R
#    • create_social_caption() - Generate formatted caption with social handles
#                                and #TidyTuesday attribution
#
# 📂 R/themes/base_theme.R
#    • create_base_theme()   - Create consistent base ggplot2 theme
#    • extend_weekly_theme() - Add weekly-specific theme customizations
#    • get_theme_colors()    - Get color palettes for highlight/text
#
# -----------------------------------------------------------------------------
# WHY CUSTOM FUNCTIONS?
# -----------------------------------------------------------------------------
# These utilities eliminate repetitive code and ensure visual consistency
# across 50+ weekly visualizations. Instead of copy-pasting 30+ lines of
# theme() code each week, I use create_base_theme() and extend as needed.
#
# -----------------------------------------------------------------------------
# VIEW SOURCE CODE:
# -----------------------------------------------------------------------------
# All helper functions are open source on GitHub:
# 🔗 https://github.com/poncest/tidytuesday/tree/main/R
#
# Main files:
#   • R/utils/fonts.R         - Font setup and management
#   • R/utils/social_icons.R  - Caption generation with icons
#   • R/themes/base_theme.R   - Reusable ggplot2 themes
#
# -----------------------------------------------------------------------------
# REPRODUCIBILITY:
# -----------------------------------------------------------------------------
# To run this script:
#
# Option 1 - Use the helper functions (recommended):
#   1. Clone the repo: https://github.com/poncest/tidytuesday/
#   2. Make sure the R/ directory structure is maintained
#   3. Run the script as-is
#
# Option 2 - Replace with standard code:
#   1. Replace setup_fonts() with your own font setup
#   2. Replace get_theme_colors() with manual color definitions
#   3. Replace create_base_theme() with theme_minimal() + theme()
#   4. Replace create_social_caption() with manual caption text
#
## ============================================================================ ##


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/La_Paz
# date     2026-01-08
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# P colorspace    2.1-1    2024-07-26 [?] CRAN (R 4.4.1)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi         1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggrepel     * 0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext      * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski        1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# glue        * 1.8.0    2024-09-30 [1] CRAN (R 4.4.2)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# P gridtext      0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable        0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here        * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P lattice       0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown      1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P Matrix        1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# P mgcv          1.9-1    2023-12-21 [?] CRAN (R 4.4.0)
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P nlme          3.1-164  2023-11-27 [?] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.3    2024-09-11 [?] CRAN (R 4.4.2)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P renv          1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# rlang         1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P splines       4.4.0    2024-04-24 [?] local
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr     * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite       2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts   1.1.0    2024-05-15 [1] CRAN (R 4.4.0)
# textshaping   0.4.0    2024-05-24 [1] CRAN (R 4.4.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# tidyverse   * 2.0.0    2023-02-22 [1] CRAN (R 4.4.3)
# P timechange    0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# P tzdb          0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr         3.0.2    2024-10-28 [?] CRAN (R 4.4.2)
# P xfun          0.52     2025-04-02 [?] CRAN (R 4.4.3)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────
# > 