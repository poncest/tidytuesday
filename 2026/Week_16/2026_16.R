
## Challenge: #TidyTuesday 2026 week 16
## Data:      Global Health Spending
## Author:    Steven Ponce
## Date:      2026-04-18

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, patchwork
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 14,
    height = 8,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 16)

financing_schemes <- tt$financing_schemes |> clean_names()
health_spending   <- tt$health_spending   |> clean_names()
spending_purpose  <- tt$spending_purpose  |> clean_names()

rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(financing_schemes)
glimpse(health_spending)
glimpse(spending_purpose)

health_spending |> distinct(indicator_code, expenditure_type, unit)
health_spending |> summarise(max_year = max(year), min_year = min(year))


## 4. TIDY DATA ----

### |- Panel A: Scatter — government vs. OOP, most recent year per country ----

# Government spending
gov_data <- health_spending |>
    filter(
        indicator_code == "gghed_che",
        unit == "% of current health expenditure"
    ) |>
    group_by(iso3_code, country_name) |>
    slice_max(year, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(iso3_code, country_name, year_gov = year, gghed_che = value)

# OOP
oop_data <- financing_schemes |>
    filter(
        indicator_code == "hf3_che",
        unit == "% of current health expenditure"
    ) |>
    group_by(iso3_code) |>
    slice_max(year, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(iso3_code, year_oop = year, oops_che = value)

# Join on iso3_code — years may differ slightly by country
scatter_data <- gov_data |>
    inner_join(oop_data, by = "iso3_code") |>
    drop_na(gghed_che, oops_che) |>
    filter((gghed_che + oops_che) <= 115)

# Annotate only the most diagnostic cases
annotate_countries <- c(
    "AFG", # lowest gov / highest OOP anchor
    "BGD", # high OOP
    "NGA", # high OOP, large population
    "DEU", # well-protected exemplar
    "USA"  # anomaly: high gov AND moderate OOP
)

scatter_plot_data <- scatter_data |>
    mutate(
        highlight = case_when(
            oops_che >= 55 ~ "high_oop",
            oops_che <= 15 & gghed_che >= 60 ~ "protected",
            TRUE ~ "other"
        ),
        label = if_else(iso3_code %in% annotate_countries, country_name, NA_character_)
    )

### |- Panel B: Time series — global medians 2000–2023 ----

# Government
trend_gov <- health_spending |>
    filter(
        indicator_code == "gghed_che",
        unit == "% of current health expenditure",
        year >= 2000
    ) |>
    group_by(year) |>
    summarise(median_pct = median(value, na.rm = TRUE), .groups = "drop") |>
    mutate(series = "gov", series_label = "Government spending")

# OOP
trend_oop <- financing_schemes |>
    filter(
        indicator_code == "hf3_che",
        unit == "% of current health expenditure",
        year >= 2000
    ) |>
    group_by(year) |>
    summarise(median_pct = median(value, na.rm = TRUE), .groups = "drop") |>
    mutate(series = "oop", series_label = "Out-of-pocket payments")

trend_data <- bind_rows(trend_gov, trend_oop)


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
    palette = list(
        "high_oop"  = "#722F37",   
        "protected" = "#2E5A87",   
        "other"     = "gray75",   
        "gov"       = "#2E5A87",   
        "oop"       = "#722F37"   
    )
)

### |- titles and caption ----
title_text <- str_glue("Where Governments Spend Less, Households Spend More")

subtitle_text <- str_glue(
    "Out-of-pocket spending above **<span style='color:#722F37'>40%</span>** signals financial hardship — ",
    "a threshold exceeded in nearly half of countries, especially where public financing is limited."
)

caption_text <- create_social_caption(
    tt_year = 2026,
    tt_week = 16,
    source_text = "WHO Global Health Expenditure Database (GHED)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        plot.title = element_text(
            size = 18, face = "bold", family = fonts$title,
            margin = margin(b = 6)
        ),
        plot.subtitle = element_markdown(
            size = 10.5, family = fonts$text, lineheight = 1.4,
            margin = margin(b = 16)
        ),
        plot.caption = element_markdown(
            size = 7.5, family = fonts$text, color = "gray50",
            margin = margin(t = 12)
        ),
        axis.title = element_text(size = 9, family = fonts$text, color = "gray30"),
        axis.text = element_text(size = 8, family = fonts$text, color = "gray40"),
        panel.grid.major = element_line(color = "gray93", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank()
    )
)

theme_set(weekly_theme)

### |- Panel A: Scatter ----
p_scatter <- scatter_plot_data |>
  ggplot(aes(x = gghed_che, y = oops_che)) +

  # Geoms
  geom_abline(
    slope = -1, intercept = 100,
    linetype = "dashed", color = "gray70", linewidth = 0.4
  ) +
  geom_hline(
    yintercept = 40,
    linetype = "dotted", color = "#722F37", linewidth = 0.5, alpha = 0.7
  ) +
  geom_point(
    data = filter(scatter_plot_data, highlight == "other"),
    color = colors$palette$other, size = 1.6, alpha = 0.6
  ) +
  geom_point(
    data = filter(scatter_plot_data, highlight == "high_oop"),
    color = colors$palette$high_oop, size = 2.2, alpha = 0.85
  ) +
  geom_point(
    data = filter(scatter_plot_data, highlight == "protected"),
    color = colors$palette$protected, size = 2.2, alpha = 0.85
  ) +
  geom_text_repel(
    aes(label = label),
    size = 2.5,
    family = fonts$text,
    color = "gray25",
    segment.color = "gray70",
    segment.size = 0.3,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = 15,
    na.rm = TRUE,
    seed = 123
  ) +

  # Annotate
  annotate(
    "text",
    x = 1, y = 83, label = "High household burden",
    size = 2.5, color = "gray50", family = fonts$text, fontface = "italic",
    hjust = 0, vjust = 1
  ) +
  annotate(
    "text",
    x = 84, y = 3, label = "Protected systems",
    size = 2.5, color = "gray50", family = fonts$text, fontface = "italic",
    hjust = 1, vjust = 0
  ) +
  annotate(
    "text",
    x = 79, y = 42, label = "40% hardship threshold",
    size = 2.5, color = "#722F37", family = fonts$text, alpha = 0.8,
    hjust = 1, vjust = 0
  ) +

  # Scales
  scale_x_continuous(
    limits = c(0, 85),
    breaks = seq(0, 80, 20),
    labels = label_number(suffix = "%")
  ) +
  scale_y_continuous(
    limits = c(0, 85),
    breaks = seq(0, 80, 20),
    labels = label_number(suffix = "%")
  ) +

  # Labs
  labs(
    x = "Government health spending (% of current health expenditure)",
    y = "Out-of-pocket payments\n(% of current health expenditure)"
  )

### |- Panel B: Time series ----
# End-of-line labels 
trend_labels <- trend_data |>
    group_by(series_label) |>
    slice_max(year, n = 1, with_ties = FALSE)

p_trend <- trend_data |>
    ggplot(aes(x = year, y = median_pct, color = series)) +
    
    # Geoms
    geom_line(linewidth = 1.1) +
    geom_text(
        data = filter(trend_labels, series == "gov"),
        aes(label = series_label),
        hjust = 0, nudge_x = 0.3,
        size = 2.6, family = fonts$text,
        color = colors$palette$gov
    ) +
    geom_text(
        data = filter(trend_labels, series == "oop") |>
            mutate(series_label = "Out-of-pocket"),
        aes(label = series_label),
        hjust = 0, nudge_x = 0.3,
        size = 2.6, family = fonts$text,
        color = colors$palette$oop
    ) +
    
    # Annotate
    annotate(
        "rect",
        xmin = 2019.5, xmax = 2021.5,
        ymin = -Inf, ymax = Inf,
        fill = "gray90", alpha = 0.5
    ) +
    annotate(
        "text", x = 2020.5, y = 53.5,
        label = "COVID-19\ngovernment\nexpansion",
        size = 2.2, color = "gray50", family = fonts$text,
        lineheight = 1.2
    ) +
    annotate(
        "text", x = 2000, y = 57,
        label = "Global shift toward public financing",
        size = 2.4, color = "gray40", family = fonts$text,
        fontface = "italic", hjust = 0
    ) +
    
    # Scales
    scale_color_manual(
        values = c(
            "gov" = colors$palette$gov,
            "oop" = colors$palette$oop
        )
    ) +
    scale_x_continuous(
        breaks = seq(2000, 2022, 4),
        expand = expansion(mult = c(0.02, 0.35))  
    ) +
    scale_y_continuous(
        breaks = seq(20, 60, 10),
        labels = label_number(suffix = "%")
    ) +
    
    # Labs
    labs(
        x = "Year",
        y = "Global median (% of current health expenditure)"
    ) +
    # Theme
    theme(legend.position = "none")

### |- combined plots ----
combined_plot <- p_scatter + p_trend +
    plot_layout(widths = c(1.4, 1)) +
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_text(
                size = 24, face = "bold", family = fonts$title,
                margin = margin(b = 6)
            ),
            plot.subtitle = element_markdown(
                size = 10, family = fonts$text, lineheight = 1.4,
                margin = margin(b = 16)
            ),
            plot.caption = element_markdown(
                size = 7.5, family = fonts$text, color = "gray50",
                margin = margin(t = 12)
            ),
            plot.margin   = margin(t = 16, r = 20, b = 12, l = 16)
        )
    )

snap(combined_plot)


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

# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-04-17
# rstudio  2026.01.2+418 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.5.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.5.3)
# P datasets     * 4.5.3    2026-03-11 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.5.3)
# dplyr        * 1.2.1    2026-04-03 [1] CRAN (R 4.5.3)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.5.3)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.5.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.5.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.5.3)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.5.3)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.5.3)
# ggrepel      * 0.9.8    2026-03-17 [1] CRAN (R 4.5.3)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.5.3)
# gh             1.5.0    2025-05-26 [1] CRAN (R 4.5.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.5.3)
# gitcreds       0.1.2    2022-09-08 [1] CRAN (R 4.5.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.5.3)
# P graphics     * 4.5.3    2026-03-11 [2] local
# P grDevices    * 4.5.3    2026-03-11 [2] local
# P grid           4.5.3    2026-03-11 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.5.3)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.5.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.5.3)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.5.3)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.5.3)
# httr2          1.2.2    2025-12-08 [1] CRAN (R 4.5.3)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.5.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.5.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.5.3)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.5.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.5.3)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# P parallel       4.5.3    2026-03-11 [2] local
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.5.3)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# rappdirs       0.3.4    2026-01-17 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.5.3)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.5.3)
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.5.3)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.5.3)
# rsvg           2.7.0    2025-09-08 [1] CRAN (R 4.5.3)
# S7             0.2.1    2025-11-14 [1] CRAN (R 4.5.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.5.3)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.5.3)
# showtext     * 0.9-8    2026-03-21 [1] CRAN (R 4.5.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.5.3)
# skimr        * 2.2.2    2026-01-10 [1] CRAN (R 4.5.3)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.5.3)
# P stats        * 4.5.3    2026-03-11 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.5.2)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.5.3)
# svglite        2.2.2    2025-10-21 [1] CRAN (R 4.5.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.5.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.5.3)
# textshaping    1.0.5    2026-03-06 [1] CRAN (R 4.5.3)
# tibble       * 3.3.1    2026-01-11 [1] CRAN (R 4.5.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.5.3)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.5.3)
# tidytuesdayR   1.3.2    2026-04-12 [1] CRAN (R 4.5.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.5.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.5.3)
# P tools          4.5.3    2026-03-11 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.5.3)
# utf8           1.2.6    2025-06-08 [1] CRAN (R 4.5.3)
# P utils        * 4.5.3    2026-03-11 [2] local
# vctrs          0.7.3    2026-04-11 [1] CRAN (R 4.5.3)
# vroom          1.7.1    2026-03-31 [1] CRAN (R 4.5.3)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.5.3)
# xfun           0.57     2026-03-20 [1] CRAN (R 4.5.3)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.5.3)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.5
# [2] C:/Program Files/R/R-4.5.3/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────
