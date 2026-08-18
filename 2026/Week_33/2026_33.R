
## Challenge: #TidyTuesday 2026 week 33
## Data:      IELTS exam results
## Author:    Steven Ponce
## Date:      2026-08-18

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, ggview
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 33)
perf_lang <- tt$performance_by_first_language
# demo_by_first_language <- tt$demo_by_first_language
# demo_by_nationality <- tt$demo_by_nationality
# demo_by_reasons <- tt$demo_by_reasons
# performance_by_nationality <- tt$performance_by_nationality
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(perf_lang)
skim_without_charts(perf_lang)
distinct(perf_lang, year)
distinct(perf_lang, part)


## 4. TIDY DATA ----
### |- harmonize cross-cohort spelling/label drift ----
language_harmonize <- c(
  "Gujurati"   = "Gujarati",
  "Ibo/lgbo"   = "Ibo/Igbo",
  "Singhalese" = "Sinhalese"
)

perf_lang_clean <- perf_lang |>
  mutate(language = recode(language, !!!language_harmonize))

### |- restrict to languages present in BOTH comparison cohorts ----
## robustness check already run in Phase 0.
languages_both_cohorts <- perf_lang_clean |>
  filter(year %in% c("2022-2023", "2024-2025")) |>
  distinct(type, language, year) |>
  count(type, language) |>
  filter(n == 2) |>
  distinct(type, language)

matched_scores <- perf_lang_clean |>
  filter(year %in% c("2022-2023", "2024-2025")) |>
  mutate(part = str_to_title(part)) |>
  semi_join(languages_both_cohorts, by = c("type", "language"))

### |- aggregate spine: mean of per-language deltas, matched languages only ----
per_language_delta <- matched_scores |>
  pivot_wider(
    id_cols = c(type, language, part),
    names_from = year, values_from = score
  ) |>
  mutate(delta = `2024-2025` - `2022-2023`)

### |- y_pos ----
component_positions <- tibble::tibble(
  part  = c("Overall", "Speaking", "Writing", "Reading", "Listening"),
  label = c("Overall score", "Speaking", "Writing", "Reading", "Listening"),
  y_pos = c(1, 2.5, 3.5, 4.5, 5.5)
)

## Row labels
label_data <- component_positions |>
  mutate(
    label = if_else(
      part %in% c("Overall", "Reading", "Listening"),
      str_glue("**{label}**"),
      label
    ),
    type_label = "Academic"
  )

delta_data <- per_language_delta |>
  summarise(delta = mean(delta, na.rm = TRUE), .by = c(type, part)) |>
  mutate(
    role = case_when(
      part == "Reading" ~ "reading",
      part == "Listening" ~ "listening",
      part == "Overall" ~ "overall",
      TRUE ~ "context"
    ),
    type_label = if_else(
      type == "General_Training", "General Training", "Academic"
    ),
    label_this = part %in% c("Reading", "Listening", "Overall")
  ) |>
  left_join(component_positions, by = "part") |>
  arrange(type, y_pos)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    reading   = "#2C6E6B",
    listening = "#A67C3D",
    context   = "#B8B0A6",
    overall   = "#3E4A54"
  )
)

### |- titles and caption ----
title_text <- str_glue("IELTS Averages Barely Moved. Reading and Listening Scores Didn't.")

subtitle_text <- str_glue("Change in mean IELTS band score by component, 2022-23 to 2024-25 cohorts.")

caption_text <- create_social_caption(
  tt_year = 2026,
  tt_week = 33,
  source_text = str_glue(
    "Unweighted mean across first-language groups reported in both the ",
    "2022-23 and 2024-25 cohorts. IELTS Test Statistics, via TidyTuesday"
  )
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_textbox_simple(
      face = "bold", family = fonts$title_1,
      size = 16, lineheight = 1.2,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$subtitle, size = 10,
      lineheight = 1.3,
      margin = margin(b = 10)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$caption, size = 5,
      color = "gray40", lineheight = 1.3,
      margin = margin(t = 12)
    ),
    plot.margin = margin(t = 20, r = 20, b = 15, l = 20),
    strip.text = element_text(
      face = "bold", family = fonts$title_1, size = 11, hjust = 0
    ),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.spacing.x = unit(1.4, "lines")
  )
)

theme_set(weekly_theme)

### |-  plot ----
p <- delta_data |>
    ggplot(aes(x = delta, y = y_pos, fill = role)) +
    geom_col(width = 0.68, orientation = "y", na.rm = TRUE) +
    geom_vline(xintercept = 0, color = "#2C3E50", linewidth = 0.4) +
    geom_richtext(
        data = label_data,
        aes(x = -0.55, y = y_pos, label = label),
        inherit.aes = FALSE,
        hjust = 1, size = 3.2, color = "#2C3E50",
        family = fonts$subtitle,
        fill = NA, label.color = NA,
        label.padding = unit(0, "pt")
    ) +
    geom_text(
        data = filter(delta_data, label_this),
        aes(
            label = label_number(style_positive = "plus", accuracy = 0.01)(delta),
            hjust = if_else(delta >= 0, -0.15, 1.15)
        ),
        size = 3, family = fonts$subtitle, color = "#2C3E50"
    ) +
    facet_wrap(~type_label, nrow = 1) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(limits = c(-0.9, 0.65)) +
    scale_y_continuous(breaks = NULL, limits = c(0.3, 6.2)) +
    scale_fill_manual(
        values = c(
            reading   = "#2C6E6B", 
            listening = "#A67C3D", 
            context   = "#B8B0A6", 
            overall   = "#3E4A54"  
        ),
        na.value = NA
    ) +
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = NULL, y = NULL
    )

# Preview
p +
    canvas(width = 9, height = 5.5, units = "in", dpi = 300)


## 6. SAVE ----
save_ggplot(
  plot = p,
  file = "2026/Week_33/2026_33.png",
  width = 9, height = 5.5
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

# ─ Session info ──────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.5.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-08-17
# rstudio  2026.08.0+187 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# base64enc      0.1-6   2026-02-02 [1] CRAN (R 4.6.0)
# bit            4.6.0   2025-03-06 [1] CRAN (R 4.6.0)
# bit64          4.8.2   2026-05-19 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# crayon         1.5.3   2024-06-20 [1] CRAN (R 4.6.0)
# curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1   2026-06-25 [1] local
# digest         0.6.39  2025-11-19 [1] CRAN (R 4.6.0)
# dplyr        * 1.2.1   2026-04-03 [1] CRAN (R 4.6.0)
# evaluate       1.0.5   2025-08-27 [1] CRAN (R 4.6.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
# fastmap        1.2.0   2024-05-15 [1] CRAN (R 4.6.0)
# forcats      * 1.0.1   2025-09-25 [1] CRAN (R 4.6.0)
# generics       0.1.4   2025-05-09 [1] CRAN (R 4.6.0)
# ggplot2      * 4.0.3   2026-04-22 [1] CRAN (R 4.6.0)
# ggrepel      * 0.9.8   2026-03-17 [1] CRAN (R 4.6.0)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.6.0)
# ggview       * 0.2.2   2025-07-05 [1] CRAN (R 4.6.0)
# gh             1.6.1   2026-07-20 [1] CRAN (R 4.6.1)
# gitcreds       0.1.2   2022-09-08 [1] CRAN (R 4.6.0)
# glue         * 1.8.1   2026-04-17 [1] CRAN (R 4.6.0)
# P graphics     * 4.6.1   2026-06-25 [1] local
# P grDevices    * 4.6.1   2026-06-25 [1] local
# P grid           4.6.1   2026-06-25 [1] local
# gridtext       0.1.6   2026-02-19 [1] CRAN (R 4.6.0)
# gtable         0.3.6   2024-10-25 [1] CRAN (R 4.6.0)
# here         * 1.0.2   2025-09-15 [1] CRAN (R 4.6.0)
# hms            1.1.4   2025-10-17 [1] CRAN (R 4.6.0)
# htmltools      0.5.9   2025-12-04 [1] CRAN (R 4.6.0)
# httr2          1.3.0   2026-07-13 [1] CRAN (R 4.6.1)
# janitor      * 2.2.1   2024-12-22 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
# knitr          1.51    2025-12-20 [1] CRAN (R 4.6.0)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# otel           0.2.0   2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
# repr           1.1.7   2024-03-22 [1] CRAN (R 4.6.0)
# rlang          1.3.0   2026-07-05 [1] CRAN (R 4.6.1)
# rprojroot      2.1.1   2025-08-26 [1] CRAN (R 4.6.0)
# rstudioapi     0.19.0  2026-06-11 [1] CRAN (R 4.6.0)
# S7             0.2.2   2026-04-22 [1] CRAN (R 4.6.0)
# scales       * 1.4.0   2025-04-24 [1] CRAN (R 4.6.0)
# sessioninfo    1.2.4   2026-06-04 [1] CRAN (R 4.6.0)
# showtext     * 0.9-8   2026-03-21 [1] CRAN (R 4.6.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.6.0)
# skimr        * 2.2.2   2026-01-10 [1] CRAN (R 4.6.0)
# snakecase      0.11.1  2023-08-27 [1] CRAN (R 4.6.0)
# P stats        * 4.6.1   2026-06-25 [1] local
# stringi        1.8.7   2025-03-27 [1] CRAN (R 4.6.0)
# stringr      * 1.6.0   2025-11-04 [1] CRAN (R 4.6.0)
# sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.6.0)
# systemfonts    1.3.2   2026-03-05 [1] CRAN (R 4.6.0)
# textshaping    1.0.5   2026-03-06 [1] CRAN (R 4.6.0)
# tibble       * 3.3.1   2026-01-11 [1] CRAN (R 4.6.0)
# tidyr        * 1.3.2   2025-12-19 [1] CRAN (R 4.6.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.6.0)
# tidytuesdayR   1.3.2   2026-04-12 [1] CRAN (R 4.6.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.6.0)
# timechange     0.4.0   2026-01-29 [1] CRAN (R 4.6.0)
# P tools          4.6.1   2026-06-25 [1] local
# tzdb           0.5.0   2025-03-15 [1] CRAN (R 4.6.0)
# utf8           1.2.6   2025-06-08 [1] CRAN (R 4.6.0)
# P utils        * 4.6.1   2026-06-25 [1] local
# vctrs          0.7.3   2026-04-11 [1] CRAN (R 4.6.0)
# vroom          1.7.1   2026-03-31 [1] CRAN (R 4.6.0)
# withr          3.0.3   2026-06-19 [1] CRAN (R 4.6.0)
# xfun           0.60    2026-07-09 [1] CRAN (R 4.6.1)
# xml2           1.6.0   2026-06-22 [1] CRAN (R 4.6.1)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────
# > 

