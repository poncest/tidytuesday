
## Challenge: #TidyTuesday 2026 week 26
## Data:      Wreck Inventory of Ireland
## Author:    Steven Ponce
## Date:      2026-06-29

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 10,
    height = 6.5,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 26)
wreck_inventory <- tt$wreck_inventory |> clean_names()

rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(wreck_inventory)


## 4. TIDY DATA ----

### |- vessel type lookups ----
# wreck_inventory |>
#     count(classification, sort = TRUE) |>
#     print(n = 30)

sail_types <- c(
  "Schooner", "Sloop", "Barque", "Brig", "Brigantine",
  "Ketch", "Lugger", "Smack", "Cutter", "Yawl",
  "Sailing Ship", "Sailing Boat", "Full-rigged ship",
  "Ship", "Galley"
)

steam_types <- c(
  "Steamship", "Steam Trawler", "Steel Steamship",
  "Iron Steamship", "Collier", "Trawler", "Submarine"
)

### |- crossover data ----
crossover <- wreck_inventory |>
  filter(
    !is.na(year),
    year >= 1800, year <= 1945,
    classification %in% c(sail_types, steam_types)
  ) |>
  mutate(
    decade = floor(year / 10) * 10,
    era    = if_else(classification %in% sail_types, "Sail", "Steam & Motor")
  ) |>
  count(decade, era) |>
  group_by(decade) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  select(decade, era, pct) |>
  pivot_wider(names_from = era, values_from = pct, values_fill = 0) |>
  arrange(decade) |>
  pivot_longer(cols = c(Sail, `Steam & Motor`), names_to = "era", values_to = "pct")

### |- pre-extract annotation coordinates ----
crossover_x <- 1905
crossover_y <- 0.50

sail_1800_pct <- 0.973
steam_1940_pct <- 0.963

# n for scope note
n_classified <- wreck_inventory |>
  filter(
    !is.na(year), year >= 1800, year <= 1945,
    classification %in% c(sail_types, steam_types)
  ) |>
  nrow()

# 1920s sparsity note coordinates
sparse_x <- 1920
sparse_y <- 0.53


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
clrs <- get_theme_colors(
  palette = c(
    "Sail"          = "#5B7FA6",
    "Steam & Motor" = "#722F37",
    "annotation"    = "#4A5568",
    "sparse"        = "#9CA3AF"
  )
)

### |- titles and caption ----
title_text <- "Ireland's Shipwrecks Chronicle the End of the Age of Sail"

subtitle_text <- str_glue(
  "For more than a century, sail-powered vessels dominated classified shipwrecks — ",
  "over **<span style='color:#5B7FA6'>9 in 10</span>** ",
  "identified wrecks were sail-powered.<br>",
  "By the 1910s, **<span style='color:#722F37'>steam and motor vessels</span>** ",
  "became the majority, reaching 96% by the 1940s."
)

caption_text <- create_social_caption(
  tt_year     = 2026,
  tt_week     = 26,
  source_text = "Wreck Inventory of Ireland (National Monuments Service)"
)


### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Panel
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # Axes
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      family = fonts$text, size = 10, color = "#4A5568",
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      family = fonts$text, size = 10, color = "#4A5568",
      margin = margin(r = 8)
    ),
    axis.text = element_text(
      family = fonts$text, size = 9, color = "#4A5568"
    ),

    # Title / subtitle
    plot.title = element_text(
      family = fonts$title_1, face = "bold", size = rel(1.85),
      color = "#1A1A2E", margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$subtitle, size = rel(0.80), lineheight = 1.4,
      color = "#4A5568", margin = margin(b = 20)
    ),
    plot.caption = element_markdown(
      family = fonts$caption, size = rel(0.5), color = "#9CA3AF",
      hjust = 0, margin = margin(t = 16), lineheight = 1.2
    ),

    # Margins
    plot.margin = margin(t = 20, r = 24, b = 12, l = 16)
  )
)

theme_set(weekly_theme)

### |- plot ----
p <- crossover |>
  ggplot(aes(x = decade, y = pct, color = era)) +

  # Geoms
  geom_line(linewidth = 1.1, lineend = "round") +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +

  # Annotate
  annotate(
    "text",
    x = 1942, y = steam_1940_pct,
    label = "Steam &\nMotor  96%",
    family = fonts$text, size = 3.3,
    color = "#722F37", fontface = "bold",
    hjust = 0, lineheight = 1.2
  ) +
  annotate(
    "text",
    x = 1942, y = 0.04,
    label = "Sail  4%",
    family = fonts$text, size = 3.3,
    color = "#5B7FA6", fontface = "bold",
    hjust = 0
  ) +
  # Scales
  scale_color_manual(
    values = c(
      "Sail"          = "#5B7FA6",
      "Steam & Motor" = "#722F37"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.02),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = seq(1800, 1940, by = 20),
    expand = expansion(mult = c(0.02, 0.14))
  ) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = glue(
      "{caption_text}<br>",
      "<span style='color:#9CA3AF'>Among records with known vessel type ",
      "(n ≈ 6,500 of 17,981 total); Unknown classification excluded.</span>"
    ),
    x = NULL,
    y = "Share of classified wrecks",
    color = NULL
  ) +

  # Theme
  theme(legend.position = "none")

### |- preview ----
snap(p)


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

# ─ Session info ─────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-06-29
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────
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
# ggplot2      * 4.0.3    2026-04-22 [1] CRAN (R 4.5.3)
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
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.5.2)
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
# ────────────────────────────────────────────────────────────────