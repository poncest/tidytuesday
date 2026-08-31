
## Challenge: #TidyTuesday 2026 week 35
## Data:      World Castles, Fortresses and Palaces
## Author:    Steven Ponce
## Date:      2026-08-31

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
# tt <- tidytuesdayR::tt_load(2026, week = 35)
# world_castles <- tt$world_castles
# rm(tt)
world_castles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-01/world_castles.csv')


## 3. EXAMINING THE DATA ----
glimpse(world_castles)
skim_without_charts(world_castles)


## 4. TIDY DATA ----

### |- century-level ruin share ----
ruin_by_century <- world_castles |>
  filter(!is.na(year)) |>
  mutate(
    century_bin = floor(year / 100) * 100,
    is_ruin = category == "ruin"
  ) |>
  summarise(
    n = n(),
    ruins = sum(is_ruin),
    ruin_share = mean(is_ruin),
    .by = century_bin
  ) |>
  arrange(century_bin)

### |- restrict to the defensible analytical window ----
plot_data <- ruin_by_century |>
  filter(century_bin >= 900, century_bin <= 1900) |>
  mutate(
    century_label = str_glue("{century_bin}s"),
    tier = if_else(century_bin %in% c(1100, 1200, 1300), "plateau", "main"),
    tier = factor(tier, levels = c("main", "plateau")),
    label_text = if_else(
      century_bin %in% c(1100, 1200, 1300, 1500, 1900),
      scales::percent(ruin_share, accuracy = 1),
      NA_character_
    )
  )

### |- known-year denominator, for caption ----
n_known_year <- sum(!is.na(world_castles$year))
n_total <- nrow(world_castles)
pct_known <- scales::percent(n_known_year / n_total, accuracy = 1)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
clrs <- get_theme_colors(
  palette = c(
    plateau = "#722F37",
    main    = "#7A7068"
  )
)

col_plateau <- "#722F37"
col_main <- "#7A7068"
col_ink <- "#2C2825"
col_stone <- "#7A7068"

fill_palette <- c(plateau = col_plateau, main = col_main)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- titles and caption ----
title_text <- str_glue("Ruin Share Peaks Across Three Medieval Centuries, Then Collapses")

subtitle_text <- str_glue(
  "About one-third of landmarks from the 1100s-1300s are ruins; ",
  "the share falls to 13% by the 1500s"
)

methodology_text <- str_glue(
  "Percent classified as ruins among landmarks with a known founding year. ",
  "Century bins use founding year; {pct_known} of {comma(n_total)} landmarks have a known year. ",
  "Centuries before 900 are omitted because sample sizes are very small ",
  "(700s n=20, 800s n=23) and estimates are highly volatile."
)

caption_text <- str_glue(
  "{methodology_text}<br>",
  create_social_caption(
    tt_year = 2026,
    tt_week = 35,
    source_text = "Castlemap / Wikidata"
  )
)

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_textbox_simple(
      size = 20,
      face = "bold",
      family = fonts$title_1,
      color = col_ink,
      lineheight = 1.1,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 12.5,
      family = fonts$title_1,
      color = col_stone,
      margin = margin(b = 16)
    ),
    plot.caption = element_textbox_simple(
      size = 6.0,
      family = fonts$caption,
      color = col_stone,
      margin = margin(t = 12)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, family = fonts$text),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  )
)

theme_set(weekly_theme)

### |-  plot ----
p <- ggplot(plot_data, aes(x = century_label, y = ruin_share, fill = tier)) +
    geom_col(width = 0.62) +
    geom_text(
        aes(label = label_text),
        vjust = -0.6,
        size = 3.4,
        family = fonts$caption,
        color = col_ink,
        na.rm = TRUE
    ) +
    scale_x_discrete(
        limits = plot_data$century_label
    ) +
    scale_y_continuous(
        limits = c(0, 0.42),
        expand = expansion(mult = c(0, 0.05)),
        labels = scales::percent_format(accuracy = 1)
    ) +
    scale_fill_manual(values = fill_palette) +
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text
    )

# Preview
p +
    canvas(width = 9, height = 6.5, units = "in", dpi = 300)


## 6. SAVE ----
save_ggplot(
    plot = p,
    file = "2026/Week_35/2026_35.png",
    width = 9, height = 6.5
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

# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-08-31
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ───────────────────────────────────────────────────────
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
# glue         * 1.8.1   2026-04-17 [1] CRAN (R 4.6.0)
# P graphics     * 4.6.1   2026-06-25 [1] local
# P grDevices    * 4.6.1   2026-06-25 [1] local
# P grid           4.6.1   2026-06-25 [1] local
# gridtext       0.1.6   2026-02-19 [1] CRAN (R 4.6.0)
# gtable         0.3.6   2024-10-25 [1] CRAN (R 4.6.0)
# here         * 1.0.2   2025-09-15 [1] CRAN (R 4.6.0)
# hms            1.1.4   2025-10-17 [1] CRAN (R 4.6.0)
# htmltools      0.5.9   2025-12-04 [1] CRAN (R 4.6.0)
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
# ──────────────────────────────────────────────────────────────────
# > 

