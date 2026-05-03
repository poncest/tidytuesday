
## Challenge: #TidyTuesday 2025 week 18
## Data:      ISTAT — Italian Industrial Production (1861–1985)
## Author:    Steven Ponce
## Date:      2025-05-03


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
    width  = 11,
    height = 10,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 18)

food_beverages <- tt$food_beverages |> clean_names()
textiles <- tt$textiles |> clean_names()
transport <- tt$transport |> clean_names()

rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(food_beverages)
glimpse(textiles)
glimpse(transport)


## 4. TIDY DATA ----

### |- Panel A: Indexed growth series ----
# Select 5 representative series across the three datasets.
# Index each to its own first non-NA year (= 100) so cross-industry
# comparisons reflect relative change, not raw volume or units.
panel_a_raw <- bind_rows(
  food_beverages |>
    select(year, beer, sugar, ethyl_alcohol_1) |>
    pivot_longer(-year, names_to = "series", values_to = "value"),
  textiles |>
    select(year, total_textiles, raw_silk) |>
    pivot_longer(-year, names_to = "series", values_to = "value")
)

panel_a_indexed <- panel_a_raw |>
  filter(!is.na(value)) |>
  filter(series %in% c("total_textiles", "raw_silk")) |>
  group_by(series) |>
  mutate(
    base_value = first(value),
    base_year  = first(year),
    indexed    = value / base_value * 100
  ) |>
  ungroup() |>
  mutate(
    series_label = case_when(
      series == "total_textiles" ~ "Total Textiles",
      series == "raw_silk" ~ "Raw Silk"
    ),
    color_role = case_when(
      series == "raw_silk" ~ "decline",
      series == "total_textiles" ~ "core"
    )
  )

### |- Panel B: Average ship weight (derived metric) ----
# avg_ship_weight = ships_weight / ships_launched
# A proxy for industrial capability: fewer but heavier ships = upgrading

panel_b <- transport |>
  filter(!is.na(ships_weight), !is.na(ships_launched), ships_launched > 0) |>
  mutate(avg_ship_weight = ships_weight / ships_launched) |>
  select(year, avg_ship_weight, ships_launched, ships_weight)

# Identify annotatable inflection points for Panel B narrative
panel_b_annotations <- panel_b |>
  summarise(
    early_median  = median(avg_ship_weight[year <= 1900], na.rm = TRUE),
    late_median = median(avg_ship_weight[year >= 1960], na.rm = TRUE),
    peak_year = year[which.max(avg_ship_weight)],
    peak_val = max(avg_ship_weight, na.rm = TRUE)
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
    palette = list(
        "rise"    = "#722F37", 
        "decline" = "#4A6FA5", 
        "core"    = "#C26A3D", 
        "neutral" = "gray75"   
    )
)

### |- titles and captions ----
title_text    <- str_glue("Italy industrialized unevenly")

subtitle_a    <- str_glue(
    "Indexed to first available year (= 100).",
    " Silk declined; textiles rose and stabilized.<br>",
    "<span style='color:{colors$palette$decline}'>Raw Silk</span>: Italy's oldest fiber industry, in long retreat;",
    " <span style='color:{colors$palette$core}'>Total Textiles</span>: postwar manufacturing anchor."
)

title_b <- str_glue("Ships became fewer but heavier")
subtitle_b <- str_glue(
    "Average gross tonnage per ship launched — a proxy for industrial capability."
)

caption_text  <- create_social_caption(
    tt_year = 2026,
    tt_week = 18,
    source_text = "ISTAT — Sommari di statistiche storiche (1861–1985)"
)

caption_b_note <- str_glue(
    "Note: Before 1951, values refer to fiscal years; from 1951 onward, calendar years."
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
      face = "bold", family = fonts$title,
      size = rel(1.3), hjust = 0, margin = margin(b = 4)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text,
      size = rel(0.85), hjust = 0,
      lineheight = 1.3, margin = margin(b = 10)
    ),
    plot.caption = element_markdown(
      family = fonts$text,
      size   = rel(0.7), hjust = 0,
      color  = "gray50", margin = margin(t = 10)
    ),
    axis.title = element_text(
      family = fonts$text, size = rel(0.8), color = "gray40"
    ),
    axis.text = element_text(
      family = fonts$text, size = rel(0.75), color = "gray50"
    ),
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_blank()
  )
)

theme_set(weekly_theme)

### |- Panel A: Indexed divergence ----

role_colors <- c(
  "decline" = colors$palette$decline,
  "core"    = colors$palette$core
)

role_linewidth <- c(
  "decline" = 1.4,
  "core"    = 1.4
)

role_alpha <- c(
  "decline" = 1,
  "core"    = 1
)

p_a <- panel_a_indexed |>
  ggplot(aes(
    x = year, y = indexed,
    group = series_label,
    color = color_role,
    linewidth = color_role,
    alpha = color_role
  )) +
  annotate("rect",
    xmin = 1915, xmax = 1918,
    ymin = -Inf, ymax = Inf,
    fill = "gray80", alpha = 0.10
  ) +
  annotate("rect",
    xmin = 1940, xmax = 1945,
    ymin = -Inf, ymax = Inf,
    fill = "gray80", alpha = 0.10
  ) +
  geom_vline(
    xintercept = 1951,
    linetype   = "dotted",
    linewidth  = 0.3,
    color      = "gray65"
  ) +
  annotate(
    "text",
    x = 1952, y = Inf,
    label = "Calendar-year reporting begins",
    hjust = 0, vjust = 1.5,
    size = 2.5,
    family = fonts$text,
    color = "gray45"
  ) +
  # Geoms
  geom_hline(
    yintercept = 100, linetype = "dashed",
    linewidth = 0.3, color = "gray60"
  ) +
  geom_line(lineend = "round") +
  geom_text(
    data = panel_a_indexed |>
      group_by(series_label, color_role) |>
      slice_max(year, n = 1) |>
      ungroup() |>
      mutate(
        y_nudge = case_when(
          series_label == "Raw Silk" ~ -12,
          series_label == "Total Textiles" ~ 8,
          TRUE ~ 0
        ),
        y_final = indexed + y_nudge
      ),
    aes(x = year, y = y_final, label = series_label, color = color_role),
    hjust = -0.12, size = 2.8,
    family = fonts$text,
    fontface = "bold",
    show.legend = FALSE
  ) +
  # Scales
  scale_color_manual(values = role_colors, guide = "none") +
  scale_linewidth_manual(values = role_linewidth, guide = "none") +
  scale_alpha_manual(values = role_alpha, guide = "none") +
  scale_x_continuous(
    breaks = seq(1870, 1980, by = 20),
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "×", scale = 0.01, accuracy = 0.1),
    breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3) * 100,
    limits = c(0, NA)
  ) +
  coord_cartesian(clip = "off") +
  # annotate
  annotate("text",
    x = 1916.5, y = Inf,
    label = "WWI", hjust = 0.5, vjust = 2,
    size = 2.3, color = "gray50", family = fonts$text
  ) +
  annotate("text",
    x = 1942.5, y = Inf,
    label = "WWII", hjust = 0.5, vjust = 2,
    size = 2.3, color = "gray50", family = fonts$text
  ) +
  annotate("text",
    x = 1944, y = 0.35,
    label = "WWII collapse",
    hjust = 0.5, vjust = 1,
    size = 2.4, color = "gray45",
    family = fonts$text
  ) +
  # Labs
  labs(
    subtitle = subtitle_a,
    x = NULL,
    y = "Production index"
  )

### |- Panel B: Ship sophistication ----

# Find key annotation years
peak_row <- panel_b |> slice_max(avg_ship_weight, n = 1)
early_row <- panel_b |>
  filter(year <= 1890) |>
  slice_min(year, n = 1)

p_b <- panel_b |>
  ggplot(aes(x = year, y = avg_ship_weight)) +
  # Geoms
  geom_line(
    color     = colors$palette$core,
    linewidth = 0.85,
    lineend   = "round"
  ) +
  # Annotate
  annotate("text",
    x = 1878, y = 1800,
    label = "Many small ships\n(capacity era)",
    hjust = 0.5, vjust = 0,
    size = 2.6, color = "gray45",
    family = fonts$text, lineheight = 1.2
  ) +
  annotate("point",
    x = peak_row$year, y = peak_row$avg_ship_weight,
    color = colors$palette$rise, size = 2.2
  ) +
  annotate("text",
    x = peak_row$year + 2, y = peak_row$avg_ship_weight + 2200,
    label = glue("{scales::comma(round(peak_row$avg_ship_weight))} tons/ship"),
    hjust = 0, vjust = 0,
    size = 2.4, color = "gray35",
    family = fonts$text, lineheight = 1.2
  ) +
  annotate("text",
    x = 1965, y = peak_row$avg_ship_weight * 0.55,
    label = "Fewer, heavier ships\n(capability era)",
    hjust = 0.5, vjust = 1,
    size = 2.6, color = "gray45",
    family = fonts$text, lineheight = 1.2
  ) +

  # Scales
  scale_x_continuous(breaks = seq(1860, 1980, by = 20)) +
  scale_y_continuous(labels = label_comma()) +
  # Scales
  labs(
    title = title_b,
    subtitle = subtitle_b,
    x = NULL,
    y = "Avg. gross tonnage per ship (tons)",
    caption = glue("{caption_b_note}<br>{caption_text}")
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      face   = "bold",
      family = fonts$title,
      size   = rel(1.05),
      hjust  = 0,
      margin = margin(b = 3)
    ),
    plot.subtitle = element_markdown(
      family   = fonts$text,
      size     = rel(0.8),
      hjust    = 0,
      color    = "gray40",
      margin   = margin(b = 8)
    )
  )

### |- Combine plots ----

p_final <- (p_a / p_b) +
  plot_layout(heights = c(1.55, 1)) +
  plot_annotation(
    title = title_text,
    theme = theme(
      plot.title = element_text(
        face   = "bold",
        family = fonts$title,
        size   = rel(1.6),
        hjust  = 0,
        margin = margin(b = 6)
      ),
      plot.margin = margin(t = 16, r = 20, b = 12, l = 16)
    )
  )

### |- preview ----
snap(p_final)


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

# ─ Session info ─────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-03
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────
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
# ────────────────────────────────────────────────────────────────────────────