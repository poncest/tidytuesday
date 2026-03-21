## Challenge: #TidyTuesday 2026 week 12
## Data:      One Million Digits of Pi
## Author:    Steven Ponce
## Date:      2026-03-21

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor,      
    scales, glue, skimr, patchwork    
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 10,
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
tt <- tidytuesdayR::tt_load(2026, week = 12)
pi_digits <- tt$pi_digits |> clean_names()
rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(pi_digits)

skim_without_charts(pi_digits)


## 4. TIDY DATA ----
### |- Compute random walk coordinates ----
# Each digit maps to a compass direction: digit × 36° (evenly divides 360°)
# Cumulative sum of unit steps gives the 2D trajectory.
walk_data <- pi_digits |>
  arrange(digit_position) |>
  mutate(
    angle_rad = digit * (2 * pi / 10), # digit × 36° in radians
    dx        = cos(angle_rad),
    dy        = sin(angle_rad),
    x         = cumsum(dx),
    y         = cumsum(dy)
  )

### |- Main chart: first 10K steps ----
n_main <- 10000L

walk_main <- walk_data |>
  slice_head(n = n_main + 1L) |>
  mutate(
    x_end         = lead(x),
    y_end         = lead(y),
    position_norm = (digit_position - 1) / n_main
  ) |>
  filter(!is.na(x_end))

### |- Inset: full 1M walk ----
walk_inset <- walk_data |>
  filter(digit_position %% 50 == 0 | digit_position == 1L) |>
  mutate(position_norm = (digit_position - 1) / max(digit_position))

### |- annotation coordinates ----
start_pt <- walk_main |> slice_head(n = 1)
end_pt <- walk_main |> slice_tail(n = 1)
start_label_x <- start_pt$x - 2
start_label_y <- start_pt$y + 2
end_label_x <- end_pt$x_end - 2
end_label_y <- end_pt$y_end + 4


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = list(
        start      = "#0D1B2A",   
        end        = "#E8C547",   
        bg         = "#0D1B2A",   
        text_main  = "#F0EDE6",   
        text_muted = "#9A9690",
        inset_bg   = "#141E2D",   
        highlight  = "#E8C547"  
    )
)

### |- color scale (shared between main + inset) ----
walk_color_scale <- scale_color_gradient(
    low   = colors$palette$start,
    high  = colors$palette$end,
    guide = "none"
)

### |- titles and caption ----
title_text    <- "A Million Decisions, No Destination"

subtitle_text <- str_glue(
    "The random walk of \u03c0: each digit maps to a compass direction ",
    "(0\u2192North, 1\u219236\u00b0\u2026 9\u2192324\u00b0).<br>",
    "After 10,000 steps the path wanders freely \u2014 ",
    "after 1,000,000 it arrives <i>nowhere in particular</i>."
)
caption_text <- create_social_caption(
    tt_year     = 2026,
    tt_week     = 12,
    source_text = "PiDay.org / Eneko Pi (one-million)"
)

### |- fonts ----
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
      face = "bold", family = fonts$title, size = rel(1.1),
      color = colors$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_text(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.7), margin = margin(b = 20), hjust = 0
    ),

    # Grid
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    
    # Axes
    axis.title = element_text(size = rel(0.6), color = "gray30"),
    axis.text = element_text(color = "gray30"),
    axis.text.y = element_text(size = rel(0.6)),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),

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
    plot.margin = margin(10, 20, 10, 20),
  )
)

# Set theme
theme_set(weekly_theme)

### |- Panel A: National Trend ----
p_main <- walk_main |>
  ggplot(aes(x = x, y = y, color = position_norm)) +
  geom_segment(
    aes(xend = x_end, yend = y_end),
    linewidth = 0.35, alpha = 0.85, lineend = "round"
  ) +
  # Start marker
  geom_point(
    data = start_pt, aes(x = x, y = y),
    color = colors$palette$text_main, size = 2.2, shape = 21,
    fill = colors$palette$end, stroke = 0.8
  ) +
  # End marker
  geom_point(
    data = end_pt, aes(x = x_end, y = y_end),
    color = colors$palette$text_main, size = 2.2, shape = 21,
    fill = colors$palette$text_muted, stroke = 0.8
  ) +
  # Start label
  annotate(
    "text",
    x = start_label_x,
    y = start_label_y,
    label = "Start\n(3.14159\u2026)",
    hjust = 1, size = 2.6, lineheight = 1.1,
    family = fonts$text, color = colors$palette$highlight
  ) +
  # End label
  annotate(
    "text",
    x = end_label_x + 4,
    y = end_label_y - 4,
    label = "Step 10,000",
    hjust = 0, size = 2.6,
    family = fonts$text, color = colors$palette$text_muted
  ) +
  walk_color_scale +
  coord_equal(xlim = range(walk_main$x), ylim = range(walk_main$y)) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = colors$palette$bg, color = NA),
    panel.background = element_rect(fill = colors$palette$bg, color = NA),
    plot.margin      = margin(5, 5, 5, 5)
  )

### |- INSET: full 1M walk ----
p_inset <- walk_inset |>
  ggplot(aes(x = x, y = y, color = position_norm)) +
  geom_path(linewidth = 0.15, alpha = 0.6, lineend = "round") +
  walk_color_scale +
  coord_equal() +
  labs(title = "All 1,000,000 steps") +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill      = colors$palette$inset_bg,
      color     = colors$palette$text_muted,
      linewidth = 0.4
    ),
    panel.background = element_rect(fill = colors$palette$inset_bg, color = NA),
    plot.title = element_text(
      size = 7, family = fonts$text, color = colors$palette$text_muted,
      margin = margin(4, 0, 2, 4), hjust = 0
    ),
    plot.margin = margin(6, 6, 6, 6)
  )

### |- Combined Plots ----
combined_plots <- p_main +
  inset_element(
    p_inset,
    left     = 0.62, right  = 0.99,
    bottom   = 0.62, top    = 0.99,
    align_to = "panel"
  ) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.background = element_rect(fill = colors$palette$bg, color = NA),
      plot.title = element_text(
        face = "bold", size = 20, family = fonts$title,
        color = colors$palette$text_main,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_markdown(
        size = 10, family = fonts$text, lineheight = 1.5,
        color = colors$palette$text_muted,
        margin = margin(b = 4)
      ),
      plot.caption = element_markdown(
        size = 7.5, family = fonts$caption,
        color = colors$palette$text_muted,
        lineheight = 1.3,
        margin = margin(t = 8)
      ),
      plot.margin = margin(20, 20, 12, 20)
    )
  )

snap(combined_plots)


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

# ─ Session info ────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-16
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   3.6.3 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/Rtmp63S9us/file1c546cc745e9". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.37   2024-08-19 [1] CRAN (R 4.3.3)
# dplyr        * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.3.1)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.3.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.3.1)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.3.1)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.3)
# gh             1.4.1    2024-03-28 [1] CRAN (R 4.3.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.3.3)
# gitcreds       0.1.2    2022-09-08 [1] CRAN (R 4.3.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.3.3)
# P graphics     * 4.3.1    2023-06-16 [2] local
# P grDevices    * 4.3.1    2023-06-16 [2] local
# P grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.3.1)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.3.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.3.1)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.3.1)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.3.1)
# httr2          1.2.2    2025-12-08 [1] CRAN (R 4.3.1)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.3.1)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# P methods      * 4.3.1    2023-06-16 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.3.1)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# P parallel       4.3.1    2023-06-16 [2] local
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.3.1)
# pillar         1.10.2   2025-04-05 [1] CRAN (R 4.3.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R.cache        0.16.0   2022-07-21 [1] CRAN (R 4.3.3)
# R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.3.3)
# R.oo           1.27.0   2024-11-01 [1] CRAN (R 4.3.3)
# R.utils        2.13.0   2025-02-24 [1] CRAN (R 4.3.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# rappdirs       0.3.4    2026-01-17 [1] CRAN (R 4.3.1)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# rlang          1.1.7    2026-01-09 [1] CRAN (R 4.3.1)
# rmarkdown      2.30     2025-09-28 [1] CRAN (R 4.3.1)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.3.1)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.3.1)
# rsvg           2.6.2    2025-03-23 [1] CRAN (R 4.3.3)
# S7             0.2.0    2024-11-07 [1] CRAN (R 4.3.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.3.1)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.3)
# P stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.3.3)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.3.1)
# styler         1.11.0   2025-10-13 [1] CRAN (R 4.3.1)
# svglite        2.1.3    2023-12-08 [1] CRAN (R 4.3.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.3.1)
# textshaping    1.0.4    2025-10-10 [1] CRAN (R 4.3.1)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.3.1)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# tidytuesdayR   1.2.1    2025-04-29 [1] CRAN (R 4.3.1)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.3.1)
# P tools          4.3.1    2023-06-16 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.3.3)
# P utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.7.1    2026-01-23 [1] CRAN (R 4.3.1)
# vroom          1.7.0    2026-01-27 [1] CRAN (R 4.3.1)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.3.3)
# xfun           0.56     2026-01-18 [1] CRAN (R 4.3.1)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.1/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
