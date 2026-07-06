
## Challenge: #TidyTuesday 2026 week 27
## Data:      UFC Athletes and Fight Data
## Author:    Steven Ponce
## Date:      2026-07-05

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, binom, patchwork
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 12,
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
tt <- tidytuesdayR::tt_load(2026, week = 27)
ultimate_ufc_dataset <- tt$ultimate_ufc_dataset |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(ultimate_ufc_dataset)


## 4. TIDY DATA ----

## |- panel A: physical-advantage win rates (corner-assignment-proof) ----
## Reach is systematically confounded with corner (Red is the betting
## favorite ~62% of the time), so advantage is defined as "did the
## fighter WITH more of the trait win," independent of corner.

adv_data <- ultimate_ufc_dataset |>
  filter(!is.na(winner)) |>
  mutate(
    reach_adv_won = case_when(
      reach_dif > 0 ~ winner == "Red",
      reach_dif < 0 ~ winner == "Blue", TRUE ~ NA
    ),
    height_adv_won = case_when(
      height_dif > 0 ~ winner == "Red",
      height_dif < 0 ~ winner == "Blue", TRUE ~ NA
    ),
    younger_won = case_when(
      age_dif < 0 ~ winner == "Red",
      age_dif > 0 ~ winner == "Blue", TRUE ~ NA
    )
  )

wilson_summary <- function(x, label) {
  x <- x[!is.na(x)]
  ci <- binom.wilson(sum(x), length(x))
  tibble(trait = label, mean = ci$mean, lower = ci$lower, upper = ci$upper, n = ci$n)
}

advantage_summary <- bind_rows(
  wilson_summary(adv_data$reach_adv_won, "Reach advantage"),
  wilson_summary(adv_data$height_adv_won, "Height advantage"),
  wilson_summary(adv_data$younger_won, "Younger fighter")
) |>
  mutate(trait = fct_reorder(trait, mean))

### |- panel B: betting-market calibration curve ----
implied_prob <- function(odds) {
  if_else(odds < 0, -odds / (-odds + 100), 100 / (odds + 100))
}

calibration_data <- ultimate_ufc_dataset |>
  filter(!is.na(r_odds), !is.na(winner)) |>
  mutate(
    r_implied = implied_prob(r_odds),
    red_won = winner == "Red",
    implied_bucket = case_when(
      r_implied < 0.30 ~ "Heavy\nunderdog",
      r_implied < 0.45 ~ "Underdog",
      r_implied < 0.55 ~ "Close to\neven",
      r_implied < 0.70 ~ "Favorite",
      TRUE ~ "Heavy\nfavorite"
    ),
    implied_bucket = fct_reorder(implied_bucket, r_implied)
  ) |>
  group_by(implied_bucket) |>
  summarise(
    n = n(),
    mean_implied = mean(r_implied),
    actual_win_rate = mean(red_won),
    .groups = "drop"
  )

avg_calibration_error <- calibration_data |>
  summarise(avg_abs_gap = mean(abs(actual_win_rate - mean_implied))) |>
  pull(avg_abs_gap)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
clrs <- get_theme_colors(
  palette = c("highlight" = "#722F37", "secondary" = "gray70")
)

### |- titles and caption ----

title_text <- str_glue("Reach, Height, and Age Barely Predict a UFC Winner")

subtitle_text <- str_glue(
  "Knowing who's **taller**, has more **reach**, or is **younger**<br>",
  "adds almost nothing beyond what the betting market already reflects.<br>",
  "Across thousands of UFC fights, its implied odds closely match actual outcomes."
)

caption_text <- create_social_caption(
  tt_year = 2026,
  tt_week = 27,
  source_text = "{fightr} R package (UFC athlete profiles, UFCStats, Kaggle, Octagon API)"
)

caption_text <- str_glue(
  "{caption_text}<br>",
  "Note: advantage defined as the fighter with more of the trait, independent of corner assignment."
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_text(
      face = "bold", family = fonts$title_1, size = rel(1.4), margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$subtitle, size = rel(0.95), lineheight = 1.2, margin = margin(b = 16)
    ),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = "bold", family = fonts$title, size = rel(0.9))
  )
)

theme_set(weekly_theme)

### |- plot ----

### |- Panel A: physical advantage win rates ----
p_advantage <- ggplot(advantage_summary, aes(x = mean, y = trait)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray30", linewidth = 0.6) +
  geom_pointrange(
    aes(xmin = lower, xmax = upper),
    color = "gray45", fill = "gray45",
    size = 0.9, linewidth = 1.1, shape = 21
  ) +
  geom_text(
    aes(label = percent(mean, accuracy = 0.1)),
    vjust = -1.6, family = fonts$text, fontface = "bold", size = 3.6
  ) +
  scale_x_continuous(
    labels = c("45%", "50%\n(coin flip)"),
    limits = c(0.44, 0.545),
    breaks = c(0.45, 0.50)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "What fans debate before the fight",
    subtitle = "Win rate for the fighter with the advantage",
    x = NULL, y = NULL
  ) +
  theme(
    plot.title = element_text(size = rel(0.95), face = "bold", family = fonts$title_1),
    plot.subtitle = element_markdown(size = rel(0.7), color = "gray30"),
    axis.text.y = element_text(size = rel(0.9), face = "bold")
  )

### |- Panel B: calibration curve ----
p_calibration <- ggplot(calibration_data, aes(x = mean_implied, y = actual_win_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_line(color = "#722F37", linewidth = 0.6, alpha = 0.45) +
  geom_point(aes(size = n), color = "#722F37") +
  geom_text(
    aes(label = implied_bucket),
    vjust = -1.5, family = fonts$text, size = 3, lineheight = 0.85, color = "gray30"
  ) +
  annotate(
    "text",
    x = 0.30, y = 0.87,
    label = str_glue("Average error: ~{percent(avg_calibration_error, accuracy = 1)}"),
    family = fonts$text, fontface = "bold", size = 3.4, color = "gray30", hjust = 0
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0.15, 0.95)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.15, 0.95)) +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  coord_fixed(clip = "off") +
  labs(
    title = "What the betting market already captures",
    subtitle = "Market-implied probability vs. actual win rate  ·  point size = fights per bucket",
    x = "Betting market implied probability",
    y = "Actual win rate"
  ) +
  theme(
    plot.title = element_text(size = rel(0.95), face = "bold", family = fonts$title_1),
    plot.subtitle = element_markdown(size = rel(0.7), color = "gray30"),
    axis.title = element_text(size = rel(0.8))
  )

### |- combine plots ----
combined_plot <- p_advantage + p_calibration +
  plot_layout(widths = c(0.72, 1.28)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        face = "bold", family = fonts$title_1, size = rel(1.6),
        margin = margin(b = 6)
      ),
      plot.subtitle = element_markdown(
        family = fonts$subtitle, size = rel(0.8), lineheight = 1.3,
        margin = margin(b = 18)
      ),
      plot.caption = element_markdown(
        family = fonts$caption, size = rel(0.55), color = "gray40",
        hjust = 0, margin = margin(t = 14), lineheight = 1.15
      )
    )
  )


### |- preview ----
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

# ─ Session info ─────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-07-05
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# binom        * 1.1-1.1  2022-05-02 [1] CRAN (R 4.5.3)
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
# ────────────────────────────────────────────────────────────────────────────────────────
