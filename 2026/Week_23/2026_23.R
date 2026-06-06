
## Challenge: #TidyTuesday 2026 week 23
## Data:      Films Based on Video Games
## Author:    Steven Ponce
## Date:      2026-06-06


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
    width  = 8,
    height = 8.5,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 23)
game_films <- tt$game_films |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(game_films)


## 4. TIDY DATA ----

### |- analysis frame: theatrical releases with an RT score ----
df_rt <- game_films |>
  filter(
    category == "Theatrical releases",
    !is.na(rotten_tomatoes)
  ) |>
  mutate(
    year = year(release_date),
    decade = case_when(
      year < 2000 ~ "1990s",
      year < 2010 ~ "2000s",
      year < 2020 ~ "2010s",
      TRUE ~ "2020s"
    ),
    decade = factor(decade, levels = c("1990s", "2000s", "2010s", "2020s"))
  )

### |- per-decade summary ----
decade_stats <- df_rt |>
  summarise(
    n = n(),
    median_rt = median(rotten_tomatoes),
    fresh_pct = mean(rotten_tomatoes >= 60),
    .by = decade
  ) |>
  arrange(decade) |>
  mutate(
    x = as.integer(decade),
    fresh_lab = percent(fresh_pct, accuracy = 1),
    fresh_disp = if_else(decade == "2020s", paste0(fresh_lab, " Fresh"), fresh_lab),
    lab_size = if_else(decade == "2020s", 4.8, 4.3),
    median_lab = if_else(
      median_rt == floor(median_rt),
      as.character(as.integer(median_rt)),
      as.character(median_rt)
    ),
    axis_lab = paste0(decade, "\nn = ", n)
  )

# Named vector for the x-axis labels (decade + n)
axis_labels <- setNames(decade_stats$axis_lab, decade_stats$decade)


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    accent = "#722F37",
    box    = "#4A5568",
    point  = "#6B7280",
    fresh  = "#B8BCC2",
    note   = "#2E3338"
  )
)
col_accent <- clrs$palette$accent
col_box <- clrs$palette$box
col_point <- clrs$palette$point
col_fresh <- clrs$palette$fresh
col_note <- clrs$palette$note

### |- titles and caption ----
title_text <- str_glue("Good Video Game Movies Used to Be Rare")

subtitle_text <- str_glue(
  "The median reviewed adaptation rose from **17** in the 1990s to **51** ",
  "in the 2020s,<br>and *Fresh* films became more than three times as common."
)

base_caption <- create_social_caption(
  tt_year     = 2026,
  tt_week     = 23,
  source_text = "Wikipedia: List of films based on video games"
)

caption_text <- str_glue(
  "{base_caption}<br>",
  "<span style='font-size:8pt;color:#9A9A9A'>Sample: 73 theatrical releases ",
  "with a Rotten Tomatoes critic score, 1993\u20132026.</span>"
)

### |- annotation text  ----
ann_text <- str_glue(
  "Just **1 of 23** reviewed adaptations<br>",
  "was *Fresh* in the 2000s.<br>",
  "Today, more than **1 in 3** are."
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_markdown(
      face = "bold", family = fonts$title_1, size = rel(1.9),
      margin = margin(b = 4)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$text, size = rel(0.85), color = "#3A3A3A",
      width = unit(1, "npc"), lineheight = 1.1, margin = margin(b = 14)
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = rel(0.55), color = "#7F7F7F",
      hjust = 0, margin = margin(t = 14), lineheight = 1.1
    ),
    # plain text (with \n)
    axis.text.x = element_text(
      family = fonts$text, size = rel(1.0),
      lineheight = 0.95, color = "#3A3A3A"
    ),
    axis.text.y = element_text(family = fonts$text, size = rel(0.9)),
    axis.title.y = element_text(
      family = fonts$text, size = rel(0.9),
      color = "#4A5568"
    ),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(t = 20, r = 24, b = 16, l = 18)
  )
)

theme_set(weekly_theme)

### |- plot ----
set.seed(1234)

p <- ggplot(df_rt, aes(x = decade, y = rotten_tomatoes)) +

  # Geoms
  geom_hline(
    yintercept = 60, color = col_fresh, linewidth = 0.5
  ) +
  geom_jitter(
    width = 0.16, height = 0, size = 1.5, alpha = 0.42, color = col_point
  ) +
  geom_boxplot(
    coef = 1.0, outlier.shape = NA, fill = NA,
    color = col_box, linewidth = 0.55, width = 0.62, fatten = 0
  ) +
  geom_segment(
    data = decade_stats,
    aes(x = x - 0.31, xend = x + 0.31, y = median_rt, yend = median_rt),
    inherit.aes = FALSE,
    color = col_accent, linewidth = 1.1
  ) +
  geom_text(
    data = decade_stats,
    aes(x = x - 0.36, y = median_rt, label = median_lab),
    inherit.aes = FALSE, hjust = 1, vjust = 0.5,
    family = fonts$text, size = 3.1, color = col_accent
  ) +
  geom_text(
    data = decade_stats,
    aes(x = x, y = 97, label = fresh_disp, size = lab_size),
    inherit.aes = FALSE,
    family = fonts$title_2, fontface = "bold", color = col_accent
  ) +
  # Annotate
  annotate(
    "richtext",
    x = 4.32, y = 62, label = "Fresh (60)",
    hjust = 0, family = fonts$text, size = 3.3, color = "#4A5568",
    fill = NA, label.color = NA
  ) +
  annotate(
    "richtext",
    x = 1.25, y = 92, label = ann_text,
    hjust = 0, vjust = 1, family = fonts$text, size = 3.9,
    color = col_note, fill = NA, label.color = NA, lineheight = 1.25
  ) +
  # Scales
  scale_size_identity() +
  scale_x_discrete(
    labels = axis_labels,
    expand = expansion(add = c(0.55, 0.75))
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_cartesian(ylim = c(0, 100), clip = "off") +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "Rotten Tomatoes critic score"
  )

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

# ─ Session info ───────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-06-05
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────
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
# R.cache        0.17.0   2025-05-02 [1] CRAN (R 4.5.3)
# R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.5.2)
# R.oo           1.27.1   2025-05-02 [1] CRAN (R 4.5.2)
# R.utils        2.13.0   2025-02-24 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# rappdirs       0.3.4    2026-01-17 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.5.3)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.5.3)
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.5.3)
# rsconnect      1.8.0    2026-04-10 [1] CRAN (R 4.5.3)
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
# styler         1.11.0   2025-10-13 [1] CRAN (R 4.5.3)
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
# ──────────────────────────────────────────────────────────────
