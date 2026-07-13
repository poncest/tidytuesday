
## Challenge: #TidyTuesday 2026 week 28
## Data:      Many penguins
## Author:    Steven Ponce
## Date:      2026-07-13

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
    width  = 12,
    height = 7,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 28)
many_penguins <- tt$many_penguins |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(many_penguins)


## 4. TIDY DATA ----

penguins_shape <- many_penguins |>
  mutate(
    beak_shape_ratio = beak_depth / beak_length_culmen,
    is_highlight = genus == "Eudyptes"
  ) |>
  filter(
    !is.na(beak_shape_ratio),
    !is.na(wing_length),
    !is.na(tarsus_length)
  )

### |- genus cluster centers for direct labeling ----
genus_summary <- penguins_shape |>
  summarise(
    x_mean = mean(wing_length),
    y_mean = mean(beak_shape_ratio),
    n = n(),
    .by = genus
  ) |>
  arrange(desc(y_mean)) |>
  mutate(is_highlight = genus == "Eudyptes")

# Label only the two anchors
genus_labels <- genus_summary |>
  filter(genus %in% c("Eudyptes", "Aptenodytes")) |>
  mutate(
    label_x = case_when(
      genus == "Eudyptes" ~ 108,
      genus == "Aptenodytes" ~ x_mean + 4
    ),
    label_y = case_when(
      genus == "Eudyptes" ~ 0.355,
      genus == "Aptenodytes" ~ y_mean - 0.018
    )
  )

### |- correlation annotations
r_shape <- cor(penguins_shape$wing_length, penguins_shape$beak_shape_ratio) |> round(2)
r_tarsus <- cor(penguins_shape$wing_length, penguins_shape$tarsus_length) |> round(2)
n_total <- nrow(penguins_shape)


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = c("highlight" = "#722F37", "muted" = "gray70", "control" = "gray55")
)

### |- titles and caption ----
title_text <- str_glue("Bigger Penguins Do Not Simply Have Deeper Bills")

subtitle_text <- str_glue(
  "Across the same 76 penguins, bill shape is only weakly related to body ",
  "size, while tarsus length scales much more clearly with wing length."
) |>
  str_wrap(width = 95) |>
  str_replace_all("\n", "<br>")

caption_text <- str_glue(
  "{create_social_caption(tt_year = 2026, tt_week = 28, source_text = 'AVONET (Tobias et al. 2022), via TidyTuesday')}<br>",
  "{str_wrap(
    paste(
      'Trend lines show pooled relationships across all 76 birds. After accounting',
      'for genus, wing length is no longer associated with bill shape (p = 0.39),',
      'whereas the tarsus-wing relationship remains (p = 0.003), although its',
      'strength varies somewhat among genera (interaction p = 0.03).'
    ),
    width = 110
  ) |> str_replace_all('\n', '<br>')}"
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
      face = "bold", size = rel(1.7), family = fonts$title_1
    ),
    plot.subtitle = element_markdown(
      size = rel(1.0), family = fonts$subtitle, lineheight = 1.15,
      margin = margin(t = 4, b = 12)
    ),
    plot.caption = element_markdown(
      size = rel(0.55), family = fonts$caption, color = "gray45",
      lineheight = 1.25, hjust = 0,
      margin = margin(t = 10)
    ),
    panel.grid.major.y = element_line(color = "gray93", linewidth = 0.25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )
)

theme_set(weekly_theme)

### |- plot ----

### |- panel A: bill shape ratio vs wing length ----
p_shape <- ggplot(penguins_shape, aes(x = wing_length, y = beak_shape_ratio)) +
  geom_point(
    aes(color = is_highlight, alpha = is_highlight),
    size = 2.4
  ) +
  geom_smooth(
    method = "lm", se = FALSE, color = "gray70",
    linewidth = 0.4, linetype = "22"
  ) +
  geom_richtext(
    data = genus_labels,
    aes(
      x = label_x, y = label_y,
      label = glue(
        "<span style='font-size:10pt;color:{ifelse(is_highlight, \"#722F37\", \"gray30\")}'>",
        "<b>{genus}</b></span><br>",
        "<span style='font-size:7.5pt;color:gray55'>n = {n}</span>"
      )
    ),
    hjust = 0, vjust = 0.5, lineheight = 1.0,
    family = fonts$text,
    fill = NA, label.color = NA,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("TRUE" = "#722F37", "FALSE" = "gray45")) +
  scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.45)) +
  scale_x_continuous(limits = c(55, 200), breaks = seq(60, 200, 40)) +
  scale_y_continuous(limits = c(0.1, 0.62), labels = label_number(accuracy = 0.1)) +
  labs(
    title = NULL,
    x = "Wing length (mm)",
    y = "Bill shape (depth \u00f7 length)"
  ) +
  annotate(
    "text",
    x = 56, y = 0.60,
    label = "Weakly related to size (r = -0.32)",
    hjust = 0, vjust = 1, size = 3, color = "gray55", fontface = "italic",
    family = fonts$text
  ) +
  annotate(
    "text",
    x = 56, y = 0.575,
    label = "Genus differences dominate",
    hjust = 0, vjust = 1, size = 3, color = "gray25", fontface = "bold",
    family = fonts$text
  )

### |- panel B: tarsus length vs wing length ----
p_control <- ggplot(penguins_shape, aes(x = wing_length, y = tarsus_length)) +
  geom_point(color = "gray55", alpha = 0.55, size = 2.2) +
  geom_smooth(
    method = "lm", se = FALSE, color = "gray35",
    linewidth = 0.7
  ) +
  scale_x_continuous(limits = c(55, 200), breaks = seq(60, 200, 40)) +
  scale_y_continuous(limits = c(15, 65)) +
  labs(
    title = NULL,
    x = "Wing length (mm)",
    y = "Tarsus length (mm)"
  ) +
  annotate(
    "text",
    x = 58, y = 60,
    label = glue("Tarsus length scales with wing length (r = {r_tarsus})"),
    hjust = 0, size = 3, color = "gray40", fontface = "italic",
    family = fonts$text, lineheight = 0.9
  )

### |- compose with patchwork  ----
combined_plot <- p_shape + p_control +
  plot_layout(widths = c(1.28, 0.72)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = weekly_theme
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

# ─ Session info ────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-07-13
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────
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
# lattice        0.22-9   2026-02-09 [2] CRAN (R 4.5.3)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.5.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.5.3)
# Matrix         1.7-4    2025-08-28 [2] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# mgcv           1.9-4    2025-11-07 [2] CRAN (R 4.5.3)
# nlme           3.1-168  2025-03-31 [2] CRAN (R 4.5.3)
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
# P splines        4.5.3    2026-03-11 [2] local
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
# ───────────────────────────────────