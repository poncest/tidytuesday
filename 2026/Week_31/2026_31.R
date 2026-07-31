
## Challenge: #TidyTuesday 2026 week 31
## Data:      Basotho Wool
## Author:    Steven Ponce
## Date:      2026-08-01

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
tt <- tidytuesdayR::tt_load(2026, week = 31)
basotho_wool <- tt$basotho_wool |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(basotho_wool)
skim_without_charts(basotho_wool)


## 4. TIDY DATA ----

## |- Restrict to the two reporters with sufficient monthly density ----
wool_only <- basotho_wool |> filter(cmd_code == 5101)

wool_compare <- wool_only |>
  filter(
    reporter_desc %in% c("South Africa", "China"),
    ref_year >= 2016
  ) |>
  summarise(
    total_value = sum(primary_value, na.rm = TRUE),
    .by = c(reporter_desc, ref_month)
  )

## |- Seasonal deviation, signed: % above/below the reporter's OWN average month ----
seasonal_profile <- wool_compare |>
  mutate(
    reporter_mean = mean(total_value),
    pct_deviation = (total_value / reporter_mean - 1) * 100,
    .by = reporter_desc
  ) |>
  mutate(
    month_abb = factor(month.abb[ref_month], levels = month.abb),
    reporter_desc = factor(reporter_desc, levels = c("South Africa", "China"))
  )

## |- n ----
china_thin <- wool_only |>
  filter(reporter_desc == "China") |>
  count(ref_month) |>
  filter(n <= 6) |>
  arrange(ref_month)

china_caveat_label <- str_glue(
  "{first(month.abb[china_thin$ref_month])}\u2013",
  "{last(month.abb[china_thin$ref_month])} based on only ",
  "{min(china_thin$n)}\u2013{max(china_thin$n)} annual observations"
)
# china_caveat_label -> "Aug–Oct based on only 4–6 annual observations"


## 5. VISUALIZATION ----

## |-  plot aesthetics ----
clrs <- get_theme_colors(
  palette = c(
    "South Africa" = "#722F37",
    "China"        = "#A8B4BC"
  )
)

col_south_africa <- "#722F37"
col_china <- "#A8B4BC"

### |-  titles and caption ----
title_text <- str_glue("One Producer, Two Calendars")

subtitle_text <- str_glue(
  "Basotho wool imported by South Africa peaks in **Oct–Nov**,<br>",
  "while imports reported by **China** follow a sharply different ",
  "seasonal rhythm."
)
caption_explainer <- str_wrap(
  paste(
    "Both panels use the shared 2016\u20132024 reporting window.",
    "Oct\u2013Nov corresponds to spring in Lesotho (Southern Hemisphere).",
    "The data show when importing countries report the wool, not when it",
    "was shorn, sold, or shipped."
  ),
  width = 110
) |>
  str_replace_all("\n", "<br>")

caption_text <- str_glue(
  "{caption_explainer}<br>",
  create_social_caption(
    tt_year = 2026,
    tt_week = 31,
    source_text = "UN Comtrade via comtradr · partner-reported imports from Lesotho"
  )
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
      face = "bold", size = rel(1.6), family = fonts$title_1,
      margin = margin(b = 8), fonts$title_1, color = clrs$title
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8), family = fonts$subtitle, lineheight = 1.15,
      margin = margin(b = 16), color = clrs$subtitle
    ),
    plot.caption = element_markdown(
      size = rel(0.5), family = fonts$caption, color = alpha(clrs$caption, 0.8) 
    ),
    strip.text = element_text(
      face = "bold", size = rel(1.05), family = fonts$title_2, hjust = 0
    ),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = rel(0.7)),
    legend.position = "none"
  )
)

theme_set(weekly_theme)

### |- plot----
p <- ggplot(seasonal_profile, aes(x = month_abb, y = pct_deviation, fill = reporter_desc)) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
  geom_text(
    data = tibble(
      reporter_desc = factor("South Africa", levels = levels(seasonal_profile$reporter_desc)),
      x_pos = 10.5,
      pct_deviation = 185,
      label = "Peak"
    ),
    aes(x = x_pos, y = pct_deviation, label = label),
    inherit.aes = FALSE,
    family = fonts$text, size = 3.1, color = col_south_africa,
    fontface = "italic", hjust = 0.5
  ) +
  geom_text(
    data = tibble(
      reporter_desc = factor("China", levels = levels(seasonal_profile$reporter_desc)),
      x_pos = 9,
      pct_deviation = -118,
      label = china_caveat_label
    ),
    aes(x = x_pos, y = pct_deviation, label = label),
    inherit.aes = FALSE,
    family = fonts$text, size = 2.8, color = "gray30", fontface = "bold.italic"
  ) +
  facet_wrap(~reporter_desc, ncol = 1) +
  scale_fill_manual(
    values = c("South Africa" = col_south_africa, "China" = col_china)
  ) +
  scale_y_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), x, "%"),
    breaks = seq(-100, 200, 50),
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "Monthly deviation from each importer's average"
  ) +
  canvas(width = 8, height = 8.7, units = "in", dpi = 300)

### |- save ----
save_ggplot(
  plot = p,
  file = "2026/Week_31/2026_31.png",
  width = 8, height = 8.7
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

# ─ Session info ────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.5.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-07-31
# rstudio  2026.07.1+147 Pacific Dogwood (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ────────────────────────────────────────────────
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
# pkgload        1.5.3   2026-06-15 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R.cache        0.17.0  2025-05-02 [1] CRAN (R 4.6.0)
# R.methodsS3    1.8.2   2022-06-13 [1] CRAN (R 4.6.0)
# R.oo           1.27.1  2025-05-02 [1] CRAN (R 4.6.0)
# R.utils        2.13.0  2025-02-24 [1] CRAN (R 4.6.0)
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
# styler         1.11.0  2025-10-13 [1] CRAN (R 4.6.0)
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
# ───────────────────────────────────────────────────────────