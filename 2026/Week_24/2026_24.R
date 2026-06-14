
## Challenge: #TidyTuesday 2026 week 24
## Data:      UK Baby Names
## Author:    Steven Ponce
## Date:      2026-06-13


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
    height = 6,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 24)
# england_wales_names <- tt$england_wales_names |> clean_names()
# ni_names <- tt$ni_names |> clean_names()
scotland_names <- tt$scotland_names |> clean_names()

rm(tt)


## 3. EXAMINING THE DATA ----
# glimpse(england_wales_names)
# glimpse(ni_names)
glimpse(scotland_names)


## 4. TIDY DATA ----

### |- names needed to cover half of births, per year & sex (n50) ----
## n50 = the number of distinct names that together account for the first 50%
## of babies. It is redaction-robust (the 50% point sits far above the
## redacted ≤2 tail) and N-robust (independent of falling birth totals).
n50_long <- scotland_names |>
  filter(!is.na(number)) |>
  arrange(year, sex, desc(number)) |>
  group_by(year, sex) |>
  mutate(cum_share = cumsum(number) / sum(number)) |>
  summarise(n50 = sum(cum_share < 0.5) + 1L, .groups = "drop")

### |- wide frame for the gap ribbon ----
ribbon_df <- n50_long |>
  pivot_wider(names_from = sex, values_from = n50) |>
  clean_names() |>
  mutate(ymin = pmin(boy, girl), ymax = pmax(boy, girl))

### |- annotation anchors (derived from data, never estimated) ----
boy_1974   <- ribbon_df |> filter(year == 1974) |> pull(boy)    
girl_1974  <- ribbon_df |> filter(year == 1974) |> pull(girl)   
ratio_1974 <- round(girl_1974 / boy_1974, 1)                    
par_year   <- 2017L
par_y      <- ribbon_df |> filter(year == par_year) |> pull(boy) 
last_year  <- max(ribbon_df$year)                                
boy_last   <- ribbon_df |> filter(year == last_year) |> pull(boy)  
girl_last  <- ribbon_df |> filter(year == last_year) |> pull(girl) 


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    girl   = "#722F37",
    boy    = "#7A7068",
    ribbon = "#722F37",
    ink    = "#2C2825",
    mute   = "#7A7068"
  )
)
col_girl <- clrs$palette$girl
col_boy <- clrs$palette$boy
col_ribbon <- clrs$palette$ribbon
col_ink <- clrs$palette$ink
col_mute <- clrs$palette$mute

### |- titles and caption ----
title_text <- str_glue("Half of Scotland's baby boys once shared just 18 names")

subtitle_text <- str_glue(
  "That number has climbed to **77** since 1974, narrowing a gap that once ",
  "made girls' names almost twice as varied."
)

methods_note <- str_glue(
  "Each line shows how many distinct names are needed to account for half of ",
  "babies born that year."
)

social_caption <- create_social_caption(
  tt_year     = 2026,
  tt_week     = 24,
  source_text = "National Records of Scotland | Names with counts of 2 or fewer are redacted"
)

caption_text <- str_glue("{methods_note}<br>{social_caption}")

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_markdown(
      family = fonts$title_1, face = "bold", size = rel(1.6),
      color = col_ink, margin = margin(b = 8)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$subtitle, size = rel(0.74), color = col_mute,
      lineheight = 1.15, width = unit(1, "npc"), margin = margin(b = 18)
    ),
    plot.caption = element_markdown(
      family = fonts$caption, size = rel(0.45), color = col_mute,
      hjust = 0, margin = margin(t = 16)
    ),
    axis.title.y = element_markdown(family = fonts$text, size = rel(0.85), color = col_mute),
    axis.text = element_markdown(family = fonts$text, color = col_mute),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 50, b = 16, l = 20)
  )
)

theme_set(weekly_theme)

### |- plot ----
p <- ggplot(ribbon_df, aes(x = year)) +

  # Geoms
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = col_ribbon, alpha = 0.10) +
  geom_line(aes(y = girl), color = col_girl, linewidth = 1.0) +
  geom_line(aes(y = boy), color = col_boy, linewidth = 1.0) +

  # Annotations
  annotate("segment",
    x = 1974, xend = 1974, y = boy_1974, yend = girl_1974,
    color = col_ink, linewidth = 0.5,
    arrow = arrow(ends = "both", length = unit(0.05, "in"), type = "closed")
  ) +
  annotate("text",
    x = 1976.5, y = (boy_1974 + girl_1974) / 2 + 3,
    label = str_glue("Girls {ratio_1974}\u00d7 as varied\nin 1974"),
    family = fonts$text, size = 3.1, color = col_mute,
    hjust = 0, lineheight = 0.95
  ) +
  annotate("point", x = par_year, y = par_y, color = col_ink, size = 1.8) +
  annotate("segment",
    x = 2008, xend = par_year - 0.4, y = 90, yend = par_y + 1.5,
    color = col_mute, linewidth = 0.3
  ) +
  annotate("text",
    x = 2007.5, y = 92,
    label = "Boys and girls reach parity\n2017 (65 vs 65)",
    family = fonts$text, size = 3.1, color = col_mute,
    hjust = 1, lineheight = 0.95
  ) +
  annotate("text",
    x = last_year + 0.5, y = girl_last, label = "Girls",
    family = fonts$text, fontface = "bold", size = 3.4,
    color = col_girl, hjust = 0
  ) +
  annotate("text",
    x = last_year + 0.5, y = boy_last, label = "Boys",
    family = fonts$text, fontface = "bold", size = 3.4,
    color = col_boy, hjust = 0
  ) +
  # Scales
  scale_x_continuous(
    breaks = c(1974, 1985, 1995, 2005, 2015, 2025),
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  scale_y_continuous(
    breaks = seq(0, 80, 20),
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    x = NULL,
    y = "Names needed to account for<br>half of babies born",
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
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

# ─ Session info ────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-06-13
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder    * 0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
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
# ───────────────────────────────────────────────────────────────────────────