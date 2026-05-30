
## Challenge: #TidyTuesday 2026 week 22
## Data:      European Parenting Leave Policies
## Author:    Steven Ponce
## Date:      2026-05-30


## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, patchwork, countrycode
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 8,
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
tt <- tidytuesdayR::tt_load(2026, week = 22)
eplp_raw <- tt$eplp |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(eplp_raw)


## 4. TIDY DATA ----

### |- sentinel cleanup ----
## -98 (numeric) and "Not applicable" (character) are NOT zeros — recode to NA.
## co_ld == 0 is a REAL value (no co-parent leave) and must survive untouched;
## na_if(x, -98) leaves 0 alone by construction.
eplp <- eplp_raw |>
  mutate(
    across(where(is.numeric), \(x) na_if(x, -98)),
    across(where(is.character), \(x) na_if(x, "Not applicable"))
  )

### |- country code ----
## EU datasets often use UK/EL rather than ISO-2 GB/GR
eplp <- eplp |>
  mutate(
    country = recode(country, "UK" = "GB", "EL" = "GR"),
    country_name = countrycode(country, origin = "iso2c", destination = "country.name")
  )

baseline_year <- min(eplp$year, na.rm = TRUE)
max_year <- max(eplp$year, na.rm = TRUE)

mat_at_baseline <- eplp |>
  filter(year == baseline_year) |>
  mutate(has_maternity = (coalesce(mat_m_ld_ab, 0) > 0 |
    coalesce(mat_v_ld_ab, 0) > 0 |
    coalesce(mat_m_ld_bb, 0) > 0 |
    coalesce(mat_v_ld_bb, 0) > 0)) |>
  summarise(n_with_maternity = sum(has_maternity), n_total = n())

### |- co-parent leave adoption year (first year co_ld > 0) ----
adoption <- eplp |>
  filter(co_ld > 0) |>
  summarise(adopt_year = min(year), .by = country_name)

### |- Panel A data: waiting-time per country ----
panel_a <- eplp |>
  distinct(country_name) |>
  left_join(adoption, by = "country_name") |>
  mutate(
    adopted = !is.na(adopt_year),
    end_x = if_else(adopted, adopt_year, max_year),
    wait = end_x - baseline_year
  ) |>
  arrange(adopted, desc(adopt_year)) |>
  mutate(country_name = fct_inorder(country_name))

### |- Panel B data: cumulative adoption wave ----
panel_b <- adoption |>
  count(adopt_year, name = "n_new") |>
  arrange(adopt_year) |>
  mutate(cumulative = cumsum(n_new))

## Complete the year grid so the step line spans the full record cleanly.
panel_b_full <- tibble(year = baseline_year:max_year) |>
  left_join(panel_b, by = c("year" = "adopt_year")) |>
  mutate(
    n_new      = replace_na(n_new, 0),
    cumulative = cumsum(n_new)
  )

n_countries <- n_distinct(eplp$country_name)
n_adopted <- nrow(adoption)


## 5. VISUALIZATION ----

### |- plot aesthetics ----

clrs <- get_theme_colors(
  palette = list(
    col_accent = "#722F37",
    col_wait = "gray75",
    col_never = "gray60",
    col_never_text = "gray68",
    col_baseline = "gray45",
    col_anno = "gray25",
    col_wave = "#722F37",
    col_sub = "gray35"
  )
)
col_accent <- clrs$palette$col_accent
col_wait <- clrs$palette$col_wait
col_never <- clrs$palette$col_never
col_never_text <- clrs$palette$col_never_text
col_baseline <- clrs$palette$col_baseline
col_anno <- clrs$palette$col_anno
col_wave <- clrs$palette$col_wave
col_sub <- clrs$palette$col_sub

### |- titles and caption ----
title_text <- str_glue("Europe built parental leave for mothers first")

subtitle_text <- str_glue(
  "Maternity leave was **already universal** across all {n_countries} countries when the dataset begins in 1970.<br>",
  "**Co-parent leave** arrived years \u2014 often decades \u2014 later."
)

source_note <- str_glue(
  "Note: Spain already had co-parent leave at the 1970 baseline, so its onset is unobserved (plotted at 1970)."
)

caption_text <- create_social_caption(
  tt_year     = 2026,
  tt_week     = 22,
  source_text = "European Parenting Leave Policies (EPLP) Dataset, Spitzer et al. (2025)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_markdown(family = fonts$title_1, face = "bold", size = rel(1.4)),
    plot.subtitle = element_markdown(
      family = fonts$aubtitle, size = rel(0.80), color = col_sub,
      lineheight = 1.1, margin = margin(t = 4, b = 10)
    ),
    axis.title = element_markdown(family = fonts$text, size = rel(0.8), color = col_sub),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank()
  )
)

theme_set(weekly_theme)

### |- Panel A: the waiting-time chart ----
p_a <- ggplot(panel_a) +
  # Geoms
  geom_segment(
    data = filter(panel_a, adopted),
    aes(x = baseline_year, xend = end_x, y = country_name, yend = country_name),
    color = col_wait, linewidth = 0.6, alpha = 0.65
  ) +
  geom_segment(
    data = filter(panel_a, !adopted),
    aes(x = baseline_year, xend = max_year, y = country_name, yend = country_name),
    color = col_never, linewidth = 0.4, linetype = "dotted", alpha = 0.6
  ) +
  geom_point(
    data = filter(panel_a, !adopted),
    aes(x = max_year, y = country_name),
    shape = 21, fill = "white", color = col_never, size = 2.4, stroke = 0.7
  ) +
  geom_point(
    data = filter(panel_a, adopted),
    aes(x = adopt_year, y = country_name),
    color = col_accent, size = 3
  ) +
  # Annotate
  annotate("text",
    x = baseline_year, y = n_countries + 0.9,
    label = "Maternity leave\nalready in place, 1970",
    hjust = 0, vjust = 1, size = 3.1, color = col_anno,
    lineheight = 0.95, family = fonts$text
  ) +
  annotate("text",
    x = 1999, y = 13.5,
    label = "Most countries adopted\nafter 2000",
    hjust = 1, vjust = 0.5, size = 3.1, fontface = "italic",
    color = col_anno, lineheight = 0.95, family = fonts$text
  ) +
  annotate("text",
    x = max_year, y = 2.0,
    label = "Germany: no co-parent leave\nrecorded through 2024",
    hjust = 1, vjust = 0, size = 2.9, color = col_never_text,
    lineheight = 0.95, family = fonts$text
  ) +
  # Scales
  scale_y_discrete(
    limits = rev,
    expand = expansion(add = c(1.1, 0.8))
  ) +
  scale_x_continuous(
    limits = c(baseline_year, max_year + 1),
    breaks = seq(1970, 2020, by = 10),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    x = NULL, y = NULL,
    title = "How long the second parent waited",
    subtitle = "Length of each line = years from 1970 to the first year co-parent leave existed."
  ) +
  # Theme
  theme(
    plot.title = element_markdown(family = fonts$title_2, face = "bold", size = rel(1.15)),
    plot.subtitle = element_markdown(
      family = fonts$text, size = rel(0.75), color = col_sub,
      margin = margin(b = 14)
    ),
    plot.margin = margin(t = 18, r = 10, b = 14, l = 6)
  )

### |- Panel B: the adoption wave ----
p_b <- ggplot(panel_b_full, aes(x = year, y = cumulative)) +
  # Annotate
  annotate("segment",
    x = 2019, xend = 2019, y = 0, yend = 13,
    color = col_sub, linewidth = 0.35
  ) +
  annotate("text",
    x = 2019, y = 13.6, label = "2019 EU directive",
    hjust = 0.5, vjust = 0, size = 2.6, color = col_anno,
    family = fonts$text
  ) +
  annotate("text",
    x = 2017.5, y = 5.5,
    label = "19 of 21 countries had\nalready adopted by 2019",
    hjust = 1, vjust = 1, size = 2.9, color = col_sub,
    lineheight = 0.98, family = fonts$text
  ) +
  # Geoms
  geom_step(color = col_wave, linewidth = 0.9, direction = "hv") +
  geom_point(
    data = slice_max(panel_b_full, year, with_ties = FALSE),
    aes(x = year, y = cumulative), color = col_wave, size = 2.6
  ) +
  geom_text(
    data = slice_max(panel_b_full, year, with_ties = FALSE),
    aes(label = glue("{cumulative} of {n_countries} countries")),
    hjust = 1, vjust = -0.9, size = 3.1, color = col_wave, family = fonts$text
  ) +
  # Scales
  scale_x_continuous(
    limits = c(baseline_year, max_year + 1),
    breaks = seq(1970, 2020, by = 10),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    x = NULL, y = "Countries offering\nco-parent leave",
    title = "Europe as a system confirms the wave",
    subtitle = "Cumulative count of countries with co-parent leave in place."
  ) +
  # Theme
  theme(
    plot.title = element_markdown(family = fonts$title_2, face = "bold", size = rel(1.15)),
    plot.subtitle = element_markdown(
      family = fonts$text, size = rel(0.75), color = col_sub,
      margin = margin(b = 6)
    ),
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    plot.margin = margin(t = 12, r = 10, b = 6, l = 6)
  )

### |- Combine plots ----
p_combined <- p_a / p_b +
  plot_layout(heights = c(1.75, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption  = str_glue("{source_note}<br>{caption_text}"),
    theme = theme(
      plot.title = element_markdown(family = fonts$title_1, face = "bold", size = rel(1.8)),
      plot.subtitle = element_textbox_simple(
        family = fonts$text, size = rel(0.80), color = col_sub,
        lineheight = 1.2, width = unit(1, "npc"), margin = margin(t = 2, b = 2)
      ),
      plot.caption = element_markdown(
        family = fonts$caption, size = rel(0.45), color = col_sub,
        hjust = 0, margin = margin(t = 12), linewidth = 1.25
      )
    )
  )

### |- preview ----
snap(p_combined)


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

# ─ Session info ───────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-28
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
#
# ─ Packages ───────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# countrycode  * 1.8.0    2026-04-16 [1] CRAN (R 4.5.3)
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
# ──────────────────────────────────────────────────────────────────────────────────