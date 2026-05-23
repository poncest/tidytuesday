
## Challenge: #TidyTuesday 2026 week 21
## Data:      Sustainable Energy for All
## Author:    Steven Ponce
## Date:      2026-05-23


## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, patchwork, ggrepel
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 14,
    height = 9,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 21)

energy_raw <- tt$energy_cleaned |> clean_names()

rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(energy_raw)


## 4. TIDY DATA ----
energy_latest <- energy_raw |>
  filter(
    !is.na(access_electricity_urban_pop_pct),
    !is.na(access_electricity_rural_pop_pct),
    !is.na(access_electricity_total_pop_pct),
    !is.na(access_non_solid_fuel_total_pop_pct)
  ) |>
  slice_max(yr, by = country_name, with_ties = FALSE) |>
  select(
    country_name, country_code, yr,
    urban_elec = access_electricity_urban_pop_pct,
    rural_elec = access_electricity_rural_pop_pct,
    total_elec = access_electricity_total_pop_pct,
    non_solid = access_non_solid_fuel_total_pop_pct
  ) |>
  mutate(
    gap_rural   = urban_elec - rural_elec,
    gap_cooking = total_elec - non_solid
  )

regional_aggregates <- c(
  "Oceania", "Southern Asia", "Eastern Asia", "South East Asia",
  "Eastern Asia (not including Japan)", "Eastern Asia (including Japan)",
  "South Eastern Asia", "Western Asia", "Central Asia",
  "Caucasus and Central Asia",
  "Sub-Saharan Africa", "Northern Africa", "Latin America and the Caribbean",
  "Caribbean", "Central America", "South America",
  "Oceania (not including Australia and New Zealand)",
  "World", "Developed regions", "Developing regions",
  "Landlocked developing countries", "Small island developing States",
  "Least developed countries", "Low income", "Lower middle income",
  "Upper middle income", "High income",
  "Macedonia, FYR"
)

## Panel A: lowest rural access among countries where cities have meaningful access
panel_a_data <- energy_latest |>
  filter(
    !country_name %in% regional_aggregates,
    urban_elec >= 60
  ) |>
  slice_min(rural_elec, n = 12, with_ties = FALSE) |>
  arrange(desc(rural_elec)) |>
  mutate(
    country_name = fct_inorder(country_name)
  )

## Panel B: largest clean-cooking gaps among highly electrified countries
panel_b_data <- energy_latest |>
  filter(
    !country_name %in% regional_aggregates,
    total_elec >= 85
  ) |>
  slice_max(gap_cooking, n = 12, with_ties = FALSE) |>
  arrange(gap_cooking) |>
  mutate(
    country_name = fct_inorder(country_name)
  )


## 5. VISUALIZATION ----
### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    accent    = "#722F37",
    light_dot = "#A8B4BC",
    dark_dot  = "#4A5568",
    bg        = "#FAFAF8",
    text      = "#2D2D2D",
    sub       = "#6B7280"
  )
)

### |- titles and caption ----
title_text <- "Getting Electricity Is Just the First Step"

subtitle_text <- str_glue(
    "Electricity access has expanded rapidly worldwide — but two gaps persist. ",
    "In many countries, rural households still lack electricity that cities take for granted.<br> ",
    "And even where electricity has arrived, **millions still cook with wood, charcoal, or dung** ",
    "instead of clean fuels — a major source of indoor air pollution and disease.<br>",
    "<span style='font-size:9pt;'>",
    "**Panel A:** Rural vs. urban electricity access &nbsp;|&nbsp; ",
    "**Panel B:** Electricity access vs. clean cooking access",
    "</span>"
)

caption_text <- create_social_caption(
    tt_year = 2026,
    tt_week = 21,
    source_text = "UN SE4ALL Database"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.background = element_rect(fill = clrs$palette$bg, color = NA),
    plot.background = element_rect(fill = clrs$palette$bg, color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_markdown(
      size   = 8,
      color  = clrs$palette$text,
      family = fonts$text
    ),
    axis.text.x = element_markdown(
      size = 7,
      color = clrs$palette$sub,
      family = fonts$text
    ),
    axis.title.x = element_markdown(
      size = 8,
      color = clrs$palette$sub,
      family = fonts$text,
      margin = margin(t = 4)
    ),
    axis.title.y = element_blank(),
    plot.title = element_markdown(
      size = 12,
      face = "bold",
      family = fonts$title,
      color = clrs$palette$text,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_markdown(
      size = 8.5,
      family = fonts$text,
      color = clrs$palette$sub,
      lineheight = 1.4,
      margin = margin(b = 10)
    ),
    plot.caption = element_markdown(
      size = 6.5,
      family = fonts$text,
      color = clrs$palette$sub,
      hjust = 0,
      margin = margin(t = 4)
    ),
    plot.caption.position = "plot",
    plot.margin = margin(12, 16, 8, 12)
  )
)

theme_set(weekly_theme)

### |- Panel A ----
p_a <- ggplot(panel_a_data) +
  # Geoms
  geom_segment(
    aes(
      x = rural_elec,
      xend = urban_elec,
      y = country_name,
      yend = country_name
    ),
    color = clrs$palette$accent,
    linewidth = 0.65,
    alpha = 0.65
  ) +
  geom_point(
    aes(x = rural_elec, y = country_name),
    color = clrs$palette$light_dot,
    size = 2.1
  ) +
  geom_point(
    aes(x = urban_elec, y = country_name),
    color = clrs$palette$dark_dot,
    size  = 2.8
  ) +
  # Annotate
  annotate("text",
    x = 2, y = 12.3,
    label = "Rural",
    hjust = 0, vjust = 0,
    size = 2.6, color = clrs$palette$light_dot,
    family = fonts$text, fontface = "bold"
  ) +
  annotate("text",
    x = 66, y = 12.3,
    label = "Urban",
    hjust = 1, vjust = 0,
    size = 2.6, color = clrs$palette$dark_dot,
    family = fonts$text, fontface = "bold"
  ) +
  annotate("text",
    x = 45,
    y = 10.8,
    label = "Cities electrified;\nvillages still wait",
    hjust = 0.5, vjust = 1,
    size = 2.4,
    color = clrs$palette$sub,
    family = fonts$text,
    fontface = "italic",
    lineheight = 1.2
  ) +
  # Scales
  scale_x_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 100),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = "**Rural Households Are Still Waiting for the Grid**",
    x = "Population with electricity access (%)",
    subtitle = "Lowest rural electricity access shown first"
  )

### |- Panel B ----
p_b <- ggplot(panel_b_data) +
  # Geoms
  geom_segment(
    aes(
      x = non_solid,
      xend = total_elec,
      y = country_name,
      yend = country_name
    ),
    color = clrs$palette$accent,
    linewidth = 0.65,
    alpha = 0.65
  ) +
  geom_point(
    aes(x = non_solid, y = country_name),
    color = clrs$palette$light_dot,
    size = 2.1
  ) +
  geom_point(
    aes(x = total_elec, y = country_name),
    color = clrs$palette$dark_dot,
    size  = 2.8
  ) +
  # Annotate
  annotate("text",
    x = 25, y = 12.3,
    label = "Cooks with\nclean fuel",
    hjust = 0, vjust = 0,
    size = 2.6, color = clrs$palette$light_dot,
    family = fonts$text, fontface = "bold"
  ) +
  annotate("text",
    x = 90, y = 12.3,
    label = "Has\nelectricity",
    hjust = 1, vjust = 0,
    size = 2.6, color = clrs$palette$dark_dot,
    family = fonts$text, fontface = "bold"
  ) +
  annotate("text",
    x = 60,
    y = 10.8,
    label = "Grid arrived;\nstove still burns wood",
    hjust = 0.5, vjust = 1,
    size = 2.4,
    color = clrs$palette$sub,
    family = fonts$text,
    fontface = "italic",
    lineheight = 1.2
  ) +
  # Scales
  scale_x_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 100),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = "**Having Electricity Doesn't Mean Cooking Cleanly**",
    x = "Population with access (%)",
    subtitle = "Countries shown have ≥85% electricity access"
  )

### |- Patchwork assembly ----
p_combined <- p_a + p_b +
  plot_layout(widths = c(0.95, 1.15)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = 26,
        face = "bold",
        family = fonts$title,
        color = clrs$palette$text,
        margin = margin(b = 6),
        lineheight = 1.2
      ),
      plot.subtitle = element_markdown(
        size = 10,
        family = fonts$text,
        color = clrs$palette$sub,
        lineheight = 1.5,
        margin = margin(b = 9)
      ),
      plot.caption = element_markdown(
        size = 6.5,
        family = fonts$text,
        color = clrs$palette$sub,
        hjust = 0,
        margin = margin(t = 10)
      ),
      plot.background = element_rect(fill = clrs$palette$bg, color = NA),
      plot.margin = margin(16, 16, 12, 16)
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

# ─ Session info ────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-23
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────
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
# ───────────────────────────────────────────────────────────────────