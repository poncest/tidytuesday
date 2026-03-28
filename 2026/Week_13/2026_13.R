## Challenge: #TidyTuesday 2026 week 13
## Data:      Coastal Ocean Temperature by Depth (Nova Scotia, Canada)
## Author:    Steven Ponce
## Date:      2026-03-28

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
    height = 12,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 13)
ocean_temperature <- tt$ocean_temperature |> clean_names()
ocean_temperature_deployments <- tt$ocean_temperature_deployments|> clean_names()
rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(ocean_temperature)
glimpse(ocean_temperature_deployments)


## 4. TIDY DATA ----

### |-  shared date components ----
ocean_temp_clean <- ocean_temperature |>
    filter(!is.na(mean_temperature_degree_c)) |>
    mutate(
        year = year(date),
        month = month(date),
        doy = yday(date),
        depth = factor(
            sensor_depth_at_low_tide_m,
            levels = c(2, 5, 10, 15, 20, 30, 40)
        )
    )

### |-  Panel 1: Heatmap data ----
heatmap_data <- ocean_temp_clean |>
    summarise(
        mean_temp = mean(mean_temperature_degree_c, na.rm = TRUE),
        .by = c(doy, sensor_depth_at_low_tide_m)
    ) |>
    mutate(
        depth_label = factor(
            paste0(sensor_depth_at_low_tide_m, "m"),
            levels = paste0(c(2, 5, 10, 15, 20, 30, 40), "m")
        )
    )

month_starts <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

temp_limits <- c(
    floor(min(heatmap_data$mean_temp, na.rm = TRUE)),
    ceiling(max(heatmap_data$mean_temp, na.rm = TRUE))
)

# Punchline metric
peak_gap <- ocean_temp_clean |>
    filter(month %in% 7:9, sensor_depth_at_low_tide_m %in% c(2, 40)) |>
    summarise(
        mean_t = mean(mean_temperature_degree_c, na.rm = TRUE),
        .by = sensor_depth_at_low_tide_m
    ) |>
    summarise(gap = round(diff(mean_t))) |>
    pull(gap) |>
    abs()

### |-  Panel 2: Thermocline depth over time ----
thermocline_data <- ocean_temp_clean |>
    select(date, doy, year, month, sensor_depth_at_low_tide_m, mean_temperature_degree_c) |>
    arrange(date, sensor_depth_at_low_tide_m) |>
    group_by(date) |>
    filter(n() >= 2) |>
    mutate(
        depth_lag   = lag(sensor_depth_at_low_tide_m),
        temp_lag    = lag(mean_temperature_degree_c),
        delta_depth = sensor_depth_at_low_tide_m - depth_lag,
        delta_temp  = mean_temperature_degree_c - temp_lag,
        gradient    = abs(delta_temp / delta_depth)
    ) |>
    filter(!is.na(gradient)) |>
    slice_max(gradient, n = 1, with_ties = FALSE) |>
    ungroup() |>
    filter(gradient >= 0.05)

thermocline_monthly <- thermocline_data |>
    summarise(
        median_depth = median(sensor_depth_at_low_tide_m, na.rm = TRUE),
        n_days = n(),
        .by = c(year, month)
    ) |>
    mutate(date_mid = ymd(paste(year, month, "15"))) |>
    filter(n_days >= 5)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = list(
        temp_low   = "#0d2b52",
        temp_mid   = "#2a9db5",
        temp_high  = "#f7d03c",
        thermo     = "#c9581a",
        thermo_pt  = "#e07b39",
        neutral    = "gray50",
        text_dark  = "#1a1a1a"
    )
)

### |- titles and caption ----
title_text <- str_glue("The Ocean Has a Memory")

subtitle_text <- str_glue(
    "Daily temperature profiles reveal persistent seasonal stratification and<br>",
    "depth-dependent thermal behavior — surface-to-deep differences reach<br>",
    "~{peak_gap}°C in late summer before the water column mixes flat in winter."
)

caption_text <- create_social_caption(
    tt_year     = 2026,
    tt_week     = 13,
    source_text = "Centre for Marine Applied Research · Nova Scotia Open Data Portal"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = fonts$text, size = 9, color = "gray40"),
        axis.title = element_text(family = fonts$text, size = 10, color = "gray30"),
        strip.text = element_text(family = fonts$text, size = 8, color = "gray30"),
        
        legend.position = "bottom", 
        legend.title = element_text(family = fonts$text, size = 8.5, color = "gray30"),
        legend.text = element_text(family = fonts$text, size = 7.5, color = "gray40"),
        legend.key.height = unit(0.35, "cm"),
        legend.key.width = unit(1.8, "cm"),
        
        plot.title = element_text(
            family = fonts$title, face = "bold", size = 20,
            color = colors$palette$text_dark, margin = margin(b = 6)
        ),
        plot.subtitle = element_markdown(
            family = fonts$text, size = 12, color = "gray35",
            lineheight = 1.4, margin = margin(b = 18)
        ),
        plot.caption = element_markdown(
            family = fonts$text, size = 7, color = "gray55",
            margin = margin(t = 12)
        ),
        plot.margin = margin(16, 20, 12, 20)
    )
)

theme_set(weekly_theme)

### |-  Panel 1: heatmap ----
p1 <- ggplot(
  heatmap_data,
  aes(x = doy, y = depth_label, fill = mean_temp)
  ) +
  # Geoms
  geom_tile(width = 1, height = 1) +
  scale_fill_gradientn(
    colors = c(
      colors$palette$temp_low,
      "#1a6e8a",
      colors$palette$temp_mid,
      "#d4a827",
      colors$palette$temp_high
    ),
    limits = temp_limits,
    oob = scales::squish,
    name = "Mean Temperature (°C)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust    = 0.5,
      barwidth       = 15,
      barheight      = 0.5
    )
  ) +
  scale_x_continuous(
    breaks = month_starts,
    labels = month_labels,
    expand = c(0, 0)
  ) +
  scale_y_discrete(limits = rev) +
  labs(
    x        = NULL,
    y        = "Sensor Depth",
    subtitle = "**Multi-year average** daily temperature by depth and day of year"
  ) +
  theme(
    plot.subtitle = element_markdown(
      family = fonts$text, size = 12, color = "gray40",
      margin = margin(b = 8)
    ),
    axis.title.y = element_text(margin = margin(r = 8)),
    legend.margin = margin(t = 4, b = 10)
  )

### |-  Panel 2: Thermocline depth ----

p2 <- ggplot(
  thermocline_monthly,
  aes(x = date_mid, y = median_depth)
) +
  geom_hline(
    yintercept = c(2, 5, 10, 15, 20, 30, 40),
    color      = "gray88",
    linewidth  = 0.3,
    linetype   = "dashed"
  ) +
  geom_point(
    aes(size = n_days),
    color = colors$palette$thermo_pt,
    alpha = 0.10,
    shape = 16
  ) +
  geom_smooth(
    method    = "loess",
    span      = 0.2,
    se        = FALSE,
    color     = colors$palette$thermo,
    linewidth = 1.5
  ) +
  annotate(
    "text",
    x          = as.Date("2024-01-01"), 
    y          = 9.5,
    label      = "Thermocline shoals to ~5–10 m\nin late summer as surface waters warm",
    size       = 2.8,
    color      = colors$palette$thermo,
    hjust      = 0,
    lineheight = 1.25
  ) +
  scale_y_reverse(
    breaks = c(2, 5, 10, 15, 20, 30, 40),
    labels = paste0(c(2, 5, 10, 15, 20, 30, 40), " m"),
    limits = c(44, 0),
    sec.axis = dup_axis(
      labels = paste0(c(2, 5, 10, 15, 20, 30, 40), " m"),
      name   = NULL
    )
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand      = c(0.02, 0)
  ) +
  scale_size_continuous(range = c(1, 3.5), guide = "none") +
  labs(
    x        = NULL,
    y        = "Thermocline Depth\n(metres, increasing downward)",
    subtitle = "**Thermocline position** — depth of steepest daily temperature gradient (≥ 0.05 °C/m)"
  ) +
  theme(
    plot.subtitle = element_markdown(
      family = fonts$text, size = 14, color = "gray40",
      margin = margin(b = 8)
    ),
    axis.title.y = element_text(margin = margin(r = 8), lineheight = 1.2),
    axis.text.y.left = element_blank(),
    axis.title.y.left = element_blank(),
    axis.text.y.right = element_text(
      family = fonts$text, size = 8, color = "gray40",
      margin = margin(l = 6)
    )
  )

### |-  Combined plot ----
combined_plot <- p1 / p2 +
    plot_layout(heights = c(1.4, 1)) &                   
    theme(plot.margin = margin(10, 20, 10, 20))

combined_plot <- combined_plot +
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_text(
                family = fonts$title, face = "bold", size = 28,
                color = colors$palette$text_dark, margin = margin(b = 6)
            ),
            plot.subtitle = element_markdown(
                family = fonts$subtitle, size = 12, color = "gray35",
                lineheight = 1.4, margin = margin(b = 20)
            ),
            plot.caption = element_markdown(
                family = fonts$caption, size = 9, color = "gray55",
                margin = margin(t = 14),
                lineheight = 1.3,
            ),
            plot.margin = margin(20, 24, 14, 24)
        )
    )

combined_plot


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
# date     2026-03-26
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpYTCNgc/file547016805d78". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.3.1)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.3.1)
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
# lattice        0.21-8   2023-04-05 [2] CRAN (R 4.3.1)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# Matrix         1.5-4.1  2023-05-18 [2] CRAN (R 4.3.1)
# P methods      * 4.3.1    2023-06-16 [2] local
# mgcv           1.8-42   2023-03-02 [2] CRAN (R 4.3.1)
# nlme           3.1-162  2023-01-31 [2] CRAN (R 4.3.1)
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.3.1)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# P parallel       4.3.1    2023-06-16 [2] local
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.3.1)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# rappdirs       0.3.4    2026-01-17 [1] CRAN (R 4.3.1)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.3.3)
# rlang          1.1.7    2026-01-09 [1] CRAN (R 4.3.1)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.3.1)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.3.1)
# rsvg           2.6.2    2025-03-23 [1] CRAN (R 4.3.3)
# S7             0.2.0    2024-11-07 [1] CRAN (R 4.3.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.3.1)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# skimr        * 2.2.2    2026-01-10 [1] CRAN (R 4.3.1)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.3)
# P splines        4.3.1    2023-06-16 [2] local
# P stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.3.3)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.3.1)
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
# 
# ───────────────────────────────────────────────────────────────────────────────────────────────────────────