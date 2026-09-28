
## Challenge: #TidyTuesday 2026 week 39
## Data:      Health metrics in urban centres worldwide
## Author:    Steven Ponce
## Date:      2026-09-28

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, ggview, patchwork
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure settings ----
fig_w      <- 12
fig_h      <- 8
out_file   <- "2026/Week_39/2026_39.png"

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 39)
health_raw <- tt$health |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(health_raw)
skim_without_charts(health_raw)


## 4. TIDY DATA ----

### |- constants ----
inc_levels <- c("Low income", "Lower Middle", "Upper Middle", "High income")
inc_labels <- c(
  "Low income", "Lower-middle income", "Upper-middle income", "High income"
)

bin_width <- 0.2
min_n_bin <- 20
support_q <- c(0.05, 0.95)
x_window <- log10(c(1000, 30000))

### |- city table ----
cities <- health_raw |>
  filter(!is.na(gc_dev_wig_2025)) |>
  transmute(
    city         = gc_ucn_mai_2025,
    income       = factor(gc_dev_wig_2025, levels = inc_levels, labels = inc_labels),
    log_density  = log10(gc_pop_tot_2025 / gc_uca_km2_2025),
    access_share = hl_shp_hos_2025,
    reporting    = !is.na(access_share)
  )

reporting_cities <- cities |> filter(reporting)

### |- binned medians ----
bin_breaks <- seq(
  floor(min(cities$log_density) * 10) / 10,
  ceiling(max(cities$log_density) * 10) / 10 + bin_width,
  by = bin_width
)

bin_mid <- head(bin_breaks, -1) + bin_width / 2

bin_medians <- function(data, by = character()) {
  data |>
    mutate(bin = cut(log_density, bin_breaks, include.lowest = TRUE, labels = FALSE)) |>
    summarise(
      median_share = median(access_share),
      n = n(),
      .by = all_of(c(by, "bin"))
    ) |>
    filter(n >= min_n_bin) |>
    mutate(log_density = bin_mid[bin]) |>
    arrange(log_density)
}

support <- reporting_cities |>
  summarise(
    lo = quantile(log_density, support_q[1]),
    hi = quantile(log_density, support_q[2]),
    .by = income
  )

binned <- reporting_cities |>
  inner_join(support, by = "income") |>
  filter(between(log_density, lo, hi)) |>
  bin_medians(by = "income") |>
  arrange(income, log_density)

# All-cities reference
pooled <- bin_medians(reporting_cities)

### |- typical-city markers ----
label_cols <- c(
  "Low income"          = "#722F37",
  "Lower-middle income" = "gray35",
  "Upper-middle income" = "gray25",
  "High income"         = "#2F5D62"
)

centroids <- reporting_cities |>
  summarise(
    med_density = median(log_density),
    med_share = median(access_share),
    n = n(),
    .by = income
  ) |>
  arrange(income) |>
  mutate(
    label = if_else(
      income == "Low income",
      as.character(glue("Typical city<br>**{round(med_share)}%** within 1 km")),
      as.character(glue("**{round(med_share)}%**"))
    ),
    lab_col = unname(label_cols[as.character(income)]),
    is_low = income == "Low income",
    lab_x = med_density + if_else(is_low, -0.04, 0.05),
    lab_y = med_share + if_else(is_low, 3, -3),
    lab_hjust = if_else(is_low, 1, 0),
    lab_vjust = if_else(is_low, 0, 1),
    lab_size = if_else(is_low, 3.3, 3.8)
  )

### |- backdrop: own-group cities only ----
backdrop_own <- reporting_cities

### |- observation-boundary labels ----
strip_labels <- tibble(
  income = factor("Low income", levels = inc_labels),
  x      = log10(c(1150, 28000)),
  y      = c(0.6, 0.75),
  hjust  = c(0, 1),
  label  = c("Hospital\ndata", "No\nhospital\ndata")
)


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
    palette = list(
        col_low    = "#722F37",
        col_lowmid = "gray62",
        col_upmid  = "gray42",
        col_high   = "#2F5D62"
    )
)

# Hardcoded named vector for scales
clr_groups <- c(
    "Low income"          = "#722F37",
    "Lower-middle income" = "gray62",
    "Upper-middle income" = "gray42",
    "High income"         = "#2F5D62"
)

x_break_vals <- c(2000, 5000, 10000, 20000)
x_break_labs <- c("2K", "5K", "10K", "20K")

### |- titles and caption ----
title_text <- str_glue(
    "Income looks like the divide. City density explains most of the gap."
)

subtitle_text <- str_glue(
    "Cities in high-income countries have about twice as many hospitals per ",
    "person as those in low-income countries, yet a smaller share of their ",
    "residents lives within 1 km of one. Compare cities of similar density, and ",
    "most of that gap narrows to a few percentage points.<br><br>",
    "Each large dot is a group's typical city. The dashed line shows all ",
    "cities with hospital data."
)

caption_text <- create_social_caption(
    tt_year = 2026,
    tt_week = 39,
    source_text = paste0(
        "GHS Urban Centre Database R2024A, European Commission JRC<br>",
        "Note: Among 11,413 urban centres with a World Bank income classification, ",
        "6,427 have hospital-access data.<br>",
        "Lines are binned medians within each group's central 90% of densities."
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
        plot.title.position = "plot",
        plot.title = element_text(
            face = "bold", family = fonts$title_1, size = 24,
            margin = margin(b = 6), color = clrs$title
        ),
        plot.subtitle = element_textbox_simple(
            family = fonts$text, size = 10.5, lineheight = 1.2,
            margin = margin(b = 14), color = clrs$subtitle
        ),
        plot.caption = element_markdown(
            family = fonts$caption, size = 8.5, hjust = 0.5,
            margin = margin(t = 12), color = clrs$caption
        ),
        strip.text = element_text(
            face = "bold", family = fonts$title_1, size = 10.5, hjust = 0
        ),
        axis.title = element_text(family = fonts$text, size = 10),
        axis.text = element_text(family = fonts$text, size = 9),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.spacing.x = unit(1.4, "lines"),
        legend.position = "none"
    )
)

theme_set(weekly_theme)

### |- top strips: density by income, reporting vs not ----
p_margin <- ggplot(cities, aes(log_density)) +
    geom_density(
        data = \(d) filter(d, reporting),
        aes(y = after_stat(scaled), fill = income),
        colour = NA, alpha = 0.45
    ) +
    geom_density(
        data = \(d) filter(d, !reporting),
        aes(y = after_stat(scaled), colour = income),
        fill = NA, linetype = "22", linewidth = 0.4
    ) +
    geom_text(
        data = strip_labels,
        aes(x, y, label = label, hjust = hjust),
        inherit.aes = FALSE, colour = "#722F37",
        size = 3, lineheight = 0.95, family = fonts$text
    ) +
    facet_wrap(~income, nrow = 1) +
    scale_fill_manual(values = clr_groups) +
    scale_colour_manual(values = clr_groups) +
    coord_cartesian(xlim = x_window, ylim = c(0, 1.05), expand = FALSE) +
    labs(title = title_text, subtitle = subtitle_text) +
    theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_blank()
    )

### |- main panels ----
p_main <- ggplot() +
    geom_point(
        data = backdrop_own, aes(log_density, access_share, colour = income),
        size = 0.45, alpha = 0.08
    ) +
    geom_line(
        data = pooled, aes(log_density, median_share),
        linetype = "22", colour = "gray25", linewidth = 0.45
    ) +
    geom_line(
        data = binned, aes(log_density, median_share, colour = income),
        linewidth = 1
    ) +
    geom_point(
        data = binned, aes(log_density, median_share, colour = income),
        size = 1.6
    ) +
    geom_point(
        data = centroids, aes(med_density, med_share, fill = income),
        shape = 21, size = 5.2, colour = "white", stroke = 1.2
    ) +
    geom_richtext(
        data = centroids,
        aes(lab_x, lab_y, label = label,
            hjust = lab_hjust, vjust = lab_vjust, size = lab_size),
        colour = centroids$lab_col,
        lineheight = 1.1, family = fonts$text,
        fill = NA, label.colour = NA
    ) +
    scale_size_identity() +
    facet_wrap(~income, nrow = 1) +
    scale_colour_manual(values = clr_groups) +
    scale_fill_manual(values = clr_groups) +
    scale_x_continuous(breaks = log10(x_break_vals), labels = x_break_labs) +
    scale_y_continuous(breaks = seq(0, 100, 25), labels = label_percent(scale = 1)) +
    coord_cartesian(xlim = x_window, ylim = c(0, 100), expand = FALSE) +
    labs(
        x = "Population density (people per km², log scale)",
        y = "Residents within 1 km of a hospital",
        caption = caption_text
    )

### |- combine ----
p <- p_margin / p_main +
    plot_layout(heights = c(1, 4))

### |-  preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in", dpi = 320)


## 6. SAVE ----
ggview::save_ggplot(
    plot   = p,
    file   = out_file,
    width  = fig_w,
    height = fig_h,
    units  = "in",
    dpi    = 320
)

# 7. HELPER FUNCTIONS DOCUMENTATION ----

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


# 8. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-09-28
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────
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
# ggtext       * 0.2.0   2026-08-28 [1] CRAN (R 4.6.1)
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
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# otel           0.2.0   2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# P parallel       4.6.1   2026-06-25 [1] local
# patchwork    * 1.3.2   2025-08-25 [1] CRAN (R 4.6.0)
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
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
# ───────────────────────────────────────────────────────────────────────────────────────
# > 