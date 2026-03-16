## Challenge: #TidyTuesday 2026 week 11
## Data:      Salmonid Mortality Data
## Author:    Steven Ponce
## Date:      2026-03-16

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor,      
    scales, glue, skimr, patchwork, lubridate     
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 12,
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
tt <- tidytuesdayR::tt_load(2026, week = 11)
monthly_losses_data <- tt$monthly_losses_data |> clean_names()
monthly_mortality_data <- tt$monthly_mortality_data |> clean_names()
rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(monthly_losses_data)
glimpse(monthly_mortality_data)

skim_without_charts(monthly_losses_data)
skim_without_charts(monthly_mortality_data)


## 4. TIDY DATA ----
### |- species label lookup ----
species_labels <- c(
  "salmon"       = "Atlantic Salmon",
  "rainbowtrout" = "Rainbow Trout"
)

### |- Panel A: national trend (Atlantic Salmon only) ----
panel_a_data <- monthly_mortality_data |>
  filter(
    geo_group == "country",
    species == "salmon"
  ) |>
  arrange(date)

baseline_2020 <- panel_a_data |>
  filter(year(date) == 2020) |>
  summarise(baseline = mean(median, na.rm = TRUE)) |>
  pull(baseline)

### |- Panel B: county-level regional comparison (Atlantic Salmon) ----
panel_b_data <- monthly_mortality_data |>
  filter(
    geo_group == "county",
    species == "salmon"
  ) |>
  group_by(region) |>
  summarise(
    avg_median = mean(median, na.rm = TRUE),
    avg_q1     = mean(q1, na.rm = TRUE),
    avg_q3     = mean(q3, na.rm = TRUE),
    .groups    = "drop"
  )

# Tolerance band around national average to handle near-average regions
national_avg_b <- mean(panel_b_data$avg_median, na.rm = TRUE)
tol <- 0.015

panel_b_data <- panel_b_data |>
  mutate(
    rank_high = min_rank(desc(avg_median)),
    rank_low = min_rank(avg_median),
    region_group = case_when(
      abs(avg_median - national_avg_b) <= tol ~ "Near average",
      avg_median > national_avg_b & rank_high <= 3 ~ "Highest",
      avg_median < national_avg_b & rank_low <= 3 ~ "Lowest",
      TRUE ~ "Middle"
    ),
    region = fct_reorder(region, avg_median)
  )

### |- Panel C: national loss composition (country level, both species) ----
panel_c_data <- monthly_losses_data |>
  filter(
    geo_group == "country",
    losses > 0
  ) |>
  mutate(
    species_label = factor(
      species_labels[species],
      levels = c("Atlantic Salmon", "Rainbow Trout")
    ),
    pct_dead = dead / losses,
    pct_discarded = discarded / losses,
    pct_escaped = escaped / losses,
    pct_other = other / losses
  ) |>
  select(
    date, species_label,
    pct_dead, pct_discarded, pct_escaped, pct_other
  ) |>
  pivot_longer(
    cols      = starts_with("pct_"),
    names_to  = "loss_type",
    values_to = "proportion"
  ) |>
  mutate(
    loss_type = recode(
      loss_type,
      "pct_dead"      = "Dead",
      "pct_discarded" = "Discarded",
      "pct_escaped"   = "Escaped",
      "pct_other"     = "Other"
    ),
    # Keep minority categories at the bottom for visibility
    loss_type = factor(
      loss_type,
      levels = c("Other", "Escaped", "Discarded", "Dead")
    )
  )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = list(
        col_salmon    = "#7A1E3A",  
        col_discarded = "#B86B4B",  
        col_escaped   = "#7C93B6",  
        col_other     = "#A7B0AB",   
        col_high      = "#7A1E3A",   
        col_low       = "#6F7D8C",   
        col_mid       = "#C9CDD3",  
        col_ribbon    = "#C9A3AE",   
        col_ref       = "#6B6B6B",   
        col_dark      = "#1F2430",   
        col_gray      = "gray50"
    )
)

### |- titles and caption ----
title_text <- "Norway's Farmed Salmon Mortality Shows Little Improvement Since 2020"

subtitle_text <- paste(
    "National trend, regional gaps, and loss composition",
    "in Atlantic salmon farming, 2020–2025"
)

caption_text <- create_social_caption(
    tt_year     = 2026,
    tt_week     = 11,
    source_text = "Norwegian Veterinary Institute · Laksetap Shiny App & API"
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
pa <- panel_a_data |>
  ggplot(aes(x = date)) +
  # Geoms
  geom_ribbon(
    aes(ymin = q1, ymax = q3),
    fill = colors$palette$col_ribbon,
    alpha = 0.28
  ) +
  geom_line(
    aes(y = median),
    color = colors$palette$col_salmon,
    linewidth = 1.0
  ) +
  geom_hline(
    yintercept = baseline_2020 * 1.06,
    linetype   = "dashed",
    color      = colors$palette$col_ref,
    linewidth  = 0.5,
    alpha      = 0.75
  ) +
  # Annotate
  annotate(
    "text",
    x      = as.Date("2020-03-30"),
    y      = baseline_2020 * 1.35,
    label  = "2020 baseline",
    size   = 2.8,
    color  = colors$palette$col_ref,
    hjust  = 0,
    family = fonts$text
  ) +
  # Scales
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand      = c(0.01, 0)
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%", accuracy = 0.1),
    expand = expansion(mult = c(0.03, 0.12))
  ) +
  # Labs
  labs(
    title    = "A. Has Mortality Improved?",
    subtitle = "National median monthly mortality with IQR · Atlantic salmon",
    x        = NULL,
    y        = "Median monthly\nmortality rate (%)"
  ) +
  # Theme
  theme(
    axis.title.y = element_text(
      size       = rel(0.78),
      lineheight = 1.2,
      angle      = 90,
      margin     = margin(r = 6)
    )
  )

### |- Panel B: Regional Gaps ----
pb <- panel_b_data |>
  ggplot(aes(y = region)) +
  # Geoms
  geom_vline(
    xintercept = national_avg_b,
    linetype   = "dashed",
    color      = colors$palette$col_ref,
    linewidth  = 0.5,
    alpha      = 0.8
  ) +
  geom_segment(
    aes(
      x    = avg_q1,
      xend = avg_q3,
      yend = region
    ),
    color = "gray80",
    linewidth = 0.5,
    alpha = 0.9
  ) +
  geom_segment(
    aes(
      x     = national_avg_b,
      xend  = avg_median,
      yend  = region,
      color = region_group
    ),
    linewidth = 0.9,
    alpha = 0.95
  ) +
  geom_point(
    aes(x = avg_median, color = region_group),
    size = 2.8
  ) +
  # Annotate
  annotate(
    "text",
    x          = national_avg_b + 0.02,
    y          = 6.5,
    label      = "National\naverage",
    size       = 2.5,
    hjust      = 0,
    lineheight = 1.05,
    color      = colors$palette$col_ref,
    family     = fonts$text
  ) +
  # Scales
  scale_color_manual(
    values = c(
      "Highest"      = colors$palette$col_high,
      "Lowest"       = colors$palette$col_low,
      "Near average" = "#8A8F99",
      "Middle"       = colors$palette$col_mid
    )
  ) +
  scale_x_continuous(
    limits = c(0.25, 1.0),
    labels = label_number(suffix = "%", accuracy = 0.1),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  # Labs
  labs(
    title    = "B. How Wide Are Regional Gaps?",
    subtitle = "Average median mortality by county · Atlantic salmon",
    x        = "Average median mortality (%)",
    y        = NULL
  ) +
  # Theme
  theme(
    axis.text.y = element_text(size = rel(0.75)),
    panel.grid.major.x = element_line(color = "gray93", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
  )

### |- Panel C: Loss Composition ----
pc <- panel_c_data |>
  ggplot(aes(x = date, y = proportion, fill = loss_type)) +
  # Geoms
  geom_area(
    position = "fill",
    alpha    = 0.88
  ) +
  geom_hline(
    yintercept = 0.75,
    linetype   = "dotted",
    color      = "white",
    linewidth  = 0.45,
    alpha      = 0.55
  ) +
  # Scales
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    expand = c(0, 0)
  ) +
  scale_x_date(
    breaks = as.Date(c("2020-01-01", "2023-01-01", "2025-01-01")),
    labels = c("2020", "2023", "2025"),
    expand = c(0.01, 0)
  ) +
  scale_fill_manual(
    values = c(
      "Dead"      = colors$palette$col_salmon,
      "Discarded" = colors$palette$col_discarded,
      "Escaped"   = colors$palette$col_escaped,
      "Other"     = colors$palette$col_other
    ),
    breaks = c("Dead", "Discarded", "Escaped", "Other")
  ) +
  # Faceta
  facet_wrap(~species_label, ncol = 2) +
  # Labs
  labs(
    title    = "C. Death Dominates — But Not Entirely",
    subtitle = "Monthly composition of total losses · Country level",
    x        = NULL,
    y        = "Share of\nmonthly losses",
    fill     = "Loss type"
  ) +
  # Theme
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.key.size = unit(0.55, "lines"),
    legend.text = element_text(size = rel(0.75), family = fonts$text),
    legend.title = element_text(
      size   = rel(0.78),
      family = fonts$text,
      face   = "bold"
    ),
    legend.margin = margin(b = 2),
    axis.title.y = element_text(
      size       = rel(0.78),
      lineheight = 1.2,
      angle      = 90,
      margin     = margin(r = 6)
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray93", linewidth = 0.3),
    plot.tag = element_text(
      face   = "bold",
      size   = rel(1.05),
      family = fonts$title,
      color  = colors$palette$col_dark
    )
  )

### |- Combined Plots ----
layout <- "
AAAA
BBCC
"

p <- pa + pb + pc +
  plot_layout(
    design  = layout,
    heights = c(1.45, 1)
  ) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        face       = "bold",
        size       = rel(1.22),
        family     = fonts$title,
        color      = colors$palette$col_dark,
        margin     = margin(b = 4),
        lineheight = 1.15
      ),
      plot.subtitle = element_text(
        size       = rel(0.88),
        family     = fonts$text,
        color      = "gray40",
        lineheight = 1.35,
        margin     = margin(b = 12)
      ),
      plot.caption = element_markdown(
        size   = rel(0.68),
        family = fonts$text,
        color  = "gray50",
        hjust  = 0,
        margin = margin(t = 10)
      ),
      plot.margin = margin(t = 20, r = 20, b = 12, l = 20)
    )
  )

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
