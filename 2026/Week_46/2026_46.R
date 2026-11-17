## Challenge: #TidyTuesday 2026 week 46
## Data:      Cholesterol and Mortality in US Adults
## Author:    Steven Ponce
## Date:      2026-11-16

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, scales, glue, ggview,
    ragg, magick, here
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure settings ----
fig_w    <- 12
fig_h    <- 7.5
fig_dpi  <- 320
out_file <- "2026/Week_46/2026_46.png"

### |- fonts ----
setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1
title_size   <- 28


## 2. READ IN THE DATA ----
## Read every field as character with no NA tokens; restore numbers column by column.
read_chr <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE)
}

tt_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-17/"
cm_raw <- read_chr(paste0(tt_url, "cholesterol_mortality.csv"))
est_raw <- read_chr(here::here("2026/Week_46/tt_2026_46_estimates.csv"))

#' - `tt_2026_46_estimates.csv`: the hazard ratios shown in the tiles. 
#' It is written by [`tt_2026_46_decision_record.R`](https://github.com/poncest/tidytuesday/tree/main/2026/Week_46), 
#' which fits survey-weighted Cox models (`survey::svycoxph()`) stratified by NHANES cycle, using CDC 20-year exam weights 
#' rebuilt from the original NHANES DEMO and DEMO_B files for 1999–2002. The chart script reads every estimate from this file; 
#' none is typed by hand.

## 3. EXAMINING THE DATA ----
glimpse(est_raw)
est_raw |> count(kind, row_key) |> print(n = Inf)


## 4. TIDY DATA ----

### |- analysis population ----
cm <- cm_raw |>
  mutate(across(c(total_chol, hdl, died), \(x) suppressWarnings(as.numeric(na_if(x, "NA")))))
an <- cm |> filter(!is.na(total_chol), !is.na(hdl), hdl <= 140)
n_adults <- nrow(an)
n_deaths <- sum(an$died)

### |- estimates ----
bins_raw <- c("<160", "160-179", "180-199", "200-219", "220-239", "240+")
bins_show <- c("Below 160", "160–179", "180–199", "200–219", "220–239", "240+")

est <- est_raw |>
  mutate(
    across(c(value, lo, hi, deaths), \(x) as.numeric(na_if(x, ""))),
    col = match(bin, bins_raw)
  )

hr <- est |> filter(kind == "hr_weighted")

### |- matrix layout: rows top to bottom = the reading order ----
## groups (1), (2), (3) are separated by wider gaps than rows within a group, so the
## matrix reads as three steps, not one heatmap
row_y <- c(crude_all = 6.3, adj_all = 4.95, adj_18_49 = 3.3, adj_50_69 = 2.3, adj_70p = 1.3)

cells <- hr |>
  mutate(
    y = unname(row_y[row_key]),
    status = case_when(
      bin == "180-199" ~ "ref",
      lo > 1 ~ "higher",
      hi < 1 ~ "lower",
      TRUE ~ "unclear"
    ),
    lab_hr = if_else(status == "ref", "1.00", sprintf("%.2f", value)),
    lab_ci = case_when(
      status == "ref" & row_key == "crude_all" ~ "comparison",
      status == "ref" ~ "",
      TRUE ~ sprintf("%.2f–%.2f", lo, hi)
    ),
    y_hr = if_else(lab_ci == "", y, y + 0.13)
  )

### |- numbers quoted in copy (all from the export) ----
pick <- function(key, b, k = "hr_weighted") est |> filter(kind == k, row_key == key, bin == b)
crude_240 <- pick("crude_all", "240+")
adj_240 <- pick("adj_all", "240+")
young_240 <- pick("adj_18_49", "240+")
young_unw <- pick("adj_18_49", "240+", "hr_unweighted")
mean_age <- est |>
  filter(kind == "mean_age_weighted") |>
  arrange(col)

pct_crude <- round((crude_240$value - 1) * 100)
dip <- pick("adj_50_69", "200-219")
pct_dip <- round((1 - dip$value) * 100)
age_240 <- mean_age$value[mean_age$bin == "240+"]
age_ref <- mean_age$value[mean_age$bin == "180-199"]


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    higher  = "#9A4A2E",
    lower   = "#4D6478",
    unclear = "#E6DFD3",
    ref     = "#FFFFFF",
    ink     = "#2C2825",
    muted   = "#7A7068",
    outline = "#7A7068"
  )
)
col_higher <- clrs$palette$higher
col_lower <- clrs$palette$lower
col_unclear <- clrs$palette$unclear
col_ref <- clrs$palette$ref
col_ink <- clrs$palette$ink
col_muted <- clrs$palette$muted
col_outline <- clrs$palette$outline
col_bg <- clrs$background

### |- titles and caption ----
title_text <- "Cholesterol's link to death looks different at different ages"

subtitle_text <- glue(
  "Each tile compares the death rate of US adults in one cholesterol group with adults at 180–199 mg/dL (1.00).<br>",
  "{sprintf('%.2f', crude_240$value)} means a {pct_crude}% higher death rate; ",
  "{sprintf('%.2f', dip$value)} means {pct_dip}% lower."
)

cue_crude <- glue(
  "Average age in this group: {round(age_240)},\nagainst {round(age_ref)} in the comparison group"
)
note_adj <- "Taking age and sex into account,\nthe highest group no longer stands out"
note_young <- glue(
  "Based on {young_240$deaths} deaths. Without survey\n",
  "weights, the interval includes 1.00:\n",
  "{sprintf('%.2f', young_unw$value)} ({sprintf('%.2f', young_unw$lo)}–{sprintf('%.2f', young_unw$hi)})"
)
note_low <- "Low cholesterol is associated with higher mortality here;\ntreatment and underlying illness are not measured."

## three caption lines
caption_lines <- c(
  "Hazard ratios with 95% intervals, survey-weighted Cox models (CDC 20-year exam weights) stratified by NHANES cycle; rows 2–5 adjusted for age and sex.",
  glue(
    "{comma(n_adults)} adults, {comma(n_deaths)} deaths, follow-up through 2019; HDL over 140 mg/dL excluded. ",
    "Colour marks intervals entirely above or below 1.00."
  ),
  "Observational: statin use, smoking and illness at the exam are not measured; the pattern does not show what changing cholesterol would do."
)

caption_text <- paste0(
  caption_lines[1], "<br>", caption_lines[2], "<br>", caption_lines[3], "<br>",
  create_social_caption(
    tt_year = 2026,
    tt_week = 46,
    source_text = "NHANES 1999–2018 and NCHS Linked Mortality Files via TidyTuesday"
  )
)

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_textbox_simple(
      face = "bold", family = title_family, size = title_size,
      color = col_ink, lineheight = 1.0, margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$subtitle, size = 11, color = col_ink,
      lineheight = 1.3, margin = margin(b = 6)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$caption, size = 6.6, color = col_muted,
      lineheight = 1.35, margin = margin(t = 8)
    ),
    legend.position = "top",
    legend.justification = "left",
    legend.location = "plot",
    legend.title = element_blank(),
    legend.text = element_text(family = fonts$text, size = 8.5, color = col_muted),
    legend.key.size = unit(0.42, "cm"),
    legend.margin = margin(t = 2, b = 0),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(14, 20, 8, 20)
  )
)

theme_set(weekly_theme)

### |- matrix ----
tile_w <- 0.94
tile_h <- 0.86
x_lab  <- 0.42        
x_note <- 6.62       

row_labels <- tibble(
    y     = unname(row_y),
    main  = c("(1)  All adults", "(2)  All adults", "Ages 18–49", "Ages 50–69", "Ages 70+"),
    sub   = c("before taking age and sex into account", "taking age and sex into account", "", "", "")
)

### |- main plot ----
p <- ggplot(cells) +
  # Geoms
  geom_tile(aes(x = col, y = y, fill = status), width = tile_w, height = tile_h, color = NA) +
  geom_tile(
    data = filter(cells, status %in% c("unclear", "ref")),
    aes(x = col, y = y), width = tile_w, height = tile_h,
    fill = NA, color = col_outline, linewidth = 0.3
  ) +
  geom_text(
    aes(
      x = col, y = y_hr, label = lab_hr,
      color = if_else(status %in% c("higher", "lower"), "on_dark", "on_light")
    ),
    family = fonts$text, fontface = "bold", size = 4.1
  ) +
  geom_text(
    aes(
      x = col, y = y - 0.19, label = lab_ci,
      color = if_else(status %in% c("higher", "lower"), "on_dark", "on_light")
    ),
    family = fonts$caption, size = 2.5
  ) +
  # column headers
  annotate("text",
    x = 1:6, y = 7.4, label = bins_show,
    family = fonts$text, fontface = "bold", size = 3.3, color = col_ink
  ) +
  annotate("text",
    x = 0.53, y = 7.85, hjust = 0, label = "Total cholesterol, mg/dL",
    family = fonts$text, size = 3.1, color = col_muted
  ) +
  # row labels
  geom_text(
    data = row_labels, aes(x = x_lab, y = y + if_else(sub == "", 0, 0.12), label = main),
    hjust = 1, family = fonts$text, fontface = "bold", size = 3.3, color = col_ink
  ) +
  geom_text(
    data = filter(row_labels, sub != ""), aes(x = x_lab, y = y - 0.2, label = sub),
    hjust = 1, family = fonts$text, size = 2.9, color = col_muted
  ) +
  annotate("text",
    x = x_lab, y = 4.18, hjust = 1, label = "(3)  By age group",
    family = fonts$text, fontface = "bold", size = 3.3, color = col_ink
  ) +
  annotate("text",
    x = x_lab, y = 3.85, hjust = 1, label = "taking age and sex into account",
    family = fonts$text, size = 2.9, color = col_muted
  ) +
  # context row: mean age (subordinate)
  annotate("text",
    x = 1:6, y = 6.95, label = round(mean_age$value),
    family = fonts$caption, size = 3, color = col_muted
  ) +
  annotate("text",
    x = x_lab, y = 6.95, hjust = 1, label = "Average age",
    family = fonts$text, size = 2.9, color = col_muted
  ) +
  # notes at the mark: one per step on the right, the low-end caveat below
  annotate("text",
    x = x_note, y = row_y[["crude_all"]], hjust = 0, label = cue_crude,
    family = fonts$text, size = 2.8, color = col_ink, lineheight = 1.15
  ) +
  annotate("text",
    x = x_note, y = row_y[["adj_all"]], hjust = 0, label = note_adj,
    family = fonts$text, size = 2.8, color = col_ink, lineheight = 1.15
  ) +
  annotate("text",
    x = x_note, y = row_y[["adj_18_49"]], hjust = 0, label = note_young,
    family = fonts$text, size = 2.8, color = col_ink, lineheight = 1.15
  ) +
  annotate("segment",
    x = 1 - tile_w / 2, xend = 1 + tile_w / 2, y = 0.73, yend = 0.73,
    color = col_ink, linewidth = 0.4
  ) +
  annotate("text",
    x = 1 - tile_w / 2, y = 0.61, hjust = 0, vjust = 1, label = note_low,
    family = fonts$text, size = 2.8, color = col_ink, lineheight = 1.15
  ) +
  # Scales
  scale_fill_manual(
    values = c(higher = col_higher, lower = col_lower, unclear = col_unclear, ref = col_ref),
    breaks = c("higher", "lower", "unclear"),
    labels = c("Higher death rate", "Lower death rate", "No clear difference")
  ) +
  scale_color_manual(values = c(on_dark = "#FFFFFF", on_light = col_ink), guide = "none") +
  guides(fill = guide_legend(override.aes = list(color = col_outline, linewidth = 0.3))) +
  scale_x_continuous(limits = c(-2.6, 9.6), expand = expansion(0)) +
  scale_y_continuous(limits = c(-0.15, 8.05), expand = expansion(0)) +
  # Labs
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)

### |-  preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in", dpi = fig_dpi)


## 6. SAVE ----
save_ggplot(plot = p, file = out_file, width = fig_w, height = fig_h,
            units = "in", dpi = fig_dpi)


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

# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.7.3
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-11-16
# rstudio  2026.11.0+203 Montauk Daisy (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ───────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# bit            4.6.0   2025-03-06 [1] CRAN (R 4.6.0)
# bit64          4.8.2   2026-05-19 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# crayon         1.5.3   2024-06-20 [1] CRAN (R 4.6.0)
# curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1   2026-06-25 [1] local
# dplyr        * 1.2.1   2026-04-03 [1] CRAN (R 4.6.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
# forcats      * 1.0.1   2025-09-25 [1] CRAN (R 4.6.0)
# generics       0.1.4   2025-05-09 [1] CRAN (R 4.6.0)
# ggplot2      * 4.0.3   2026-04-22 [1] CRAN (R 4.6.0)
# ggtext       * 0.2.0   2026-08-28 [1] CRAN (R 4.6.1)
# ggview       * 0.2.2   2025-07-05 [1] CRAN (R 4.6.0)
# glue         * 1.8.1   2026-04-17 [1] CRAN (R 4.6.0)
# P graphics     * 4.6.1   2026-06-25 [1] local
# P grDevices    * 4.6.1   2026-06-25 [1] local
# P grid           4.6.1   2026-06-25 [1] local
# gridtext       0.1.6   2026-02-19 [1] CRAN (R 4.6.0)
# gtable         0.3.6   2024-10-25 [1] CRAN (R 4.6.0)
# here         * 1.0.2   2025-09-15 [1] CRAN (R 4.6.0)
# hms            1.1.4   2025-10-17 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magick       * 2.9.1   2026-02-28 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg         * 1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
# rlang          1.3.0   2026-07-05 [1] CRAN (R 4.6.1)
# rprojroot      2.1.1   2025-08-26 [1] CRAN (R 4.6.0)
# rstudioapi     0.19.0  2026-06-11 [1] CRAN (R 4.6.0)
# S7             0.2.2   2026-04-22 [1] CRAN (R 4.6.0)
# scales       * 1.4.0   2025-04-24 [1] CRAN (R 4.6.0)
# sessioninfo    1.2.4   2026-06-04 [1] CRAN (R 4.6.0)
# showtext     * 0.9-8   2026-03-21 [1] CRAN (R 4.6.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.6.0)
# P stats        * 4.6.1   2026-06-25 [1] local
# stringi        1.8.7   2025-03-27 [1] CRAN (R 4.6.0)
# stringr      * 1.6.0   2025-11-04 [1] CRAN (R 4.6.0)
# sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.6.0)
# systemfonts    1.3.2   2026-03-05 [1] CRAN (R 4.6.0)
# textshaping    1.0.5   2026-03-06 [1] CRAN (R 4.6.0)
# tibble       * 3.3.1   2026-01-11 [1] CRAN (R 4.6.0)
# tidyr        * 1.3.2   2025-12-19 [1] CRAN (R 4.6.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.6.0)
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
# ──────────────────────────────────────────────────────────────────
# > 
