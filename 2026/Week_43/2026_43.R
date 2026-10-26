## Challenge: #TidyTuesday 2026 week 43
## Data:      IKEA Sofas and Their Swedish Places
## Author:    Steven Ponce
## Date:      2026-10-26

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, scales, 
    glue, ggview
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure settings ----
fig_w   <- 10
fig_h   <- 8
fig_dpi <- 320
out_file   <- "2026/Week_43/2026_43.png"

setup_fonts()
fonts <- get_font_families()


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 43)
ikea_seating_raw <- tt$ikea_seating |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(ikea_seating_raw)


## 4. TIDY DATA ----

### |- 2026 standard VAT rates, euro markets ----
vat_rates <- c(
  at = 0.20, be = 0.21, de = 0.19, ee = 0.24, es = 0.21, fi = 0.255,
  fr = 0.20, hr = 0.25, ie = 0.23, it = 0.22, lt = 0.21, lv = 0.21,
  nl = 0.21, pt = 0.23, si = 0.22, sk = 0.23
)

market_names <- c(
  at = "Austria", be = "Belgium", de = "Germany", ee = "Estonia",
  es = "Spain", fi = "Finland", fr = "France", hr = "Croatia",
  ie = "Ireland", it = "Italy", lt = "Lithuania", lv = "Latvia",
  nl = "Netherlands", pt = "Portugal", si = "Slovenia", sk = "Slovakia"
)

### |- euro listings, one row per market x article ----
euro <- ikea_seating_raw |>
  filter(currency == "EUR") |>
  distinct(country, item_no, .keep_all = TRUE) |>
  mutate(price_net = price / (1 + unname(vat_rates[country])))


### |- fixed cohort: articles listed in all 16 euro markets ----
cohort <- euro |>
  add_count(item_no, name = "n_markets") |>
  filter(n_markets == 16) |>
  mutate(
    article_median = median(price_net),
    rel_price = price_net / article_median,
    side = case_when(
      price_net < article_median - 0.005 ~ "below",
      price_net > article_median + 0.005 ~ "above",
      TRUE ~ "tie"
    ),
    .by = item_no
  )

### |- per-market summary ----
market_summary <- cohort |>
  summarise(
    median_rel = median(rel_price),
    below = sum(side == "below"),
    tie = sum(side == "tie"),
    above = sum(side == "above"),
    .by = country
  ) |>
  mutate(market = unname(market_names[country])) |>
  arrange(median_rel)

market_levels <- market_summary$market # cheapest at the bottom

cohort <- cohort |>
  mutate(market = factor(unname(market_names[country]), levels = market_levels))

market_summary <- market_summary |>
  mutate(
    market = factor(market, levels = market_levels),
    named = country %in% c("ee", "ie", "de")
  )

### |- counts quoted in the copy ----
n_ee_below <- market_summary$below[market_summary$country == "ee"]
n_ie_above <- market_summary$above[market_summary$country == "ie"]
n_de_below <- market_summary$below[market_summary$country == "de"]
n_de_above <- market_summary$above[market_summary$country == "de"]

# Baltic markets: shared shelf price (with VAT), different VAT
baltic_prices <- cohort |>
  filter(country %in% c("ee", "lv", "lt")) |>
  select(item_no, country, price) |>
  pivot_wider(names_from = country, values_from = price)

n_baltic_same <- sum(baltic_prices$ee == baltic_prices$lv & baltic_prices$lv == baltic_prices$lt)
n_lv_lt_same <- sum(baltic_prices$lv == baltic_prices$lt)

### |- count-column layout (secondary to the dots) ----
count_cols <- tibble(
  side = c("below", "tie", "above"),
  x = c(1.74, 1.88, 2.02)
)

count_labels <- market_summary |>
  select(market, named, below, tie, above) |>
  pivot_longer(c(below, tie, above), names_to = "side", values_to = "n") |>
  left_join(count_cols, by = "side")


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    dot = "#2C2825",
    median = "#722F37",
    ref = "#B9B1A6",
    muted = "#7A7068",
    ink = "#2C2825"
  )
)

### |- titles and caption ----
title_text <- "For the same IKEA furniture, Estonia runs low and Ireland high"

subtitle_text <- str_glue(
  "Across 34 identical IKEA articles sold in all 16 euro markets, Estonia's pre-VAT price falls below each<br>",
  "article's market median on **{n_ee_below} of 34**; Ireland's rises above it on **{n_ie_above}**. ",
  "Between them, Germany splits **{n_de_below}–{n_de_above}**.<br><br>",
  "<span style='font-size:9.5pt;color:#7A7068'>Each dot is one article's price before VAT, relative to that article's ",
  "median across the 16 markets.</span>"
)

annotation_text <- str_glue(
  "The three Baltic markets share a shelf price on {n_baltic_same} of 34 articles ",
  "(Latvia and Lithuania on all {n_lv_lt_same}). Estonia's higher VAT (24% versus 21%) ",
  "leaves it with a lower pre-VAT price."
)

disclosure_text <- paste0(
  "Snapshot of ikea.com listings on 28 Sep 2026. Pre-VAT price = shelf price ÷ (1 + 2026 standard VAT rate).<br>",
  "VAT rates: VATupdate. Log scale: +25% and −20% sit the same distance from the median.<br>",
  "Tie = price equal to the article's median. The 34 articles include one children's armchair frame."
)

caption_text <- paste0(
  disclosure_text, "<br>",
  create_social_caption(
    tt_year = 2026,
    tt_week = 43,
    source_text = "ikea.com via TidyTuesday (curated by Georgios Karamanis)"
  )
)

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_text(
      face = "bold", family = fonts$title_1, size = 24,
      color = "#2C2825", margin = margin(b = 8)
    ),
    plot.subtitle = element_markdown(
      family = fonts$subtitle, size = 11.5, color = "#2C2825",
      lineheight = 1.25, margin = margin(b = 14)
    ),
    plot.caption = element_markdown(
      family = fonts$caption, size = 7, color = "#7A7068",
      hjust = 0, lineheight = 1.3, margin = margin(t = 14)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.text.y = element_text(family = fonts$text, size = 10.5, color = "#2C2825", hjust = 1),
    axis.text.x = element_text(family = fonts$text, size = 9, color = "#7A7068"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(20, 24, 14, 20)
  )
)

theme_set(weekly_theme)

### |- final plot ----

### |- axis: percent from the article median, on a log scale ----
x_breaks <- c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5)
x_labels <- c("−30%", "−20%", "−10%", "median", "+10%", "+20%", "+30%", "+40%", "+50%")

p <- ggplot(cohort, aes(x = rel_price, y = market)) +

  # reference line at the article median
  geom_vline(xintercept = 1, color = "#B9B1A6", linewidth = 0.5) +

  # one dot per article
  geom_point(
    position = position_jitter(height = 0.18, width = 0, seed = 43),
    color = "#2C2825", alpha = 0.42, size = 1.7, stroke = 0
  ) +

  # market median tick
  geom_segment(
    data = market_summary,
    aes(
      x = median_rel, xend = median_rel,
      y = as.integer(market) - 0.34, yend = as.integer(market) + 0.34
    ),
    color = "#722F37", linewidth = 1.3, lineend = "butt"
  ) +

  # direct label for the median tick, on the top row only
  annotate(
    "text",
    x = market_summary$median_rel[market_summary$market == last(market_levels)],
    y = length(market_levels) + 0.55,
    label = "market median",
    family = fonts$text, size = 2.8, color = "#722F37", hjust = 0.5
  ) +

  # count column, secondary: grey, small, named markets in ink
  geom_text(
    data = count_labels,
    aes(x = x, y = market, label = n, color = named),
    family = fonts$text, size = 3.1, hjust = 0.5
  ) +
  annotate(
    "text",
    x = count_cols$x, y = length(market_levels) + 0.85,
    label = count_cols$side,
    family = fonts$text, size = 2.8, color = "#7A7068", hjust = 0.5
  ) +
  annotate(
    "text",
    x = mean(count_cols$x), y = length(market_levels) + 1.45,
    label = "articles of 34",
    family = fonts$text, size = 2.8, color = "#7A7068", fontface = "bold", hjust = 0.5
  ) +

  # Estonia-Latvia annotation, in the empty right side of the Baltic rows
  annotate(
    "text",
    x = 1.12, y = 2,
    label = str_wrap(annotation_text, 46), # 46 keeps "24% versus 21%" on one line, no orphan
    family = fonts$text, size = 3, color = "#7A7068",
    hjust = 0, vjust = 0.5, lineheight = 1.1
  ) +
  scale_color_manual(values = c(`TRUE` = "#2C2825", `FALSE` = "#7A7068"), guide = "none") +
  scale_x_log10(
    breaks = x_breaks, labels = x_labels,
    limits = c(0.66, 2.1), expand = expansion(0)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.9))) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  )

### |-  preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in", dpi = 320)


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

# ─ Session info ─────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-10-26
# rstudio  2026.10.0+233 Blue Mistflower (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# bit            4.6.0   2025-03-06 [1] CRAN (R 4.6.0)
# bit64          4.8.2   2026-05-19 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
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
# httr2          1.3.0   2026-07-13 [1] CRAN (R 4.6.1)
# janitor      * 2.2.1   2024-12-22 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# otel           0.2.0   2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
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
# snakecase      0.11.1  2023-08-27 [1] CRAN (R 4.6.0)
# P stats        * 4.6.1   2026-06-25 [1] local
# stringi        1.8.7   2025-03-27 [1] CRAN (R 4.6.0)
# stringr      * 1.6.0   2025-11-04 [1] CRAN (R 4.6.0)
# sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.6.0)
# tibble       * 3.3.1   2026-01-11 [1] CRAN (R 4.6.0)
# tidyr        * 1.3.2   2025-12-19 [1] CRAN (R 4.6.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.6.0)
# tidytuesdayR   1.3.2   2026-04-12 [1] CRAN (R 4.6.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.6.0)
# timechange     0.4.0   2026-01-29 [1] CRAN (R 4.6.0)
# P tools          4.6.1   2026-06-25 [1] local
# tzdb           0.5.0   2025-03-15 [1] CRAN (R 4.6.0)
# P utils        * 4.6.1   2026-06-25 [1] local
# vctrs          0.7.3   2026-04-11 [1] CRAN (R 4.6.0)
# vroom          1.7.1   2026-03-31 [1] CRAN (R 4.6.0)
# withr          3.0.3   2026-06-19 [1] CRAN (R 4.6.0)
# xml2           1.6.0   2026-06-22 [1] CRAN (R 4.6.1)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────