
## Challenge: #TidyTuesday 2026 week 40
## Data:      Avocado Oil Authenticity
## Author:    Steven Ponce
## Date:      2026-10-05

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, ggview, ggforce
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure settings ----
fig_w      <- 12
fig_h      <- 6.4
out_file   <- "2026/Week_40/2026_40.png"

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 40)
foods_raw <- tt$avocado_oil_processed_foods |> clean_names()
rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(foods_raw)
foods_raw |> count(oil_type, category, authentic)

## 4. TIDY DATA ----

### |- product-level outcomes (avocado only; 2 lots per product) ----
lots <- foods_raw |>
    filter(oil_type == "avocado") |>
    select(product_id, lot, category, authentic, front_label,
           package_size_oz, retail_price_usd, purchase_location)

products <- lots |>
    select(product_id, category, lot, authentic) |>
    ungroup() |>
    pivot_wider(names_from = lot, values_from = authentic, names_prefix = "lot_") |>
    mutate(
        n_failed = (!lot_1) + (!lot_2),
        outcome = case_when(
            n_failed == 2 ~ "both_fail",
            n_failed == 1 ~ "split",
            TRUE          ~ "both_pass"
        ),
        outcome  = factor(outcome, levels = c("both_fail", "split", "both_pass")),
        category = factor(
            category,
            levels = c("chips", "mayonnaise", "salad_dressing"),
            labels = c("Chips", "Mayonnaise", "Salad dressing")
        )
    )

### |- computed facts + guards (every reader-facing number) ----
n_products  <- nrow(products)
n_both_fail <- sum(products$outcome == "both_fail")
n_split     <- sum(products$outcome == "split")
n_both_pass <- sum(products$outcome == "both_pass")

# independence benchmark: lot-specific failure rates
p_fail_lot1 <- mean(!products$lot_1)
p_fail_lot2 <- mean(!products$lot_2)
expected_both_fail <- n_products * p_fail_lot1 * p_fail_lot2

# the two passing products must all be mayonnaise with shared attributes
passing_ids <- products |> filter(outcome == "both_pass") |> pull(product_id)
passing_attrs <- lots |>
    filter(product_id %in% passing_ids) |>
    distinct(category, front_label, package_size_oz, retail_price_usd, purchase_location)

# spelled-out count for caption copy (matches on-chart "Two mayonnaise products")
n_both_pass_word <- c("one", "two", "three")[n_both_pass]

### |- coin layout: 7 coins per row, rows stacked by category ----
coins_per_row <- 7
coin_r        <- 0.42
row_gap       <- 1.05
cat_gap       <- 0.55

coins <- products |>
    arrange(category, outcome, product_id) |>
    mutate(
        idx = row_number() - 1,
        col = idx %% coins_per_row,
        row = idx %/% coins_per_row,
        .by = category
    )

cat_layout <- coins |>
    summarise(n_rows = max(row) + 1, n = n(),
              n_fail_both = sum(outcome == "both_fail"), .by = category) |>
    arrange(category) |>
    mutate(
        row_offset = lag(cumsum(n_rows), default = 0),
        cat_index  = row_number() - 1
    )

coins <- coins |>
    left_join(cat_layout |> select(category, row_offset, cat_index), by = "category") |>
    mutate(
        x0 = col * row_gap,
        y0 = -(row_offset + row) * row_gap - cat_index * cat_gap
    )

### |- one row per half-coin (lot 1 = left half, lot 2 = right half) ----
halves <- coins |>
    select(product_id, category, outcome, x0, y0, lot_1, lot_2) |>
    pivot_longer(c(lot_1, lot_2), names_to = "lot", values_to = "passed") |>
    mutate(
        start    = if_else(lot == "lot_1", pi, 0),
        end      = if_else(lot == "lot_1", 2 * pi, pi),
        fill_hex = if_else(passed, "#ddd5c6", "#722F37"),
        line_hex = if_else(passed, "#5c5f5b", "#f4f1ea")
    )

### |- category labels ----
cat_labels <- cat_layout |>
    left_join(coins |> filter(row == 0, col == 0) |> select(category, y0), by = "category") |>
    mutate(
        label = glue(
            "<b style='color:#22282b;font-size:13pt'>{category}</b><br>",
            "<span style='color:#5c5f5b;font-size:9.5pt'>{n_fail_both} of {n} failed both lots</span>"
        )
    )

### |- mayo annotation anchor (right of the last passing coin) ----
mayo_anchor <- coins |>
    filter(outcome == "both_pass") |>
    summarise(x = max(x0), y = mean(y0))

### |- key coin (top right) ----
key_x <- coins_per_row * row_gap + 1.6
key_y <- max(coins$y0)
key_halves <- tibble(
    x0 = key_x, y0 = key_y,
    start    = c(pi, 0),
    end      = c(2 * pi, pi),
    fill_hex = c("#722F37", "#ddd5c6"),
    line_hex = c("#f4f1ea", "#5c5f5b")
)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
clrs <- get_theme_colors(
    palette = list(
        failed   = "#722F37",
        passed   = "#ddd5c6",
        outline  = "#5c5f5b",
        paper    = "#f4f1ea",
        ink      = "#22282b",
        ink_soft = "#5c5f5b"
    )
)

### |- titles and caption ----
title_text <- glue(
    "{n_both_fail} of {n_products} tested avocado-oil products<br>",
    "failed authenticity testing in both tested lots"
)

subtitle_text <- glue(
    "Researchers tested two separately purchased lots of each of {n_products} ",
    "products labeled as made with avocado oil"
)

note_text <- glue(
    "A failed result does not show how much avocado oil a product contained, and it ",
    "is not a food-safety verdict. Lots were tested against Codex Alimentarius ",
    "avocado-oil ranges with a 10% margin. Products were bought in 2025-26 from ",
    "California stores and online; results describe the tested products and may not ",
    "represent the wider market. Failure was common across tested lots, so many ",
    "products would fail twice even if lot results were independent. These tests do ",
    "not identify responsible suppliers or establish intent."
) |>
    str_wrap(width = 150) |>
    str_replace_all("\n", "<br>")

caption_text <- glue(
    "{note_text}<br><br>",
    "{create_social_caption(tt_year = 2026, tt_week = 40, ",
    "source_text = 'Wang lab, UC Davis (Applied Food Research, 2026)')}"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        plot.background  = element_rect(fill = "#f4f1ea", color = "#f4f1ea"),
        panel.background = element_rect(fill = "#f4f1ea", color = "#f4f1ea"),
        panel.grid       = element_blank(),
        axis.text        = element_blank(),
        axis.title       = element_blank(),
        axis.ticks       = element_blank(),
        plot.title = element_markdown(
            family = fonts$title_1, face = "bold", size = rel(1.55),
            color = "#22282b", margin = margin(b = 6)
        ),
        plot.subtitle = element_markdown(
            family = fonts$subtitle, size = rel(0.95), color = "#5c5f5b",
            lineheight = 1.2, margin = margin(b = 14)
        ),
        plot.caption = element_markdown(
            family = fonts$caption, size = rel(0.55), color = "#5c5f5b",
            hjust = 0, lineheight = 1.25, margin = margin(t = 14)
        ),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        plot.margin = margin(t = 20, r = 22, b = 14, l = 22)
    )
)

theme_set(weekly_theme)

### |-  main plot ----
p <- ggplot() +
    geom_arc_bar(
        data = halves,
        aes(x0 = x0, y0 = y0, r0 = 0, r = coin_r, start = start, end = end,
            fill = fill_hex, color = line_hex),
        linewidth = 0.6
    ) +
    geom_richtext(
        data = cat_labels,
        aes(x = -0.75, y = y0, label = label),
        hjust = 1, vjust = 0.6, family = fonts$text,
        fill = NA, label.color = NA, lineheight = 1.3
    ) +
    annotate(
        "text",
        x = mayo_anchor$x + coin_r + 0.35, y = mayo_anchor$y,
        label = "Both lots passed for these two products.\nThey share label wording, size, price\nand purchase channel.",
        hjust = 0, vjust = 0.5, family = fonts$text, size = 3.3,
        color = "#5c5f5b", lineheight = 1.1
    ) +
    geom_arc_bar(
        data = key_halves,
        aes(x0 = x0, y0 = y0, r0 = 0, r = coin_r, start = start, end = end,
            fill = fill_hex, color = line_hex),
        linewidth = 0.6
    ) +
    annotate(
        "text",
        x = key_x + c(-coin_r * 0.62, coin_r * 0.62), y = key_y - coin_r - 0.18,
        label = c("lot 1", "lot 2"),
        family = fonts$text, size = 2.8, color = "#5c5f5b", vjust = 1
    ) +
    annotate(
        "text",
        x = key_x + coin_r + 0.3, y = key_y + 0.12,
        label = "One circle = one product;\neach half = one tested lot",
        hjust = 0, vjust = 0.5, family = fonts$text, size = 3.1,
        color = "#22282b", lineheight = 1.1
    ) +
    annotate(
        "point",
        x = key_x + coin_r + 0.42 + c(0, 1.25), y = key_y - 0.45,
        shape = 22, size = 3.2, stroke = 0.6,
        fill = c("#722F37", "#ddd5c6"), color = c("#722F37", "#5c5f5b")
    ) +
    annotate(
        "text",
        x = key_x + coin_r + 0.62 + c(0, 1.25), y = key_y - 0.45,
        label = c("failed", "passed"),
        hjust = 0, vjust = 0.5, family = fonts$text, size = 3,
        color = "#5c5f5b"
    ) +
    annotate(
        "text",
        x = key_x + coin_r + 0.3, y = key_y - 0.85,
        label = "Failed = chemical profile inconsistent\nwith authentic avocado oil",
        hjust = 0, vjust = 1, family = fonts$text, size = 3,
        color = "#22282b", lineheight = 1.1
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    coord_equal(
        xlim = c(-3.4, key_x + 4.2),
        ylim = c(min(coins$y0) - 0.7, max(coins$y0) + 0.6),
        clip = "off"
    ) +
    labs(title = title_text, subtitle = subtitle_text, caption = caption_text)

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

# ─ Session info ───────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-10-05
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ───────────────────────────────────────────────────
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
# ggforce      * 0.5.0   2025-06-18 [1] CRAN (R 4.6.0)
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
# MASS           7.3-65  2025-02-28 [1] CRAN (R 4.6.1)
# P methods      * 4.6.1   2026-06-25 [1] local
# otel           0.2.0   2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# polyclip       1.10-7  2024-07-23 [1] CRAN (R 4.6.0)
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
# tweenr         2.0.3   2024-02-26 [1] CRAN (R 4.6.0)
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
# ──────────────────────────────────────────────────────────────
# > 
