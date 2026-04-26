
## Challenge: #TidyTuesday 2026 week 17
## Data:      US Agricultural Tariffs
## Author:    Steven Ponce
## Date:      2026-04-26

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, patchwork
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 11,
    height = 8,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 17)

agreements <- tt$agreements |> clean_names()
quantity_codes <- tt$quantity_codes |> clean_names()
tariff_agricultural <- tt$tariff_agricultural |> clean_names()
tariff_codes <- tt$tariff_codes |> clean_names()

rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(agreements)
glimpse(quantity_codes)
glimpse(tariff_agricultural)
glimpse(tariff_codes)


## 4. TIDY DATA ----

### |- Chapter groups: 8 editorial buckets ----
chapter_groups <- c(
    "01" = "Live Animals",
    "02" = "Meat",
    "03" = "Fish & Seafood",
    "04" = "Dairy & Eggs",
    "08" = "Fruit & Nuts",
    "10" = "Grains & Cereals",
    "17" = "Sugar & Confections",
    "22" = "Beverages & Spirits"
)

### |- Current standard (MFN) ad valorem rates — Panel A ----
mfn_current <- tariff_agricultural |>
    filter(
        agreement == "mfn",
        rate_type_code == "7",
        begin_effective_date <= today(),
        end_effective_date >= today()
    ) |>
    mutate(chapter = substr(hts8, 1, 2)) |>
    filter(chapter %in% names(chapter_groups)) |>
    mutate(chapter_label = chapter_groups[chapter]) |>
    group_by(hts8, chapter_label) |>
    summarise(ad_val_rate = mean(ad_val_rate, na.rm = TRUE), .groups = "drop") |>
    mutate(rate_pct = ad_val_rate * 100) |>
    filter(rate_pct <= 100)

### |- Sort order: descending median tariff rate ----
chapter_order <- mfn_current |>
    group_by(chapter_label) |>
    summarise(median_rate = median(rate_pct, na.rm = TRUE)) |>
    arrange(median_rate) |>
    pull(chapter_label)

mfn_current <- mfn_current |>
    mutate(chapter_label = factor(chapter_label, levels = chapter_order))

### |- Trade deal counts per chapter — Panel B ----
deal_counts <- tariff_agricultural |>
    filter(
        !agreement %in% c("mfn", "col2"),
        !str_detect(agreement, "\\*|\\+"),
        begin_effective_date <= today(),
        end_effective_date >= today()
    ) |>
    mutate(chapter = substr(hts8, 1, 2)) |>
    filter(chapter %in% names(chapter_groups)) |>
    mutate(chapter_label = chapter_groups[chapter]) |>
    group_by(chapter_label) |>
    summarise(n_deals = n_distinct(agreement), .groups = "drop") |>
    mutate(chapter_label = factor(chapter_label, levels = chapter_order))

### |- Highlight flag: top 2 highest-tariff categories ----
top2 <- tail(chapter_order, 2)

mfn_current <- mfn_current |>
    mutate(is_top2 = chapter_label %in% top2)

### |- Median labels per chapter (for Panel A) ----
chapter_medians <- mfn_current |>
    group_by(chapter_label) |>
    summarise(median_rate = median(rate_pct, na.rm = TRUE), .groups = "drop") |>
    mutate(
        label_med  = glue("{round(median_rate, 1)}%"),
        is_top2    = chapter_label %in% top2
    )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
    palette = list(
        "burgundy" = "#722F37",
        "gray_mid" = "#9E9E9E",
        "gray_bar" = "#BDBDBD",
        "text_dark" = "#2C2C2C",
        "text_muted" = "#7A7A7A"
    )
)

### |- titles and caption ----
title_text <- str_glue("Every Product Group Has Trade Deals, But Import Taxes Still Vary Widely")

subtitle_text <- str_glue(
    "Trade deals offering lower tariffs are nearly identical across product groups (right), ",
    "while standard import tax rates vary widely (left) — ",
    "based on current U.S. tariffs (2025)."
)
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 17,
    source_text = "USITC Tariff Database"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        axis.title.x = element_text(size = 8, color = colors$palette$text_muted, margin = margin(t = 6)),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9, color = colors$palette$text_dark, hjust = 1),
        axis.text.x = element_text(size = 8, color = colors$palette$text_muted),
        panel.grid.major.x = element_line(color = "gray97", linewidth = 0.2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 12, 10, 12)
    )
)

theme_set(weekly_theme)

### |- Panel A: Boxplot + median labels ----
pa <- ggplot(mfn_current, aes(x = rate_pct, y = chapter_label)) +
    # Geoms
    geom_boxplot(
        aes(
            fill = is_top2,
            color = is_top2
        ),
        width = 0.55,
        coef = 1.0,
        outlier.shape = NA,
        linewidth = 0.35
    ) +
    geom_text(
        data = filter(chapter_medians, is_top2),
        aes(x = median_rate, y = chapter_label, label = label_med),
        nudge_x = 1.5,
        hjust = 0,
        size = 3.4,
        color = colors$palette$burgundy,
        family = fonts$text,
        fontface = "bold",
        inherit.aes = FALSE
    ) +
    # Scales
    scale_fill_manual(
        values = c("TRUE" = colors$palette$burgundy, "FALSE" = colors$palette$gray_mid),
        guide  = "none"
    ) +
    scale_color_manual(
        values = c("TRUE" = colors$palette$burgundy, "FALSE" = "gray45"),
        guide  = "none"
    ) +
    scale_x_continuous(
        labels = label_percent(scale = 1, suffix = "%"),
        breaks = c(0, 10, 20, 30),
        limits = c(0, 30),
        expand = expansion(mult = c(0.01, 0.04))
    ) +
    # Labs
    labs(
        x     = "Import tax rate (%)",
        title = "A · Tariffs Vary Widely Across Products"
    ) +
    # Theme
    theme(
        plot.title = element_text(
            size = 12, face = "bold",
            family = fonts$title, color = colors$palette$text_dark,
            margin = margin(b = 8)
        ),
        plot.margin = margin(10, 12, 10, 12)
    )

### |- Panel B: Trade deal counts ----
pb <- ggplot(deal_counts, aes(x = n_deals, y = chapter_label)) +
    # Geoms
    geom_col(
        fill  = colors$palette$gray_bar,
        width = 0.55
    ) +
    geom_text(
        aes(label = n_deals),
        hjust = -0.3,
        size = 2.8,
        color = colors$palette$text_muted,
        family = fonts$text
    ) +
    # Annotation
    annotate(
        "text",
        x = 11,
        y = 8.5,
        label = "Nearly identical across all groups (17–19 agreements)",
        hjust = 0.5,
        size = 2.5,
        color = colors$palette$text_dark,
        family = fonts$text,
        fontface = "italic"
    ) +
    # Scales
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.22)),
        breaks = c(0, 5, 10, 15, 20),
        limits = c(0, 22)
    ) +
    # Labs
    labs(
        x = "Number of trade agreements",
        title = "B · Trade Deals Are Nearly Identical Across Products"
    ) +
    # Theme
    theme(
        axis.text.y = element_blank(),
        plot.title = element_text(
            size = 12, face = "bold",
            family = fonts$title, color = colors$palette$text_dark,
            margin = margin(b = 8)
        )
    )

### |- Combined plots ----
combined_plots <- pa + pb +
    plot_layout(widths = c(1.4, 1)) +
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_textbox_simple(
                size = 22,
                face = "bold",
                family = fonts$title,
                color = colors$palette$text_dark,
                margin = margin(b = 6),
                lineheight = 1.2
            ),
            plot.subtitle = element_textbox_simple(
                size = 11,
                family = fonts$text,
                color = colors$palette$text_muted,
                margin = margin(t = 5, b = 10),
                lineheight = 1.2
            ),
            plot.caption = element_textbox_simple(
                size = 7,
                family = fonts$text,
                color = colors$palette$text_muted,
                margin = margin(t = 12),
                lineheight = 1.3
            ),
            plot.margin = margin(16, 16, 18, 16),
            plot.background = element_rect(fill = "white", color = NA)
        )
    )

### |- preview ----
snap(combined_plots)


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
# date     2026-04-26
# rstudio  2026.01.2+418 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.5.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.5.3)
# P datasets     * 4.5.3    2026-03-11 [2] local
# dplyr        * 1.2.1    2026-04-03 [1] CRAN (R 4.5.3)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.5.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.5.3)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.5.3)
# ggplot2      * 4.0.3    2026-04-22 [1] CRAN (R 4.5.3)
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
# httr2          1.2.2    2025-12-08 [1] CRAN (R 4.5.3)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.5.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.5.3)
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
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.5.3)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.5.3)
# rsvg           2.7.0    2025-09-08 [1] CRAN (R 4.5.3)
# S7             0.2.1    2025-11-14 [1] CRAN (R 4.5.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.5.3)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.5.3)
# showtext     * 0.9-8    2026-03-21 [1] CRAN (R 4.5.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.5.3)
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