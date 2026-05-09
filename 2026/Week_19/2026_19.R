
## Challenge: #TidyTuesday 2025 week 19
## Data:      Twinned Cities
## Author:    Steven Ponce
## Date:      2025-05-09


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
    width  = 12,
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
tt <- tidytuesdayR::tt_load(2026, week = 19)

cities <- tt$cities |> clean_names()
links <- tt$links |> clean_names()

rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(cities)
glimpse(links)


## 4. TIDY DATA ----

### |- enrich links with country/continent metadata ----
links_e <- links |>
  left_join(
    cities |> select(id, country_src = country, cont_src = continent),
    by = c("source" = "id")
  ) |>
  left_join(
    cities |> select(id, country_tgt = country, cont_tgt = continent),
    by = c("target" = "id")
  )

### |- international links only (filter domestic, 9.8% of total) ----
intl <- links_e |>
  filter(
    country_src != country_tgt,
    !is.na(country_src),
    !is.na(country_tgt)
  )

### |- PANEL A: top countries by international links ----
# Count cities per country for normalization denominator
cities_per_country <- cities |>
  count(country, name = "n_cities")

panel_a_data <- bind_rows(
  intl |> select(country = country_src, continent = cont_src),
  intl |> select(country = country_tgt, continent = cont_tgt)
) |>
  count(country, continent, name = "intl_links") |>
  left_join(cities_per_country, by = "country") |>
  mutate(
    links_per_city = intl_links / n_cities,
    # Highlight = postwar reconciliation bloc countries only
    # Russia excluded — current geopolitical context complicates the framing
    reconciliation_country = country %in% c(
      "Czechia", "France", "Germany", "Hungary", "Italy",
      "Lithuania", "Moldova", "Poland", "Portugal", "Romania",
      "Spain", "Ukraine", "United Kingdom"
    )
  ) |>
  arrange(desc(intl_links)) |>
  slice_head(n = 20) |>
  mutate(country = fct_reorder(country, intl_links))

### |- PANEL B: top bilateral corridors (replaces heatmap) ----
# Canonical pair: alphabetically sorted so each corridor appears once
country_continent <- cities |>
  distinct(country, continent)

panel_b_data <- intl |>
  mutate(
    country_a = if_else(country_src < country_tgt, country_src, country_tgt),
    country_b = if_else(country_src < country_tgt, country_tgt, country_src)
  ) |>
  count(country_a, country_b, name = "n_links") |>
  arrange(desc(n_links)) |>
  slice_head(n = 20) |>
  left_join(country_continent |> rename(country_a = country, cont_a = continent),
    by = "country_a"
  ) |>
  left_join(country_continent |> rename(country_b = country, cont_b = continent),
    by = "country_b"
  ) |>
  mutate(
    pair_label = glue("{country_a} \u2013 {country_b}"),
    pair_label = fct_reorder(pair_label, n_links),
    # Highlight = both countries in the postwar Central/Eastern European
    reconciliation_pair = country_a %in% c(
      "Czechia", "France", "Germany", "Hungary",
      "Lithuania", "Moldova", "Poland", "Romania", "Ukraine"
    ) & country_b %in% c(
      "Czechia", "France", "Germany", "Hungary",
      "Lithuania", "Moldova", "Poland", "Romania", "Ukraine"
    ),
    is_hero = country_a == "Germany" & country_b == "Poland"
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
    palette = list(
        bar_europe  = "#722F37",   
        bar_other   = "gray70",    
        bar_germany = "#722F37",   
        bar_europe_b = "#722F37",  
        bar_mixed    = "gray70",   
        annotation   = "#3d3d3d",  
        bg          = "#FAFAF8"    
    )
)

### |- titles and caption ----
title_text    <- str_glue("After the war, cities did the work of reconciliation")

subtitle_text <- str_glue(
    "Europe dominates the global sister-city network — especially across Central and Eastern Europe."
)

caption_text <- create_social_caption(
    tt_year = 2026,
    tt_week = 19,
    source_text  = "Wikidata via Wikipedia"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme  <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        # Panel
        panel.background = element_rect(fill = colors$palette$bg, color = NA),
        plot.background = element_rect(fill = colors$palette$bg, color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        # Axes
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(
            family = fonts$text, size = 7.5, color = "gray40"
        ),
        axis.text.y = element_text(
            family = fonts$text, size = 8, color = "gray25", hjust = 1
        ),
        
        # Title / subtitle / caption
        plot.title = element_markdown(
            family = fonts$title,
            size = 18,
            face = "bold",
            color = "gray10",
            margin = margin(b = 6)
        ),
        plot.subtitle = element_markdown(
            family = fonts$text,
            size = 10,
            color = "gray35",
            lineheight = 1.4,
            margin = margin(b = 16)
        ),
        plot.caption = element_markdown(
            family = fonts$text,
            size = 7,
            color = "gray55",
            hjust = 0,
            margin = margin(t = 12)
        ),
        plot.margin = margin(16, 20, 12, 16)
    )
)

theme_set(weekly_theme)

### |- PANEL A: horizontal bar chart ----
p_a <- panel_a_data |>
    ggplot(aes(
        x    = intl_links,
        y    = country,
        fill = reconciliation_country
    )) +
    # Geoms
    geom_col(width = 0.72, show.legend = FALSE) +
    geom_text(
        aes(label = comma(intl_links, accuracy = 1)),
        hjust = -0.15,
        family = fonts$text,
        size = 2.8,
        color = "gray35"
    ) +
    # Scalea
    scale_fill_manual(values = c(
        "TRUE" = colors$palette$bar_europe,
        "FALSE" = colors$palette$bar_other
    )) +
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.18)),
        labels = comma_format(accuracy = 1)
    ) +
    # Labs
    labs(
        title    = "International sister-city links by country",
        subtitle = "Top 20 countries · **European** countries highlighted · *Hungary, Lithuania & Czechia rank highest per city*"
    ) +
    # Theme
    theme(
        plot.title = element_markdown(
            family = fonts$title, size = 13, face = "bold", color = "gray15",
            margin = margin(b = 3)
        ),
        plot.subtitle = element_markdown(
            family = fonts$subtitle, size = 8.5, color = "gray45",
            margin = margin(b = 10)
        ),
        plot.margin = margin(8, 12, 8, 8)
    )

### |- PANEL B: top bilateral corridors ----
p_b <- panel_b_data |>
    ggplot(aes(
        x    = n_links,
        y    = pair_label,
        fill = reconciliation_pair
    )) +
    # Geoms
    geom_col(width = 0.72, show.legend = FALSE) +
    geom_hline(
        yintercept = 14.5,
        color      = "gray80",
        linewidth  = 0.4,
        linetype   = "dashed"
    ) +
    geom_text(
        aes(label = comma(n_links, accuracy = 1)),
        hjust = -0.15,
        family = fonts$text,
        size = 2.8,
        color = "gray35"
    ) +
    # Annotate
    annotate(
        "text",
        x = 380, y = 15.1,
        label = "Reconciliation corridors",
        family = fonts$text, size = 2.4,
        color = colors$palette$bar_europe_b,
        hjust = 1, fontface = "italic"
    ) +
    annotate(
        "text",
        x = 380, y = 14.0,
        label = "Global corridors",
        family = fonts$text, size = 2.4,
        color = "gray55",
        hjust = 1, fontface = "italic"
    ) +
    # Scales
    scale_fill_manual(
        values = c(
            "TRUE" = colors$palette$bar_europe_b,
            "FALSE" = colors$palette$bar_mixed
        )
    ) +
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.05)),
        labels = comma_format(accuracy = 1)
    ) +
    # Labw
    labs(
        title    = "Strongest bilateral sister-city corridors",
        subtitle = "Top 20 country pairs · **Reconciliation corridors** highlighted · Germany appears in 8 of the top 13"
    ) +
    # Theme
    theme(
        plot.title = element_markdown(
            family = fonts$title, size = 13, face = "bold", color = "gray15",
            margin = margin(b = 3)
        ),
        plot.subtitle = element_markdown(
            family = fonts$subtitle, size = 8.5, color = "gray45",
            margin = margin(b = 10)
        ),
        panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
        plot.margin = margin(8, 16, 8, 8)
    )

### |- combine plots ----

combined_plot <- p_a + plot_spacer() + p_b +
    plot_layout(widths = c(1, 0.02, 1)) +
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_markdown(
                family = fonts$title,
                size = 24,
                face = "bold",
                color = "gray10",
                margin = margin(b = 6)
            ),
            plot.subtitle = element_markdown(
                family = fonts$subtitle,
                size = 12,
                color = "gray35",
                lineheight = 1.4,
                margin = margin(b = 4)
            ),
            plot.caption = element_markdown(
                family = fonts$caption,
                size = 7,
                color = "gray40",
                hjust = 0,
                margin = margin(t = 10)
            ),
            plot.background = element_rect(fill = colors$palette$bg, color = NA),
            plot.margin = margin(16, 20, 12, 16),
        )
    )

### |- preview ----
snap(combined_plot)


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

# ─ Session info ────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-08
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────
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
# ───────────────────────────────────
