
## Challenge: #TidyTuesday 2026 week 34
## Data:      Country Music Lyrics
## Author:    Steven Ponce
## Date:      2026-08-22

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, ggview
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
# tt <- tidytuesdayR::tt_load(2026, week = 34)
# country_lyrics <- tt$country_lyrics
# rm(tt)
country_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-25/country_lyrics.csv')


## 3. EXAMINING THE DATA ----
glimpse(country_lyrics)
skim_without_charts(country_lyrics)


## 4. TIDY DATA ----
hero_producers <- c(
  "Michael Knox",
  "Jeff Stevens",
  "Byron Gallimore",
  "Ross Copperman",
  "Jay Joyce",
  "Dann Huff"
)

producer_edges <- country_lyrics |>
  filter(!is.na(producer)) |>
  separate_longer_delim(producer, delim = ",") |>
  mutate(producer = str_squish(producer)) |>
  filter(producer %in% hero_producers) |>
  count(producer, artist, name = "n_songs")

#
producer_check <- producer_edges |>
  mutate(
    artist_share = n_songs / sum(n_songs),
    .by = producer
  ) |>
  summarise(
    total_songs = sum(n_songs),
    distinct_artists = n_distinct(artist),
    top_artist_share = max(artist_share),
    .by = producer
  ) |>
  arrange(match(producer, hero_producers))

# Two-column layout
reach_left <- 0.70
reach_right <- 2.10
col_gap <- 2.30
label_offset <- 0.12
row_gap <- 2.00

layout_spec <- tribble(
  ~producer, ~side, ~row,
  "Michael Knox", "left", 1,
  "Ross Copperman", "right", 1,
  "Jeff Stevens", "left", 2,
  "Jay Joyce", "right", 2,
  "Byron Gallimore", "left", 3,
  "Dann Huff", "right", 3
)

row_layout <- layout_spec |>
  left_join(producer_check, by = "producer") |>
  mutate(
    half_span = pmax((distinct_artists - 1) / 2, 0.5)
  ) |>
  summarise(
    row_half_span = max(half_span),
    .by = row
  ) |>
  arrange(row) |>
  mutate(
    row_full_span = row_half_span * 2,
    baseline_y = -(
      cumsum(row_full_span) -
        row_half_span +
        row_gap * (row_number() - 1)
    )
  ) |>
  select(row, row_half_span, baseline_y)

artist_breadth_range <- c(1, 17)
palette_fn <- scales::colour_ramp(c("#5C6068", "#90718A", "#4A154B"))

producer_layout <- layout_spec |>
  left_join(producer_check, by = "producer") |>
  left_join(row_layout, by = "row") |>
  mutate(
    half_span = pmax((distinct_artists - 1) / 2, 0.5),
    hub_x = if_else(
      side == "left",
      0,
      col_gap
    ),
    reach = if_else(
      side == "left",
      reach_left,
      reach_right
    ),
    node_x = hub_x + reach,
    producer_label_x = hub_x - label_offset,
    stat_label = str_glue(
      "{total_songs} songs across {distinct_artists} ",
      "{if_else(distinct_artists == 1, 'artist', 'artists')}"
    ),
    color_val = scales::rescale(
      distinct_artists,
      from = artist_breadth_range
    ),
    label_color = palette_fn(color_val)
  )

# Build artist nodes 
nodes <- producer_edges |>
  left_join(
    producer_layout |>
      select(
        producer,
        hub_x,
        node_x,
        baseline_y,
        distinct_artists
      ),
    by = "producer"
  ) |>
  mutate(
    producer = factor(
      producer,
      levels = layout_spec$producer
    )
  ) |>
  arrange(
    producer,
    desc(n_songs),
    artist
  ) |>
  mutate(
    rank = row_number(),
    k    = n(),
    .by  = producer
  ) |>
  mutate(
    y_local = rank - (k + 1) / 2,
    y_abs = baseline_y + y_local,
    artist_label_x = node_x + label_offset,
    color_val = scales::rescale(
      distinct_artists,
      from = artist_breadth_range
    ),
    line_color = palette_fn(color_val)
  )

# Visual Key

key_gap <- 2

key_y <- row_layout$baseline_y[row_layout$row == 1] +
  row_layout$row_half_span[row_layout$row == 1] +
  key_gap

key_data <- tibble(
  x    = 0,
  xend = reach_left,
  y    = key_y
)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors()

bg_color <- "#F8F9FA"
title_color    <- colors$title
text_color     <- colors$text
subtitle_color <- colors$subtitle
caption_color  <- colors$caption

### |- titles and caption ----
title_text <- paste0(
  "Some country producers stick with one artist. ",
  "Others work across the genre."
)

subtitle_text <- paste0(
  "Among recurring producers of 2014\u20132019 Country Airplay hits, ",
  "some appear almost entirely with one artist while one standout spans 17 acts."
)

methodology_text <- str_wrap(
  paste0(
    "Six producers selected to illustrate the range of artist relationships ",
    "among producers credited on \u2265 3 songs. ",
    "Line width represents songs produced together."
  ),
  width = 130
) |>
  str_replace_all("\n", "<br>")

caption_text <- str_glue(
  "{methodology_text}<br>",
  "{create_social_caption(
      tt_year = 2026,
      tt_week = 34,
      source_text = \"Grady Smith's Country Music Lyrics dataset\"
    )}"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot ----
p <- ggplot(nodes) +
  # Producer - artist relationships
  geom_curve(
    aes(
      x = hub_x, y = baseline_y, xend = node_x, yend = y_abs,
      linewidth = n_songs, color = line_color
    ),
    curvature = 0.35, alpha = 0.75, lineend = "round"
  ) +
  geom_point(
    data = producer_layout,
    aes(x = hub_x, y = baseline_y),
    color = bg_color, size = 4.5
  ) +
  # Producer origin nodes
  geom_point(
    data = producer_layout,
    aes(x = hub_x, y = baseline_y, color = label_color), size = 3.2
  ) +
  # Artist labels
  geom_text(
    aes(x = artist_label_x, y = y_abs, label = artist),
    hjust = 0, size = 2.8, color = text_color, family = fonts$text
  ) +
  # Producer labels
  geom_text(
    data = producer_layout,
    aes(x = producer_label_x, y = baseline_y, label = producer, color = label_color),
    hjust = 1, fontface = "bold", size = 3.6, family = fonts$title_2
  ) +
  # Producer summary labels
  geom_text(
    data = producer_layout,
    aes(x = producer_label_x, y = baseline_y - 0.55, label = stat_label),
    hjust = 1, size = 2.4, color = text_color, family = fonts$text
  ) +
  # Visual key
  geom_segment(
    data = key_data,
    aes(x = x, y = y, xend = xend, yend = y),
    linewidth = 1, color = text_color, lineend = "round"
  ) +
  geom_point(
    data = key_data,
    aes(x = x, y = y),
    size = 2.2, color = text_color
  ) +
  geom_text(
    data = key_data,
    aes(x = x, y = y + 0.5, label = "PRODUCER"),
    hjust = 0.5, size = 2.6, fontface = "bold",
    color = text_color, family = fonts$text
  ) +
  geom_text(
    data = key_data,
    aes(x = xend, y = y + 0.5, label = "ARTIST"),
    hjust = 0.5, size = 2.6, fontface = "bold",
    color = text_color, family = fonts$text
  ) +
  geom_text(
    data = key_data,
    aes(
      x = (x + xend) / 2, y = y - 0.5,
      label = "thicker lines = more songs produced together"
    ),
    hjust = 0.5, size = 2.2, fontface = "italic",
    color = text_color, family = fonts$text
  ) +
  scale_linewidth_continuous(range = c(0.3, 3), guide = "none") +
  scale_color_identity() +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.08))) +
  coord_cartesian(clip = "off") +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    plot.title = element_textbox_simple(
      face = "bold", family = fonts$title_1, size = 24,
      lineheight = 1.2, color = title_color, margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$subtitle, size = 15, lineheight = 1.3,
      color = subtitle_color, margin = margin(t = 5, b = 10)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$caption, size = 7.5, color = caption_color,
      lineheight = 1.3, margin = margin(t = 12)
    ),
    plot.margin = margin(t = 14, r = 40, b = 10, l = 70)
  )

# Preview
p +
    canvas(width = 12.5, height = 11.5, units = "in", dpi = 300)


## 6. SAVE ----
save_ggplot(
  plot = p,
  file = "2026/Week_34/2026_34.png",
  width = 12.5, height = 11.5
)


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

# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.5.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-08-20
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ───────────────────────────────────────────────────────
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
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.6.0)
# ggview       * 0.2.2   2025-07-05 [1] CRAN (R 4.6.0)
# glue         * 1.8.1   2026-04-17 [1] CRAN (R 4.6.0)
# P graphics     * 4.6.1   2026-06-25 [1] local
# P grDevices    * 4.6.1   2026-06-25 [1] local
# P grid           4.6.1   2026-06-25 [1] local
# gridtext       0.1.6   2026-02-19 [1] CRAN (R 4.6.0)
# gtable         0.3.6   2024-10-25 [1] CRAN (R 4.6.0)
# here         * 1.0.2   2025-09-15 [1] CRAN (R 4.6.0)
# hms            1.1.4   2025-10-17 [1] CRAN (R 4.6.0)
# htmltools      0.5.9   2025-12-04 [1] CRAN (R 4.6.0)
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
