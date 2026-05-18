
## Challenge: #TidyTuesday 2026 week 20
## Data:      Crossref Metadata Coverage (Bédard-Vallée, 2026)
## Author:    Steven Ponce
## Date:      2025-05-18


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
    height = 7,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 20)

member_participation_stats_by_country <- tt$member_participation_stats_by_country |> clean_names()
metadata_coverage_stats_by_country <- tt$metadata_coverage_stats_by_country |> clean_names()

rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(member_participation_stats_by_country)
glimpse(metadata_coverage_stats_by_country)


## 4. TIDY DATA ----

### |- Region lookup ----
region_labels <- c(
  "EAS" = "East Asia & Pacific",
  "ECS" = "Europe & Central Asia",
  "LCN" = "Latin America & Caribbean",
  "MEA" = "Middle East & North Africa",
  "NAC" = "North America",
  "SAS" = "South Asia",
  "SSF" = "Sub-Saharan Africa"
)

### |- Panel A: Current ORCID coverage by region (latest snapshot, journal articles) ----
panel_a_data <- metadata_coverage_stats_by_country |>
  filter(
    document_type == "journal-article",
    current_up_to == max(current_up_to)
  ) |>
  mutate(region_label = region_labels[region_id]) |>
  filter(!is.na(region_label)) |>
  summarise(
    with_orcid_for_authors = sum(with_orcid_for_authors),
    n_dois = sum(n_dois),
    .by = c(region_id, region_label)
  ) |>
  mutate(
    orcid_rate = with_orcid_for_authors / n_dois,
    # Narrative ordering: highlighted emerging regions at top of chart,
    # gray established regions below — groups the story visually
    narrative_order = case_when(
      region_label == "Sub-Saharan Africa" ~ 2,
      region_label == "South Asia" ~ 1,
      TRUE ~ orcid_rate + 10
    ),
    region_label = fct_reorder(region_label, narrative_order)
  ) |>
  # Insert blank row to create visual gap between gray and highlighted groups
  (\(x) {
    separator <- tibble(
      region_id              = NA_character_,
      region_label           = factor(" ", levels = c(levels(x$region_label), " ")),
      with_orcid_for_authors = NA_real_,
      n_dois                 = NA_real_,
      orcid_rate             = NA_real_,
      narrative_order        = 2.5
    )
    bind_rows(x, separator) |>
      mutate(region_label = fct_reorder(region_label, narrative_order))
  })()

### |- Panel B: ORCID acceleration (monthly 2025-2026, journal articles) ----

# Monthly window only (Jan 2025 onward)
monthly_data <- metadata_coverage_stats_by_country |>
  filter(
    document_type == "journal-article",
    current_up_to >= as.Date("2025-01-01")
  ) |>
  mutate(region_label = region_labels[region_id]) |>
  filter(!is.na(region_label)) |>
  summarise(
    with_orcid_for_authors = sum(with_orcid_for_authors),
    n_dois = sum(n_dois),
    .by = c(current_up_to, region_id, region_label)
  ) |>
  mutate(orcid_rate = with_orcid_for_authors / n_dois) |>
  arrange(region_id, current_up_to)

# Baseline: Jan 2025 rate per region
baseline <- monthly_data |>
  filter(current_up_to == min(current_up_to)) |>
  select(region_id, baseline_rate = orcid_rate)

# Filter: only regions with a meaningful baseline (>= 3%)
eligible_regions <- baseline |>
  filter(baseline_rate >= 0.03) |>
  pull(region_id)

panel_b_data <- monthly_data |>
  filter(region_id %in% eligible_regions) |>
  left_join(baseline, by = "region_id") |>
  mutate(orcid_indexed = orcid_rate / baseline_rate * 100)

# Identify fastest and slowest growing regions for annotation
region_growth <- panel_b_data |>
  filter(current_up_to == max(current_up_to)) |>
  arrange(desc(orcid_indexed))

top_regions <- region_growth |>
  slice_head(n = 2) |>
  pull(region_id)
bottom_regions <- region_growth |>
  slice_tail(n = 1) |>
  pull(region_id)

# End-of-line label data
end_labels <- panel_b_data |>
  filter(current_up_to == max(current_up_to)) |>
  mutate(label = glue("{region_label}\n({round(orcid_indexed, 0)})"))


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    EAS = "#4E79A7",
    ECS = "#722F37",
    LCN = "#F28E2B",
    MEA = "#76B7B2",
    NAC = "#59A14F",
    SAS = "#EDC948",
    SSF = "#B07AA1",
    highlight = "#722F37",
    muted = "gray70",
    reference = "gray40"
  )
)

region_colors <- c(
  "East Asia & Pacific" = colors$palette$EAS,
  "Europe & Central Asia" = colors$palette$ECS,
  "Latin America & Caribbean" = colors$palette$LCN,
  "Middle East & North Africa" = colors$palette$MEA,
  "North America" = colors$palette$NAC,
  "South Asia" = colors$palette$SAS,
  "Sub-Saharan Africa" = colors$palette$SSF
)

### |- titles and caption ----
title_text <- str_glue("Research is Becoming More Connected")
subtitle_text <- str_glue(
    "Crossref metadata adoption reveals that emerging regions are modernizing fastest —\n",
    "even as established systems still lead in absolute ORCID author coverage, 2025\u20132026"
)

caption_text <- create_social_caption(
    tt_year = 2026,
    tt_week = 20,
    source_text = "Crossref · Bédard-Vallée (2026)"
)

panel_a_title <- "Emerging Regions Still Have Lower ORCID Coverage"
panel_a_sub <- "ORCID author coverage by region · journal articles · Apr 2026"

panel_b_title <- "But Adoption Is Rising Faster"
panel_b_sub <- "Indexed ORCID author coverage by region, Jan 2025 = 100\nJournal articles · regions below 3% ORCID baseline in Jan 2025 excluded from index"

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        plot.title  = element_markdown(
            size = 18, face = "bold", family = fonts$title,
            margin = margin(b = 4)
        ),
        plot.subtitle = element_text(
            size = 9.5, family = fonts$text, color = "gray40",
            lineheight = 1.3, margin = margin(b = 16)
        ),
        plot.caption = element_markdown(
            size = 8, family = fonts$text, color = "gray50",
            margin = margin(t = 12)
        ),
        panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_markdown(size = 9, color = "gray30", family = fonts$text),
        axis.title.y = element_markdown(size = 9, color = "gray30", family = fonts$text),
        axis.text = element_text(size = 8, color = "gray40", family = fonts$text),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = fonts$text),
        legend.key.size = unit(0.4, "cm"),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7.5,
                                  color = "gray40", family = fonts$text)
    )
)

theme_set(weekly_theme)

### |- Panel A: Horizontal bar chart — current ORCID coverage by region ----

# Accent colors matched exactly to Panel B for cross-panel continuity
accent_regions <- c("Sub-Saharan Africa", "South Asia")

bar_fill_values <- panel_a_data |>
  mutate(bar_color = case_when(
    region_label == "Sub-Saharan Africa" ~ region_colors["Sub-Saharan Africa"],
    region_label == "South Asia" ~ region_colors["South Asia"],
    region_label == " " ~ NA_character_,
    TRUE ~ "gray78"
  )) |>
  select(region_label, bar_color) |>
  deframe()

label_color_values <- panel_a_data |>
  mutate(lbl_color = case_when(
    region_label %in% accent_regions ~ "gray15",
    region_label == " " ~ NA_character_,
    TRUE ~ "gray45"
  )) |>
  select(region_label, lbl_color) |>
  deframe()

p_a <- ggplot(
  panel_a_data,
  aes(x = orcid_rate, y = region_label, fill = region_label)
) +
  # Geoms
  geom_col(width = 0.55, alpha = 0.9, na.rm = TRUE) +
  geom_text(
    data = panel_a_data |> filter(!is.na(orcid_rate)),
    aes(
      label = percent(orcid_rate, accuracy = 0.1),
      color = region_label
    ),
    hjust = -0.15, size = 3, family = fonts$text
  ) +
  # Scale
  scale_fill_manual(values = bar_fill_values, guide = "none", na.value = NA) +
  scale_color_manual(values = label_color_values, guide = "none", na.value = NA) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.20)),
    limits = c(0, NA)
  ) +
  scale_y_discrete() +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = panel_a_title,
    subtitle = panel_a_sub,
    x = "Share of journal articles with ORCID author identifier",
    y = NULL
  ) +
  # Theme
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3)
  )


### |- Panel B: ORCID momentum line chart ----

# Highlight lines vs muted lines
highlight_ids <- top_regions

# Keep only 3 background comparator regions to avoid noisy texture
comparator_ids <- region_growth |>
  filter(!region_id %in% highlight_ids) |>
  slice(c(1, 2, 3)) |>
  pull(region_id)

p_b <- ggplot(
  panel_b_data,
  aes(
    x = current_up_to, y = orcid_indexed,
    color = region_label, group = region_label
  )
) +

  # Geoms
  geom_hline(
    yintercept = 100, color = "gray70",
    linetype = "solid", linewidth = 0.4
  ) +
  annotate("text",
    x = as.Date("2025-01-15"), y = 101,
    label = "Jan 2025 baseline", hjust = 0, vjust = 0,
    size = 2.5, color = "gray55", fontface = "italic"
  ) +
  geom_line(
    data = panel_b_data |> filter(region_id %in% comparator_ids),
    linewidth = 0.6, alpha = 0.12, color = "gray75"
  ) +
  geom_line(
    data = panel_b_data |> filter(region_id %in% highlight_ids),
    linewidth = 1.1, alpha = 0.9
  ) +
  geom_text_repel(
    data = end_labels |> filter(region_id %in% highlight_ids),
    aes(label = label),
    hjust = 0, direction = "y", nudge_x = 45,
    size = 2.7, family = fonts$text,
    segment.size = 0.3, segment.color = "gray60",
    box.padding = 0.1
  ) +
  geom_point(
    data = panel_b_data |>
      filter(
        region_id %in% highlight_ids,
        current_up_to == max(current_up_to)
      ),
    size = 2.5, stroke = 0.3
  ) +
  # Scales
  scale_color_manual(values = region_colors, guide = "none") +
  scale_x_date(
    date_labels = "%b '%y",
    date_breaks = "3 months",
    expand = expansion(mult = c(0.02, 0.28))
  ) +
  scale_y_continuous(
    labels = function(x) glue("{x}"),
    breaks = c(100, 125, 150, 175, 200)
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = panel_b_title,
    subtitle = panel_b_sub,
    x = NULL,
    y = "Indexed ORCID coverage<br>(Jan 2025 = 100)"
  )

### |- Combined layout ----
p_combined <- p_a + p_b +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = 20, face = "bold", family = fonts$title,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = 11, family = fonts$text, color = "gray30",
        lineheight = 1.3, margin = margin(b = 20)
      ),
      plot.caption = element_markdown(
        size = 8, family = fonts$text, color = "gray50",
        margin = margin(t = 16)
      ),
      plot.background = element_rect(fill = colors$background, color = NA),
      plot.margin = margin(20, 20, 12, 20)
    )
  )

### |- preview ----
p_combined


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
# date     2026-05-18
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
# ───────────────────────────────────────────────────────────────────