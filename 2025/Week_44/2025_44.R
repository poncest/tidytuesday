## Challenge: #TidyTuesday 2025 week 44
## Data:      Lead concentration in Flint water samples in 2015
## Author:    Steven Ponce
## Date:      2025-11-01

## NOTE: This script uses custom helper functions for theming and formatting.
## See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details,
## or view source code at: https://github.com/poncest/tidytuesday/tree/main

## Using Flint, Michigan, lead data in introductory statistics
## Loux, Travis ; Gibson, Andrew K.
## Teaching statistics, 2019, Vol.41 (3), p.85-88


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  janitor,    # Simple Tools for Examining and Cleaning Dirty Data
  scales,     # Scale Functions for Visualization
  glue,       # Interpreted String Literals
  ggrepel,    # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  patchwork   # The Composer of Plots
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 14,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 44)

flint_mdeq <- tt$flint_mdeq |> clean_names()
flint_vt <- tt$flint_vt |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(flint_mdeq)
glimpse(flint_vt)


## 4. TIDY DATA ----
mdeq_tidy <- flint_mdeq |>
  mutate(
    excluded = is.na(lead2),
    source = "MDEQ"
  ) |>
  select(sample, lead, excluded, notes, source)

vt_tidy <- flint_vt |>
  reframe(
    sample, lead,
    excluded = FALSE,
    notes = NA_character_,
    source = "Virginia Tech"
  )

combined_data <- bind_rows(mdeq_tidy, vt_tidy) |>
  mutate(
    source = factor(source, levels = c("MDEQ", "Virginia Tech")),
    status = if_else(excluded, "Excluded by MDEQ", "Included")
  )

source_labels <- c(
  "MDEQ" = "Michigan Dept. of Environmental Quality\n(Government)",
  "Virginia Tech" = "Virginia Tech\n(Citizen)"
)

stats_tbl <- combined_data |>
  group_by(source) |>
  summarise(
    p90 = quantile(lead, 0.90, na.rm = TRUE),
    median = median(lead, na.rm = TRUE),
    mean = mean(lead, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

risk_lvls <- c(
  "Zero Detection",
  "Trace Levels (>0–5 ppb)",
  "Elevated (5–10 ppb)",
  "Concerning (10–15 ppb)",
  "Above EPA Action Level (>15 ppb)"
)

risk_data <- combined_data |>
  mutate(
    risk_category = case_when(
      lead == 0 ~ risk_lvls[1],
      lead > 0 & lead <= 5 ~ risk_lvls[2],
      lead > 5 & lead <= 10 ~ risk_lvls[3],
      lead > 10 & lead <= 15 ~ risk_lvls[4],
      lead > 15 ~ risk_lvls[5],
      TRUE ~ NA_character_
    ) |> factor(levels = risk_lvls)
  ) |>
  count(source, risk_category, name = "count") |>
  group_by(source) |>
  mutate(
    percentage = 100 * count / sum(count),
    total = sum(count)
  ) |>
  ungroup() |>
  mutate(text_color = if_else(risk_category %in% risk_lvls[1:2], "black", "white"))

summary_pct <- risk_data |>
  summarise(
    above_15 = sum(percentage[risk_category == "Above EPA Action Level (>15 ppb)"]),
    above_10 = sum(percentage[risk_category %in% c(
      "Concerning (10–15 ppb)",
      "Above EPA Action Level (>15 ppb)"
    )]),
    .by = source
  ) |>
  arrange(source)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get basic theme colors
colors <- get_theme_colors(
  palette = c(
    "Zero Detection" = "#F5E6D3",
    "Trace Levels (>0–5 ppb)" = "#D4A574",
    "Elevated (5–10 ppb)" = "#A67C52",
    "Concerning (10–15 ppb)" = "#7D5A3F",
    "Above EPA Action Level (>15 ppb)" = "#5D3A1A",
    "MDEQ" = "#B8860B",
    "Virginia Tech" = "#8B4513"
  )
)

### |- titles and caption ----
title_text <- str_glue(
  "Flint Lead Contamination: Government vs. Citizen Testing (2015)"
)

subtitle_text <- str_glue(
  "Comparison of water samples collected by MDEQ (n = 71) and Virginia Tech (n = 271)"
)

pct_leq_30 <- combined_data |>
  summarise(p = mean(lead <= 30, na.rm = TRUE) * 100) |>
  pull(p) |>
  round(0)

caption_text <- create_social_caption_02(
    tt_year = 2025,
    tt_week = 44,
    note_text = glue::glue(
        "**Note:** EPA health goal is 0 ppb (no safe level). ",
        "Action level of 15 ppb triggers mandatory intervention.<br><br>",
        "**Chart Guide:**<br>",
        "• **Top:** Share of samples by risk category<br>",
        "• **Bottom:** Cumulative % below each concentration<br><br>",
        "Chart shows 0–30 ppb range ({pct_leq_30}% of samples). ",
        "Some samples exceed 100 ppb (not shown)."
    ),
    source_text = "Loux & Gibson (2018)."
)

### |-  fonts ----
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
    plot.title = element_markdown(
      face = "bold", family = fonts$title, size = rel(1.4),
      color = colors$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_text(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.9), margin = margin(b = 20), hjust = 0
    ),

    ## Grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),

    # Axes
    axis.title = element_text(size = rel(0.9), color = "gray30"),
    axis.text = element_text(color = "gray30"),
    axis.text.y = element_text(size = rel(0.95)),
    axis.ticks = element_blank(),

    # Facets
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(
      face = "bold",
      color = "gray20",
      size = rel(1),
      margin = margin(t = 8, b = 8)
    ),
    panel.spacing = unit(2, "lines"),

    # Legend elements
    legend.position = "plot",
    legend.title = element_text(
      family = fonts$tsubtitle,
      color = colors$text, size = rel(0.8), face = "bold"
    ),
    legend.text = element_text(
      family = fonts$tsubtitle,
      color = colors$text, size = rel(0.7)
    ),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- P1: risk categories plot ----
risk_subtitle <- glue(
  "Percent of samples by contamination level (MDEQ vs. Virginia Tech, 2015)"
)

n_mdeq <- stats_tbl$n[stats_tbl$source == "MDEQ"]
n_vt <- stats_tbl$n[stats_tbl$source == "Virginia Tech"]

p1 <-
  ggplot(risk_data, aes(source, percentage, fill = risk_category)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.8, width = 0.68) +
  # Geoms
  geom_text(
    aes(
      label = ifelse(
        percentage >= 3,
        sprintf("%d samples (%.1f%%)", count, percentage),
        ""
      ),
      color = text_color,
    ),
    position = position_stack(vjust = 0.5), family = "mono",
    size = 3.5, lineheight = 0.9,
    show.legend = FALSE
  ) +
  # Scales
  scale_color_identity(guide = "none") +
  scale_fill_manual(
    values = colors$palette,
    breaks = rev(risk_lvls),
    labels = rev(risk_lvls),
    name = "Lead Concentration Category"
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1), breaks = seq(0, 100, 20), expand = c(0.01, 0)
  ) +
  # Annotate
  annotate("text",
    x = 1, y = 104, label = glue("n = {n_mdeq} samples"),
    fontface = "bold", size = 3.5, color = "gray30"
  ) +
  annotate("text",
    x = 2, y = 104, label = glue("n = {n_vt} samples"),
    fontface = "bold", size = 3.5, color = "gray30"
  ) +
  # Labs
  labs(
    title = "Distribution of Water Samples by Lead Risk Category",
    subtitle = risk_subtitle, x = NULL, y = "Percentage of Samples"
  ) +
  # Theme
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "gray70", linewidth = 0.4),
    legend.title = element_text(face = "bold", size = 9.5),
    legend.text = element_text(size = 8.5, family = "Arial"),
    legend.key.size = unit(0.9, "lines"),
    legend.margin = margin(6, 6, 6, 6),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(lineheight = 0.95)
  )

### |- P2: Cumulative plot ----
ecdf_subtitle <- glue(
  "At 90th percentile: MDEQ = 18 ppb, Virginia Tech = 26.6 ppb (EPA goal: 0 ppb, no safe level)\n",
  "Steeper curves = more samples concentrated at lower lead levels."
)

annot_pts <- stats_tbl |>
  reframe(
    role = if_else(source == "MDEQ", "Government", "Citizen"),
    x = p90,
    y = 0.90,
    lab = glue("{source}: {round(p90,1)} ppb (>15)"),
    .by = source
  )

p2 <-
  ggplot(combined_data, aes(lead, color = source)) +
    # Geoms
    stat_ecdf(linewidth = 1.8, alpha = 0.9) +
    geom_vline(xintercept = 15, linetype = "dashed", color = "#D32F2F", linewidth = 0.8) +
    geom_hline(yintercept = 0.9, linetype = "dotted", color = "gray40", linewidth = 0.7) +
    geom_point(
      data = annot_pts,
      aes(x, y, fill = source, color = source),
      shape = 21, size = 3.5, stroke = 1.3, show.legend = FALSE
    ) +
    geom_label_repel(
      data = annot_pts, aes(x, y, label = lab, color = source),
      nudge_y = c(-0.07, -0.07), label.padding = unit(0.15, "lines"),
      label.size = 0, show.legend = FALSE, seed = 44
    ) +
    # Annotate
    annotate("text",
      x = 16, y = 0.55,
      label = "EPA Action Level:\n15 ppb (requires\nintervention)",
      color = "#D32F2F", size = 3.5, hjust = 0, fontface = "bold", family = "mono"
    ) +
    annotate("text",
      x = 0.5, y = 0.95,
      label = "90th Percentile\n(10% of samples above this line)",
      color = "gray30", size = 3.2, hjust = 0, family = "mono"
    ) +
    # Scales
    scale_color_manual(
      values = colors$palette,
      breaks = c("MDEQ", "Virginia Tech"),
      labels = c(
        glue("{source_labels['MDEQ']} (n={n_mdeq})"),
        glue("{source_labels['Virginia Tech']} (n={n_vt})")
      ),
      name = "Data Source"
    ) +
    scale_fill_manual(values = colors$palette, guide = "none") +
    scale_x_continuous(
      trans = "sqrt", limits = c(0, 30), breaks = seq(0, 30, 5),
      expand = c(0.01, 0)
    ) +
    scale_y_continuous(
      labels = label_percent(accuracy = 1),
      breaks = seq(0, 1, 0.1), expand = c(0.01, 0)
    ) +
    # Labs
    labs(
      title = "Cumulative Distribution of Lead Levels in Water Samples",
      subtitle = ecdf_subtitle,
      x = "Lead Concentration (parts per billion)",
      y = "Cumulative Percentage of Samples"
    ) +
    guides(color = guide_legend(
      title = "Data Source",
      override.aes = list(linetype = 1, linewidth = 1.8)
    )) +
    # Theme
    theme(
      legend.position = "inside",
      legend.position.inside = c(1.35, 0.15),
      legend.justification = c("right", "bottom"),
      legend.background = element_rect(fill = "white", color = "gray70", linewidth = 0.4),
      legend.title = element_text(face = "bold", size = 9.5),
      legend.text = element_text(size = 8.5, family = "Arial"),
      legend.key.size = unit(1, "lines"),
      legend.margin = margin(6, 6, 6, 6),
    )

### |- Combined plot ----
combined_plots <- p1 / p2

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = rel(1.85),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.15,
        margin = margin(t = 8, b = 5)
      ),
      plot.subtitle = element_text(
        size = rel(0.95),
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.88),
        lineheight = 1.2,
        margin = margin(t = 2, b = 15)
      ),
      plot.caption = element_markdown(
        size = rel(0.7),
        family = "Arial",
        color = colors$caption,
        hjust = 0,
        lineheight = 1.3,
        margin = margin(t = 12, b = 5),
      )
    )
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

# ─ Session info ───────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-10-31
# rstudio  2025.09.1+401 Cucumberleaf Sunflower (desktop)
# pandoc   NA
#
# ─ Packages ───────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# P colorspace     2.1-1    2024-07-26 [?] CRAN (R 4.4.1)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggrepel      * 0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gh             1.4.1    2024-03-28 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P gitcreds       0.1.2    2022-09-08 [?] CRAN (R 4.4.0)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.4.2)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here         * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P httr2          1.0.1    2024-04-01 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P patchwork    * 1.3.0    2024-09-16 [?] CRAN (R 4.4.1)
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.3    2024-09-11 [?] CRAN (R 4.4.2)
# P rappdirs       0.3.3    2021-01-31 [?] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats        * 4.4.0    2024-04-24 [?] local
# stringi        1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite        2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts    1.1.0    2024-05-15 [1] CRAN (R 4.4.0)
# textshaping    0.4.0    2024-05-24 [1] CRAN (R 4.4.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidytuesdayR   1.1.2    2024-09-09 [?] CRAN (R 4.4.2)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.4.3)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr          3.0.2    2024-10-28 [?] CRAN (R 4.4.2)
# P xfun           0.52     2025-04-02 [?] CRAN (R 4.4.3)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# P yaml           2.3.8    2023-12-11 [?] CRAN (R 4.4.0)
#
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
#
# ────────────────────────────────
