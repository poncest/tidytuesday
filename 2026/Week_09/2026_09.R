## Challenge: #TidyTuesday 2026 week 09
## Data:      Golem Grad Tortoise Data
## Author:    Steven Ponce
## Date:      2026-03-03

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, 
    scales, glue, patchwork, binom
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 9,
    height = 9,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 09)
clutch_size_raw <- tt$clutch_size_cleaned |> clean_names()
tortoise_body_raw <- tt$tortoise_body_condition_cleaned |> clean_names()
rm(tt)



## 3. EXAMINING THE DATA ----
glimpse(clutch_size_raw)
glimpse(tortoise_body_raw)


## 4. TIDY DATA ----

### |- locality label mapping ----
loc_map <- c(
  "Plateau" = "Island",
  "Konjsko" = "Mainland",
  "Beach"   = "Island (Beach)"
)

### |- clean tortoise body data ----
tortoise_body <- tortoise_body_raw |>
  mutate(
    sex          = str_to_upper(sex),
    locality     = str_to_title(locality),
    season       = str_to_title(season),
    locality_lbl = recode(locality, !!!loc_map),
    locality_lbl = factor(locality_lbl, levels = c("Mainland", "Island"))
  )

### |- clean clutch data ----
clutch_size <- clutch_size_raw |>
  mutate(
    locality     = str_to_title(locality),
    locality_lbl = recode(locality, !!!loc_map),
    locality_lbl = factor(locality_lbl, levels = c("Mainland", "Island"))
  )

## ── PANEL A: % Female Captures Over Time ----
pa_data <- tortoise_body |>
    filter(
        sex %in% c("M", "F"),
        locality == "Plateau"
    ) |> # island only — matches headline claim
    count(year, sex) |>
    pivot_wider(names_from = sex, values_from = n, values_fill = 0) |>
    mutate(
        total    = M + F,
        ci       = binom::binom.wilson(F, total),
        pct_f    = F / total,
        ci_lower = ci$lower,
        ci_upper = ci$upper
    )

## ── PANEL B: Female Body Condition Index — Island vs. Mainland ----
pb_data <- tortoise_body |>
    filter(
        sex == "F",
        locality %in% c("Plateau", "Konjsko"),
        !is.na(body_condition_index)
    )

# summary stats for annotation
pb_stats <- pb_data |>
    group_by(locality_lbl) |>
    summarise(
        med = median(body_condition_index, na.rm = TRUE),
        n = n(),
        .groups = "drop"
    )

## ── PANEL C: Clutch Size by Locality ----
pc_data <- clutch_size |>
    filter(
        !is.na(eggs),
        locality %in% c("Beach", "Konjsko", "Plateau")
    ) |>
    # consolidate island localities
    mutate(
        locality_lbl = case_when(
            locality_lbl == "Mainland" ~ "Mainland",
            TRUE ~ "Island"
        ),
        locality_lbl = factor(locality_lbl, levels = c("Mainland", "Island"))
    )

pc_stats <- pc_data |>
    group_by(locality_lbl) |>
    summarise(
        mean_eggs = mean(eggs, na.rm = TRUE),
        med_eggs  = median(eggs, na.rm = TRUE),
        n         = n(),
        .groups   = "drop"
    )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
      "accent" = "#C0392B",   
      "neutral" = "gray70",    
      "ribbon"  = "#C0392B"   
    )
)

### |- titles and caption ----
title_text    <- str_glue("Too Many Males, Too Few Females")

subtitle_text <- str_glue(
    "Island captures show a persistent male bias; ",
    "females are in poorer condition and lay smaller clutches."
)

caption_text <- create_social_caption(
    tt_year      = 2026,
    tt_week      = 09,
    source_text  = "Bertolero et al. (2026) · Ecology Letters · #TidyTuesday"
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
    plot.title = element_text(
      face = "bold", family = fonts$title, size = rel(1.1),
      color = colors$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_markdown(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.7), margin = margin(b = 20), hjust = 0
    ),

    # Grid
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.25),

    # Axes
    axis.title = element_text(size = rel(0.6), color = "gray30"),
    axis.text = element_text(color = "gray30"),
    axis.text.y = element_text(size = rel(0.6)),
    axis.ticks = element_blank(),

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

## ── PANEL A: % Female Captures Over Time ----
pa <- ggplot(pa_data, aes(x = year, y = pct_f)) +
  # Geom
  geom_hline(
    yintercept = 0.5, linetype = "dashed",
    color = "gray80", linewidth = 0.4
  ) +
  annotate("text",
    x = 2008, y = 0.52, label = "50% parity",
    size = 2.6, color = "gray65", hjust = 0, fontface = "italic"
  ) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
    fill = colors$palette$ribbon, alpha = 0.15
  ) +
  geom_line(color = colors$palette$accent, linewidth = 1) +
  geom_point(
    color = colors$palette$accent, size = 2.2, shape = 21,
    fill = "white", stroke = 1.2
  ) +
  # Scales
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.55),
    breaks = seq(0, 0.5, 0.1)
  ) +
  scale_x_continuous(breaks = seq(2008, 2023, 3)) +
  # Labs
  labs(
    title    = "A · Persistent male bias in captures",
    subtitle = "Island (Plateau) captures only · Wilson 95% CI shaded",
    x        = NULL,
    y        = "% of captures that are female"
  )

## ── PANEL B: Female Body Condition Index — Island vs. Mainland ----
pb <- ggplot(
  pb_data,
  aes(
    x = locality_lbl, y = body_condition_index,
    fill = locality_lbl
  )
) +
  # Geoms
  geom_violin(alpha = 0.55, color = NA, width = 0.85) +
  geom_boxplot(
    width = 0.12, fill = "white", color = "gray40",
    outlier.shape = NA, linewidth = 0.5
  ) +
  geom_text(
    data = pb_stats,
    aes(
      x = locality_lbl,
      y = max(pb_data$body_condition_index, na.rm = TRUE) + 0.3,
      label = glue("median: {round(med, 1)}")
    ),
    size = 2.6, color = "gray45", fontface = "italic",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = pb_stats,
    aes(
      x = locality_lbl,
      y = min(pb_data$body_condition_index, na.rm = TRUE) - 0.4,
      label = glue("n = {comma(n)}")
    ),
    size = 2.8, color = "gray50", inherit.aes = FALSE
  ) +
  # Scales
  scale_fill_manual(values = c("Mainland" = colors$palette$neutral, "Island" = colors$palette$accent)) +
  scale_y_continuous(
    n.breaks = 5,
    limits = c(
      min(pb_data$body_condition_index, na.rm = TRUE) - 0.6,
      max(pb_data$body_condition_index, na.rm = TRUE) + 0.7
    )
  ) +
  # Labs
  labs(
    title    = "B · Island females in poorer condition",
    subtitle = "Body condition index · Females only",
    x        = NULL,
    y        = "Body condition index"
  )

## ── PANEL C: Clutch Size by Locality ----
pc <- ggplot(pc_data, aes(x = eggs, y = locality_lbl, color = locality_lbl)) +
  # Geoms
  geom_jitter(
    size = 3, alpha = 0.75,
    position = position_jitter(height = 0.12, seed = 42)
  ) +
  geom_point(
    data = pc_stats,
    aes(x = mean_eggs, y = locality_lbl, color = locality_lbl),
    shape = 23, size = 5.5, fill = "white",
    stroke = 1.5, inherit.aes = FALSE
  ) +
  geom_text(
    data = pc_stats,
    aes(
      x = mean_eggs, y = locality_lbl,
      label = glue("mean: {round(mean_eggs, 1)}")
    ),
    nudge_y = 0.28, size = 2.8, color = "gray40",
    fontface = "italic", inherit.aes = FALSE
  ) +
  geom_text(
    data = pc_stats,
    aes(
      x = 13.8, y = locality_lbl,
      label = glue("n = {n}")
    ),
    size = 2.8, color = "gray50", hjust = 1, inherit.aes = FALSE
  ) +
  # Scales
  scale_color_manual(values = c("Mainland" = colors$palette$neutral, "Island" = colors$palette$accent)) +
  scale_x_continuous(breaks = 2:14, limits = c(2, 14)) +
  scale_y_discrete() +
  # Labs
  labs(
    title    = "C · Island females lay fewer eggs",
    subtitle = "Eggs per clutch · Diamond = mean per group",
    x        = "Eggs per clutch",
    y        = NULL
  )

## ── Combined Plots ----
# combined_plots <- 
(pa / pb / pc) +
    plot_annotation(
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text,
        theme    = theme(
            plot.title = element_markdown(
                size = rel(1.4),
                family = fonts$title,
                face = "bold",
                color = colors$title,
                lineheight = 1.15,
                margin = margin(t = 0, b = 5)
            ),
            plot.subtitle = element_markdown(
                size = rel(0.8),
                family = fonts$subtitle,
                color = alpha(colors$subtitle, 0.88),
                lineheight = 1.5,
                margin = margin(t = 5, b = 5)
            ),
            plot.caption = element_markdown(
                size = rel(0.5),
                family = fonts$subtitle,
                color = colors$caption,
                hjust = 0,
                lineheight = 1.4,
                margin = margin(t = 20, b = 5)
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

# ─ Session info ─────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-03
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# binom        * 1.1-1.1  2022-05-02 [1] CRAN (R 4.4.3)
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
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
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
# P tidytuesdayR   1.2.1    2025-04-29 [?] CRAN (R 4.4.3)
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
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────