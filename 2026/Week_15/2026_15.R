
## Challenge: #TidyTuesday 2026 week 15
## Data:      Bird Sightings at Sea
## Author:    Steven Ponce
## Date:      2026-04-11

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, patchwork, binom
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 10,
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
tt <- tidytuesdayR::tt_load(2026, week = 15)
beaufort_scale <- tt$beaufort_scale |> clean_names()
birds <- tt$birds |> clean_names()
sea_states <- tt$sea_states |> clean_names()
ships <- tt$ships |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(beaufort_scale)
glimpse(birds)
glimpse(sea_states)
glimpse(ships)


## 4. TIDY DATA ----

# Join birds to ship conditions via record_id
# Filter sentinel count values (99999 = "over 100,000")
# Only use full 10-minute census records for comparability

bird_ship <- birds |>
    filter(!is.na(record_id)) |>
    inner_join(ships, by = "record_id") |>
    filter(
        census_method == "full",
        count < 99999,
        !is.na(sea_state_class),
        !is.na(wind_speed_class)
    )

### |- Panel A: Environmental Envelope ----
# Collapse 12 Beaufort classes into 5 meaningful bins for readability.

beaufort_bins <- tibble::tribble(
    ~wind_speed_class, ~wind_bin, ~wind_bin_order,
    0,  "Calm\n(Bf 0–1)",     1,
    1,  "Calm\n(Bf 0–1)",     1,
    2,  "Light\n(Bf 2–3)",    2,
    3,  "Light\n(Bf 2–3)",    2,
    4,  "Moderate\n(Bf 4–5)", 3,
    5,  "Moderate\n(Bf 4–5)", 3,
    6,  "Strong\n(Bf 6–7)",   4,
    7,  "Strong\n(Bf 6–7)",   4,
    8,  "Gale+\n(Bf 8–11)",   5,
    9,  "Gale+\n(Bf 8–11)",   5,
    10, "Gale+\n(Bf 8–11)",   5,
    11, "Gale+\n(Bf 8–11)",   5
)

envelope_data <- bird_ship |>
    distinct(record_id, wind_speed_class, sea_state_class) |>
    left_join(beaufort_bins, by = "wind_speed_class") |>
    left_join(
        sea_states |> select(sea_state_class, sea_state_description),
        by = "sea_state_class"
    ) |>
    count(wind_bin, wind_bin_order, sea_state_class, sea_state_description,
          name = "n_obs") |>
    mutate(
        wind_label = fct_reorder(wind_bin, wind_bin_order),
        sea_label  = glue("SS {sea_state_class}: {str_to_title(sea_state_description)}"),
        sea_label  = fct_reorder(sea_label, sea_state_class)
    )

### |- Panel B: Feeding Rate × Sea State ----
# For each sea state class: compute feeding rate with Wilson CI
# Feeding rate = proportion of bird observations where feeding = TRUE
# Restrict to sea states with >= 30 observations for reliability

feeding_rate_data <- bird_ship |>
    filter(!is.na(feeding)) |>
    group_by(sea_state_class) |>
    summarise(
        n_obs      = n(),
        n_feeding  = sum(feeding, na.rm = TRUE),
        .groups    = "drop"
    ) |>
    filter(n_obs >= 30) |>
    # Wilson confidence intervals (statistically appropriate for proportions)
    mutate(
        ci    = map2(n_feeding, n_obs, ~ binom.wilson(.x, .y)),
        rate  = map_dbl(ci, ~ .x$mean),
        lower = map_dbl(ci, ~ .x$lower),
        upper = map_dbl(ci, ~ .x$upper)
    ) |>
    left_join(
        sea_states |> select(sea_state_class, sea_state_description),
        by = "sea_state_class"
    ) |>
    # Complete sea states 0-6 after CI computation so ghost rows never hit binom.wilson.
    # Missing states join sea_states for labels, then bind cleanly as NA rows.
    (\(computed) {
        present <- unique(computed$sea_state_class)
        missing <- setdiff(0:6, present)
        if (length(missing) > 0) {
            ghost <- tibble::tibble(sea_state_class = missing) |>
                left_join(
                    sea_states |> select(sea_state_class, sea_state_description),
                    by = "sea_state_class"
                ) |>
                mutate(
                    n_obs = 0L, n_feeding = 0L,
                    ci = list(NULL), rate = NA_real_, lower = NA_real_, upper = NA_real_
                )
            dplyr::bind_rows(computed, ghost)
        } else {
            computed
        }
    })() |>
    mutate(
        sea_label = glue("SS {sea_state_class}\n{str_to_title(sea_state_description)}"),
        sea_label = fct_reorder(sea_label, sea_state_class),
        # na.rm = TRUE so ghost NA rows don't poison max()
        is_peak   = !is.na(rate) & rate == max(rate, na.rm = TRUE)
    )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = list(
        "accent"   = "#1B6CA8",   
        "low"      = "#D9EAF7",  
        "high"     = "#0D3B6E",   
        "peak"     = "#722F37",   
        "gray"     = "gray75",
        "bg"       = "#F8F9FA"
    )
)

### |- titles and caption ----
title_text    <- str_glue("Rough Seas Suppress Feeding — They Don't Enhance It")

subtitle_text <- str_glue(
    "Seabird feeding rates **decline sharply as sea states worsen** — calm-to-slight conditions<br>",
    "yield the highest foraging activity. Surveys clustered in moderate wind and wave conditions<br>",
    "*(Panel A)*, yet feeding rates *(Panel B)* decline steadily beyond slight conditions — suggesting<br>",
    "birds feed opportunistically, not because rough water forces prey to the surface."
)

caption_text <- create_social_caption(
    tt_year = 2026,
    tt_week = 15,
    source_text = "Te Papa Tongarewa — Museum of New Zealand"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        # Panel
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
        panel.grid.minor   = element_blank(),
        
        # Axes
        axis.ticks = element_blank(),
        axis.text = element_text(family = fonts$text, size = 7.5, color = "gray40"),
        axis.title = element_text(family = fonts$text, size = 8.5, color = "gray30"),
        
        # Strip (for any facets)
        strip.text  = element_text(family = fonts$text, size = 8, face = "bold"),
        
        # Legend
        legend.position = "right",
        legend.title = element_text(family = fonts$text, size = 7.5),
        legend.text = element_text(family = fonts$text, size = 7),
        legend.key.size = unit(0.4, "cm")
    )
)

theme_set(weekly_theme)

### |-  Panel A: Environmental Envelope heatmap ----

p_envelope <- envelope_data |>
  ggplot(aes(x = wind_label, y = sea_label, fill = n_obs)) +
  # Geoms
  geom_tile(color = "white", linewidth = 0.6) +
  scale_fill_gradient(
    low = colors$palette$low,
    high = colors$palette$high,
    name = "Observations",
    labels = comma,
    guide = guide_colorbar(barwidth = 0.4, barheight = 4)
  ) +
  # Scales
  scale_x_discrete(position = "bottom") +
  # Labs
  labs(
    title = "Panel A — Survey Conditions",
    subtitle = "Surveys concentrate in slight–moderate seas with light–moderate winds — rougher conditions were rarely sampled",
    x = "Wind Condition",
    y  = "Sea State"
  ) +
  # Them
  theme(
    plot.title = element_text(
      family = fonts$title, size = 10, face = "bold", color = "gray20",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      family = fonts$text, size = 7.5, color = "gray45",
      margin = margin(b = 8)
    ),
    axis.text.x = element_text(size = 7.5, lineheight = 1.3),
    axis.text.y = element_text(size = 7.5),
    panel.grid = element_blank()
  )

### |-  Panel B: Feeding Rate × Sea State ----

# Identify peak for annotation
peak_row <- feeding_rate_data |> filter(is_peak)

p_feeding <- feeding_rate_data |>
  ggplot(aes(x = sea_label, y = rate)) +

  # Geoms
  geom_pointrange(
    aes(ymin = lower, ymax = upper, color = is_peak),
    linewidth = 0.7,
    size = 0.2,
    show.legend = FALSE
  ) +
  geom_hline(
    yintercept = peak_row$rate,
    color      = "gray80",
    linewidth  = 0.4,
    linetype   = "dashed"
  ) +
  geom_text(
    aes(y = lower - 0.007, label = glue("n={comma(n_obs)}")),
    size = 2.3,
    color = "gray55",
    family = fonts$text
  ) +
  # Annotate
  annotate(
    "text",
    x = as.character(peak_row$sea_label),
    y = peak_row$upper + 0.009,
    label = glue("Peak feeding\n({percent(peak_row$rate, accuracy = 0.1)})"),
    size = 2.8,
    color = colors$palette$peak,
    family = fonts$text,
    lineheight = 1.1,
    vjust = 0
  ) +
  # Scales
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    expand = expansion(mult = c(0.14, 0.18))
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_color_manual(
    values = c("FALSE" = colors$palette$accent, "TRUE" = colors$palette$peak)
  ) +
  # Labs
  labs(
    title = "Panel B — Feeding Rate by Sea State",
    subtitle = "Proportion of observations with active feeding · Wilson 95% CI · full censuses only · n ≥ 30",
    x = "Sea State",
    y = "Feeding Rate"
  ) +
  # Theme
  theme(
    plot.title = element_text(
      family = fonts$title, size = 10, face = "bold", color = "gray20",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      family = fonts$text, size = 7.5, color = "gray45",
      margin = margin(b = 8)
    ),
    axis.text.x = element_text(size = 7.5, lineheight = 1.3),
    panel.grid.major.x = element_blank()
  )

### |-  Combined layout ----
combined_plot <- p_envelope / p_feeding +
  plot_layout(heights = c(1, 1.4)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        family = fonts$title,
        size = 18,
        face = "bold",
        color  = "gray10",
        lineheight = 1.1,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_markdown(
        family = fonts$text,
        size = 10,
        color = "gray35",
        lineheight = 1.5,
        margin = margin(b = 16)
      ),
      plot.caption = element_markdown(
        family = fonts$text,
        size = 7,
        color = "gray55",
        hjust = 0,
        margin = margin(t = 12)
      ),
      plot.margin = margin(20, 20, 12, 20),
      plot.background = element_rect(fill = colors$palette$bg, color = NA),
      panel.background = element_rect(fill = colors$palette$bg, color = NA)
    )
  )

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

# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-04-11
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.3.1)
# binom        * 1.1-1.1  2022-05-02 [1] CRAN (R 4.3.3)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.5    2025-04-23 [1] CRAN (R 4.3.1)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.3.1)
# dplyr        * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.3.1)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.3.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.3.1)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.3.1)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.3.1)
# ggrepel      * 0.9.8    2026-03-17 [1] CRAN (R 4.3.1)
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
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# rappdirs       0.3.4    2026-01-17 [1] CRAN (R 4.3.1)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.3.3)
# rlang          1.1.7    2026-01-09 [1] CRAN (R 4.3.1)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.3.1)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.3.1)
# rsvg           2.6.2    2025-03-23 [1] CRAN (R 4.3.3)
# S7             0.2.0    2024-11-07 [1] CRAN (R 4.3.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.3.1)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# skimr        * 2.2.2    2026-01-10 [1] CRAN (R 4.3.1)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.3)
# P stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.3.3)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.3.1)
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
# 
# ─────────────────────────────────────────────────────────────────────────