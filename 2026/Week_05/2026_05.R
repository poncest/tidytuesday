## Challenge: #TidyTuesday 2026 week 05
## Data:      Edible Plants Database
## Author:    Steven Ponce
## Date:      2026-02-01

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, 
    scales, glue, patchwork, ineq, ggrepel
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 10,
    height = 7,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 05)
edible_plants <- tt$edible_plants |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(edible_plants)


## 4. TIDY DATA ----

# Helper: parse numeric ranges like "55-75", "55 – 75"...
parse_range <- function(x, return = c("midpoint", "lower", "upper")) {
  return <- match.arg(return)

  map_dbl(x, function(val) {
    if (is.na(val) || val == "") {
      return(NA_real_)
    }

    val <- str_trim(val)
    val <- str_to_lower(val)

    # Standardize separators
    val <- str_replace_all(val, c("–" = "-", "—" = "-", " to " = "-", "–" = "-"))
    val <- str_replace_all(val, "[^0-9\\.-]+", "") # keep digits, dot, dash

    # Handle "~75" style after stripping non-numeric (becomes "75")
    if (str_detect(val, "^\\d+\\.?\\d*\\+$")) {
      num <- as.numeric(str_remove(val, "\\+$"))
      return(case_when(
        return == "midpoint" ~ num,
        return == "lower" ~ num,
        return == "upper" ~ NA_real_
      ))
    }

    if (str_detect(val, "^\\d+\\.?\\d*-$")) {
      num <- as.numeric(str_remove(val, "-$"))
      return(case_when(
        return == "midpoint" ~ num,
        return == "lower" ~ num,
        return == "upper" ~ NA_real_
      ))
    }

    # Normal range "55-75"
    if (str_detect(val, "-")) {
      parts <- str_split(val, "-", n = 2)[[1]] |>
        str_trim() |>
        as.numeric()

      parts <- parts[!is.na(parts)]

      if (length(parts) == 2) {
        lo <- parts[1]
        hi <- parts[2]
        return(case_when(
          return == "midpoint" ~ mean(c(lo, hi)),
          return == "lower" ~ lo,
          return == "upper" ~ hi
        ))
      }

      if (length(parts) == 1) {
        return(parts[1])
      }
    }

    suppressWarnings(as.numeric(val))
  })
}

plants_clean <- edible_plants |>
  mutate(
    cultivation_clean = case_when(
      str_to_lower(cultivation) %in% c("legume") ~ "Legumes",
      str_to_lower(cultivation) %in% c("brassica", "brassicas") ~ "Brassicas",
      str_to_lower(cultivation) %in% c("allium") ~ "Alliums",
      str_to_lower(cultivation) %in% c("cucurbit") ~ "Cucurbits",
      str_to_lower(cultivation) %in% c("solanaceae", "solanum") ~ "Nightshades",
      str_to_lower(cultivation) %in% c("umbelliferae") ~ "Umbellifers",
      str_to_lower(cultivation) %in% c("lamiaceae") ~ "Herbs",
      str_to_lower(cultivation) %in% c("chenopodiaceae") ~ "Chenopods",
      str_to_lower(cultivation) %in% c("salad") ~ "Salad Greens",
      TRUE ~ "Other"
    ),
    days_harvest_mid = parse_range(days_harvest, "midpoint"),
    days_harvest_low = parse_range(days_harvest, "lower"),
    days_harvest_high = parse_range(days_harvest, "upper"),
    days_germ_mid = parse_range(days_germination, "midpoint")
  )

plot_data <- plants_clean |>
  filter(
    cultivation_clean != "Other",
    !is.na(days_germ_mid),
    !is.na(days_harvest_mid)
  ) |>
  group_by(cultivation_clean) |>
  summarise(
    germ_median = median(days_germ_mid, na.rm = TRUE),
    harvest_median = median(days_harvest_mid, na.rm = TRUE),
    harvest_q25 = quantile(days_harvest_mid, 0.25, na.rm = TRUE),
    harvest_q75 = quantile(days_harvest_mid, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 3) |>
  mutate(
    growing_days = pmax(0, harvest_median - germ_median),
    cult_label = glue("{cultivation_clean} (n={n})"),
    # Longest at top
    cult_label = fct_reorder(cult_label, harvest_median, .desc = TRUE),
    is_anchor = cult_label %in% c(
      "Chenopods (n=3)",
      "Alliums (n=7)"
    )
  )

# Dynamic x-range
x_max <- max(plot_data$harvest_q75, plot_data$harvest_median, na.rm = TRUE)
x_pad <- max(10, round(0.12 * x_max))
x_lim <- c(0, x_max + x_pad)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
      col_germination = "#A5D6A7",
      col_growing = "#2E7D32",
      col_sprout = "#FFC107",
      col_harvest = "#D32F2F",
      col_iqr = "gray80",
      col_grid = "gray85"
  )
)

### |- titles and caption ----
title_text <- "From Seed to Table: How Long Until Harvest?"

subtitle_text <- str_glue(
    "Median time to harvest by plant family. ",
    "<span style='color:{colors$palette$col_germination};'>**Light green**</span> = germination period, ",
    "<span style='color:{colors$palette$col_growing};'>**dark green**</span> = growing period.<br>",
    "<span style='color:{colors$palette$col_sprout};'>**●**</span> Sprouted, ",
    "<span style='color:{colors$palette$col_harvest};'>**●**</span> Harvest ready. ",
    "Gray bars show variability (interquartile range)."
)

caption_text <- create_social_caption(
    tt_year = 2026,
    tt_week = 05,
    source_text = "GROW Observatory Edible Plant Database"
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
      face = "bold", family = fonts$title, size = rel(1.3),
      color = colors$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_markdown(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.8), margin = margin(b = 20), hjust = 0
    ),

    # Grid
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.25),

    # Axes
    axis.title = element_text(size = rel(0.8), color = "gray30"),
    axis.text = element_text(color = "gray30"),
    axis.text.y = element_text(size = rel(0.85)),
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

### |- final plot ----
plot_data |>
    ggplot(aes(y = cult_label)) +
    # Geoms
    geom_vline(
        xintercept = c(30, 60, 90, 120),
        linetype = "dotted",
        color = colors$palette$col_grid,
        linewidth = 0.4, 
        alpha = 0.6
    ) +
    geom_segment(
        aes(x = harvest_q25, xend = harvest_q75, yend = cult_label),
        linewidth = 10,
        color = colors$palette$col_iqr,
        lineend = "round",
        alpha = 0.75
    ) +
    geom_segment(
        aes(x = 0, xend = germ_median, yend = cult_label),
        linewidth = 5,
        color = colors$palette$col_germination,
        lineend = "round"
    ) +
    geom_segment(
        aes(x = germ_median, xend = harvest_median, yend = cult_label),
        linewidth = 5,
        color = colors$palette$col_growing,
        lineend = "round"
    ) +
    geom_point(aes(x = germ_median), size = 4.2, color = colors$palette$col_sprout) +
    geom_point(aes(x = harvest_median), size = 4.2, color = colors$palette$col_harvest) +
    geom_text(
        aes(x = harvest_median, label = glue("{round(harvest_median)} days")),
        hjust = -0.18,
        size = 3.4,
        fontface = "bold",
        color = colors$text,
        family = fonts$subtitle
    ) +
    geom_text(
        aes(
            x = -2,                    
            label = cult_label,
            fontface = if_else(is_anchor, "bold", "plain")
        ),
        hjust = 1,
        family = fonts$subtitle,
        color = "gray30",
        size = 3.5
    ) +
    # Scales
    scale_x_continuous(
        breaks = c(0, 30, 60, 90, 120),
        labels = c("Planted", "1 month", "2 months", "3 months", "4 months"),
        expand = expansion(mult = c(0.02, 0))
    ) +
    scale_y_discrete(NULL, labels = NULL) +
    coord_cartesian(
        xlim = c(-15, x_lim[2]),
        clip = "off"
    ) +
    # Labs
    labs(
        title = title_text,
        subtitle = subtitle_text,
        x = NULL, y = NULL,
        caption = caption_text
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.2),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.15,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8),
      family = "sans",
      color = colors$subtitle,
      lineheight = 1.5,
      margin = margin(t = 5, b = 5)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
      family = fonts$subtitle,
      color = colors$caption,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(t = 10, b = 5)
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

# ─ Session info ────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-02-01
# rstudio  2026.01.0+392 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────
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
# P digest         0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
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
# ineq         * 0.2-13   2014-07-21 [1] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman         0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P patchwork    * 1.3.0    2024-09-16 [?] CRAN (R 4.4.1)
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache        0.16.0   2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3    1.8.2    2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo           1.26.0   2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils        2.12.3   2023-11-18 [?] CRAN (R 4.4.0)
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
# P styler         1.10.3   2024-04-07 [?] CRAN (R 4.4.0)
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
# ───────────────────────────────────────────────────────────────────────────────────────────────
# > 