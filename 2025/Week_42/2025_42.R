## Challenge: #TidyTuesday 2025 week 42
## Data:      Historic UK Meteorological & Climate Data
## Author:    Steven Ponce
## Date:      2025-10-19

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details,
##       or view source code at: https://github.com/poncest/personal-website/tree/master/R


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Easily Install and Load the 'Tidyverse'
  ggtext,        # Improved Text Rendering Support for 'ggplot2'
  showtext,      # Using Fonts More Easily in R Graphs
  janitor,       # Simple Tools for Examining and Cleaning Dirty Data
  scales,        # Scale Functions for Visualization
  glue,          # Interpreted String Literals
  patchwork      # The Composer of Plots
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 12,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 42)

historic_station_met <- tt$historic_station_met |> clean_names()
station_meta <- tt$station_meta |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(historic_station_met)
glimpse(station_meta)


## 4. TIDY DATA ----

# data prep
met_data <- historic_station_met |>
  left_join(station_meta, by = "station") |>
  mutate(
    date = make_date(year, month, 1),
    decade = floor(year / 10) * 10,
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Autumn"
    ),
    era = case_when(
      year < 1950 ~ "1900–1949",
      year < 2000 ~ "1950–1999",
      TRUE ~ "2000–present"
    )
  )

# P1: Long-term trends across stations
trends <- met_data |>
  filter(year >= 1900) |>
  summarise(
    avg_tmax = mean(tmax, na.rm = TRUE),
    avg_rain = mean(rain, na.rm = TRUE),
    avg_sun = mean(sun, na.rm = TRUE),
    avg_frost = mean(af, na.rm = TRUE),
    .by = year
  ) |>
  pivot_longer(
    starts_with("avg_"),
    names_to  = "variable",
    values_to = "value"
  ) |>
  mutate(
    variable = recode(variable,
      avg_tmax  = "Maximum Temperature",
      avg_rain  = "Monthly Rainfall",
      avg_sun   = "Monthly Sunshine",
      avg_frost = "Air Frost Days"
    ),
    variable = factor(variable,
      levels = c("Maximum Temperature", "Monthly Rainfall", "Monthly Sunshine", "Air Frost Days")
    ),
    variable_label = case_match(
      variable,
      "Maximum Temperature" ~ "Maximum Temperature (°C)",
      "Monthly Rainfall" ~ "Monthly Rainfall (mm)",
      "Monthly Sunshine" ~ "Monthly Sunshine (hours)",
      "Air Frost Days" ~ "Air Frost Days (days)"
    )
  )

last_points <- trends |>
  group_by(variable_label) |>
  slice_max(year, n = 1, with_ties = FALSE)

# P2: Extreme weather events timeline
annual_summary <- met_data |>
  summarise(
    total_rain = sum(rain, na.rm = TRUE),
    max_temp = max(tmax, na.rm = TRUE),
    min_temp = min(tmin, na.rm = TRUE),
    n_stations = dplyr::n(),
    .by = year
  ) |>
  filter(year >= 1900, n_stations >= 12)

# thresholds
thr <- list(
  rain_hi = quantile(annual_summary$total_rain, 0.95, na.rm = TRUE),
  heat_hi = quantile(annual_summary$max_temp, 0.95, na.rm = TRUE),
  cold_lo = quantile(annual_summary$min_temp, 0.05, na.rm = TRUE)
)

extreme_events <- annual_summary |>
  mutate(
    rain_extreme = total_rain > thr$rain_hi,
    heat_extreme = max_temp > thr$heat_hi,
    cold_extreme = min_temp < thr$cold_lo
  ) |>
  pivot_longer(
    ends_with("_extreme"),
    names_to = "event_type",
    values_to = "is_extreme"
  ) |>
  filter(is_extreme) |>
  mutate(
    event_label = factor(
      case_match(
        event_type,
        "rain_extreme" ~ "Extreme Rainfall",
        "heat_extreme" ~ "Extreme Heat",
        "cold_extreme" ~ "Extreme Cold"
      ),
      levels = c("Extreme Heat", "Extreme Rainfall", "Extreme Cold")
    )
  ) |>
  select(year, event_label)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get basic theme colors
colors <- get_theme_colors(
  palette =  list(
      heat        = "#D55E00",  
      cold        = "#1D3557",  
      temp_warm   = "#E69F00",  
      rain        = "#0072B2",  
      sun         = "#F0E442",  
      frost       = "#56B4E9", 
      neutral_dark  = "#2B2D42",
      neutral_mid   = "#8D99AE",
      neutral_light = "#EDF2F4"
  )
)

### |- titles and caption ----
title_text <- str_glue(
    "The UK's Climate Is Warming — and Extremes Are Increasing"
    )

subtitle_text <- str_glue(
    "Summers are getting hotter and hard-freeze years are increasingly rare, while extreme-heat years cluster after ~1990 \n",
    "signaling rising heat risk.\n"
)

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 42,
  source_text = str_glue(
      "Historical monthly data for meteorological stations, via data.gov.uk",
      "<br>Metric = station means by year | Trends = LOESS (span 0.25) | Extremes = annual 95th percentile"
    )
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
    plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.4), color = colors$title, margin = margin(b = 10)),
    plot.subtitle = element_text(face = "italic", family = fonts$subtitle, lineheight = 1.2, color = colors$subtitle, size = rel(0.9), margin = margin(b = 20)),

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
    legend.title = element_text(family = fonts$tsubtitle, 
                                color = colors$text, size = rel(0.8), face = "bold"),
    legend.text = element_text(family = fonts$tsubtitle, 
                               color = colors$text, size = rel(0.7)),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- P1: trends plot ----
p1 <- ggplot(trends, aes(year, value)) +
  geom_line(color = colors$palette$neutral_mid, linewidth = 0.4, alpha = 0.6) +
  # Geoms
  geom_smooth(aes(color = variable), method = "loess", linewidth = 1.8, se = TRUE, alpha = 0.15, span = 0.25) +
  geom_point(
    data = last_points, aes(year, value),
    inherit.aes = FALSE, size = 2.2, color = "grey20"
  ) +
  # Scales
  scale_color_manual(
    values = c(
      "Maximum Temperature" = colors$palette$temp_warm,
      "Monthly Rainfall"    = colors$palette$rain,
      "Monthly Sunshine"    = colors$palette$sun,
      "Air Frost Days"      = colors$palette$frost
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(1900, 2020, 40),
    limits = c(1900, 2024)
  ) +
  # Labs
  labs(
    title = "Long-term Trends",
    subtitle = "Smoothed station means with underlying annual variability (1900–present)",
    x = NULL, y = NULL
  ) +
  # Facets
  facet_wrap(~variable_label, scales = "free_y", ncol = 2) +
  # Theme
  theme(
    strip.text = element_text(face = "bold", size = 12, color = colors$palette$neutral_dark, margin = margin(b = 8, t = 4)),
    strip.background = element_rect(fill = colors$palette$neutral_light, color = NA),
    panel.grid.major = element_line(color = colors$palette$neutral_light, linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )

### |- P2: timeline plot ----
p2 <- ggplot(extreme_events, aes(x = year, y = event_label, color = event_label)) +
  # Annotate
  annotate("rect",
    xmin = 1990, xmax = 2022, ymin = -Inf, ymax = Inf,
    fill = alpha(colors$palette$neutral_light, 0.35), color = NA
  ) +
  # Geom
  geom_vline(
    xintercept = c(1990, 2003), linetype = c("dashed", "dotted"),
    linewidth = c(0.5, 0.6), color = alpha(colors$palette$neutral_mid, 0.8)
  ) +
  geom_point(
    size = 4.2, alpha = 0.95, stroke = 0.4,
    position = position_jitter(width = 0.2, height = 0.04, seed = 123)
  ) +
  # Annotate
  annotate("text",
    x = 2006, y = "Extreme Heat", label = "Post-1990 clustering",
    size = 3.2, color = colors$palette$neutral_mid, vjust = -3
  ) +
  annotate("text",
    x = 2003, y = "Extreme Heat", label = "2003 heatwave",
    size = 3.0, color = colors$palette$neutral_mid, vjust = 3.6
  ) +
  # Scales
  scale_color_manual(
    values = c(
      "Extreme Heat" = colors$palette$heat,
      "Extreme Rainfall" = alpha(colors$palette$rain, 0.5),
      "Extreme Cold" = alpha(colors$palette$cold, 0.5)
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(1900, 2020, 20),
    expand = expansion(mult = c(0.02, 0.02)),
    limits = c(1900, 2024)
  ) +
  # Labs
  labs(
    title = "Extreme Weather Events",
    subtitle = "Extreme-heat events surge after ~1990 — clear evidence of a warming climate",
    x = NULL, y = NULL
  ) +
  # Theme
  theme(
    panel.grid.major.y = element_line(color = alpha(colors$palette$neutral_light, 0.9), linewidth = 0.7),
    panel.grid.major.x = element_line(color = alpha(colors$palette$neutral_light, 0.5), linewidth = 0.25),
    panel.grid.minor   = element_blank()
  )

### |- Combined plot ----
p1 / p2 +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(1.55),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_text(
        size = rel(0.85),
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.9),
        lineheight = 1.2,
        margin = margin(t = 0, b = 0)
      ),
      plot.caption = element_markdown(
        size = rel(0.55),
        family = fonts$caption,
        color = colors$caption,
        hjust = 0.5,
        margin = margin(t = 10)
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

# ─ Session info ───────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-10-19
# rstudio  2025.09.1+401 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────
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
# P lattice        0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P Matrix         1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# P mgcv           1.9-1    2023-12-21 [?] CRAN (R 4.4.0)
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P nlme           3.1-164  2023-11-27 [?] CRAN (R 4.4.0)
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
# P splines        4.4.0    2024-04-24 [?] local
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
# ──────────────────────────────────
# > 