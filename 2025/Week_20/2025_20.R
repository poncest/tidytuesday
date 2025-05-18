## Challenge: #TidyTuesday 2025 week 20
## Data:      Water Quality at Sydney Beaches
## Author:    Steven Ponce
## Date:      2025-05-18


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Easily Install and Load the 'Tidyverse'
  ggtext,        # Improved Text Rendering Support for 'ggplot2'
  showtext,      # Using Fonts More Easily in R Graphs
  janitor,       # Simple Tools for Examining and Cleaning Dirty Data
  scales,        # Scale Functions for Visualization
  glue           # Interpreted String Literals
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  8,
  height =  10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 20)

water_quality_raw <- tt$water_quality |> clean_names()
weather_raw <- tt$weather |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(water_quality_raw)
glimpse(weather_raw)
skimr::skim(water_quality_raw)
skimr::skim(weather_raw)


## 4. TIDYDATA ----

### |-  tidy data ----

# Process and categorize raw water quality data
water_quality_processed <- water_quality_raw |>
    mutate(
        bacteria_category = case_when(
            enterococci_cfu_100ml <= 40 ~ "Good (<= 40 CFU)",
            enterococci_cfu_100ml > 40 & enterococci_cfu_100ml <= 200 ~ "Moderate (41-200 CFU)",
            enterococci_cfu_100ml > 200 ~ "Poor (> 200 CFU)",
            TRUE ~ "Unknown"
        ),
        bacteria_category = factor(
            bacteria_category,
            levels = c("Good (<= 40 CFU)", "Moderate (41-200 CFU)", "Poor (> 200 CFU)", "Unknown")
        )
    )

# Summarize water quality at the beach level
beach_reliability <- water_quality_processed |>
    group_by(swim_site, region) |>
    summarise(
        total_samples = n(),
        good_samples = sum(enterococci_cfu_100ml <= 40, na.rm = TRUE),
        reliability_index = good_samples / total_samples * 100,
        .groups = "drop"
    ) |>
    filter(total_samples >= 50) |>
    mutate(
        reliability_rating = case_when(
            is.na(reliability_index) ~ "Unknown",
            reliability_index >= 90 ~ "Excellent (≥90%)",
            reliability_index >= 80 ~ "Very Good (80-89%)",
            reliability_index >= 70 ~ "Good (70-79%)",
            reliability_index >= 60 ~ "Moderate (60-69%)",
            reliability_index < 60 ~ "Needs Improvement (<60%)"
        ),
        reliability_rating = factor(
            reliability_rating,
            levels = c(
                "Excellent (≥90%)", "Very Good (80-89%)", "Good (70-79%)",
                "Moderate (60-69%)", "Needs Improvement (<60%)"
            )
        )
    )

# Classify beaches and select top/bottom per type
beach_by_type <- beach_reliability |>
    mutate(
        beach_type = case_when(
            grepl("Harbour|Harbor|Bay|River|Cove", swim_site) ~ "Harbor/Bay/River",
            grepl("Beach|Ocean", swim_site) ~ "Ocean Beach",
            TRUE ~ "Other"
        )
    ) |>
    group_by(beach_type) |>
    mutate(rank_in_type = min_rank(desc(reliability_index))) |>
    filter(rank_in_type <= 5 | rank_in_type > n() - 5) |>
    mutate(
        status = if_else(reliability_index >= 80, "Meeting Standard", "Below Standard"),
        short_name = swim_site |>
            str_replace(" Beach$", "") |>
            str_replace(" Harbour$", "") |>
            str_replace(" Bay$", "") |>
            str_replace(" Baths$", "") |>
            str_replace(" Reserve$", "") |>
            str_replace(" Pool$", ""),
        short_name = reorder(short_name, reliability_index)
    )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = c(
        "Meeting Standard" = "#1b9e77", 
        "Below Standard" = "#d95f02"
    )
)

### |-  titles and caption ----
title_text <- str_glue("Sydney Beaches: Water Quality Reliability")

subtitle_text <- str_glue(
    "Reliability Index: % of samples meeting 'good' standards (≤ 40 CFU/100ml)\n",
    "80% reliability is the recommended threshold for safe swimming\n",
    "Showing top and bottom 5 beaches by type"
)

# Create caption
caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 20,
  source_text =  "BeachwatchNSW, Open-Meteo"
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
    # Axis elements
    axis.title = element_text(color = colors$text, face = "bold", size = rel(0.8)),
    axis.text = element_text(color = colors$text, size = rel(0.7)),

    # Grid elements
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "red", linewidth = 0.05),

    # Legend elements
    legend.position = "plot",
    legend.direction = "horizontal",
    legend.title = element_text(family = fonts$text, size = rel(0.8), face = "bold"),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),

    # Style facet labels
    strip.text = element_text(size = rel(0.75), face = "bold",
                              color = colors$title, margin = margin(b = 5, t = 5)
    ),

    # Add spacing
    panel.spacing = unit(1.1, "lines"),
    strip.background = element_rect(fill = "#e0e0e0", color = NA),

    # Plot margins
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
  )
)

# Set theme
theme_set(weekly_theme)

# Final plot -----
ggplot(beach_by_type, aes(x = reliability_index, y = short_name)) +
  # Geoms
  geom_col(aes(fill = status), width = 0.7) +
  geom_text(aes(label = sprintf("%d%%", round(reliability_index))),
    hjust = -0.2,
    size = 3.5
  ) +
  geom_text(
      data = tibble(
          beach_type = "Harbor/Bay/River",
          status = c("Meeting Standard", "Below Standard"),
          x = 95,
          y = c(7, 4),  
          label = c("Meeting Standard", "Below Standard")
      ),
      aes(x = x, y = y, label = label, color = status),
      hjust = 0,
      size = 4,
      fontface = "bold",
      inherit.aes = FALSE
  ) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "gray40", linewidth = 0.3) +

  # Scales
  scale_fill_manual(values = colors$palette) +
  scale_color_manual(values = colors$palette) +
  scale_x_continuous(
    limits = c(0, 125),
    breaks = c(0, 20, 40, 60, 80, 100),
    labels = c("0%", "20%", "40%", "60%", "80%", "100%")
  ) +
  # Facets
  facet_wrap(~beach_type,
    scales = "free_y", ncol = 1,
    labeller = labeller(beach_type = c(
      "Harbor/Bay/River" = "Harbor/Bay/River Locations",
      "Ocean Beach" = "Ocean Beaches",
      "Other" = "Other Swimming Locations"
    ))
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = NULL,
    fill = "Status",
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.8),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 15)
    ),
    plot.subtitle = element_text(
      size = rel(0.85),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 1.2,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.6),
      family = fonts$caption,
      color = colors$caption,
      hjust = 0.5,
      margin = margin(t = 10)
    ),
    legend.key = element_rect(fill = NA),
    strip.text = element_markdown(
      lineheight = 1.2,
      padding = margin(5, 5, 5, 5)
    )
  ) 
  

# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-05-18
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# P digest         0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P evaluate       0.23     2023-11-01 [?] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
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
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P httr2          1.0.1    2024-04-01 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.50     2025-03-16 [?] RSPM
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.3    2024-09-11 [?] CRAN (R 4.4.2)
# P rappdirs       0.3.3    2021-01-31 [?] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr          2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
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
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
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
# ────────────────────────────────────────────────────────────────
# > 