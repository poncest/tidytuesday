## Challenge: #TidyTuesday 2025 week 25
## Data:      Measles cases across the world
## Author:    Steven Ponce
## Date:      2025-06-24


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  janitor,    # Simple Tools for Examining and Cleaning Dirty Data
  scales,     # Scale Functions for Visualization
  glue,       # Interpreted String Literals
  patchwork,  # The Composer of Plots
  scico,      # Colour Palettes Based on the Scientific Colour-Maps 
  ggrepel     # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  10,
  height =  8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 25)

cases_year <- tt$cases_year |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)

#' Note: The number of cases of measles and rubella officially reported by a
#' WHO Member State is only available by July of each year. If any numbers from
#' this provisional data are quoted, they should be properly sourced with a date
#' (i.e. "provisional data based on monthly data reported to WHO (Geneva) as
#' of June 2025"


## 3. EXAMINING THE DATA ----
glimpse(cases_year)


## 4. TIDYDATA ----

# Data Preparation

country_profiles <- cases_year |>
  group_by(country) |>
  filter(
    n() >= 5, # At least 5 years of data
    sum(measles_total, na.rm = TRUE) >= 10 # Meaningful case numbers
  ) |>
  summarise(
    # Core metrics for MDS
    avg_incidence = mean(measles_incidence_rate_per_1000000_total_population, na.rm = TRUE),
    max_incidence = max(measles_incidence_rate_per_1000000_total_population, na.rm = TRUE),
    cv_incidence = sd(measles_incidence_rate_per_1000000_total_population, na.rm = TRUE) /
      (mean(measles_incidence_rate_per_1000000_total_population, na.rm = TRUE) + 0.001),
    lab_confirmation_rate = mean(measles_lab_confirmed / (measles_total + 0.001), na.rm = TRUE),
    years_with_cases = n(),
    .groups = "drop"
  ) |>
  mutate(
    cv_incidence = pmin(cv_incidence, 5), # Cap CV at reasonable level
    lab_confirmation_rate = pmin(lab_confirmation_rate, 1) # Cap at 100%
  ) |>
  filter(complete.cases(across(avg_incidence:years_with_cases)))


# MDS Analysis

# Prepare MDS matrix with log transformation and scaling
mds_matrix <- country_profiles |>
  transmute(
    log_avg_incidence = log10(avg_incidence + 1),
    log_max_incidence = log10(max_incidence + 1),
    log_cv_incidence = log10(cv_incidence + 1),
    lab_confirmation_rate,
    years_with_cases
  ) |>
  scale() |>
  as.matrix()

rownames(mds_matrix) <- country_profiles$country

# Perform MDS and calculate quality metrics
mds_result <- cmdscale(dist(mds_matrix), k = 2, eig = TRUE)
variance_explained <- sum(mds_result$eig[1:2]) / sum(abs(mds_result$eig))
stress <- sum((dist(mds_matrix) - dist(mds_result$points))^2) / sum(dist(mds_matrix)^2)

# Plot Data
plot_data <- tibble(
  country = country_profiles$country,
  MDS1 = mds_result$points[, 1],
  MDS2 = mds_result$points[, 2],
  avg_incidence = country_profiles$avg_incidence,
  lab_confirmation_rate = country_profiles$lab_confirmation_rate
)

# Outliers Data
outlier_datasets <- list(
  top_left = plot_data |> filter(MDS1 <= -3 & MDS2 > 1),
  bottom_right = plot_data |> filter(MDS1 > 2.8 & MDS2 > -1)
)

# Shared Plot Styling & Annotations
base_label_style <- list(
  size = 3.2,
  fontface = "bold",
  color = "black",
  bg.color = "white",
  bg.r = 0.12,
  box.padding = 0.6,
  point.padding = 0.4,
  segment.color = "grey50",
  segment.size = 0.3,
  segment.alpha = 0.8,
  max.overlaps = 12,
  seed = 42,
  min.segment.length = 0.2
)

# Annotation tibble ----
zone_annotations <- tibble(
  zone = c("crisis", "success"),
  xmin = c(-3.1, 0.9),
  xmax = c(-1.9, 2.1),
  ymin = c(1.8, -1.7),
  ymax = c(3.2, -0.7),
  x_text = c(-2.5, 1.5),
  y_text = c(2.5, -1.2),
  label = c(
    "CRISIS ZONE\nHigh burden +\nPoor surveillance",
    "SUCCESS ZONE\nLow burden +\nGood surveillance"
  ),
  color = c("darkred", "darkgreen")
)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = NULL
)

### |-  titles and caption ----
title_text <- str_glue("Global Measles Surveillance: Quality vs Disease Burden")

subtitle_text <- str_glue(
  "Analysis of {nrow(plot_data)} countries reveals surveillance systems under pressure. WHO Provisional Measles Data (as of June 2025)\n",
  "Countries cluster by outbreak characteristics and surveillance capabilities\n\n",
  "Note: Countries positioned closer together have similar outbreak patterns and surveillance capabilities\n",
  "All data is provisional - official annual figures available July 2025\n",
  "Model quality: {round(variance_explained * 100, 1)}% variance explained, Stress = {round(stress, 3)}"
)

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 25,
  source_text =  "WHO Provisional monthly measles and rubella data"
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
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    axis.title.x = element_text(color = colors$text, face = "bold", size = rel(0.8), margin = margin(t = 15)),
    axis.title.y = element_text(color = colors$text, face = "bold", size = rel(0.8), margin = margin(r = 10)),

    # Grid elements
    panel.grid.major.y = element_line(color = "gray50", linewidth = 0.05),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    # Legend elements
    legend.position = "plot",
    legend.title = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.8), face = "bold"),
    legend.text = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.7)),

    # Plot margins
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
  )
)

# Set theme
theme_set(weekly_theme)

# Initial Plot ----
p <- ggplot(plot_data, aes(x = MDS1, y = MDS2)) +

  # Geoms
  geom_hline(yintercept = 0, color = "grey90", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey90", linewidth = 0.25) +
  geom_point(aes(color = lab_confirmation_rate, size = avg_incidence),
    alpha = 0.85, stroke = 0.2
  ) +
  # Scales
  scale_color_scico(
    name = "Lab Confirmation\nRate",
    labels = scales::percent_format(accuracy = 1),
    palette = "vik",
    direction = -1,
    trans = "identity",
    breaks = c(0, 0.25, 0.50, 0.75, 1.0),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 10,
      barheight = 1,
      nbin = 100
    )
  ) +
  scale_size_continuous(
    range = c(1.5, 10),
    name = "Average Incidence (per million)",
    trans = "sqrt",
    breaks = c(1, 10, 100, 500, 2000),
    labels = c("1", "10", "100", "500", "2,000+"),
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(alpha = 0.8, shape = 21),
      direction = "horizontal",
      nrow = 1
    )
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "MDS Dimension 1 →\n(Higher values = Lower disease burden)",
    y = "MDS Dimension 2 →\n(Higher values = More outbreak variability)",
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.8),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.72),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 1.2,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
      family = fonts$caption,
      color = colors$caption,
      hjust = 0.5,
      margin = margin(t = 10)
    ),
    # Legend styling
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(b = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.spacing.x = unit(1, "cm"),
    legend.box.spacing = unit(0.5, "cm"),

    # Grid and axes
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95", linewidth = 0.3),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
  )

# Final Plot ----
p +
  # top-left outliers  
  exec(geom_text_repel,
    data = outlier_datasets$top_left,
    mapping = aes(label = country),
    nudge_x = -0.5,
    nudge_y = -0.4,
    direction = "x",
    !!!base_label_style
  ) +

  # bottom-right outliers 
  exec(geom_text_repel,
    data = outlier_datasets$bottom_right,
    mapping = aes(label = country),
    !!!base_label_style
  ) +

  # zone rectangles
  pmap(zone_annotations, ~ annotate("rect",
    xmin = ..2, xmax = ..3,
    ymin = ..4, ymax = ..5,
    fill = colors$background,
    alpha = 0.7
  )) +

  # zone text labels
  pmap(zone_annotations, ~ annotate("text",
    x = ..6, y = ..7,
    label = ..8,
    hjust = 0.5, vjust = 0.5,
    size = 3.5, fontface = "bold",
    color = ..9
  ))


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-06-24
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────
# ! package      * version    date (UTC) lib source
# P annotater      0.2.3      2024-01-26 [?] CRAN (R 4.4.0)
# V base         * 4.4.1      2024-04-24 [2] local (on disk 4.4.0)
# P bit            4.0.5      2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5      2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0      2022-10-03 [?] CRAN (R 4.4.0)
# cli            3.6.3      2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1      2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0      2024-04-24 [?] local
# P crayon         1.5.2      2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1      2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0      2024-04-24 [?] local
# P digest         0.6.35     2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
# P fansi          1.0.6      2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2      2024-05-13 [1] CRAN (R 4.4.1)
# forcats      * 1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
# generics       0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
# P ggrepel      * 0.9.5      2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2      2022-09-16 [?] CRAN (R 4.4.0)
# P gh             1.4.1      2024-03-28 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2   2023-08-12 [?] CRAN (R 4.4.0)
# P gitcreds       0.1.2      2022-09-08 [?] CRAN (R 4.4.0)
# glue         * 1.8.0      2024-09-30 [1] CRAN (R 4.4.2)
# P graphics     * 4.4.0      2024-04-24 [?] local
# P grDevices    * 4.4.0      2024-04-24 [?] local
# P grid           4.4.0      2024-04-24 [?] local
# P gridtext       0.1.5      2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5      2024-04-22 [1] CRAN (R 4.4.0)
# P here         * 1.0.1      2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3      2023-03-21 [?] CRAN (R 4.4.0)
# P httr2          1.0.1      2024-04-01 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0      2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8      2023-12-04 [?] CRAN (R 4.4.0)
# labeling       0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle      1.0.4      2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3      2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3      2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3      2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13       2024-06-04 [1] CRAN (R 4.4.2)
# P methods      * 4.4.0      2024-04-24 [?] local
# munsell        0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1      2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0      2024-04-24 [?] local
# P patchwork    * 1.3.0      2024-09-16 [?] CRAN (R 4.4.1)
# P pillar         1.9.0      2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3      2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache        0.16.0     2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3    1.8.2      2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo           1.26.0     2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils        2.12.3     2023-11-18 [?] CRAN (R 4.4.0)
# P R6             2.5.1      2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.3      2024-09-11 [?] CRAN (R 4.4.2)
# P rappdirs       0.3.3      2021-01-31 [?] CRAN (R 4.4.0)
# P Rcpp           1.0.12     2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5      2024-01-10 [?] CRAN (R 4.4.0)
# P renv           1.0.7      2024-04-11 [?] CRAN (R 4.4.0)
# rlang          1.1.4      2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4      2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0     2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0      2023-10-08 [?] CRAN (R 4.4.0)
# scales       * 1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
# scico        * 1.5.0.9000 2024-11-29 [1] Github (thomasp85/scico@e94d08c)
# P sessioninfo    1.2.2      2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7      2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0        2020-06-04 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1     2023-08-27 [?] CRAN (R 4.4.0)
# P stats        * 4.4.0      2024-04-24 [?] local
# stringi        1.8.4      2024-05-06 [1] CRAN (R 4.4.0)
# P stringr      * 1.5.1      2023-11-14 [?] CRAN (R 4.4.0)
# P styler         1.10.3     2024-04-07 [?] CRAN (R 4.4.0)
# P svglite        2.1.3      2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts     * 0.8.9      2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts    1.1.0      2024-05-15 [1] CRAN (R 4.4.0)
# textshaping    0.4.0      2024-05-24 [1] CRAN (R 4.4.0)
# P tibble       * 3.2.1      2023-03-20 [?] CRAN (R 4.4.0)
# tidyr        * 1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
# P tidytuesdayR   1.1.2      2024-09-09 [?] CRAN (R 4.4.2)
# tidyverse    * 2.0.0      2023-02-22 [1] CRAN (R 4.4.3)
# P timechange     0.3.0      2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0      2024-04-24 [?] local
# P tzdb           0.4.0      2023-05-12 [?] CRAN (R 4.4.0)
# P utf8           1.2.4      2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0      2024-04-24 [?] local
# P vctrs          0.6.5      2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5      2023-12-05 [?] CRAN (R 4.4.0)
# P withr          3.0.2      2024-10-28 [?] CRAN (R 4.4.2)
# P xfun           0.52       2025-04-02 [?] CRAN (R 4.4.3)
# P xml2           1.3.6      2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────
# > 