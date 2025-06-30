## Challenge: #TidyTuesday 2025 week 26
## Data:      Weekly US Gas Prices
## Author:    Steven Ponce
## Date:      2025-06-30


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  janitor,    # Simple Tools for Examining and Cleaning Dirty Data
  scales,     # Scale Functions for Visualization
  glue        # Interpreted String Literals
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
tt <- tidytuesdayR::tt_load(2025, week = 26)

weekly_gas_prices_raw <- tt$weekly_gas_prices |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(weekly_gas_prices_raw)


## 4. TIDYDATA ----

# Data preparation
gas_prices <- weekly_gas_prices_raw |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    quarter = quarter(date)
  )

# Define major events
major_events <- tibble(
  event = c("2008 Financial Crisis", "COVID-19 Pandemic", "Russia-Ukraine War"),
  start_date = as.Date(c("2007-12-01", "2020-03-01", "2022-02-24")),
  end_date = as.Date(c("2009-06-01", "2021-12-31", "2025-06-30"))
)

# Plot data
spread_data <- gas_prices |>
  filter(
    grade == "all",
    (fuel == "gasoline" & formulation == "all") |
      (fuel == "diesel")
  ) |>
  select(date, fuel, price) |>
  pivot_wider(names_from = fuel, values_from = price) |>
  filter(!is.na(gasoline), !is.na(diesel)) |>
  mutate(
    spread = gasoline - diesel,
    diesel_premium = diesel - gasoline,
    positive_premium = ifelse(diesel_premium >= 0, diesel_premium, 0),
    negative_premium = ifelse(diesel_premium < 0, diesel_premium, 0)
  )

# Key statistics for annotations
summary_stats <- spread_data |>
  summarise(
    avg_premium = mean(diesel_premium, na.rm = TRUE),
    max_premium = max(diesel_premium, na.rm = TRUE),
    min_premium = min(diesel_premium, na.rm = TRUE),
    recent_avg = mean(diesel_premium[date >= as.Date("2020-01-01")], na.rm = TRUE)
  )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c("#0267C1", "#D65108", "#b22222", "#888888", "gray40")
)

### |- titles and caption ----
title_text <- str_glue("The Evolution of Diesel's Premium Over Gasoline")

subtitle_text <- str_glue(
    "<span style='color:#0267C1'>**Weekly price differences**</span> and ",
    "<span style='color:#b22222'>**long-term trend**</span> showing diesel\\'s growing premium since 2005<br>",
    "Recent avg (2020+): {dollar(summary_stats$recent_avg, accuracy = 0.01)}"
)

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 26,
  source_text =  "EIA"
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
p <- spread_data |>
  ggplot(aes(x = date, y = diesel_premium)) +

  # Geoms
  geom_rect(
    data = major_events,
    aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    alpha = 0.15,
    fill = "#ced4da"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    color = "#555555",
    linewidth = 0.5,
    alpha = 0.8
  ) +
  geom_hline(
    yintercept = summary_stats$avg_premium,
    linetype = "dashed",
    color = "#888888",
    linewidth = 0.6,
    alpha = 0.7
  ) +
  geom_line(
    color = "gray",
    linewidth = 0.5,
    alpha = 0.9
  ) +
  geom_area(
    aes(y = positive_premium),
    fill = colors$palette[1], # diesel premium
    alpha = 0.6
  ) +
  geom_area(
    aes(y = negative_premium),
    fill = colors$palette[2], # gasoline premium
    alpha = 0.6
  ) +
  geom_smooth(
    se = FALSE,
    color = colors$palette[3],
    alpha = 0.15,
    linewidth = 0.8,
    method = "loess",
    span = 0.4
  ) +

  # Scales
  scale_y_continuous(
    labels = dollar_format(accuracy = 0.01),
    breaks = seq(-0.5, 2, by = 0.5),
    limits = c(-0.5, 1.85)
  ) +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.03))
  ) +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "Price Difference ($/gallon)",
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
    plot.subtitle = element_markdown(
      size = rel(0.95),
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

    # Grid and panel styling
    panel.grid.major.x = element_line(
      color = "#e9ecef",
      linewidth = 0.2
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "#e9ecef",
      linewidth = 0.2
    ),
    panel.grid.minor.y = element_line(
      color = "#dde2e6",
      linewidth = 0.1
    )
  )

# Annotated plot
p <- p +
  annotate(
    "text",
    x = as.Date("2008-09-01"),
    y = 1.55,
    label = "2008\nFinancial Crisis",
    size = 3,
    color = colors$palette[4],
    fontface = "bold",
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = as.Date("2021-03-01"),
    y = 1.45,
    label = "COVID-19\nPandemic",
    size = 3,
    color = colors$palette[4],
    fontface = "bold",
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = as.Date("2023-10-15"),
    y = 1.65,
    label = "Russia-Ukraine\nWar",
    size = 3,
    color = colors$palette[4],
    fontface = "bold",
    hjust = 0.5
  ) +
  annotate(
    "segment",
    x = as.Date("2012-05-01"), xend = as.Date("2012-05-01"),
    y = 0.95, yend = 0.80,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    color = colors$palette[1],
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = as.Date("2012-05-01"),
    y = 1.1,
    label = "Positive Values:\nDiesel costs more\nthan gasoline",
    size = 3.2,
    color = colors$palette[1],
    hjust = 0.5,
    fontface = "bold",
    lineheight = 1.1
  ) +
  annotate(
    "segment",
    x = as.Date("1996-06-01"), xend = as.Date("1996-06-01"),
    y = -0.30, yend = -0.15,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    color = colors$palette[2],
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = as.Date("1996-06-01"),
    y = -0.4,
    label = "Negative Values:\nGasoline costs more\nthan diesel",
    size = 3.2,
    color = colors$palette[2],
    hjust = 0.5,
    fontface = "bold",
    lineheight = 1.1
  ) +
  annotate(
    "text",
    x = as.Date("1994-03-01"),
    y = summary_stats$avg_premium + 0.05,
    label = "Historical average",
    size = 3,
    color = colors$palette[4],
    hjust = 0,
    fontface = "italic"
  )

print(p)


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-06-30
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
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
# P pacman         0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
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
# P splines        4.4.0    2024-04-24 [?] local
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
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────
# > 