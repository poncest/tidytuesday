## Challenge: #TidyTuesday 2025 week 19
## Data:      Seismic Events at Mount Vesuvius
## Author:    Steven Ponce
## Date:      2025-05-10


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
  width  =  10,
  height =  12,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 19)

vesuvius_raw <- tt$vesuvius |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(vesuvius_raw)
skimr::skim(vesuvius_raw)


## 4. TIDYDATA ----

### |-  tidy data ----
vesuvius <- vesuvius_raw |>
  mutate(
    date = as.Date(time),
    year = year(time),
    month = month(time),
    day = day(time),
    hour = hour(time),
    weekday = wday(time, label = TRUE),
    # Create a month-year for time series
    month_year = floor_date(time, "month"),
  )

# P1: Monthly event counts with  trend decomposition -----
monthly_counts <- vesuvius |>
  count(year, month) |>
  mutate(date = make_date(year, month, 1))

# Calculate a 6-month moving average
monthly_counts <- monthly_counts |>
  arrange(date) |>
  mutate(moving_avg = zoo::rollmean(n, k = 6, fill = NA, align = "right"))

# P2: Time-to-next events analysis -----
events_ordered <- vesuvius |>
  arrange(time) |>
  mutate(
    next_event_time = lead(time),
    hours_to_next = as.numeric(difftime(next_event_time, time, units = "hours"))
  ) |>
  filter(!is.na(hours_to_next))

# Calculate statistics
median_time <- median(events_ordered$hours_to_next, na.rm = TRUE)
mean_time <- mean(events_ordered$hours_to_next, na.rm = TRUE)
cv <- sd(events_ordered$hours_to_next, na.rm = TRUE) / mean_time

# P3: Event rate change analysis ----
events_per_month <- vesuvius |>
  count(year, month) |>
  mutate(date = make_date(year, month, 1)) |>
  arrange(date) |>
  mutate(
    prev_month = lag(n),
    pct_change = (n - prev_month) / prev_month * 100,
    change_type = ifelse(pct_change > 0, "Increase", "Decrease")
  ) |>
  filter(!is.na(pct_change)) |>
  # Limit extreme values for better visualization (cap at ±200%)
  mutate(pct_change = pmin(pmax(pct_change, -200), 200))

# Calculate statistics
increase_count <- sum(events_per_month$change_type == "Increase")
decrease_count <- sum(events_per_month$change_type == "Decrease")
total_months <- nrow(events_per_month)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = c(
        "gray_line" = "#808080",
        "moving_avg" = "#21908C", 
        "trend_line" = "#440154",
        "confidence" = "#440154",
        "positive_change" = "#3B528B",
        "negative_change" = "#5DC863",
        "median_line" = "#FDE725",
        "hist_bar" = "#21908CFF", 
        "Decrease" = "#21908CFF", 
        "Increase" = "#3B528B"
    )
)

### |-  titles and caption ----
title_text <- str_glue("Volatility and Change Patterns in Mount Vesuvius Seismic Activity (2011-2023)")

subtitle_text <- str_glue("Analysis reveals dynamic behavior with high variability across different time scales")

# Create caption
caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 19,
  source_text =  "Italian Istituto Nazionale di Geofisica e Vulcanologia (INGV)"
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
    plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.14), color  = colors$title, margin = margin(b = 10)),
    plot.subtitle = element_text(family = fonts$subtitle, color = colors$subtitle, size = rel(0.78), margin = margin(b = 20)),

    # Axis elements
    axis.title = element_text(color = colors$text, face = "bold", size = rel(0.8)),
    axis.text = element_text(color = colors$text, size = rel(0.7)),

    # Grid elements
    panel.grid.minor = element_line(color = "gray80", linewidth = 0.05),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.05),

    # Legend elements
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(family = fonts$text, size = rel(0.8), face = "bold"),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),

    # Style facet labels
    strip.text = element_text(size = rel(0.75), face = "bold",
                              color = colors$title, margin = margin(b = 8, t = 8)
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

# P1: Monthly event counts with  trend decomposition -----
p1 <- ggplot(monthly_counts, aes(x = date)) +
  # Geoms
  geom_hline(yintercept = seq(0, 200, by = 50), color = "gray90", linewidth = 0.3) +
  geom_line(aes(y = n), color = colors$palette["gray_line"], linewidth = 0.5) +
  geom_line(aes(y = moving_avg), color = colors$palette["moving_avg"], linewidth = 1.2) +
  geom_smooth(aes(y = n),
    method = "loess", span = 0.3, color = colors$palette["trend_line"],
    se = TRUE, fill = colors$palette["confidence"], alpha = 0.15
  ) +
  # Annotate
  annotate("rect", 
    xmin = as.Date("2011-12-01"), xmax = as.Date("2014-01-01"), 
    ymin = 100, ymax = 210, 
    fill = "white", color = "gray80", alpha = 0.8
    ) +
  annotate("text",
    x = as.Date("2012-01-01"), y = 190,
    label = "Monthly counts", color = colors$palette["gray_line"], hjust = 0, size = 3.5, fontface = "plain"
  ) +
  annotate("text",
    x = as.Date("2012-01-01"), y = 155,
    label = "6-month average", color = colors$palette["moving_avg"], hjust = 0, size = 3.5, fontface = "plain"
  ) +
  annotate("text",
    x = as.Date("2012-01-01"), y = 120,
    label = "Long-term trend", color = colors$palette["trend_line"], hjust = 0, size = 3.5, fontface = "plain"
  ) +
  annotate("text",
    x = as.Date("2023-01-01"), y = 20,
    label = "Purple band shows 95% confidence interval", color = colors$palette["confidence"],
    hjust = 0.5, size = 3, fontface = "italic"
  ) +
  # Scales
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 200, by = 50)) +
  # Labs
  labs(
    title = "Long-term Patterns in Seismic Activity",
    subtitle = "Monthly event counts with smoothing reveal cyclical patterns.\nDespite short-term volatility, seismic activity follows multi-year cycles",
    x = NULL,
    y = "Number of Events"
  )

# P2: Time-to-next events analysis -----
p2 <- ggplot(events_ordered, aes(x = hours_to_next)) +
  # Geoms
  geom_histogram(
    aes(y = after_stat(count)),
    breaks = c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000),
    fill = colors$palette["hist_bar"], color = "#333333", alpha = 0.9
  ) +
  geom_vline(
    xintercept = median_time,
    linetype = "dashed",
    color = colors$palette["median_line"], 
    linewidth = 1
  ) +
  # Annotate
  annotate("label",
    x = 200, y = 1000,
    label = paste0(
      "Median: ", round(median_time, 1), " hours\n",
      "Mean: ", round(mean_time, 1), " hours\n",
      "Coef. of Variation: ", round(cv, 2)
    ),
    color = "black", fill = "white", alpha = 0.8, size = 3.5, fontface = "plain"
  ) +
  # Scales
  scale_x_log10(
    breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000),
    labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10", "20", "50", "100", "200", "500", "1000"),
    limits = c(0.05, 2000)
  ) +
  # Labs
  labs(
    title = "Waiting Times Between Consecutive Events",
    subtitle = "Distribution reveals characteristic timing between earthquakes.\nHigh CoV indicates clustered, non-random earthquake timing",
    x = "Hours to Next Event (log scale)",
    y = "Frequency"
  )

# P3: Event rate change analysis ----
p3 <- ggplot(events_per_month, aes(x = date, y = pct_change, fill = change_type)) +
  # Geoms
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.7) +
  geom_hline(yintercept = c(-50, 50, 100, -100), linetype = "dotted", color = "gray70") +
  geom_col(width = 25) +

  # Annotate 
  annotate("rect",
    xmin = as.Date("2022-04-01"), xmax = as.Date("2022-07-01"),
    ymin = 175, ymax = 190,
    fill = colors$palette["Increase"], 
  ) +
  annotate("text",
    x = as.Date("2022-08-01"), y = 180,
    label = paste0("Increases: ", increase_count, " months (", round(increase_count / total_months * 100), "%)"),
    color = "black", hjust = 0, size = 3.5, fontface = "plain"
  ) +
  annotate("rect",
    xmin = as.Date("2022-04-01"), xmax = as.Date("2022-07-01"),
    ymin = -100, ymax = -85,
    fill = colors$palette["Decrease"]
  ) +
  annotate("text",
    x = as.Date("2022-08-01"), y = -95,
    label = paste0("Decreases: ", decrease_count, " months (", round(decrease_count / total_months * 100), "%)"),
    color = "black", hjust = 0, size = 3.5, fontface = "plain"
  ) +
  annotate("text",
    x = median(events_per_month$date), y = -95,     
    label = "Values capped at ±200% for visualization clarity",
    color = "gray30", size = 3, fontface = "italic"
 ) +
  # Scales 
  scale_fill_manual(
    values = colors$palette,
    name = "Change"
  ) +
  scale_x_date(
    date_breaks = "2 year",
    date_labels = "%Y",
    limits = c(min(events_per_month$date), max(events_per_month$date))
  ) +
  scale_y_continuous(
    breaks = c(-200, -100, -50, 0, 50, 100, 200),
    labels = c("-200%", "-100%", "-50%", "0%", "+50%", "+100%", "+200%")
  ) +
  # Labs 
  labs(
    title = "Month-to-Month Volatility in Seismic Activity",
    subtitle = "Percentage change shows high variability between consecutive months.\nNearly balanced increases/decreases suggest a dynamic but stable volcanic system",
    x = NULL,
    y = "Percent Change"
  ) +
  # Theme
  theme(
    plot.subtitle = element_text(lineheight = 1.2),
    legend.position = "none"
  )
  
# Combined Plot ----
combined_plot <- (p2 / p3 / p1) +
    plot_layout(
        heights = c(0.9, 0.9, 1.3),  
        ) 

combined_plot +    
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_text(
                size = rel(1.4),
                family = fonts$title,
                face = "bold",
                color = colors$title,
                lineheight = 1.1,
                margin = margin(t = 5, b = 5)
            ),
            plot.subtitle = element_text(
                size = rel(0.95),
                family = fonts$subtitle,
                color = alpha(colors$subtitle, 0.9),
                lineheight = 1.2,
                margin = margin(t = 5, b = 10)
            ),
            plot.caption = element_markdown(
                size = rel(0.6),
                family = fonts$caption,
                color = colors$caption,
                hjust = 0.5,
                margin = margin(t = 10)
            )
        )
    )
  

# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-05-09
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────
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
# P zoo            1.8-12   2023-04-13 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────
