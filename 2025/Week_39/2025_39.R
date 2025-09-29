## Challenge: #TidyTuesday 2025 week 39
## Data:      Crane Observations at Lake Hornborgasjön, Sweden (1994–2024)
## Author:    Steven Ponce
## Date:      2025-09-29

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  scales,      # Scale Functions for Visualization
  glue,        # Interpreted String Literals,
  ggrepel,     # Automatically Position Non-Overlapping Text Labels with'ggplot2'
  patchwork    # The Composer of Plots
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 12,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 39)

cranes_raw <- tt$cranes |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(cranes_raw)


## 4. TIDY DATA ----
# Clean & tag season
cranes <- cranes_raw |>
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE),
    day_of_year = yday(date),
    season = case_when(
      month(date) %in% 3:5 ~ "Spring",
      month(date) %in% 9:11 ~ "Fall",
      TRUE ~ "Other"
    )
  ) |>
  filter(!is.na(observations)) |>
  mutate(season = factor(season, levels = c("Spring", "Fall", "Other")))

# P1: Seasonal slice (Spring & Fall only)
season_df <- cranes |> filter(season %in% c("Spring", "Fall"))

# P1 labels: find peak (median across days) per season for direct labeling
labs_df <- season_df |>
  group_by(season, day_of_year) |>
  summarise(obs = median(observations), .groups = "drop") |>
  group_by(season) |>
  slice_max(obs, n = 1, with_ties = FALSE)

# P2: Choose highlight years (earliest, midpoint actual year, latest)
yrs <- sort(unique(cranes$year))
mid_year <- yrs[ceiling(length(yrs) / 2)]
highlight_years <- c(first(yrs), mid_year, last(yrs))

# P2: Build cumulative-by-year data and flag highlighted years
cum_df <- cranes |>
  arrange(year, date) |>
  group_by(year) |>
  mutate(cumulative = cumsum(observations)) |>
  ungroup() |>
  mutate(is_hi = year %in% highlight_years)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get basic theme colors
colors <- get_theme_colors(
  palette = c(
      Spring = "#0072B2", Fall = "#E69F00",
      "1994" = "#6A51A3", "2009" = "#1F78B4", "2024" = "#FDB863"
      )
)

### |- titles and caption ----
title_text <- str_glue("Lake Hornborgasjön cranes: seasonal peaks and long-term growth")

subtitle_text <- str_glue(
  "Spring migration dominates the daily peaks; annual totals are much higher today than in the 1990s"
)

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 39,
  source_text = "Hornborgasjön Field Station"
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
    plot.subtitle = element_text(family = fonts$subtitle, lineheight = 1.2, color = colors$subtitle, size = rel(0.9), margin = margin(b = 20)),

    # Axis elements
    axis.line = element_blank(),
    axis.ticks = element_blank(),

    # Grid elements
    panel.grid.major = element_line(color = "gray90", linetype = "solid", linewidth = 0.3),
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),

    # Axis elements
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    axis.title.x = element_text(color = colors$text, face = "bold", size = rel(0.8), margin = margin(t = 15)),
    axis.title.y = element_text(color = colors$text, face = "bold", size = rel(0.8), margin = margin(r = 10)),

    # Legend elements
    legend.position = "plot",
    legend.title = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.8), face = "bold"),
    legend.text = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.7)),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  P1: season plot ----
p1 <-
  ggplot(season_df, aes(day_of_year, observations, color = season)) +
  # Geoms
  geom_point(alpha = 0.18, size = 1) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs", k = 20),
    se = TRUE, alpha = 0.12, linewidth = 1.6
  ) +
  geom_label_repel(
    data = labs_df,
    aes(day_of_year, obs, label = paste0(season, " migration")),
    nudge_y = 2000,
    direction = "y",
    seed = 42,
    size = 3.8,
    fill = "white",
    label.size = 0,
    segment.color = "grey60"
  ) +
  # Scales
  scale_color_manual(values = colors$palette) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  scale_x_continuous(
    breaks = c(60, 91, 121, 244, 274, 305),
    labels = c("Mar", "Apr", "May", "Sep", "Oct", "Nov"),
    limits = c(55, 310) # trims dead space
  ) +
  # Labs
  labs(
    title = "Spring migration attracts the largest flocks",
    subtitle = "Daily counts (1994–2024) show spring peaks are earlier and consistently higher than fall",
    x = NULL, y = "Number of cranes observed",
  ) +
  # Theme
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
  )


### |-  P2: Cumulative line chart ----
p2 <-
  ggplot(cum_df, aes(day_of_year, cumulative, group = year)) +
  # Geoms
  geom_line(
    data = ~ filter(.x, !is_hi),
    color = "grey85", linewidth = 0.7, alpha = 0.9
  ) +
  geom_line(
    data = ~ filter(.x, is_hi),
    aes(color = factor(year)), linewidth = 1.4
  ) +
  geom_text(
    data = cum_df |>
      filter(is_hi) |>
      group_by(year) |>
      slice_max(day_of_year, n = 1, with_ties = FALSE),
    aes(label = paste0(year, ": ", scales::label_number(big.mark = ",")(cumulative))),
    hjust = -0.05, vjust = 0.5, size = 3.4
  ) +
  # Scales
  scale_color_manual(values = colors$palette) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_continuous(
    breaks = c(60, 121, 182, 244, 305, 365),
    labels = c("Mar", "May", "Jul", "Sep", "Nov", "Dec"), limits = c(55, 365)
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = "Annual crane observations have more than tripled since the 1990s",
    subtitle = "Recent years now exceed 400K cranes compared with ~75K in the mid-1990s",
    x = NULL, y = "Cumulative number of cranes",
  ) +
  # Theme
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 40, 10, 10),
  )

### |-  Combined plots ----
combined_plots <- (p1 / p2) +
  plot_layout(heights = c(1.1, 1.3))

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(1.8),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.95),
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.9),
        lineheight = 1.2,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size = rel(0.65),
        family = fonts$caption,
        color = colors$caption,
        hjust = 0.5,
        margin = margin(t = 10)
      )
    )
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-09-29
# rstudio  2025.09.0+387 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────
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
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────
# > 
