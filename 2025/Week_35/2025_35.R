## Challenge: #TidyTuesday 2025 week 35
## Data:      Australian Frogs
## Author:    Steven Ponce
## Date:      2025-08-31

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,   # Easily Install and Load the 'Tidyverse'
    ggtext,      # Improved Text Rendering Support for 'ggplot2'
    showtext,    # Using Fonts More Easily in R Graphs
    janitor,     # Simple Tools for Examining and Cleaning Dirty Data
    scales,      # Scale Functions for Visualization
    glue,        # Interpreted String Literals,
    ggbeeswarm,  # Categorical Scatter (Violin Point) Plots
    viridis,     # Colorblind-Friendly Color Maps for R
    lubridate    # Make Dealing with Dates a Little Easier
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 300
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 35)

frogID_data <- tt$frogID_data |> clean_names()
frog_names <- tt$frog_names |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(frogID_data)
glimpse(frog_names)


## 4. TIDYDATA ----
frog_data_clean <- frogID_data |>
  rename_all(~ str_replace_all(., "([A-Z])", "_\\1") |> str_to_lower()) |>
  rename_all(~ str_remove(., "^_")) |>
  mutate(
    month = month(event_date, label = TRUE),
    # Define seasons for Southern Hemisphere (Australia)
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "Summer",
      month %in% c("Mar", "Apr", "May") ~ "Autumn",
      month %in% c("Jun", "Jul", "Aug") ~ "Winter",
      month %in% c("Sep", "Oct", "Nov") ~ "Spring"
    ),
    hour = hour(event_time)
  ) |>
  left_join(frog_names, by = "scientific_name") |>
  mutate(
    common_name = if_else(is.na(common_name) | common_name == "—",
      scientific_name, common_name
    ),
    state_province = str_trim(state_province),
    subfamily_clean = str_trim(coalesce(subfamily, "Unknown"))
  ) |>
  filter(!is.na(state_province), !is.na(season))

# Species seasonal activity
species_seasonal <- frog_data_clean |>
  filter(!is.na(subfamily_clean)) |>
  # Get species with enough records
  group_by(scientific_name) |>
  filter(n() >= 50) |>
  ungroup() |>
  # Count by species and season
  count(scientific_name, common_name, subfamily_clean, season) |>
  # Get top subfamilies
  group_by(subfamily_clean) |>
  mutate(total_subfamily_records = sum(n)) |>
  ungroup() |>
  arrange(desc(total_subfamily_records)) |>
  filter(subfamily_clean %in% unique(subfamily_clean)[1:5]) |> # Top 5 for clarity
  mutate(
    season_ordered = factor(season, levels = c("Spring", "Winter", "Autumn", "Summer"))
  )

# Calculate medians for reference
season_medians <- species_seasonal |>
  group_by(season_ordered) |>
  summarise(median_recordings = median(n), .groups = "drop")

# Add context metrics
season_stats <- species_seasonal |>
  group_by(season_ordered) |>
  summarise(
    total_species = n(),
    total_recordings = sum(n),
    median_recordings = median(n),
    .groups = "drop"
  ) |>
  mutate(
    season_label = paste0(
      season_ordered, "\n(", total_species, " species,\n",
      scales::comma(total_recordings), " recordings)"
    )
  )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get basic theme colors
colors <- get_theme_colors()

### |- titles and caption ----
title_text <- str_glue("Australian Frogs Show Distinct Seasonal Calling Patterns")

subtitle_text <- str_glue(
    "Species within families vary widely in calling seasons, revealing ecological niche partitioning<br>",
    "**Spring** emerges as peak season with highest species diversity and recording activity<br><br>",
    "FrogID citizen science data: **Jan 1 - Nov 9, 2023** (n=135,154 total recordings)<br>",
    "**White circles** show seasonal medians | **Each dot** represents one frog species"
)

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 35,
  source_text = "FrogID dataset 6.0"
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
    plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.2), color = colors$title, margin = margin(b = 10)),
    plot.subtitle = element_text(family = fonts$subtitle, lineheight = 1.2, color = colors$subtitle, size = rel(0.78), margin = margin(b = 20)),

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

### |-  final plot ----
species_seasonal |>
  ggplot(aes(y = season_ordered, x = n)) +
  # Geoms
  geom_beeswarm(aes(color = subfamily_clean),
    size = 2.5, alpha = 0.7, cex = 1
  ) +
  geom_point(
    data = season_stats,
    aes(y = season_ordered, x = median_recordings),
    shape = 21, size = 4, fill = "white", color = "black", stroke = 1.2
  ) +
  # Scales
  # scale_color_viridis_d(name = "Frog Subfamily", option = "plasma") +
    colorspace::scale_colour_discrete_qualitative(palette = ) +
  scale_x_log10(labels = comma_format()) +
  scale_y_discrete(labels = season_stats$season_label) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    y = NULL,
    x = "Recordings per Species (Log Scale)"
  ) +
  # Theme
  theme(
    axis.text.y = element_text(size = 10, hjust = 0.5),
    legend.position = "right",
    plot.title = element_text(
      size = rel(1.5),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.80),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 1.2,
      margin = margin(t = 5, b = 10)
    ),
    plot.caption = element_markdown(
      size = rel(0.5),
      family = fonts$caption,
      color = colors$caption,
      hjust = 0.5,
      margin = margin(t = 10)
    )
  )
 

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
# date     2025-08-31
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P beeswarm       0.4.0    2021-06-01 [?] CRAN (R 4.4.0)
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
# P ggbeeswarm   * 0.7.2    2023-04-29 [?] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gh             1.4.1    2024-03-28 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P gitcreds       0.1.2    2022-09-08 [?] CRAN (R 4.4.0)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.4.2)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridExtra      2.3      2017-09-09 [?] CRAN (R 4.4.0)
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
# P tidytuesdayR   1.1.2    2024-09-09 [?] CRAN (R 4.4.2)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.4.3)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vipor          0.4.7    2023-12-18 [?] CRAN (R 4.4.0)
# P viridis      * 0.6.5    2024-01-29 [?] CRAN (R 4.4.0)
# viridisLite  * 0.4.2    2023-05-02 [1] CRAN (R 4.4.0)
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