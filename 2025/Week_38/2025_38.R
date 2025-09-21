## Challenge: #TidyTuesday 2025 week 38
## Data:      FIDE Chess Player Ratings
## Author:    Steven Ponce
## Date:      2025-09-21

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  scales,      # Scale Functions for Visualization
  glue,        # Interpreted String Literals,
  patchwork    # The Composer of Plots
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 12,
  units  = "in",
  dpi    = 300
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 38)

fide_ratings_august <- tt$fide_ratings_august |> clean_names()
fide_ratings_september <- tt$fide_ratings_september |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(fide_ratings_august)
glimpse(fide_ratings_september)


## 4. TIDYDATA ----
# helper function
clean_fide_data <- function(data, min_rating = 1000, max_rating = 3500) {
  data |>
    group_by(id) |>
    slice_max(rating, n = 1, with_ties = FALSE) |>
    ungroup() |>
    filter(!is.na(rating), rating >= min_rating, rating <= max_rating, !is.na(id)) |>
    mutate(
      name = str_squish(name),
      fed = str_squish(fed),
      sex = factor(sex, levels = c("M", "F")),
      title = na_if(str_squish(title), ""),
      wtitle = na_if(str_squish(wtitle), ""),
      rating = as.numeric(rating),
      games = as.numeric(games),
      k = as.numeric(k),
      bday = as.numeric(bday)
    ) |>
    filter(!is.na(name), !is.na(fed))
}

# Data prep
fide_august_clean <- clean_fide_data(fide_ratings_august)
fide_september_clean <- clean_fide_data(fide_ratings_september)

# Rating comparison
cur_year <- 2025

rating_comparison <- fide_august_clean |>
  select(id, name, fed, sex, title, wtitle, bday,
    rating_aug = rating, games_aug = games
  ) |>
  inner_join(
    fide_september_clean |>
      select(id, rating_sep = rating, games_sep = games),
    by = "id"
  ) |>
  mutate(
    rating_change = rating_sep - rating_aug,
    total_games = games_aug + games_sep,
    has_title = !is.na(title) | !is.na(wtitle),
    current_age = ifelse(!is.na(bday), cur_year - bday, NA_real_)
  ) |>
  filter(total_games > 0)

# Activity (cap at 30 to keep facets readable)
activity_data <- fide_september_clean |>
  filter(games > 0, games <= 30) |>
  mutate(activity_level = case_when(
    games <= 3 ~ "Low (1–3)",
    games <= 7 ~ "Moderate (4–7)",
    games <= 15 ~ "High (8–15)",
    TRUE ~ "Very High (16+)"
  )) |>
  mutate(
    activity_level = factor(activity_level,
      levels = c(
        "Low (1–3)",
        "Moderate (4–7)",
        "High (8–15)",
        "Very High (16+)"
      )
    )
  )

# Federations (titled players)
federation_data <- fide_september_clean |>
  filter(!is.na(title) | !is.na(wtitle)) |>
  count(fed, name = "titled_players") |>
  arrange(desc(titled_players)) |>
  slice_head(n = 15)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get basic theme colors
colors <- get_theme_colors(
  palette = list(
    ink = "#243245",
    text = "#435369",
    grid = "#E4E9F0",
    edge = "#C9D3E1",
    bg = "#F7FAFC",
    panel = "#FFFFFF",
    p1 = "#00A0E3", 
    p2 = "#2B3FB7",
    p3 = "#0B1F2A"
  )
)

### |- titles and caption ----
title_text <- str_glue("Chess Dreams and Breakthroughs: A Global Perspective")

subtitle_text <- str_glue(
  "Individual excellence emerges from active engagement within strong chess ecosystems"
)

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 38,
  source_text = "Data: FIDE Rating Lists (Aug–Sep 2025)"
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

# helpers for medians & formatting
num_lab <- label_number(accuracy = 1, big.mark = ",")
add_median <- function(y, label) {
  list(
    geom_hline(yintercept = y, linetype = "22", linewidth = 0.5, color = "gray50"),
    annotate(
      "label",
      x = Inf, y = y, hjust = 1, vjust = -0.2,
      label = label, size = 3, label.size = 0,
      family = fonts$text, fill = colors$palette$panel, color = colors$palette$text
    )
  )
}

### |-  P1: Breakthroughs — top 20 improvers ----
break_df <- rating_comparison |>
  filter(rating_change > 0) |>
  arrange(desc(rating_change)) |>
  slice_head(n = 20) |>
  mutate(name_short = str_trunc(name, 28))

med_improve <- median(break_df$rating_change)

p1 <-
  ggplot(break_df, aes(x = reorder(name_short, rating_change), y = rating_change)) +
  # geoms
  geom_col(fill = colors$palette$p2, width = 0.7, alpha = 0.95) +
  geom_text(aes(label = paste0("+", rating_change)),
    hjust = -0.15, family = fonts$text, color = colors$palette$text, size = 3.2
  ) +
  add_median(med_improve, paste0("Median ", med_improve)) +
  # scales
  scale_x_discrete() +
  scale_y_continuous(labels = num_lab, expand = expansion(mult = c(0, 0.10))) +
  coord_flip(clip = "off") +
  labs(
    title = "Chess Rating Breakthroughs",
    subtitle = "Top 20 rating gains, August to September 2025",
    x = NULL, y = "Rating improvement (Elo)"
  ) +
  # theme
  theme(
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

### |-  P2: Activity — faceted histograms ----
p2 <-
  ggplot(activity_data, aes(games)) +
  # geoms
  geom_histogram(bins = 18, fill = colors$palette$p1, color = "white", linewidth = 0.25, alpha = 0.95) +
  # scales
  scale_x_continuous(breaks = pretty_breaks(4)) +
  scale_y_continuous(labels = num_lab) +
  # labs
  labs(
    title = "Player Activity Patterns",
    subtitle = "Distribution of games played in September 2025 by activity level",
    x = "Games played (September)", y = "Number of players"
  ) +
  # facets
  facet_wrap(~activity_level, ncol = 4, scales = "free_y") +
  # theme
  theme(
    strip.text = element_text(face = "bold", color = colors$palette$ink),
    strip.background = element_rect(fill = colors$palette$bg, color = colors$palette$edge),
    panel.grid.major.x = element_line(color = colors$palette$grid),
    plot.margin = margin(10, 10, 5, 10)
  )

### |-  P3:  Federations — titled players (top 15) ----
med_titled <- median(federation_data$titled_players)

p3 <-
  ggplot(federation_data, aes(x = reorder(fed, titled_players), y = titled_players)) +
  # geoms
  geom_col(fill = alpha(colors$palette$p3, 0.92), width = 0.7) +
  geom_text(aes(label = num_lab(titled_players)),
    hjust = -0.15,
    family = fonts$text, color = colors$palette$text, size = 3.2
  ) +
  add_median(med_titled, paste0("Median ", med_titled)) +
  # scales
  scale_x_discrete() +
  scale_y_continuous(labels = num_lab, expand = expansion(mult = c(0, 0.10))) +
  coord_flip(clip = "off") +
  # labs
  labs(
    title = "Chess Federation Powerhouses",
    subtitle = "Countries with the most titled players",
    x = NULL, y = "Number of titled players"
  ) +
  # theme
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

### |-  Combined plots ----
combined_plots <- p2 / (p1 | p3) +
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
# date     2025-09-21
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────
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
# ────────────────────────────────────────────────────────────────
# > 
