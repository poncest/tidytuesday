## Challenge: #TidyTuesday 2025 week 40
## Data:      EuroLeague Basketball
## Author:    Steven Ponce
## Date:      2025-10-05

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  scales,      # Scale Functions for Visualization
  glue,        # Interpreted String Literals
  tidytext     # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools 
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
  height = 6,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 40)

euroleague_basketball <- tt$euroleague_basketball |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(euroleague_basketball)


## 4. TIDY DATA ----
teams_clean <- euroleague_basketball |>
  mutate(
    across(
      c(
        team, home_city, arena, country, last_season,
        years_of_final_four_appearances, years_of_titles_won
      ),
      \(x) if_else(is.na(x), NA_character_, str_squish(x))
    ),
    capacity_num = parse_number(capacity),
    last_season_num = parse_number(last_season),
    final_four_appearances = as.integer(final_four_appearances),
    titles_won = as.integer(titles_won)
  )

# Country aggregation
country_summary <- teams_clean |>
  summarise(
    n_teams = n(),
    tot_titles = sum(titles_won, na.rm = TRUE),
    tot_ff = sum(final_four_appearances, na.rm = TRUE),
    .by = country
  )

# Long df
country_long <- country_summary |>
  filter(tot_ff > 0 | tot_titles > 0) |> # Only show countries with achievements
  mutate(
    # Highlight only Greece 
    highlight = country == "Greece",
    `Final Four apps` = tot_ff,
    Titles = tot_titles
  ) |>
  select(country, highlight, `Final Four apps`, Titles) |>
  pivot_longer(
    cols = c(`Final Four apps`, Titles),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    country_ordered = reorder_within(country, value, metric)
  )

# Greece's stats
greece_stats <- country_summary |>
  filter(country == "Greece")


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get basic theme colors
colors <- get_theme_colors(
  palette = c("TRUE" = "#0066CC", "FALSE" = "gray70")
)

### |- titles and caption ----
title_text <- str_glue("European Basketball Success by Nation")

subtitle_text <- str_glue(
  "Greece dominates with 27 Final Four appearances and 10 titles, nearly double Spain's performance<br>",
  "Shows only countries with Final Four appearances or titles<br>"
)

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 40,
  source_text = "EuroLeague Basketball via Wikipedia"
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
    legend.title = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.8), face = "bold"),
    legend.text = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.7)),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- final plot ----
ggplot(country_long, aes(x = value, y = country_ordered)) +
  # Geoms
  geom_col(
    aes(fill = highlight, alpha = highlight),
    width = 0.65
  ) +
  geom_text(
    aes(
      label = if_else(value > 0, as.character(value), ""),
      color = highlight
    ),
    hjust = -0.3,
    size = 3.3,
    fontface = "bold"
  ) +
  geom_text(
    data = tibble(
      metric = "Final Four apps",
      y_pos = 7.8,
      x_pos = 1,
      label = "Greece: 37% conversion rate\n(10 titles from 27 Final Four apps)"
    ),
    aes(x = x_pos, y = y_pos, label = label),
    hjust = 0,
    vjust = 1,
    size = 2.8,
    color = colors$palette[1],
    fontface = "italic",
    lineheight = 0.9,
    inherit.aes = FALSE
  ) +
  # Scales
  scale_y_reordered(expand = expansion(mult = c(0, 0.12))) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.18)),
    breaks = pretty_breaks(n = 4)
  ) +
  scale_fill_manual(
    values = colors$palette,
    guide = "none"
  ) +
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.65),
    guide = "none"
  ) +
  scale_color_manual(
    values = c("TRUE" = "gray20", "FALSE" = "gray50"),
    guide = "none"
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = NULL,
  ) +
  # Facets
  facet_wrap(~metric, nrow = 1, scales = "free") +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.8),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.75),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 1.2,
      margin = margin(t = 5, b = 10)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
      family = fonts$caption,
      color = colors$caption,
      hjust = 0.5,
      margin = margin(t = 10)
    )
  )
  

# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-10-05
# rstudio  2025.09.0+387 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────
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
# P janeaustenr    1.0.0    2022-08-26 [?] CRAN (R 4.4.0)
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
# rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P SnowballC      0.7.1    2023-04-25 [?] CRAN (R 4.4.0)
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
# P tidytext     * 0.4.2    2024-04-10 [?] CRAN (R 4.4.0)
# P tidytuesdayR   1.1.2    2024-09-09 [?] CRAN (R 4.4.2)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.4.3)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tokenizers     0.3.0    2022-12-22 [?] CRAN (R 4.4.0)
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
# ────────────────────────────
# > 
