## Challenge: #TidyTuesday 2025 week 33
## Data:      Scottish Munros
## Author:    Steven Ponce
## Date:      2025-08-18

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,   # Easily Install and Load the 'Tidyverse'
    ggtext,      # Improved Text Rendering Support for 'ggplot2'
    showtext,    # Using Fonts More Easily in R Graphs
    janitor,     # Simple Tools for Examining and Cleaning Dirty Data
    scales,      # Scale Functions for Visualization
    glue         # Interpreted String Literals
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
tt <- tidytuesdayR::tt_load(2025, week = 33)

munros <- tt$scottish_munros |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(munros)


## 4. TIDYDATA ----
breaks <- seq(900, 1350, 50)
labels <- paste0(seq(900, 1300, 50), "–", seq(950, 1350, 50), " m")

pyramid_data <- munros |>
  filter(!is.na(height_m), !is.na(x2021)) |>
  mutate(
      # Bins cut
    height_bin = cut(height_m,
      breaks = breaks, labels = labels,
      include.lowest = TRUE, right = FALSE
    ),
    x2021 = factor(x2021, levels = c("Munro", "Munro Top"))
  ) |>
  filter(!is.na(height_bin)) |>
  count(height_bin, x2021, name = "n") |>
  # Complete the data to show all combinations, filling with 0
  complete(height_bin, x2021, fill = list(n = 0)) |>
  # Label placement
  mutate(
    n_signed = if_else(x2021 == "Munro", n, -n),
    big_bar = abs(n_signed) >= 8,
    tiny_bar = abs(n_signed) <= 3,
    label_pos = case_when(
      big_bar ~ n_signed / 2,
      n_signed > 0 & tiny_bar ~ n_signed + 4.5,
      n_signed > 0 ~ n_signed + 3,
      n_signed < 0 & tiny_bar ~ n_signed - 4.5,
      TRUE ~ n_signed - 3
    ),
    label_hjust = if_else(big_bar, 0.5, if_else(n_signed > 0, 0, 1)),
    label_text = if_else(n > 0, as.character(abs(n_signed)), ""),
    label_col = if_else(big_bar, "white", "gray20")
  )

# symmetric limits (nice round)
lim <- ceiling((max(pyramid_data$n) + 2) / 10) * 10

# totals for subtitle 
totals <- pyramid_data |>
  group_by(x2021) |>
  summarise(N = sum(n), .groups = "drop") |>
  pivot_wider(names_from = x2021, values_from = N)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = c("Munro" = "#4B5945", "Munro Top" = "#9A7E6F")
    )

### |- titles and caption ----
title_text <- str_glue("Height Distribution: Scottish Munros vs. Munro Tops")

subtitle_text <- str_glue(
    "Peak-count pyramid by elevation band (2021) — ",
    "<span style='color:{colors$palette['Munro']}'><b>Munros</b></span> (n = {totals$Munro}) vs ",
    "<span style='color:{colors$palette['Munro Top']}'><b>Munro Tops</b></span> (n = {totals$`Munro Top`})"
)

caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 33,
    source_text = "Database of British and Irish hills"
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
        plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.2), color  = colors$title, margin = margin(b = 10)),
        plot.subtitle = element_text(family = fonts$subtitle, lineheight = 1.2, color = colors$subtitle, size = rel(0.78), margin = margin(b = 20)),
        
        # Axis elements
        axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        
        # Grid elements
        panel.grid.major.x = element_line(color = "gray90",linetype = "solid", linewidth = 0.3),
        panel.grid.major.y = element_blank(),
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
ggplot(pyramid_data, aes(x = height_bin, y = n_signed, fill = x2021)) +
  # Geoms
  geom_col(width = 0.78, color = "white", linewidth = 0.3, alpha = 0.92) +
  geom_hline(yintercept = 0, color = "#5f6b73", linewidth = 0.6) +
  geom_text(
    aes(y = label_pos, label = label_text, hjust = label_hjust, color = label_col),
    fontface = "bold", size = 3.2
  ) +
  # Scales
  scale_color_identity() +
  scale_fill_manual(values = colors$palette, guide = "none") +
  scale_y_continuous(
    limits = c(-lim, lim),
    breaks = seq(-lim, lim, 20),
    labels = function(x) abs(x),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_flip() +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Height range (m)",
    y = "Number of peaks"
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.65),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.85),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 1.2,
      margin = margin(t = 0, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
      family = fonts$caption,
      color = colors$caption,
      hjust = 1,
      margin = margin(t = 10)
    )
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-08-18
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────
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
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr          3.0.2    2024-10-28 [?] CRAN (R 4.4.2)
# P xfun           0.52     2025-04-02 [?] CRAN (R 4.4.3)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────
# > 