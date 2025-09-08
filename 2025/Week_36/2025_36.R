## Challenge: #TidyTuesday 2025 week 36
## Data:      Henley Passport Index Data
## Author:    Steven Ponce
## Date:      2025-09-08

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
  width  = 10,
  height = 12,
  units  = "in",
  dpi    = 300
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 36)

country_lists <- tt$country_lists |> clean_names()
rank_by_year <- tt$rank_by_year |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(country_lists)
glimpse(rank_by_year)


## 4. TIDYDATA ----

#' On 5 May 2023, the World Health Organization (WHO) officially declared 
#' that COVID-19 was no longer a Public Health Emergency of International 
#' Concern (PHEIC).

# Calculate regional performance vs global median for 2024
regional_gaps_2024 <- rank_by_year |>
  filter(year == 2024) |>
  mutate(
    global_median_2024 = median(visa_free_count, na.rm = TRUE)
  ) |>
  group_by(region) |>
  summarise(
    regional_median = median(visa_free_count, na.rm = TRUE),
    global_median = first(global_median_2024),
    countries_count = n(),
    .groups = "drop"
  ) |>
  mutate(
    gap_from_global = regional_median - global_median,
    performance_category = ifelse(gap_from_global >= 0, "Above Global Median", "Below Global Median"),
    region_clean = case_when(
      region == "NORTH_AMERICA" ~ "Americas",
      region == "SOUTH_AMERICA" ~ "Americas",
      region == "CENTRAL_AMERICA" ~ "Americas",
      region == "EUROPE" ~ "Europe",
      region == "ASIA" ~ "Asia",
      region == "AFRICA" ~ "Africa",
      region == "MIDDLE_EAST" ~ "Middle East",
      region == "OCEANIA" ~ "Oceania",
      region == "CARIBBEAN" ~ "Caribbean",
      TRUE ~ str_to_title(str_replace_all(region, "_", " "))
    )
  ) |>
  # Combine Americas if they exist separately
  group_by(region_clean) |>
  summarise(
    gap_from_global = weighted.mean(gap_from_global, countries_count),
    countries_count = sum(countries_count),
    performance_category = ifelse(gap_from_global >= 0, "Above Global Median", "Below Global Median"),
    .groups = "drop"
  ) |>
  arrange(desc(gap_from_global))

# Create performance clusters for 2024
performance_clusters_2024 <- rank_by_year |>
  filter(year == 2024) |>
  mutate(
    region_clean = case_when(
      region == "NORTH_AMERICA" ~ "Americas",
      region == "SOUTH_AMERICA" ~ "Americas",
      region == "CENTRAL_AMERICA" ~ "Americas",
      region == "EUROPE" ~ "Europe",
      region == "ASIA" ~ "Asia",
      region == "AFRICA" ~ "Africa",
      region == "MIDDLE_EAST" ~ "Middle East",
      region == "OCEANIA" ~ "Oceania",
      region == "CARIBBEAN" ~ "Caribbean",
      TRUE ~ str_to_title(str_replace_all(region, "_", " "))
    ),
    performance_cluster = case_when(
      visa_free_count >= 180 ~ "Global Elite (180+)",
      visa_free_count >= 150 ~ "Strong Performers (150-179)",
      visa_free_count >= 100 ~ "Middle Powers (100-149)",
      visa_free_count >= 50 ~ "Emerging Markets (50-99)",
      TRUE ~ "Restricted Access (<50)"
    ),
    performance_cluster = factor(performance_cluster, levels = c(
      "Global Elite (180+)", "Strong Performers (150-179)",
      "Middle Powers (100-149)", "Emerging Markets (50-99)",
      "Restricted Access (<50)"
    ))
  )

# Calculate cluster composition by region
cluster_composition <- performance_clusters_2024 |>
  count(region_clean, performance_cluster, .drop = FALSE) |>
  group_by(region_clean) |>
  mutate(
    total_countries = sum(n),
    proportion = n / total_countries,
    percentage = round(proportion * 100, 1)
  ) |>
  ungroup() |>
  filter(total_countries > 0) |>
  arrange(region_clean, performance_cluster)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get basic theme colors
colors <- get_theme_colors(
  palette = c(
    "Above Global Median" = "#2A9D8F",
    "Below Global Median" = "#E76F51",
    "Global Elite (180+)" = "#264653",
    "Strong Performers (150-179)" = "#2A9D8F",
    "Middle Powers (100-149)" = "#457B9D",
    "Emerging Markets (50-99)" = "#F4A261",
    "Restricted Access (<50)" = "#E76F51"
  )
)

### |- titles and caption ----
title_text <- str_glue("The Global Passport Divide: Regional Inequality in Visa-Free Access")

subtitle_text <- str_glue(
  "Europe dominates global passport power while Africa, Asia, and Middle East lag significantly behind"
)

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 36,
  source_text = "Henley Passport Index Data"
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
    legend.position = "bottom",
    legend.title = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.8), face = "bold"),
    legend.text = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.7)),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  P1: Regional power gaps ----
p1 <- regional_gaps_2024 |>
  mutate(
    region_clean = fct_reorder(region_clean, gap_from_global),
    label_position = ifelse(gap_from_global >= 0, gap_from_global + 2, gap_from_global - 2),
    label_hjust = ifelse(gap_from_global >= 0, 0, 1)
  ) |>
  ggplot(aes(x = region_clean, y = gap_from_global, fill = performance_category)) +
  # Geoms
  geom_col(width = 0.75, alpha = 0.9) +
  geom_hline(yintercept = 0, color = "#212121", size = 0.8) +
  geom_text(
    aes(
      y = label_position,
      label = paste0(ifelse(gap_from_global >= 0, "+", ""), round(gap_from_global, 0)),
      hjust = label_hjust
    ),
    color = colors$text,
    family = fonts$text,
    fontface = "bold",
    size = 4
  ) +
  # Scales
  scale_fill_manual(values = colors$palette) +
  scale_y_continuous(
    limits = c(-50, 100),
    breaks = seq(-60, 100, 25),
    expand = c(0.02, 0)
  ) +
  coord_flip() +
  # Labs
  labs(
    title = "Regional Passport Performance vs Global Median (2024)",
    subtitle = str_glue(
      "Difference in median visa-free access from global median\n",
      "Positive values indicate above-average regional performance"
    ),
    fill = "Performance Category:",
    x = NULL,
    y = "Difference from Global Median (Visa-Free Destinations)"
  ) +
  # Theme
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.5)
  )

### |- P2: Performance cluster composition ----
p2 <- cluster_composition |>
  left_join(
    regional_gaps_2024 |> select(region_clean, gap_from_global),
    by = "region_clean"
  ) |>
  mutate(
    region_clean = fct_reorder(region_clean, gap_from_global),
    label_text = ifelse(n > 0, as.character(n), "")
  ) |>
  ggplot(aes(x = proportion, y = region_clean, fill = performance_cluster)) +
  # Geoms
  geom_col(position = "fill", alpha = 0.9, width = 0.75) +
  geom_text(
    aes(label = label_text),
    position = position_fill(vjust = 0.5),
    color = "white",
    family = fonts$text,
    fontface = "bold",
    size = 3.5
  ) +
  # Scales
  scale_fill_manual(values = colors$palette) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  # Labs
  labs(
    title = "Regional Composition by Passport Performance Clusters (2024)",
    subtitle = str_glue(
      "Proportion of countries in each performance tier by region\n",
      "Numbers show count of countries in each cluster"
    ),
    fill = "Performance\nCluster:",
    x = "Proportion of Regional Countries",
    y = NULL,
  ) +
  # Theme
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.5),
  ) +
  # Guides
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

### |-  Combined plots ----
combined_plots <- p1 / p2 +
  plot_layout(heights = c(1, 1))

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
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
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-09-08
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────
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
# ───────────────────────────────────────
# > 
