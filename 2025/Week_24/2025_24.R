## Challenge: #TidyTuesday 2025 week 24
## Data:      API Specs
## Author:    Steven Ponce
## Date:      2025-06-17


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  janitor,    # Simple Tools for Examining and Cleaning Dirty Data
  scales,     # Scale Functions for Visualization
  glue,       # Interpreted String Literals
  patchwork   # The Composer of Plots
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
tt <- tidytuesdayR::tt_load(2025, week = 24)

# categories_raw <- tt$api_categories |> clean_names()
info_raw <- tt$api_info |> clean_names()
apis_raw <- tt$apisguru_apis |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(info_raw)
glimpse(apis_raw)


## 4. TIDYDATA ----
apis_minimal <- apis_raw |>
  left_join(info_raw, by = "name") |>
  select(name, added, updated, provider_name) |>
  mutate(
    added_date = as_date(added),
    updated_date = as_date(updated)
  ) |>
  filter(!is.na(added_date), !is.na(updated_date))

# P1. The API Graveyard Data ----
maintenance_analysis <- apis_minimal |>
  mutate(
    days_between_add_update = as.numeric(updated_date - added_date),
    same_day_update = days_between_add_update == 0,
    days_for_viz = ifelse(days_between_add_update == 0, 0.5, days_between_add_update)
  ) |>
  filter(days_between_add_update >= 0)

# Calculate key statistics
total_apis <- nrow(maintenance_analysis)
never_updated_count <- sum(maintenance_analysis$same_day_update)
never_updated_pct <- round(never_updated_count / total_apis * 100, 1)
updated_count <- total_apis - never_updated_count
updated_pct <- round(updated_count / total_apis * 100, 1)

# P2. Provider Analysis ----
provider_analysis <- apis_minimal |>
  mutate(
    days_between_add_update = as.numeric(updated_date - added_date),
    same_day_update = days_between_add_update == 0
  ) |>
  filter(days_between_add_update >= 0) |>
  group_by(provider_name) |>
  filter(n() >= 3) |>
  summarise(
    total_apis = n(),
    never_updated_apis = sum(same_day_update),
    abandonment_rate = round((never_updated_apis / total_apis) * 100, 1),
    .groups = "drop"
  ) |>
  filter(abandonment_rate > 0) |>
  arrange(desc(abandonment_rate)) |>
  head(15) |> # Top 15 by abandonment rate
  mutate(
    provider_clean = str_trunc(str_to_title(provider_name), 35),
    provider_clean = fct_reorder(provider_clean, abandonment_rate)
  )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c("#95A5A6", "#E74C3C", "#7F8C8D")
)

### |-  titles and caption ----
caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 24,
  source_text =  "APIs.guru"
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

# P1. The API Graveyard Plot ----
p1 <- maintenance_analysis |>
  ggplot(aes(x = added_date, y = days_for_viz)) +
  # Geoms
  geom_point(
    data = maintenance_analysis |> filter(!same_day_update),
    color = colors$palette[1], size = 1.5, alpha = 0.6
  ) +
  geom_point(
    data = maintenance_analysis |> filter(same_day_update),
    color = colors$palette[2], size = 2.5, alpha = 0.9
  ) +
  # Scales
  scale_y_log10(
    labels = comma_format(),
    limits = c(0.1, NA),
    breaks = c(0.5, 1, 10, 100, 1000)
  ) +
  # Annotations
  annotate("text",
    x = as.Date("2016-06-01"), y = 12,
    label = paste0("Gray dots: Updated after addition\n(", updated_count, " APIs • ", updated_pct, "%)"),
    color = colors$palette[1], size = 3.5, fontface = "bold",
    hjust = 0, vjust = 0
  ) +
  annotate("text",
    x = as.Date("2016-06-01"), y = 0.9,
    label = paste0("Red dots: Never updated\n(", never_updated_count, " APIs • ", never_updated_pct, "%)"),
    color = colors$palette[2], size = 3.5, fontface = "bold",
    hjust = 0, vjust = 0
  ) +
  # Labs
  labs(
    title = "<span style='color:#E74C3C'>**The API Graveyard**</span>: A Tale of Digital Neglect",
    subtitle = paste0("**", never_updated_pct, "%** of APIs are <span style='color:#E74C3C'>**never updated**</span> after being added to the catalog<br><br>",
                      "<span style='color:#1a1a1a; font-size:14px'>**Days Since Last Update (log scale)**</span>"),
    x = "Date Added to APIs.guru",
    y = NULL,
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.4),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 0, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.95),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 1.2,
      margin = margin(t = 5, b = 10)
    ),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# P2. Provider Responsibility Breakdown Plot ----
p2 <- provider_analysis |>
  ggplot(aes(x = provider_clean, y = abandonment_rate)) +
  # Geoms
  geom_col(fill = colors$palette[2], alpha = 0.7, width = 0.7) +
  geom_text(aes(label = paste0(abandonment_rate, "% •  (", never_updated_apis, "/", total_apis, ")")),
    hjust = -0.1, size = 3, color = "gray50", fontface = "bold"
  ) +
  # Scales
  scale_y_continuous(
    limits = c(0, 124),
    expand = c(0, 0)
  ) +
  coord_flip() +
  # Labs
  labs(
    title = "**API Maintenance Patterns by Provider**",
    subtitle = "Providers with the highest rates of APIs that were never updated after initial publication",
    x = NULL,
    y = "% of APIs Never Updated After Addition",
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.4),
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
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.caption = element_text(size = 8, color = colors$palette[3])
  )


# Final plot -----
combined_plot <- p1 / p2 +
  plot_layout(heights = c(1.2, 1))

combined_plot +
  plot_annotation(
    caption = caption_text,
    theme = theme(
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

# ─ Session info ─────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-06-17
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────
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
# ────────────────────────────────────────────────────────────────────
# > 