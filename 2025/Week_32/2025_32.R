## Challenge: #TidyTuesday 2025 week 32
## Data:      Extreme Weather Attribution Studies
## Author:    Steven Ponce
## Date:      2025-08-12

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,   # Easily Install and Load the 'Tidyverse'
    ggtext,      # Improved Text Rendering Support for 'ggplot2'
    showtext,    # Using Fonts More Easily in R Graphs
    janitor,     # Simple Tools for Examining and Cleaning Dirty Data
    scales,      # Scale Functions for Visualization
    glue,        # Interpreted String Literals
    zoo          # S3 Infrastructure for Regular and Irregular Time Series
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 12,
    height = 8,
    units  = "in",
    dpi    = 300
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 32)

attribution_studies <- tt$attribution_studies |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(attribution_studies)


## 4. TIDYDATA ----
attribution_clean <- attribution_studies |>
  clean_names() |>
  mutate(publication_year = as.integer(publication_year)) |>
  mutate(
    cb_region = str_to_title(cb_region),
    cb_region = case_when(
      cb_region == "Usa" ~ "USA",
      cb_region == "Northern Hemisphere" ~ "Northern Hemisphere",
      TRUE ~ cb_region
    ),
    rapid_study = factor(rapid_study, levels = c("Yes", "No"))
  )

# how many ending years to treat as potentially partial
PARTIAL_YEARS <- 1

regional_evolution_data <- attribution_clean |>
  filter(!is.na(publication_year), !is.na(cb_region), publication_year >= 2010) |>
  group_by(cb_region) |>
  mutate(region_total_studies = n()) |>
  ungroup() |>
  filter(region_total_studies >= 30) |>
  # Annual counts by region/year/rapid
  count(cb_region, publication_year, rapid_study, name = "studies") |>
  # Complete years ONLY within each region up to its own last observed year
  group_by(cb_region, rapid_study) |>
  complete(
    publication_year = seq(min(publication_year, na.rm = TRUE),
      max(publication_year, na.rm = TRUE),
      by = 1
    ),
    fill = list(studies = 0)
  ) |>
  ungroup() |>
  # yearly totals & rapid adoption
  group_by(cb_region, publication_year) |>
  summarise(
    total_studies = sum(studies),
    rapid_studies = sum(studies[rapid_study == "Yes"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(rapid_adoption_rate = rapid_studies / pmax(total_studies, 1)) |>
  # Normalize + Smoothing (centered, but exclude partial-year leakage)
  group_by(cb_region) |>
  mutate(
    total_normalized = if (max(total_studies) > min(total_studies)) {
      (total_studies - min(total_studies)) / (max(total_studies) - min(total_studies))
    } else {
      0.5
    },
    # region-wise bounds
    min_y = min(publication_year),
    max_y = max(publication_year),
    # last year to be treated as "complete" for smoothing windows
    last_complete_y = max_y - PARTIAL_YEARS
  ) |>
  arrange(cb_region, publication_year) |>
  # compute centered rollmean across full series
  mutate(
    total_smooth_tmp = zoo::rollmean(total_normalized, k = 3, fill = NA, align = "center"),
    rapid_smooth_tmp = zoo::rollmean(rapid_adoption_rate, k = 3, fill = NA, align = "center")
  ) |>
  # Mask smoothed values whose 3-yr window would include partial years
  mutate(
    valid_center = publication_year >= (min_y + 1) & publication_year <= (last_complete_y - 1),
    total_smooth = ifelse(valid_center, total_smooth_tmp, NA_real_),
    rapid_smooth = ifelse(valid_center, rapid_smooth_tmp, NA_real_)
  ) |>
  # context metrics & labels
  mutate(
    total_regional_studies = sum(total_studies),
    avg_rapid_adoption = mean(rapid_adoption_rate, na.rm = TRUE),
    research_start_year = min(publication_year[total_studies > 0]),
    development_pattern = case_when(
      avg_rapid_adoption > 0.30 ~ "Early Adopter",
      avg_rapid_adoption > 0.15 ~ "Moderate Adopter",
      TRUE ~ "Traditional"
    )
  ) |>
  ungroup() |>
  # field development phases (for backdrop)
  mutate(
    development_phase = case_when(
      publication_year <= 2013 ~ "Emergence",
      publication_year <= 2018 ~ "Growth",
      TRUE ~ "Maturation"
    ),
    phase_color = case_when(
      development_phase == "Emergence" ~ "#f0f0f0",
      development_phase == "Growth" ~ "#f5f5f5",
      TRUE ~ "#fafafa"
    ),
    facet_order_metric = total_regional_studies * (2024 - research_start_year),
    region_label = paste0(cb_region, "\n(", total_regional_studies, " studies)")
  ) |>
  arrange(development_pattern, desc(total_regional_studies)) |>
  ungroup()


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = c(
        "Study Volume" = "#2E8B57", 
        "Rapid Adoption" = "#FF8C00"
    )
)

### |- titles and caption ----
title_text <- str_glue("Regional evolution of extreme weather attribution science")

subtitle_text <- str_glue(
    "How different regions developed attribution research over time (2010–2024)\n",
    "Study volume normalized within each region for fair comparison\n",
    "Thin lines = annual data • Thick lines = centered 3-year MA (excludes partial years)"
)

caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 32,
    source_text = "Our World in Data"
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
        panel.grid.major = element_line(color = "gray90",linetype = "solid", linewidth = 0.3),
        panel.grid.minor = element_blank(), 
   
        # Axis elements
        axis.text = element_text(color = colors$text, size = rel(0.7)),
        axis.title.x = element_text(color = colors$text, face = "bold", size = rel(0.8), margin = margin(t = 15)),
        axis.title.y = element_text(color = colors$text, face = "bold", size = rel(0.8), margin = margin(r = 10)),
        
        # Legend elements
        legend.position = "top",
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
ggplot(regional_evolution_data, aes(x = publication_year)) +
  # Annotate (bkg phases)
  annotate("rect",
    xmin = 2010, xmax = 2013.5, ymin = -Inf, ymax = Inf,
    fill = "#f0f0f0", alpha = 0.6
  ) +
  annotate("rect",
    xmin = 2013.5, xmax = 2018.5, ymin = -Inf, ymax = Inf,
    fill = "#f8f8f8", alpha = 0.6
  ) +
  annotate("rect",
    xmin = 2018.5, xmax = 2024, ymin = -Inf, ymax = Inf,
    fill = "#fdfdfd", alpha = 0.6
  ) +
  # Geoms
  geom_line(aes(y = total_normalized, color = "Study Volume", linetype = "Study Volume"),
    alpha = 0.4, linewidth = 0.7
  ) +
  geom_line(aes(y = rapid_adoption_rate, color = "Rapid Adoption", linetype = "Rapid Adoption"),
    alpha = 0.4, linewidth = 0.7
  ) +
  geom_line(aes(y = total_smooth, color = "Study Volume", linetype = "Study Volume"),
    linewidth = 2, na.rm = TRUE
  ) +
  geom_line(aes(y = rapid_smooth, color = "Rapid Adoption", linetype = "Rapid Adoption"),
    linewidth = 2, na.rm = TRUE
  ) +
  # Annotate (phase labels)
  annotate("text",
    x = 2011.75, y = 0.95, label = "Emergence",
    size = 3, alpha = 0.5, fontface = "italic"
  ) +
  annotate("text",
    x = 2016, y = 0.95, label = "Growth",
    size = 3, alpha = 0.5, fontface = "italic"
  ) +
  annotate("text",
    x = 2021, y = 0.95, label = "Maturation",
    size = 3, alpha = 0.5, fontface = "italic"
  ) +
  # Scales
  scale_color_manual(
    values = colors$palette,
    name = "Metric (Normalized)",
    labels = c(
      "Rapid Adoption" = "Rapid Adoption Rate",
      "Study Volume" = "Study Volume (Normalized)"
    )
  ) +
  scale_linetype_manual(
    values = c("Study Volume" = "solid", "Rapid Adoption" = "22"),
    name = "Metric (Normalized)",
    labels = c(
      "Rapid Adoption" = "Rapid Adoption Rate",
      "Study Volume" = "Study Volume (Normalized)"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1), breaks = seq(0, 1, 0.25)
  ) +
  scale_x_continuous(breaks = seq(2010, 2024, 4)) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    x = "Publication Year",
    y = "Normalized Scale (0–100%)",
    caption = paste0(
      "Background shading shows field development phases • ",
      "Facets ordered by research maturity & volume • ",
      "Numbers in region labels = total studies<br><br>",
      caption_text
    )
  ) +
  # Facets
  facet_wrap(~ fct_reorder(region_label, facet_order_metric, .desc = TRUE),
    ncol = 4, scales = "free_x"
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
    plot.subtitle = element_text(
      size = rel(0.85),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 1.2,
      margin = margin(t = 0, b = 10)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
      family = fonts$caption,
      color = colors$caption,
      hjust = 1,
      margin = margin(t = 10)
    ),
    strip.text = element_text(size = rel(0.70), face = "bold", margin = margin(b = 10)),
    panel.spacing.x = unit(1.5, "cm"),
    panel.spacing.y = unit(1.2, "cm"),
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-08-12
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────
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
# P lattice        0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
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
# P zoo          * 1.8-12   2023-04-13 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────
# > 