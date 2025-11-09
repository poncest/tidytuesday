## Challenge: #TidyTuesday 2025 week 45
## Data:      WHO Tuberculosis Global Burden Estimates
## Author:    Steven Ponce
## Date:      2025-11-09

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details
##       
##       IMPORTANT: Uses WEIGHTED BY DEATHS approach for regional aggregation
##       This provides more accurate representation of actual mortality burden


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,     # Easily Install and Load the 'Tidyverse'
    ggtext,        # Improved Text Rendering Support for 'ggplot2'
    showtext,      # Using Fonts More Easily in R Graphs
    janitor,       # Simple Tools for Examining and Cleaning Dirty Data
    scales,        # Scale Functions for Visualization
    glue,          # Interpreted String Literals
    ggrepel        # Non-overlapping text labels
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 7,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
who_tb_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-11/who_tb_data.csv')


## 3. EXAMINING THE DATA ----
glimpse(who_tb_data)
skimr::skim(who_tb_data) |> summary()


## 4. TIDY DATA ----
### |- calculate HIV-associated mortality proportion (WEIGHTED BY DEATHS) ----
WEIGHTED_BY_DEATHS <- TRUE

tb_hiv_ratio <- who_tb_data |>
  filter(!is.na(e_mort_tbhiv_num), !is.na(e_mort_num), e_mort_num > 0) |>
  mutate(
    hiv_prop = e_mort_tbhiv_num / e_mort_num,
    wt = if (WEIGHTED_BY_DEATHS) e_mort_num else 1
  ) |>
  group_by(g_whoregion, year) |>
  summarise(
    avg_hiv_prop = weighted.mean(hiv_prop, w = wt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(avg_hiv_prop = 100 * avg_hiv_prop) |>
  filter(!is.na(g_whoregion), !is.na(avg_hiv_prop))

### |- prepare endpoint labels ----
# Africa gets prominent label
endpoint_labels_africa <- tb_hiv_ratio |>
  filter(g_whoregion == "Africa") |>
  filter(year == max(year))

# All other regions get smaller, gray labels for transparency
endpoint_labels_others <- tb_hiv_ratio |>
  filter(g_whoregion != "Africa") |>
  group_by(g_whoregion) |>
  filter(year == max(year)) |>
  ungroup()

# Africa series (sorted)
africa <- tb_hiv_ratio |>
    filter(g_whoregion == "Africa") |>
    arrange(year) |>
    ungroup()

africa_first <- africa |> slice_head(n = 1)
africa_last  <- africa |> slice_tail(n = 1)

africa_change_pp <- africa_last$avg_hiv_prop - africa_first$avg_hiv_prop
start_year <- africa_first$year
end_year   <- africa_last$year
end_val    <- africa_last$avg_hiv_prop


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Define color palette - Africa as hero, others as context
region_colors <- c(
    "Africa" = "#1E3A8A",                    
    "Americas" = "gray60",                   
    "Eastern Mediterranean" = "gray60",      
    "Europe" = "gray60",                     
    "South-East Asia" = "gray60",            
    "Western Pacific" = "gray60"             
)

# Line widths - Africa bold, others thin
region_linewidths <- c(
    "Africa" = 1.5,
    "Americas" = 0.6,
    "Eastern Mediterranean" = 0.6,
    "Europe" = 0.6,
    "South-East Asia" = 0.6,
    "Western Pacific" = 0.6
)

colors <- get_theme_colors(
    palette = list(
        col_africa = "#1E3A8A",
        col_gray = "gray60"
    )
)

### |- titles and caption ----
title_text <- str_glue("The Changing Face of TB Mortality")

subtitle_text <- str_glue(
    "HIV-associated deaths as a share of total TB mortality, by WHO region<br>",
    "**Africa** shows remarkable progress from {round(africa_first$avg_hiv_prop)}% ",
    "to {round(africa_last$avg_hiv_prop)}%, yet still bears the heaviest burden"
)

caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 45,
    source_text = "WHO Global Tuberculosis Database | Regional estimates weighted by mortality burden"
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
    plot.title = element_markdown(
      face = "bold", family = fonts$title, size = rel(1.4),
      color = colors$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_text(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.9), margin = margin(b = 20), hjust = 0
    ),

    ## Grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),

    # Axes
    axis.title = element_text(size = rel(0.8), color = "gray30"),
    axis.text = element_text(color = "gray30"),
    axis.text.y = element_text(size = rel(0.85)),
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
    legend.title = element_text(
      family = fonts$tsubtitle,
      color = colors$text, size = rel(0.8), face = "bold"
    ),
    legend.text = element_text(
      family = fonts$tsubtitle,
      color = colors$text, size = rel(0.7)
    ),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |-  main plot ----
ggplot(tb_hiv_ratio, aes(
    x = year, y = avg_hiv_prop,
    color = g_whoregion,
    linewidth = g_whoregion)
    ) +
  # Geoms
  geom_line(alpha = 0.9) +
  geom_text_repel(
    data = bind_rows(endpoint_labels_africa, endpoint_labels_others),
    aes(
      x = year,
      y = avg_hiv_prop,
      label = g_whoregion,
      size = g_whoregion,
      fontface = ifelse(g_whoregion == "Africa", "bold", "plain")
    ),
    hjust = 0,
    color = ifelse(bind_rows(endpoint_labels_africa, endpoint_labels_others)$g_whoregion == "Africa",
      "#1E3A8A", "gray40"
    ),
    lineheight = 0.9,
    nudge_x = 1,
    direction = "y",
    segment.size = 0.2,
    segment.color = "gray70",
    min.segment.length = 0,
    max.overlaps = 20,
    xlim = c(NA, 2038),
    show.legend = FALSE
  ) +
  # Scales
  scale_size_manual(
    values = c(
      "Africa" = 4.5,
      "Americas" = 2.8,
      "Eastern Mediterranean" = 2.8,
      "Europe" = 2.8,
      "South-East Asia" = 2.8,
      "Western Pacific" = 2.8
    )
  ) +
  scale_color_manual(values = region_colors) +
  scale_linewidth_manual(values = region_linewidths) +
  scale_x_continuous(
    breaks = seq(2000, 2024, by = 5),
    limits = c(2000, 2028),
    expand = expansion(mult = c(0.02, 0))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 65),
    breaks = seq(0, 65, by = 10)
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "HIV-Associated TB Deaths\n(% of Total TB Mortality)"
  ) +
  # Annotate
  annotate(
    "text",
    x = 2021, y = 22,
    label = glue("{round(abs(africa_change_pp),1)} pp decline since {start_year}"),
    family = fonts$text, size = 3.6, hjust = 0, color = colors$palette$col_africa
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.85),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.15,
      margin = margin(t = 8, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.9),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.88),
      lineheight = 1.2,
      margin = margin(t = 2, b = 5)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
      family = "Arial",
      color = colors$caption,
      hjust = 0,
      lineheight = 1.3,
      margin = margin(t = 12, b = 5),
    ),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
  )


# 6. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts,
# and formatting across all my #TidyTuesday projects. The core analysis logic
# (data tidying and visualization) uses only standard tidyverse packages.
#
# -----------------------------------------------------------------------------
# FUNCTIONS USED IN THIS SCRIPT:
# -----------------------------------------------------------------------------
#
# 📂 R/utils/fonts.R
#    • setup_fonts()       - Initialize Google Fonts with showtext
#    • get_font_families() - Return standardized font family names
#
# 📂 R/utils/social_icons.R
#    • create_social_caption() - Generate formatted caption with social handles
#                                and #TidyTuesday attribution
#
# 📂 R/themes/base_theme.R
#    • create_base_theme()   - Create consistent base ggplot2 theme
#    • extend_weekly_theme() - Add weekly-specific theme customizations
#    • get_theme_colors()    - Get color palettes for highlight/text
#
# -----------------------------------------------------------------------------
# WHY CUSTOM FUNCTIONS?
# -----------------------------------------------------------------------------
# These utilities eliminate repetitive code and ensure visual consistency
# across 50+ weekly visualizations. Instead of copy-pasting 30+ lines of
# theme() code each week, I use create_base_theme() and extend as needed.
#
# -----------------------------------------------------------------------------
# VIEW SOURCE CODE:
# -----------------------------------------------------------------------------
# All helper functions are open source on GitHub:
# 🔗 https://github.com/poncest/tidytuesday/tree/main/R
#
# Main files:
#   • R/utils/fonts.R         - Font setup and management
#   • R/utils/social_icons.R  - Caption generation with icons
#   • R/themes/base_theme.R   - Reusable ggplot2 themes
#
# -----------------------------------------------------------------------------
# REPRODUCIBILITY:
# -----------------------------------------------------------------------------
# To run this script:
#
# Option 1 - Use the helper functions (recommended):
#   1. Clone the repo: https://github.com/poncest/tidytuesday/
#   2. Make sure the R/ directory structure is maintained
#   3. Run the script as-is
#
# Option 2 - Replace with standard code:
#   1. Replace setup_fonts() with your own font setup
#   2. Replace get_theme_colors() with manual color definitions
#   3. Replace create_base_theme() with theme_minimal() + theme()
#   4. Replace create_social_caption() with manual caption text
#
## ============================================================================ ##


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-11-09
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# P colorspace    2.1-1    2024-07-26 [?] CRAN (R 4.4.1)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# P digest        0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P evaluate      0.23     2023-11-01 [?] CRAN (R 4.4.0)
# P fansi         1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap       1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggrepel     * 0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext      * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski        1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# glue        * 1.8.0    2024-09-30 [1] CRAN (R 4.4.2)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# P gridtext      0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable        0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here        * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools     0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr         1.50     2025-03-16 [?] RSPM
# labeling      0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown      1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache       0.16.0   2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3   1.8.2    2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo          1.26.0   2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils       2.12.3   2023-11-18 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.3    2024-09-11 [?] CRAN (R 4.4.2)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P renv          1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# P repr          1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang         1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr         2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr     * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P styler        1.10.3   2024-04-07 [?] CRAN (R 4.4.0)
# P svglite       2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts   1.1.0    2024-05-15 [1] CRAN (R 4.4.0)
# textshaping   0.4.0    2024-05-24 [1] CRAN (R 4.4.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# tidyverse   * 2.0.0    2023-02-22 [1] CRAN (R 4.4.3)
# P timechange    0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# P tzdb          0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr         3.0.2    2024-10-28 [?] CRAN (R 4.4.2)
# P xfun          0.52     2025-04-02 [?] CRAN (R 4.4.3)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# P yaml          2.3.8    2023-12-11 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────
# > 