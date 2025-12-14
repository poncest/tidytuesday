## Challenge: #TidyTuesday 2025 week 51
## Data:      The Languages of the World
## Author:    Steven Ponce
## Date:      2025-12-14

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,     # Easily Install and Load the 'Tidyverse'
    ggtext,        # Improved Text Rendering Support for 'ggplot2'
    showtext,      # Using Fonts More Easily in R Graphs
    janitor,       # Simple Tools for Examining and Cleaning Dirty Data
    skimr,         # Compact and Flexible Summaries of Data
    scales,        # Scale Functions for Visualization
    glue,          # Interpreted String Literals
    treemapify,    # Create Treemap Visualizations
    patchwork,     # The Composer of Plots
    maps           # Draw Geographical Maps
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 14,
  height = 12,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 51)

endangered_status <- tt$endangered_status |> clean_names()
families <- tt$families |> clean_names()
languages <- tt$languages |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(endangered_status)
glimpse(families)
glimpse(languages)


## 4. TIDY DATA ----
### |- Join all datasets ----
languages_combined <- languages |>
  left_join(endangered_status, by = "id") |>
  left_join(families, by = c("family_id" = "id")) |>
  mutate(
    status_label = case_when(
      status_code == 1 ~ "Not Endangered",
      status_code == 2 ~ "Threatened",
      status_code == 3 ~ "Shifting",
      status_code == 4 ~ "Moribund",
      status_code == 5 ~ "Nearly Extinct",
      status_code == 6 ~ "Extinct",
      TRUE ~ "Unknown"
    ),
    endangerment_broad = case_when(
      status_code == 1 ~ "Safe",
      status_code %in% 2:3 ~ "Threatened/Shifting",
      status_code %in% 4:5 ~ "Critical",
      status_code == 6 ~ "Extinct",
      TRUE ~ "Unknown"
    ),
    is_endangered = status_code %in% 2:5,
    n_countries = str_count(countries, ";") + 1
  )

### |- Overall endangerment summary ----
endangerment_summary <- languages_combined |>
  count(status_label, endangerment_broad, sort = TRUE) |>
  mutate(pct = n / sum(n))

### |- Pacific region critical languages ----
pacific_critical <- languages_combined |>
  filter(
    endangerment_broad == "Critical",
    !is.na(latitude), !is.na(longitude),
    longitude >= 100, longitude <= 180,
    latitude >= -50, latitude <= 20
  )

# Calculate total critical languages globally 
total_critical <- languages_combined |>
  filter(endangerment_broad == "Critical") |>
  nrow()

pacific_pct <- round(nrow(pacific_critical) / total_critical * 100)

### |- data for treemap labels  ----
endangerment_summary2 <- endangerment_summary |>
  filter(endangerment_broad != "Unknown") |>
  group_by(status_label) |>
  summarise(n = sum(n), .groups = "drop") |>
  mutate(
    pct = n / sum(n),
    status_label = fct_reorder(status_label, -n),
    label_txt = glue("{status_label}\n{comma(n)} ({scales::percent(pct, accuracy = 0.1)})"),
    text_color = if_else(status_label %in% c("Moribund", "Nearly Extinct"), "white", "gray10")
  )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    col_not_endangered = "#E8EEF2",
    col_shifting = "#D0D0D0",
    col_threatened = "#A8A8A8",
    col_extinct = "#8A8A8A",
    col_moribund = "#E63946",
    col_nearly_extinct = "#C41E3A",
    col_map_bg = "gray96",
    col_map_border = "gray65",
    col_ink = "gray10",
    col_rule = "gray85"
  )
)

### |- titles and caption ----
main_title <- "Global language endangerment: scale and geographic concentration"

treemap_subtitle <- "Share of 8,612 documented languages by endangerment status (Glottolog 5.2.1)"

map_title <- "Papunesia: a global hotspot for critical endangerment"
map_subtitle <- glue(
    "{comma(nrow(pacific_critical))} of {comma(total_critical)} critically endangered languages ",
    "({pacific_pct}%) fall within the Pacific focus region"
)

caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 51,
    source_text = "Glottolog 5.2.1 | Max Planck Institute for Evolutionary Anthropology"
)

key_takeaway <- glue(
    "Nearly **half of the world’s critically endangered languages** ",
    "are concentrated in the **Pacific region**, making **Papunesia** the global epicenter of language loss risk."
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
    plot.title = element_text(
      face = "bold", family = fonts$title, size = rel(1.4),
      color = colors$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_markdown(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.9), margin = margin(b = 20), hjust = 0
    ),

    # Grid
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.25),

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
      size = rel(0.9),
      margin = margin(t = 6, b = 4)
    ),
    panel.spacing = unit(1.5, "lines"),

    # Legend elements
    legend.position = "plot",
    legend.title = element_text(
      family = fonts$subtitle,
      color = colors$text, size = rel(0.8), face = "bold"
    ),
    legend.text = element_text(
      family = fonts$tsubtitle,
      color = colors$text, size = rel(0.7)
    ),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(20, 20, 20, 20),
    
    # Panel framing (subtle  containers) 
    panel.border     = element_rect(color = colors$palette$col_rule, fill = NA, linewidth = 0.8),
  )
)

# Set theme
theme_set(weekly_theme)

### |- Plot 1: Treemap ----
p1 <- ggplot(
    endangerment_summary2,
    aes(area = n, fill = status_label)
) +
    geom_treemap(color = "white", size = 2) +
    geom_treemap_text(
        aes(label = label_txt, color = text_color),
        place = "centre",
        fontface = "bold",
        size = 12,
        grow = FALSE,
        family = fonts$text,
        reflow = TRUE
    ) +
    scale_fill_manual(
        values = c(
            "Not Endangered"  = colors$palette$col_not_endangered,
            "Shifting"        = colors$palette$col_shifting,
            "Threatened"      = colors$palette$col_threatened,
            "Extinct"         = colors$palette$col_extinct,
            "Moribund"        = colors$palette$col_moribund,
            "Nearly Extinct"  = colors$palette$col_nearly_extinct
        )
    ) +
    scale_color_identity() +
    labs(
        # tag = "A",
        title = "Endangerment status distribution",
        subtitle = treemap_subtitle
    ) +
    theme(
        legend.position = "none",
        plot.margin = margin(t = 6, r = 10, b = 6, l = 6)
    )

### |- Plot 2: Map  ----
world_map <- map_data("world")

p2 <- ggplot() +
    geom_polygon(
        data = world_map,
        aes(x = long, y = lat, group = group),
        fill = colors$palette$col_map_bg,
        color = colors$palette$col_map_border,
        linewidth = 0.28
    ) +
    geom_point(
        data = pacific_critical,
        aes(x = longitude, y = latitude),
        shape = 21,
        fill  = colors$palette$col_moribund,
        color = "white",
        stroke = 0.35,
        alpha = 0.75,
        size  = 1.9
    ) +
    coord_fixed(
        xlim = c(100, 180),
        ylim = c(-50, 20),
        ratio = 1.3,
        expand = FALSE
    ) +
    labs(
        # tag = "B",
        title = map_title,
        subtitle = map_subtitle
    ) +
    theme_void() +
    theme(
        plot.title = element_text(
            face = "bold", family = fonts$title, size = 19.6,
            color = colors$title, margin = margin(b = 10), hjust = 0
        ),
        plot.subtitle = element_markdown(
            face = "italic", family = fonts$subtitle, lineheight = 1.2,
            color = colors$subtitle, size = 12.6, margin = margin(b = 20), hjust = 0
        ),
        panel.border = element_rect(color = colors$palette$col_rule, fill = NA, linewidth = 0.8),
        plot.margin = margin(t = 6, r = 6, b = 6, l = 10)
    )

### |- Combine plots ----
p1 + p2 +
    plot_annotation(
        title    = main_title,
        subtitle = key_takeaway,
        caption  = caption_text,
        theme =  theme(
            plot.title = element_markdown(
                size = rel(2.3),
                family = fonts$title,
                face = "bold",
                color = colors$title,
                lineheight = 1.15,
                margin = margin(t = 8, b = 5)
            ),
            plot.subtitle = element_markdown(
                size = rel(0.92),
                family = fonts$subtitle,
                color = alpha(colors$subtitle, 0.88),
                lineheight = 1.5,
                margin = margin(t = 5, b = 20)
            ),
            plot.caption = element_markdown(
                size = rel(0.65),
                family = fonts$subtitle,
                color = colors$caption,
                hjust = 0,
                lineheight = 1.4,
                margin = margin(t = 20, b = 5)
            )
        )
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

  # ─ Session info ──────────────────────────────────────────────
  # setting  value
  # version  R version 4.4.1 (2024-06-14 ucrt)
  # os       Windows 11 x64 (build 26100)
  # system   x86_64, mingw32
  # ui       RStudio
  # language (EN)
  # collate  English_United States.utf8
  # ctype    English_United States.utf8
  # tz       America/New_York
  # date     2025-12-12
  # rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
  # pandoc   NA
  # 
  # ─ Packages ──────────────────────────────────────────────────
  # ! package      * version  date (UTC) lib source
  # V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
  # P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
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
  # P evaluate       0.23     2023-11-01 [?] CRAN (R 4.4.0)
  # P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
  # farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
  # P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
  # forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
  # generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
  # P ggfittext      0.10.2   2024-02-01 [?] CRAN (R 4.4.0)
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
  # P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
  # P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
  # magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
  # P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
  # P maps         * 3.4.2    2023-12-15 [?] CRAN (R 4.4.0)
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
  # P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
  # rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
  # P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
  # P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
  # P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
  # scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
  # P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
  # P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
  # P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
  # P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
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
  # P tidytuesdayR   1.2.1    2025-04-29 [?] CRAN (R 4.4.3)
  # tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.4.3)
  # P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
  # P tools          4.4.0    2024-04-24 [?] local
  # P treemapify   * 2.5.6    2023-09-30 [?] CRAN (R 4.4.0)
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
  # ─────────────────────────────────────────────────────────────
  # > 