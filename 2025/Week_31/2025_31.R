## Challenge: #TidyTuesday 2025 week 31
## Data:      Income Inequality Before and After Taxes
## Author:    Steven Ponce
## Date:      2025-08-04

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,  # Easily Install and Load the 'Tidyverse'
    ggtext,     # Improved Text Rendering Support for 'ggplot2'
    showtext,   # Using Fonts More Easily in R Graphs
    janitor,    # Simple Tools for Examining and Cleaning Dirty Data
    scales,     # Scale Functions for Visualization
    glue,       # Interpreted String Literals
    ggridges,   # Ridgeline Plots in 'ggplot2'
    patchwork   # The Composer of Plots
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
tt <- tidytuesdayR::tt_load(2025, week = 31)

income_inequality_processed <- tt$income_inequality_processed |> clean_names()
income_inequality_raw <- tt$income_inequality_raw |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(income_inequality_processed)
glimpse(income_inequality_raw)

## 4. TIDYDATA ----
# Helper Function
classify_region <- function(entity) {
    case_when(
        entity %in% c(
            "Brazil", "Chile", "Colombia", "Dominican Republic",
            "Guatemala", "Mexico", "Panama", "Paraguay", "Peru", "Uruguay"
        ) ~ "Latin America",
        entity %in% c("United States", "Canada") ~ "North America",
        entity %in% c(
            "Germany", "France", "United Kingdom", "Sweden", "Denmark",
            "Norway", "Finland", "Netherlands", "Belgium", "Austria",
            "Switzerland", "Italy", "Spain", "Portugal", "Ireland",
            "Luxembourg", "Greece", "Cyprus", "Malta"
        ) ~ "Western Europe",
        entity %in% c(
            "Poland", "Czechia", "Slovakia", "Hungary", "Slovenia",
            "Estonia", "Lithuania", "Croatia", "Bulgaria",
            "Romania", "Serbia"
        ) ~ "Eastern Europe",
        entity %in% c("Japan", "South Korea", "Singapore", "Taiwan", "Hong Kong") ~ "East Asia",
        entity %in% c("Australia", "New Zealand") ~ "Oceania",
        entity %in% c("Israel", "Turkey") ~ "Middle East",
        entity %in% c("South Africa") ~ "Africa",
        TRUE ~ "Other"
    )
}

# Base data with regional classification
base_data <- income_inequality_processed |>
    mutate(
        region = classify_region(entity),
        redistribution_effect = gini_mi_eq - gini_dhi_eq
    )

# PANEL 1: Ridge plot data
ridge_data <- base_data |>
    filter(!is.na(gini_dhi_eq), region != "Other") |>
    group_by(region) |>
    filter(n() >= 10) |>
    ungroup() |>
    mutate(region = fct_reorder(region, gini_dhi_eq, .fun = median, .desc = FALSE))

# PANEL 2: Redistribution data
redistribution_data <- base_data |>
    filter(!is.na(redistribution_effect), !is.na(gini_mi_eq), !is.na(gini_dhi_eq)) |>
    group_by(entity, region) |>
    summarise(
        avg_redistribution = mean(redistribution_effect, na.rm = TRUE),
        avg_pretax_gini = mean(gini_mi_eq, na.rm = TRUE),
        avg_posttax_gini = mean(gini_dhi_eq, na.rm = TRUE),
        observations = n(),
        .groups = "drop"
    ) |>
    # Get the 15 countries with minimal redistribution
    slice_min(avg_redistribution, n = 15) |> 
    mutate(
        is_latin = region == "Latin America",
        bar_color = ifelse(is_latin, "highlight", "main"),
        text_color = ifelse(is_latin, "highlight", "main"),
        country_label = paste0(entity, "\n(", round(avg_pretax_gini, 2), ")"),
        entity = fct_reorder(entity, avg_redistribution, .desc = FALSE)
    )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = c(
        "highlight" = "#2b1731",   
        "main" = "#9995be",        
        "dark_line" = "#2b1731",   
        "text_dark" = "#4D4D4D",   
        "text_medium" = "#7F7F7F",  
        "text_light" = "#B2B2B2",  
        "grid_line" = "#EAEAEA"  
    )
)

### |- titles and caption ----
title_text <- str_glue("The Geography of Inequality and the Redistribution Gap")

subtitle_text <- str_glue(
    "The distribution of post-tax inequality is sharply divided by geography, with the<br>",
    "highest levels concentrated in <span style='color:", colors$palette["highlight"], ";'>**Latin America**</span>."
)

caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 31,
    source_text =  "Our World in Data"
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
        plot.subtitle = element_markdown(family = fonts$subtitle, lineheight = 1.2, color = colors$subtitle, size = rel(0.78), margin = margin(b = 20)),
        
        # Axis elements
        axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        
        # Grid elements
        panel.grid.major.y = element_line(color = "gray90",linetype = "solid", linewidth = 0.3),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        
        # Axis elements
        axis.text = element_text(color = colors$text, size = rel(0.7)),
        axis.title.x = element_text(color = colors$text, face = "bold", size = rel(0.8), margin = margin(t = 15)),
        axis.title.y = element_text(color = colors$text, face = "bold", size = rel(0.8), margin = margin(r = 10)),
        
        # Legend elements
        legend.position = "plot",
        legend.title = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.8), face = "bold"),
        legend.text = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.7)),
        
        # Plot margin
        plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
    )
)

# Set theme
theme_set(weekly_theme)

# PANEL 1: Ridge plot
ridge_plot <- ridge_data |>
    ggplot(aes(x = gini_dhi_eq, y = region)) +
    # Geoms
    geom_density_ridges(
        aes(fill = ifelse(region == "Latin America", "highlight", "main")),
        scale = 2.2,
        alpha = 0.75,
        bandwidth = 0.015,
        rel_min_height = 0.01,
        quantile_lines = TRUE,
        quantiles = 2,
        color = colors$palette["dark_line"]
    ) +
    # Scales
    scale_fill_manual(
        values = colors$palette,
        guide = "none"  
    ) +
    scale_x_continuous(
        name = "Post-tax Gini Coefficient",
        breaks = seq(0.15, 0.55, 0.05),
        labels = number_format(accuracy = 0.01),
        limits = c(0.15, 0.6),
        expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_discrete(name = NULL, expand = expansion(mult = c(0.05, 0.15))) +
    # Labs
    labs(
        title = "A. The Geography of Inequality",
        subtitle = paste0(
            "The distribution of post-tax inequality is sharply divided by geography,<br>",
            "with the highest levels concentrated in <span style='color:", colors$palette["highlight"], ";'>**Latin America**</span>."
        )
    ) 

# PANEL 2: Minimal Redistributors
redistributors_plot <- redistribution_data |>
    ggplot(aes(x = entity, y = avg_redistribution)) +
    # Geoms
    geom_col(aes(fill = bar_color), width = 0.7, alpha = 0.85) +
    geom_text(
        aes(label = round(avg_redistribution, 3), color = bar_color),
        hjust = -0.1,
        vjust = 0.5,
        size = 3.2,
        fontface = "bold"
    ) +
    # Scales
    scale_y_continuous(
        name = "Redistribution Effect\n(How Much Inequality Was Reduced)",
        breaks = seq(0, 0.16, 0.04),
        labels = number_format(accuracy = 0.01),
        limits = c(0, 0.2),
        expand = expansion(mult = c(0, 0.02))
    ) +
    scale_x_discrete(
        name = "Countries ordered by redistribution effectiveness\n(numbers show pre-tax inequality)",
        labels = redistribution_data$country_label,
        expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_fill_manual(
        values = colors$palette,
        guide = "none"  
    ) +
    scale_color_manual(
        values = colors$palette,
        guide = "none"  
    ) +
    coord_flip() +
    # Labs
    labs(
        title = "B. The Global Redistribution Gap",
        subtitle = paste0(
            "Countries with minimal redistribution despite high pre-tax inequality.<br>",
            "Countries like <span style='color:", colors$palette["highlight"], ";'>**Dominican Republic**</span> and ",
            "<span style='color:", colors$palette["highlight"], ";'>**Brazil**</span> highlight Latin America's pattern."
        )
    ) +
    # Theme
    theme(
        # Remove all gridlines
        panel.grid.major.y = element_blank(),
        
        # Remove x-axis text and ticks
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        
        # Adjust y-axis text size for readability with pre-tax labels
        axis.text.y = element_text(size = rel(0.95), lineheight = 0.9)
    )

### |-  combined plot ----
combined_plots <- ridge_plot | redistributors_plot

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
                hjust = 0.5,
                margin = margin(t = 5, b = 5)
            ),
            plot.subtitle = element_markdown(
                size = rel(1),
                family = fonts$subtitle,
                color = alpha(colors$subtitle, 0.9),
                lineheight = 1.2,
                hjust = 0.5,
                margin = margin(t = 5, b = 10)
            ),
            plot.caption = element_markdown(
                size = rel(0.7),
                family = fonts$caption,
                color = colors$caption,
                hjust = 0.5,
                margin = margin(t = 10)
            )
        )
    )

# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-08-04
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────
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
# P ggridges     * 0.5.6    2024-01-23 [?] CRAN (R 4.4.0)
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
# ────────────────────────────────────────────────────────────────────────────────
# >