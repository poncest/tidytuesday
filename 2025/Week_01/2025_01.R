
## Challenge: #TidyTuesday 2025 week 01
## Data:      Aid Worker Security Incidents
## Author:    Steven Ponce
## Date:      2025-01-07


## 0. DATA SOURCE ----
#'
#' Aid Worker Security Incidents via Makeover Monday 2024 wk 46
#' Link to article: https://humanitarianoutcomes.org/sites/default/files/publications/figures_at_a_glance_2024.pdf
#' Source: Aid Worker Security Database (https://www.aidworkersecurity.org/incidents/search)
#' Data Link: https://data.world/makeovermonday/2024w46-aid-worker-security-incidents


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,      # Easily Install and Load the 'Tidyverse'
    ggtext,         # Improved Text Rendering Support for 'ggplot2'
    showtext,       # Using Fonts More Easily in R Graphs
    janitor,        # Simple Tools for Examining and Cleaning Dirty Data
    skimr,          # Compact and Flexible Summaries of Data
    scales,         # Scale Functions for Visualization
    glue,           # Interpreted String Literals
    here,           # A Simpler Way to Find Your Files
    patchwork,      # The Composer of Plots
    camcorder,      # Record Your Plot History 
    readxl          # Read Excel Files
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  10,
    height =  12,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))



## 2. READ IN THE DATA ----
aid_raw <- read_excel("data/Aid Worker Incidents.xlsx") |> 
    clean_names()


## 3. EXAMINING THE DATA ----
glimpse(aid_raw)
skim(aid_raw)


## 4. TIDYDATA ----

# data plot ---
# data for both risk score and incidents
country_analysis <- aid_raw |>
    group_by(country) |>  
    mutate(risk_score = total_killed * 3 + total_wounded * 2 + total_kidnapped) |>
    summarise(
        avg_risk = mean(risk_score),
        incidents = n(),
        total_affected = sum(total_affected),
        .groups = 'drop'
    ) |>
    arrange(desc(incidents)) |>
    slice_head(n = 10)

# Vulnerability Heatmap
vulnerability_matrix <- aid_raw |>
    select(un, ingo, icrc, nrcs_and_ifrc, nngo, means_of_attack) |>
    pivot_longer(-means_of_attack, 
                 names_to = "org_type", 
                 values_to = "count") |>
    group_by(means_of_attack, org_type) |>
    summarise(total_affected = sum(count), .groups = 'drop') |>
    mutate(
        org_type = case_when(
            org_type == "un" ~ "UN",
            org_type == "ingo" ~ "INGO",
            org_type == "icrc" ~ "ICRC",
            org_type == "nrcs_and_ifrc" ~ "NRCS/IFRC",
            org_type == "nngo" ~ "NNGO"
        ),
        means_of_attack = str_to_title(means_of_attack)
    )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c("#f7fbff", "#9ecae1", "#2171b5", "#084594"))

### |-  titles and caption ----
title_text    <- str_glue("Aid Worker Security: A Global Analysis of Risks and Incidents")
subtitle_text <- str_glue("Analysis of attack patterns and their impact on humanitarian organizations worldwide")

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 01,
    source_text = "Aid Worker Security Database via Makeover Monday"
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
        # Weekly-specific modifications
        axis.line.x           = element_line(color = "#252525", linewidth = .2),
        
        panel.spacing.x       = unit(2, 'lines'),
        panel.spacing.y       = unit(1, 'lines'),
        panel.grid.major.x    = element_blank(),
        panel.grid.major.y    = element_line(color = alpha(colors[5], 0.2), linewidth = 0.2),
        panel.grid.minor      = element_blank(),
    )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----

# 1. Risk Score Plot
p1 <- ggplot(country_analysis, 
             aes(x = reorder(country, avg_risk), 
                 y = avg_risk)) +
    geom_col(fill = colors$palette[4]) +
    coord_flip() +
    labs(
        title = "Risk Score by Country",
        subtitle = "Risk Score = (Deaths × 3) + (Injuries × 2) + Kidnappings",  
        x = NULL,
        y = "Risk Score"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = rel(1)),
        plot.subtitle = element_text(size = 10, color = colors$text),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
    )

# 2. Incidents Count Plot
p2 <- ggplot(country_analysis, 
             aes(x = reorder(country, avg_risk), 
                 y = incidents)) +
    geom_col(fill = colors$palette[2]) +
    coord_flip() +
    labs(
        title = "Number of Incidents",
        x = NULL,
        y = "Number of Incidents"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = rel(1)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )

# 3. Vulnerability Heatmap
p3 <- ggplot(vulnerability_matrix, 
                             aes(x = org_type, 
                                 y = means_of_attack, 
                                 fill = total_affected)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradientn(
        colors = colors$palette,
        na.value = "#f0f0f0",
        name = "Total\nAffected"
    ) +
    labs(
        title = "Attack Impact by Organization Type and Method",
        subtitle = "Total number of aid workers affected by each type of attack",
        x = "Organization Type",
        y = NULL
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12, color = "gray30"),
        plot.caption = element_text(size = 8, color = "gray50"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
    )

# Final combined plot 
combined_plot <- (p1 + p2) / p3 +
    plot_layout(heights = c(1,1)) 

combined_plot <- combined_plot +
    plot_annotation(
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text,
        theme = theme(
            plot.title = element_text(
                family = fonts$title, 
                size   = rel(1.9), 
                face   = "bold",
                color  = colors$title,
                margin = margin(b = 10)
            ),
            plot.subtitle = element_text(
                family = fonts$text,
                lineheight = 1.1,
                size   = rel(1.1),
                color  = colors$subtitle,
                margin = margin(b = 15)
            ),
            plot.caption = element_markdown(
                family = fonts$caption,
                size   = rel(0.65),
                color  = colors$caption,
                hjust  = 0.5,
                margin = margin(t = 5)
            ),
            plot.margin = margin(10, 10, 10, 10),
        )
    ) 


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/La_Paz
# date     2025-01-06
# rstudio  2024.12.0+467 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P camcorder   * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger    1.1.0    2016-07-27 [?] CRAN (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# P digest        0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi         1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap       1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
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
# P knitr         1.46     2024-04-06 [?] CRAN (R 4.4.0)
# labeling      0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown      1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P patchwork   * 1.3.0    2024-09-16 [?] CRAN (R 4.4.1)
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.3    2024-09-11 [?] CRAN (R 4.4.2)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P readxl      * 1.4.3    2023-07-06 [?] CRAN (R 4.4.0)
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
# P skimr       * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr     * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite       2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts   1.1.0    2024-05-15 [1] CRAN (R 4.4.0)
# textshaping   0.4.0    2024-05-24 [1] CRAN (R 4.4.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidyverse   * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange    0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# P tzdb          0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# withr         3.0.1    2024-07-31 [1] CRAN (R 4.4.1)
# P xfun          0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────
# > 