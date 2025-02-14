
## Challenge: #TidyTuesday 2025 week 07
## Data:      Agencies from the FBI Crime Data API
## Author:    Steven Ponce
## Date:      2025-02-14           


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
    camcorder,      # Record Your Plot History 
    maps            # Draw Geographical Maps
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  8,
    height =  7,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 07) 

agencies <- tt$agencies |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(agencies)
skim(agencies)



## 4. TIDYDATA ----

### |-  tidy data ----

# Define constants
continental_bounds <- list(
    states_exclude = c("alaska", "hawaii"),
    long = c(-125, -65),
    lat = c(25, 50),
    outlier_thresh = -125
)

# Get continental US map data
continental_states <- map_data("state") |>
    filter(!region %in% continental_bounds$states_exclude)

# Function to check if point is within continental bounds
is_continental <- function(long, lat, bounds) {
    between(long, bounds$long[1], bounds$long[2]) &
        between(lat, bounds$lat[1], bounds$lat[2])
}

# Process agencies data
cleaned_agencies <- agencies |>
    # Initial filtering
    filter(
        !agency_type %in% c(NA, "Unknown", "NA"),
        !state %in% str_to_title(continental_bounds$states_exclude),
        !(agency_type == "Other" & longitude < continental_bounds$outlier_thresh)
    ) |>
    # Filter to continental bounds
    filter(is_continental(longitude, latitude, continental_bounds))

# Calculate agency counts and create labels
agency_counts <- cleaned_agencies |>
    count(agency_type) |>
    arrange(desc(n)) |>
    mutate(
        label = str_glue("{agency_type}\n(n = {format(n, big.mark=',')})"), 
        pct_total = n/sum(n) * 100,
        agency_type = factor(agency_type, levels = agency_type)
    )

# Prepare final dataset for plotting
filtered_agencies <- cleaned_agencies |>
    left_join(agency_counts |> select(agency_type, label),
              by = "agency_type") |>
    mutate(
        # Set factor levels based on counts
        agency_type = factor(agency_type, 
                             levels = levels(agency_counts$agency_type)),
        # Create labels for faceting
        agency_label = factor(agency_type,
                              levels = levels(agency_type),
                              labels = agency_counts$label)
    )

# Extract total for use in plot
total_agencies <- agency_counts$total_agencies[1]

# Calculate summary statistics 
summary_stats <- list(
    total_agencies = sum(agency_counts$n),
    num_categories = n_distinct(filtered_agencies$agency_type),
    city_county_pct = round(100 * sum(agency_counts$n[1:2])/sum(agency_counts$n))
)

# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
    palette = c(
        "City"                  = "#AD225E",         
        "County"                = "#D67195",       
        "State Police"          = "#228B7D",  
        "Other State Agency"    = "#B8860B", 
        "University or College" = "#2D439E",
        "Other"                 = "#8B4513",         
        "Tribal"                = "#666666"         
    )
)

### |-  titles and caption ----
title_text <- str_glue("Geographic Distribution of U.S. Law Enforcement Agencies")

subtitle_text <-  str_glue(
    "Showing **{format(summary_stats$total_agencies, big.mark=',')}** agencies across {summary_stats$num_categories} categories<br>",
    "**City** and **County** agencies account for **{summary_stats$city_county_pct}%** of all agencies"
)

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 07,
    source_text = "FBI Crime Data API"
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
        # Remove axes
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        # Style facet labels
        strip.text = element_text(
            size = rel(0.75),
            face = "bold",
            family = fonts$text,
            color = colors$title,
            margin = margin(b = 8, t = 8)
        ),
        
        # Add spacing
        panel.spacing = unit(1.2, "lines"),
        
        # Plot margins 
        plot.margin = margin(10, 10, 10, 10),
    )
)

# Set theme
theme_set(weekly_theme)

### |-  Map  ----
ggplot() +
    # Geoms
    geom_polygon(
        data = continental_states, 
        aes(x = long, y = lat, group = group),
        fill = "gray95",     
        color = "gray80", 
        linewidth = 0.3
    ) +
    geom_point(
        data = filtered_agencies,
        aes(x = longitude, 
            y = latitude,
            color = agency_type,
            alpha = agency_type),  
        size = 0.5
    ) +
    
    # Scales
    scale_alpha_manual(
        values = c(
            "City" = 0.4,
            "County" = 0.4,
            "Other" = 0.7,
            "Other State Agency" = 0.7,
            "State Police" = 0.7,
            "Tribal" = 0.8,    
            "University or College" = 0.7
        ),
        guide = "none"
    ) +
    scale_color_manual(
        values = colors$palette,
        guide = "none"
    ) +
    coord_fixed(
        1.3,
        xlim = continental_bounds$long,
        ylim = continental_bounds$lat
    ) +
    
    # Labs
    labs(
        x = NULL,
        y = NULL,
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text,
    ) +
    
    # Facets 
    facet_wrap(
        ~agency_label, 
        ncol = 3,
        scales = "fixed"
    ) +
    
    # Theme
    theme(
        plot.title = element_text(
            size = rel(1.3),
            family = fonts$title,
            face = "bold",
            color = colors$title,
            lineheight = 1.1,
            margin = margin(t = 0, b = 8)
        ),
        plot.subtitle = element_markdown(
            size = rel(0.7),
            lineheight = (1.2),
            family = fonts$subtitle,
            color = colors$subtitle,
            margin = margin(b = 15)
        ),
        plot.caption = element_markdown(
            size   = rel(0.5),
            family = fonts$caption,
            color  = colors$caption,
            hjust  = 1,
            margin = margin(t = 0, b = 0)
        )
    )



# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-02-13
# rstudio  2024.12.0+467 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# P annotater      0.2.3    2024-01-26 [?] CRAN (R 4.4.0)
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P cachem         1.0.8    2023-05-01 [?] CRAN (R 4.4.0)
# P callr          3.7.6    2024-03-25 [?] CRAN (R 4.4.0)
# P camcorder    * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# P desc           1.4.3    2023-12-10 [?] CRAN (R 4.4.0)
# P devtools       2.4.5    2022-10-11 [?] CRAN (R 4.4.0)
# P digest         0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P ellipsis       0.3.2    2021-04-29 [?] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# P fs             1.6.4    2024-04-25 [?] CRAN (R 4.4.0)
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
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P htmlwidgets    1.6.4    2023-12-06 [?] CRAN (R 4.4.0)
# P httpuv         1.6.15   2024-03-26 [?] CRAN (R 4.4.0)
# P httr2          1.0.1    2024-04-01 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.46     2024-04-06 [?] CRAN (R 4.4.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P later          1.3.2    2023-12-06 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P maps         * 3.4.2    2023-12-15 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P memoise        2.0.1    2021-11-26 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# P mime           0.12     2021-09-28 [?] CRAN (R 4.4.0)
# P miniUI         0.1.1.1  2018-05-18 [?] CRAN (R 4.4.0)
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgbuild       1.4.4    2024-03-17 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P pkgload        1.3.4    2024-01-16 [?] CRAN (R 4.4.0)
# P processx       3.8.4    2024-03-16 [?] CRAN (R 4.4.0)
# P profvis        0.3.8    2023-05-02 [?] CRAN (R 4.4.0)
# P promises       1.3.0    2024-04-05 [?] CRAN (R 4.4.0)
# P ps             1.7.6    2024-01-18 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.3    2024-09-11 [?] CRAN (R 4.4.2)
# P rappdirs       0.3.3    2021-01-31 [?] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P remotes        2.5.0    2024-03-17 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P shiny          1.8.1.1  2024-04-02 [?] CRAN (R 4.4.0)
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
# P tidytuesdayR   1.1.2    2024-09-09 [?] CRAN (R 4.4.2)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P urlchecker     1.0.1    2021-11-30 [?] CRAN (R 4.4.0)
# P usethis        2.2.3    2024-02-19 [?] CRAN (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr          3.0.2    2024-10-28 [?] CRAN (R 4.4.2)
# P xfun           0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# P xtable         1.8-4    2019-04-21 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────────────────────────
# > 