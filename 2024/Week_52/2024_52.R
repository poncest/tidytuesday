
## Challenge: #TidyTuesday 2024 week 52
## Data:      Global Holidays and Travel
## Author:    Steven Ponce
## Date:      2024-12-09


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
    camcorder       # Record Your Plot History 
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  10,
    height =  10,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))



## 2. READ IN THE DATA ----
# tt <- tidytuesdayR::tt_load(2024, week = 52)
#
# spells_raw  <- tt$spells |> clean_names()
#
# tidytuesdayR::readme(tt)
# rm(tt)

# Option 2: Read directly from GitHub
global_holidays_raw <- readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/global_holidays.csv') |> 
    clean_names()

monthly_passengers_raw <- readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/monthly_passengers.csv') |> 
    clean_names()



## 3. EXAMINING THE DATA ----
glimpse(global_holidays_raw)
skim(global_holidays_raw)

glimpse(monthly_passengers_raw)
skim(monthly_passengers_raw)



## 4. TIDYDATA ----

# Clean and join the datasets
monthly_passengers_clean <- monthly_passengers_raw |>
    mutate(
        date = ymd(paste(year, month, "01", sep = "-")),
        total_passengers = coalesce(total, total_os)     # Use total_os when total is NA
    )

monthly_holidays_clean <- global_holidays_raw |>
    mutate(
        year = year(date),
        month = month(date)
    ) |>
    group_by(iso3, year, month) |>
    summarise(
        holiday_count = n(),
        public_holidays = sum(type == "Public holiday"),
        .groups = "drop"
    )

combined_data <- monthly_passengers_clean |>
    left_join(monthly_holidays_clean, by = c("iso3", "year", "month"))

# Housekeeping
rm(global_holidays_raw, monthly_passengers_raw, monthly_holidays_clean, monthly_passengers_clean)


# data plot ---
volatility_df <- combined_data |>
    # Calculate summary statistics by country
    group_by(iso3) |>
    summarise(
        mean_traffic = mean(total_passengers, na.rm = TRUE),
        sd_traffic = sd(total_passengers, na.rm = TRUE),
        cv = sd_traffic / mean_traffic,
        avg_holidays = mean(holiday_count, na.rm = TRUE),
        total_observations = n(),
        traffic_size = sum(total_passengers, na.rm = TRUE),
        .groups = "drop"
    ) |>
    # Remove NA, infinite, or outlier values
    filter(
        complete.cases(cv, avg_holidays),
        total_observations >= 12,              # At least one year of data
        cv >= 0,                               # Ensure no negative coefficients of variation
        cv <= quantile(cv, 0.95, na.rm = TRUE) # Remove extreme outliers
    ) |>
    # Add size categories for visualization
    mutate(
        size_category = cut(
            traffic_size,
            breaks = quantile(traffic_size, probs = seq(0, 1, 0.25), na.rm = TRUE),
            labels = c("Small", "Medium", "Large", "Very Large"),
            include.lowest = TRUE
        )
    )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c("#4B79B7", "#F8F9FA", "#2C3E50", "#34495E", "#7F8C8D"))

### |-  titles and caption ----
title_text    <- str_glue("More Holidays Associated with Lower Air Traffic Volatility\nin Larger Markets")
subtitle_text <- str_glue("Higher holiday frequency correlates with reduced traffic volatility, especially in larger markets<br>
                          Lower CV values indicate more stable traffic patterns<br><br>
                          **Coefficient of Variation in Traffic**")

# Create caption
caption_text <- create_social_caption(
    tt_year = 2024,
    tt_week = 52,
    source_text = "WorldPop Hub"
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
        
        strip.text            = element_textbox(
            size              = rel(0.9),
            face              = 'bold',
            color             = colors[3],
            fill              = alpha(colors[1], 0.1),
            box.color         = alpha(colors[1], 0.5),
            halign            = 0.5,
            linetype          = 1,
            r                 = unit(3, "pt"),
            width             = unit(1, "npc"),
            padding           = margin(5, 10, 5, 10),
            margin            = margin(b = 10)
        ),
        
        legend.margin         = margin(-25, 5, 0, 0), # align the legend with the y-axis label
        legend.position       = "top",
        legend.title          = element_text(size = rel(0.7)),
        legend.text           = element_text(size = rel(0.6)),
        legend.justification.top = "right",
    )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----
ggplot(volatility_df, aes(x = avg_holidays, y = cv)) +
    # Add reference line for overall median
    geom_hline(
        yintercept = median(volatility_df$cv),
        linetype = "dashed",
        color = "gray50",
        alpha = 0.3
    ) +
    # Add points
    geom_point(
        aes(
            size = traffic_size,
            alpha = cv  # Vary transparency by CV
        ),
        color = colors$palette[1]
    ) +
    # Add trend line
    geom_smooth(
        color = colors$palette[3],
        method = "loess",
        linewidth = 1,
        se = TRUE
    ) +
    # Add labels for extreme points (cv)
    ggrepel::geom_text_repel(
        data = volatility_df |> 
            group_by(size_category) |>  
            filter(cv == max(cv) | cv == min(cv)),
        aes(label = iso3),
        size = 3,
        color = colors$palette[4],
        max.overlaps = 2,
        box.padding = 0.5,
        segment.color = colors[5],
        segment.alpha = 0.5
    ) +
    # Add single annotation for the median line
    geom_text(
        data = volatility_df |> filter(size_category == "Small"),
        x = 9,  
        y = median(volatility_df$cv) + 0.02, 
        label = "Industry median volatility",
        size = 3,
        color = "gray50",
        hjust = 1,
        vjust = -0.5
    ) +
    # Add correlation annotation in each facet
    geom_text(
        data = volatility_df |>  
            group_by(size_category) |>
            summarise(
                cor = cor(avg_holidays, cv),
                .groups = "drop"
            ),
        aes(x = 8, y = 0.65, 
            label = sprintf("r = %.2f", cor)),
        size = 3,
        hjust = 1
    ) +
    
    # Scales
    scale_y_continuous(
        breaks = seq(0, 1, by = .25),
        limits = c(-.25, .75),
        labels = percent_format()
    ) +
    scale_x_continuous(
        breaks = seq(2, 8, by = 2),
        limits = c(1, 9),
        expand = expansion(mult = c(0.02, 0.08))          
    ) +
    scale_size_continuous(
        range = c(2, 8),
        labels = scales::label_number(scale = 1e-6, suffix = "M")
    ) +
    scale_alpha_continuous(
        range = c(0.4, 0.8),
        guide = "none"
    ) +
    # Labs
    labs(
        x = "Average Number of Holidays per Month",
        y = NULL,
        size  = "Annual Passenger Traffic (M)",
        color = "Market Size",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text
    ) +
    # Facets
    facet_wrap(
        ~size_category,
        labeller = as_labeller(function(x) paste(x, "Market")),
        scales = "fixed"
    ) +
    # Theme
    theme(
        plot.title = element_text(
            size   = rel(2),
            family = fonts$title,
            face   = "bold",
            color  = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.subtitle = element_markdown(
            size   = rel(1),
            family = fonts$subtitle,
            color  = colors$subtitle,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.caption = element_markdown(
            family = fonts$caption,
            size   = rel(0.65),
            color  = colors$caption,
            hjust  = 0.5,
            margin = margin(t = 10)
        )
    )  


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-12-11
# rstudio  2024.09.1+394 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder   * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
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
# P ggrepel       0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
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
# P lattice       0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown      1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P Matrix        1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# P mgcv          1.9-1    2023-12-21 [?] CRAN (R 4.4.0)
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P nlme          3.1-164  2023-11-27 [?] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
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
# P skimr       * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P splines       4.4.0    2024-04-24 [?] local
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
# P vroom         1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# withr         3.0.1    2024-07-31 [1] CRAN (R 4.4.1)
# P xfun          0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────
# > 
