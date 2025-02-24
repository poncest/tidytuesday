
## Challenge: #TidyTuesday 2025 week 08
## Data:      Academic Literature on Racial and Ethnic Disparities in Reproductive Medicine in the US
## Author:    Steven Ponce
## Date:      2025-02-24          


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
    patchwork       # The Composer of Plots # The Composer of Plots # The Composer of Plots
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  14,
    height =  12,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 08) 

article_dat <- tt$article_dat |> clean_names()
model_dat <- tt$model_dat |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(article_dat)
skim(article_dat)

glimpse(model_dat)
skim(model_dat)


## 4. TIDYDATA ----

### |-  tidy data ----

# P1. Racial Categories -----
racial_categories_data <- article_dat |>
    select(starts_with("race")) |>
    select(matches("race\\d$")) |>  # Select only race categories, not sample sizes
    pivot_longer(everything(), names_to = "category", values_to = "race") |>
    filter(!is.na(race)) |>
    # Standardize category names and combine similar ones
    mutate(race = case_when(
        str_detect(tolower(race), "white.*non.?hispanic|non.?hispanic.*white") ~ "Non-Hispanic White",
        str_detect(tolower(race), "black.*non.?hispanic|non.?hispanic.*black") ~ "Non-Hispanic Black",
        str_detect(tolower(race), "white") ~ "White",
        str_detect(tolower(race), "black|african") ~ "Black/African American",
        str_detect(tolower(race), "hispanic|latino") ~ "Hispanic/Latino",
        str_detect(tolower(race), "asian|pacific") ~ "Asian/Pacific Islander",
        TRUE ~ race
    )) |>
    count(race) |>
    # Filter out very rare categories
    filter(n >= 5) |>
    # Calculate percentage
    mutate(percentage = n/sum(n) * 100) |>
    # Sort by frequency
    arrange(desc(n)) 


# P2. Sample Size Distribution -----
sample_size_distribution <- article_dat |>
    # Select sample size columns
    select(matches("_ss$")) |>
    # Pivot to long format
    pivot_longer(everything(), 
                 names_to = "group", 
                 values_to = "size") |>
    # Remove missing values and unreasonable values (-99)
    filter(!is.na(size), size > 0, size != -99) |>
    # Group by racial/ethnic category
    group_by(group) |>
    # Calculate summary statistics
    summarise(
        median_size = median(size),
        mean_size = mean(size),
        q25 = quantile(size, 0.25),
        q75 = quantile(size, 0.75),
        max_size = max(size),
        count = n()
    ) |>
    # Sort by median size for better visualization
    arrange(desc(median_size)) |>
    # Create more readable labels
    mutate(
        group_label = case_when(
            group == "race1_ss" ~ "Reference group",
            group == "race2_ss" ~ "Second reported group",
            group == "race3_ss" ~ "Third reported group",
            group == "race4_ss" ~ "Fourth reported group",
            group == "race5_ss" ~ "Fifth reported group",
            group == "race6_ss" ~ "Sixth reported group",
            group == "race7_ss" ~ "Seventh reported group",
            group == "race8_ss" ~ "Eighth reported group",
            group == "eth1_ss" ~ "Reference ethnic group",
            group == "eth2_ss" ~ "Second ethnic group",
            group == "eth3_ss" ~ "Third ethnic group",
            group == "eth4_ss" ~ "Fourth ethnic group",
            group == "eth5_ss" ~ "Fifth ethnic group",
            group == "eth6_ss" ~ "Sixth ethnic group",
            TRUE ~ group
        )
    ) |>
    # Only include groups with sufficient data
    filter(count >= 5)


# P3. Effect Size Distribution -----
effect_size_data <- model_dat |>
    # Filter for relevant measure types and remove invalid data
    filter(
        measure %in% c("OR", "RR", "HR"),
        point != -99,
        point < 10,       
        point > 0.1        
    ) |>
    # Add significance indicator
    mutate(
        significance = case_when(
            lower != -99 & upper != -99 & (lower > 1 | upper < 1) ~ "Significant",
            lower != -99 & upper != -99 ~ "Non-significant",
            TRUE ~ "CI Not Reported"
        ),
        # Create a categorized effect size for potential faceting
        effect_category = case_when(
            point < 0.5 ~ "Strong Negative",
            point < 0.8 ~ "Moderate Negative",
            point < 1.0 ~ "Weak Negative",
            point == 1.0 ~ "No Effect",
            point <= 1.25 ~ "Weak Positive",
            point <= 2.0 ~ "Moderate Positive",
            TRUE ~ "Strong Positive"
        ),
        # Rename measure types 
        measure = case_when(
            measure == "OR" ~ "Odds Ratio",
            measure == "RR" ~ "Risk Ratio",
            measure == "HR" ~ "Hazard Ratio",
            TRUE ~ measure
        )
    ) 



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
    palette = c(
        "#4A6FE3",
        "#D8E1F3",
        "Odds Ratio" = "#4A6FE3",   
        "Risk Ratio" = "#29A2C6",   
        "Hazard Ratio" = "#5D7CBA"
    )
)

### |-  titles and caption ----
title_text <- str_glue("Racial and Ethnic Disparities in Reproductive Medicine Research (2010-2023)")

subtitle_text <- str_glue(
    "Analysis of __318 studies__ from high-impact journals examining disparities in reproductive health<br><br>",
    "This visualization reveals critical patterns in how disparities are studied: inconsistent racial categorization unequal sample sizes across groups,<br> ",
    "and effect sizes clustering slightly above 1.0, suggesting systematic disadvantages for non-reference groups. These methodological choices <br>",
    "impact our understanding of disparities."
)

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 08,
    source_text =  "Racial and ethnic disparities in reproductive medicine in the United States: a narrative review of contemporary high-quality evidence" 
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
        
        # Plot margins 
        plot.margin = margin(10, 10, 10, 10),
    )
)

# Set theme
theme_set(weekly_theme)

# P1. Racial Categories -----
p1 <- ggplot(data = racial_categories_data,
             aes(x = reorder(race, n), y = n)) +
    # Geoms
    geom_bar(stat = "identity", 
             fill =  colors$palette[1],     
             alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", percentage)),
              hjust = -0.2,
              size = 3.5) +
    # Scales
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.15)), 
        breaks = pretty_breaks()
    ) +
    coord_flip() +
    # Labs
    labs(title = "Racial Categories Used in Reproductive Medicine Studies",
         subtitle = "Percentage of studies using each racial category (2010-2023)",
         x = NULL,
         y = "Number of Studies") +
    # Theme
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 40, 20, 20)
    ) 


# P2. Sample Size Distribution -----
p2 <- ggplot(data = sample_size_distribution, 
             aes(x = reorder(group_label, median_size), y = median_size)) +
    # Geoms
    geom_pointrange(
        aes(ymin = q25, ymax = q75),
        size = 0.7,
        color = colors$palette[1],
        fill =  colors$palette[2],
        shape = 21,
        stroke = 1.2
    ) +
    annotate(
        "text", 
        x = 10.8, 
        y = 35, 
        label = "Reference groups have\nmuch larger sample sizes",
        lineheight = 0.9,
        hjust = 0,
        vjust = 1,
        size = 3.5,
        color = "gray30",  
        fontface = "italic"
    ) +
    # Scales
    coord_flip() +
    scale_y_log10(
        labels = label_comma(),
        breaks = c(10, 100, 1000, 10000, 100000)
    ) +
    # Labs
    labs(
        title = "Sample Size Distribution by Racial/Ethnic Group",
        subtitle = "Median sample size with interquartile range (log scale)",
        x = NULL,
        y = "Sample Size (log scale)"
    ) +
    # Theme
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    )


# P3. Effect Size Distribution -----
p3 <- ggplot(data = effect_size_data,
             aes(x = point, fill = measure)) +
    # Geoms
    geom_density(alpha = 0.6, adjust = 1.5) +
    geom_vline(                 # Vertical reference line at 1 (no effect)
        xintercept = 1, 
        linetype = "dashed", 
        color = "gray30",
        size = 0.7
    ) +
    annotate(
        "text", 
        x = 0.7, 
        y = 1.2, 
        label = "Decreased risk/odds",
        hjust = 1,
        size = 3.5,
        fontface = "italic",
        color = "gray30"
    ) +
    annotate(
        "text", 
        x = 1.7, 
        y = 1.2, 
        label = "Increased risk/odds",
        hjust = 0,
        size = 3.5,
        fontface = "italic",
        color = "gray30"
    ) +
    # Scales
    scale_fill_manual(
        values =  colors$palette
    ) +
    scale_x_log10(
        breaks = c(0.2, 0.5, 1, 2, 5),
        labels = c("0.2", "0.5", "1.0", "2.0", "5.0")
    ) +
    # Labs
    labs(
        title = "Distribution of Effect Sizes in Racial/Ethnic Disparity Studies",
        subtitle = "By measure type, showing patterns of reported disparities",
        x = "Effect Size (log scale)",
        y = "Density",
        fill = "Measure Type"
    ) +
    # Theme
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    )

# Combined Plot -----
combined_plot <- (p1 + p2) / p3 +
    plot_layout(heights = c(1, 1)) +
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_text(
                size   = rel(2.1),
                family = fonts$title,
                face   = "bold",
                color  = colors$title,
                lineheight = 1.1,
                margin = margin(t = 5, b = 5)
            ),
            plot.subtitle = element_markdown(
                size   = rel(0.95),
                family = fonts$subtitle,
                color  = colors$subtitle,
                lineheight = 1.2,
                margin = margin(t = 5, b = 5)
            ),
            plot.caption = element_markdown(
                size   = rel(0.75),
                family = fonts$caption,
                color  = colors$caption,
                hjust  = 0.5,
                margin = margin(t = 10)
            ),
            plot.margin = margin(t = 20, r = 10, b = 20, l = 10),
        )
    )

combined_plot

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
# date     2025-02-24
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder    * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# P digest         0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
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
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P httr2          1.0.1    2024-04-01 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.46     2024-04-06 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman         0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
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
# P xfun           0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────
# > 