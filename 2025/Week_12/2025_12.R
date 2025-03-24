
## Challenge: #TidyTuesday 2025 week 12
## Data:      Text Data From Amazon's Annual Reports
## Author:    Steven Ponce
## Date:      2025-03-23  


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
    tidytext,       # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools
    # RColorBrewer,   # ColorBrewer Palettes
    # textdata,       # Download and Load Various Text Datasets
    patchwork       # The Composer of Plots
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  10,
    height =  8,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 12) 

report_words_clean <- tt$report_words_clean |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(report_words_clean)
skim(report_words_clean)


## 4. TIDYDATA ----

### |-  tidy data ----
text_df <- report_words_clean |>
    group_by(year, word) |>
    summarize(
        count = n(), 
        .groups = "drop"
    )

# Define word categories with targeted keywords
focus_categories <- list(
    innovation = c("innovation", "innovative", "invent", "patent", "technology", "research", 
                   "development", "ai", "machine", "learning", "automation"),
    customer = c("customer", "consumer", "satisfaction", "service", "experience", "value", 
                 "delivery", "prime", "shopping", "retail", "personalization"),
    finance = c("revenue", "profit", "margin", "earning", "cost", "expense", "financial", 
                "billion", "million", "investment", "growth", "cash", "flow", "capital"),
    cloud = c("aws", "cloud", "server", "compute", "storage", "database", "web", "services",
              "infrastructure", "platform", "hosting", "data"),
    operations = c("operations", "logistics", "fulfillment", "warehouse", "distribution", 
                   "supply", "chain", "inventory", "shipping", "transportation", "facility")
)

# Calculate category indices   
focus_indices <- map_dfr(names(focus_categories), function(category) {
    text_df |>
        filter(word %in% focus_categories[[category]]) |>
        group_by(year) |>
        summarize(
            word_count = sum(count),
            total_words = n(),
            index = word_count / total_words * 100,
            category = category,
            .groups = "drop"
        )
})
    
# Cleaner category label mapping
category_labels <- c(
    "innovation" = "Innovation & Technology",
    "customer" = "Customer Experience",
    "finance" = "Financial Performance",
    "cloud" = "Cloud Services (AWS)",
    "operations" = "Operations & Logistics"
)
    
# Key events/milestones for annotation
key_events <- tibble(
    year = c(2006, 2010, 2015, 2017, 2020),
    event = c("AWS\nLaunched", "AWS Revenue\nSurpasses $1B", "AWS Becomes\nProfitable", "Whole Foods\nAcquisition", "COVID-19\nPandemic"),
    index = c(5500, 3500, 5000, 4500, 4800),
    category = NA
)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = c(
        "innovation" = "#E69F00",
        "customer" = "#56B4E9", 
        "finance" = "#009E73",
        "cloud" = "#0072B2",
        "operations" = "#D55E00"
    )
)

### |-  titles and caption ----
title_text <- str_glue("Evolution of Business Priorities in Amazon's Annual Reports")
subtitle_text <- str_glue("Tracking the relative importance of business areas from 2005 to 2023")

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 12,
    source_text =  "Amazon's Annual Reports" 
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
        plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.14), margin = margin(b = 10)),
        plot.subtitle = element_text(family = fonts$subtitle, color = colors$text, size = rel(0.78), margin = margin(b = 20)),
        
        # Axis elements
        axis.title = element_text(color = colors$text, face = "bold", size = rel(0.8)),
        axis.text = element_text(color = colors$text, size = rel(0.7)),
        
        # Grid elements
        panel.grid.minor = element_line(color = "gray80", linewidth = 0.05),
        panel.grid.major = element_line(color = "gray80", linewidth = 0.02),
        
        # Legend elements
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(family = fonts$text, size = rel(0.8), face = "bold"),
        legend.text = element_text(family = fonts$text, size = rel(0.7)),
        
        # two-row legend
        legend.box.spacing = unit(0.4, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.spacing.x = unit(0.2, "cm"),
 
        legend.box = "horizontal",
        legend.box.just = "left",
        
        # Plot margins 
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    )
)

# Set theme
theme_set(weekly_theme)

### |-  plot  ----
ggplot() +
    # Geoms
    geom_line(
        data = focus_indices, 
        aes(x = year, y = index, color = category),
        linewidth = 1.2
    ) +
    geom_point(
        data = focus_indices, 
        aes(x = year, y = index, color = category),
        size = 3
    ) +
    geom_vline(
        data = key_events,
        aes(xintercept = year),
        linetype = "dashed",
        color = "gray50",
        alpha = 0.7
    ) +
    geom_text(
        data = key_events,
        aes(x = year, y = index, label = event),
        hjust = -0.1,
        vjust = 0,
        size = 3,
        fontface = "bold",
        color = "gray30"
    ) +
    # Annotate
    annotate(
        "text",
        x = max(focus_indices$year) + 0.5,
        y = filter(focus_indices, year == max(year) & category == "cloud")$index,
        label = "Cloud services\ndominate recent\nreporting",
        hjust = 0,
        size = 3.5,
        fontface = "italic",
        color = colors$palette[4]
    ) +
    annotate(
        "text",
        x = max(focus_indices$year) + 0.5,
        y = filter(focus_indices, year == max(year) & category == "finance")$index,
        label = "Financial focus\nremains strong",
        hjust = 0,
        size = 3.5,
        fontface = "italic",
        color = colors$palette[3]
    ) +
    # Scales
    scale_color_manual(
        values = colors$palette,
        labels = category_labels,
        name = NULL
    ) +
    scale_x_continuous(
        breaks = seq(min(focus_indices$year), max(focus_indices$year), by = 2),
        expand = expansion(mult = c(0.02, 0.15)) 
    ) +
    scale_y_continuous(
        labels = function(x) format(x, big.mark = ","),
        expand = expansion(mult = c(0.02, 0.02))
    ) +
    # Labs
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = "Year",
        y = "Focus Index (mentions per 100,000 words)"
    ) +
    # Legend
    guides(
        color = guide_legend(
            nrow = 2,
            byrow = TRUE,
            override.aes = list(size = 3)
        )
    ) +
    # Theme
    theme(
        plot.title = element_text(
            size = rel(2),
            family = fonts$title,
            face = "bold",
            color = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.subtitle = element_text(
            size = rel(1),
            family = fonts$subtitle,
            color = alpha(colors$subtitle, 0.9),
            lineheight = 1.2,
            margin = margin(t = 5, b = 30)
        ),
        plot.caption = element_markdown(
            size   = rel(0.65),
            family = fonts$caption,
            color  = colors$caption,
            hjust  = 0.5,
            margin = margin(t = 10)
        )
    ) 
    

# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-23
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder    * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
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
# P janeaustenr    1.0.0    2022-08-26 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.46     2024-04-06 [?] CRAN (R 4.4.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lattice        0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P Matrix         1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
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
# P SnowballC      0.7.1    2023-04-25 [?] CRAN (R 4.4.0)
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
# P tidytext     * 0.4.2    2024-04-10 [?] CRAN (R 4.4.0)
# P tidytuesdayR   1.1.2    2024-09-09 [?] CRAN (R 4.4.2)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tokenizers     0.3.0    2022-12-22 [?] CRAN (R 4.4.0)
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
# ────────────────────────────────────────────────────────────────
# >