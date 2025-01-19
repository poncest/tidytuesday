
## Challenge: #TidyTuesday 2025 week 03
## Data:      The History of Himalayan Mountaineering Expeditions
## Author:    Steven Ponce
## Date:      2025-01-19


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
    camcorder       # Record Your Plot History 
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  14,
    height =  14,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))



## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 03) 

exped_tidy <- tt$exped_tidy |> clean_names()
peaks_tidy <- tt$peaks_tidy |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(exped_tidy)
skim(exped_tidy)

glimpse(peaks_tidy)
skim(peaks_tidy)


## 4. TIDYDATA ----

### |-  plot data ----

# 1. First Ascent Timeline
first_ascents_data <- peaks_tidy |>
  filter(!is.na(pyear)) |>
  group_by(pyear) |>
  summarise(
    first_ascents = n(),
    .groups = "drop"
  ) |>
  # Add flag for special years
  mutate(
    highlight = case_when(
      pyear == 1953 ~ "Everest",
      pyear >= 2020 & pyear <= 2021 ~ "Covid",
      TRUE ~ "Regular"
    )
  )


# 2. Success Rate by Team Size Category
team_success_data <- exped_tidy |>
  filter(totmembers > 0) |>
  mutate(
    team_size = cut(
      totmembers,
      breaks = c(0, 5, 10, 15, Inf),
      labels = c("1-5", "6-10", "11-15", "15+"),
      right = TRUE
    )
  ) |>
  group_by(team_size) |>
  summarise(
    total = n(),
    successes = sum(success1 == TRUE, na.rm = TRUE),
    success_rate = successes / total
  ) |>
  # Create text for labels
  mutate(
    label_position = success_rate,
    success_pct = paste0(round(success_rate * 100), "%"),
    total_label = paste0("n = ", total)
  )

# 3. Distribution of Climbing Status by Mountain Range
climbing_status_data <- peaks_tidy |>
  # Count peaks by range and status
  group_by(himal_factor, pstatus_factor) |>
  summarise(count = n(), .groups = "drop") |>
  # Calculate total peaks per range for sorting
  group_by(himal_factor) |>
  mutate(
    total_peaks = sum(count),
    pct = count / total_peaks,
    # Create labels with count and percentage for larger values
    label = if_else(count >= 3,
      as.character(count),
      ""
    ), # Only show labels for count >= 3
    # Create total peaks label with consistent format
    total_label = paste0(total_peaks, " peaks")
  ) |>
  ungroup() |>
  # Sort by total peaks
  mutate(
    himal_factor = fct_reorder(himal_factor, total_peaks)
  )

# 4. Accidents vs. Expedition Size 
accident_data <- exped_tidy |>
  filter(
    totmembers > 0,
    totmembers <= 30
  ) |>
  group_by(totmembers) |>
  summarise(
    total_expeditions = n(),
    accidents = sum(mdeaths + hdeaths > 0, na.rm = TRUE),
    accident_rate = accidents / total_expeditions,
    .groups = "drop"
  ) |>
  # Filter for more reliable statistics
  filter(total_expeditions >= 5)



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
    primary   = "#2E86C1",    # Main blue for time series/success
    secondary = "#E67E22",    # Orange for contrasts
    success   = "#27AE60",    # Green for success metrics
    risk      = "#E74C3C",    # Red for risks/accidents
    neutral   = "gray90"      # Background elements
    )
)


### |-  titles and caption ----
title_text <- str_glue("The Paradox of Himalayan Climbing Expeditions")

subtitle_text <- str_glue("While __larger teams__ achieve __higher success rates__, they also face __increased risks__.<br><br>
                          Analysis of climbing patterns, team dynamics, and safety implications from 1925 to 2024")

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 03,
    source_text = "The Himalayan Database"
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
        plot.title = element_text(face = "bold", size = rel(1.14), margin = margin(b = 10)),
        plot.subtitle = element_text(color = colors$text, size = rel(0.78), margin = margin(b = 20)),
        
        # Axis formatting
        axis.title = element_text(color = colors$text, size = 10),
        axis.text = element_text(color = colors$text, size = 9),
        
        # Legend formatting 
        legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.margin = margin(b = 10),
        
        # Grid customization
        panel.grid.minor = element_blank(),
        
        # Plot margins 
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        
        # Legend formatting 
        legend.box.margin = margin(b = 15),
        legend.spacing = unit(0.2, "cm"),
        legend.box.spacing = unit(0.2, "cm"),
        legend.key.size = unit(0.8, "lines")
    )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----

# 1. First Ascent Timeline
p1 <- ggplot(first_ascents_data,
             aes(x = pyear, y = first_ascents)) +
    # Geoms
    geom_hline(
        yintercept = seq(0, 14, 2), 
        color = colors$palette["neutral"], 
        linewidth = 0.3
    ) +
    geom_step(
        color = colors$palette["primary"],
        linewidth = 0.8
    ) +
    geom_smooth(
        aes(x = pyear, y = first_ascents),
        method = "loess",
        color = alpha(colors$palette["primary"], 0.2),
        se = FALSE,
        linewidth = 0.8,
        span = 0.3
    ) +
    geom_point(
        aes(color = highlight),
        size = 2,
        alpha = 0.7
    ) +
    # Scales
    scale_x_continuous(
        breaks = c(1920, 1940, 1960, 1980, 2000, 2020),
        expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
        breaks = seq(0, 14, 2),
        limits = c(0, 14),
        expand = expansion(mult = c(0, 0.1))
    ) +
    scale_color_manual(
        values = c(
            "Everest" = colors$palette["secondary"],
            "Covid" = colors$palette["risk"],
            "Regular" = colors$palette["primary"]
        ),
        guide = "none"
    ) +
    # Annotations
    annotate(
        "text",
        x = 1953,
        y = 14,
        label = "First Everest\nAscent",
        size = 3,
        color = colors$palette["secondary"],
        hjust = 0.5
    ) +
    annotate(
        "segment",
        x = 1953,
        xend = 1953,
        y = 9,
        yend = 13,
        color = colors$palette["secondary"],
        alpha = 0.5,
        linewidth = 0.5
    ) +
    annotate(
        "text",
        x = 2020,
        y = 1,
        label = "COVID-19\nPandemic",
        size = 3,
        color = colors$palette["risk"],
        hjust = 0.5
    ) +
    # Labs
    labs(
        title = "The Dawn of Himalayan Climbing",
        subtitle = "Number of first ascents recorded each year (1925-2024)",
        x = "Year",
        y = "Number of First Ascents"
    ) +
    # Theme
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray95")
    )

# 2. Success Rate by Team Size Category
p2 <- ggplot(team_success_data) +
    # Geoms
    geom_col(
        aes(x = team_size, y = success_rate),
        fill = colors$palette["success"],
        width = 0.7
    ) +
    geom_text(
        aes(x = team_size, y = success_rate, label = success_pct),
        nudge_y = -0.03,
        color = "white",
        fontface = "bold",
        size = 3.5
    ) +
    geom_text(
        aes(x = team_size, y = 0, label = total_label),
        nudge_y = 0.05,
        color = "white",
        size = 3
    ) +
    geom_hline(
        yintercept = 0.5,
        linetype = "dashed",
        color = "gray70",
        linewidth = 0.3
    ) +
    # Scales
    scale_y_continuous(
        labels = percent,
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2),
        expand = expansion(mult = c(0.02, 0.02))
    ) +
    # Labs
    labs(
        title = "Larger Teams, Higher Success Rates",
        subtitle = "Success rate and total number of expeditions by team size",
        x = "Team Size (Number of Members)",
        y = "Success Rate"
    ) +                                    
    # Theme 
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray95")
    )

# 3. Distribution of Climbing Status by Mountain Range
p3 <- ggplot(climbing_status_data,
             aes(x = count, 
                 y = himal_factor,
                 fill = pstatus_factor)) +  
    # Geoms
    geom_col(
        position = "stack",
        width = 0.7
    ) +
    geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 3,
        fontface = "bold"
    ) +
    geom_text(
        data = subset(climbing_status_data, !duplicated(himal_factor)),
        aes(x = -1, label = sprintf("%d peaks", total_peaks)),  # Simplified label
        hjust = 1,
        size = 3,
        color = "gray30"
    ) +
    # Scales
    scale_x_continuous(
        expand = expansion(mult = c(0.3, 0.05)),  # Increased left expansion
        breaks = seq(0, 80, 20)
    ) +
    scale_fill_manual(
        name = "Status",
        breaks = c("Unclimbed", "Climbed"),
        values = setNames(
            c(colors$palette["secondary"], colors$palette["primary"]),
            c("Unclimbed", "Climbed")
        )
    ) +
    # Labs
    labs(
        title = "Mountain Ranges: Conquests and Challenges",
        subtitle = "Number of climbed and unclimbed peaks in each mountain range",
        x = "Number of Peaks",
        y = NULL
    ) +
    # Theme 
    theme(
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray95")
    )

# 4. Accidents vs. Expedition Size 
p4 <- ggplot(accident_data) +
    # Geoms
    geom_hline(
        yintercept = seq(0, 0.3, 0.05),
        color = "gray95",
        linewidth = 0.3
    ) +
    geom_line(
        aes(x = totmembers, y = accident_rate),
        color = colors$palette["risk"],
        linewidth = 0.5,
        alpha = 0.6
    ) +
    geom_point(
        aes(x = totmembers, y = accident_rate,
            size = total_expeditions),
        color = colors$palette["risk"],
        alpha = 0.7,
        stroke = 0
    ) +
    geom_label(
        aes(x = 18, y = 0.25,
            label = "Larger teams tend to have higher\naccident rates, possibly due to\nmore complex logistics and\nincreased exposure time"),
        size = 3,
        color = colors$text,
        fill = alpha(colors$palette["primary"], 0.01),
        label.size = 0.25,  
        label.padding = unit(0.5, "lines"),  
        hjust = 0
    ) +
    # Scales
    scale_y_continuous(
        labels = percent_format(),
        limits = c(-0.02, 0.3),
        breaks = seq(0, 0.3, 0.05),
        expand = expansion(mult = c(0, 0.02))
    ) +
    scale_x_continuous(
        breaks = seq(0, 30, 5),
        expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_size_continuous(
        range = c(2, 8),
        breaks = c(10, 25, 50, 100),
        labels = c("10", "25", "50", "100+"),
        guide = guide_legend(
            title.position = "top",
            override.aes = list(color = colors$palette["risk"], alpha = 0.7)
        )
    ) +
    # Labs
    labs(
        title = "Larger Teams Face Higher Risks",
        subtitle = "Accident rates increase with team size, based on Himalayan expeditions data",
        x = "Team Size (Number of Members)",
        y = "Accident Rate",
        size = "Number of Expeditions"
    ) +
    # Theme 
    theme(
        panel.grid.major = element_line(color = "gray95"),
        legend.key = element_blank()
    )

# Combine plots 
combined_plot <- (p1 | p3) / (p4 | p2) +  
    plot_layout(
        heights = c(1, 1),  
        widths = c(1, 1)    
    ) 

combined_plot +
    # Add title, subtitle, and caption 
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_text(
                size   = rel(2.6),
                family = fonts$title,
                face   = "bold",
                color  = colors$title,
                lineheight = 1.1,
                margin = margin(t = 5, b = 5)
            ),
            plot.subtitle = element_markdown(
                size   = rel(1.2),
                family = fonts$subtitle,
                color  = colors$subtitle,
                lineheight = 1.2,
                margin = margin(t = 5, b = 15)
            ),
            plot.caption = element_markdown(
                size   = rel(0.7),
                family = fonts$caption,
                color  = colors$caption,
                hjust  = 0.5,
                margin = margin(t = 10)
            )
        )
    ) & 
    # Add spacing between plots
    theme(panel.spacing = unit(2, "cm"))


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
# date     2025-01-18
# rstudio  2024.12.0+467 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────
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
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.46     2024-04-06 [?] CRAN (R 4.4.0)
# P lattice        0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P Matrix         1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# P mgcv           1.9-1    2023-12-21 [?] CRAN (R 4.4.0)
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P nlme           3.1-164  2023-11-27 [?] CRAN (R 4.4.0)
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
# P splines        4.4.0    2024-04-24 [?] local
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
# ───────────────────────────────────────────────────────────────────────────────────────────────────
# > 