
## Challenge: #TidyTuesday 2025 week 05
## Data:      Donuts, Data, and D'oh - A Deep Dive into The Simpsons
## Author:    Steven Ponce
## Date:      2025-01-02


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
    tidytext,       # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools
    ggrepel         # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
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
tt <- tidytuesdayR::tt_load(2025, week = 05) 

characters   <- tt$simpsons_characters |> clean_names()
episodes     <- tt$simpsons_episodes |> clean_names()
locations    <- tt$simpsons_locations |> clean_names()
script_lines <- tt$simpsons_script_lines |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(characters)
glimpse(episodes)
glimpse(locations)
glimpse(script_lines)


## 4. TIDYDATA ----

### |-  tidy data ----

# Set seed for reproducibility
set.seed(123)

# P1. Character Speaking Pattern
speaking_patterns <- script_lines |>
    filter(
        !is.na(raw_character_text), 
        speaking_line == TRUE,
        !(episode_id %in% c(597, 598, 599, 600))  # filter S28
    ) |> 
    group_by(raw_character_text) |>
    summarise(
        total_lines = n(),
        avg_words = mean(word_count, na.rm = TRUE)
    ) |>
    ungroup() |> 
    filter(total_lines > 50) |>
    mutate(
        character_type = case_when(
            raw_character_text %in% c("Homer Simpson", "Marge Simpson", 
                                      "Bart Simpson", "Lisa Simpson") ~ "Simpson Family",
            TRUE ~ "Supporting Characters"
        ),
        show_label = raw_character_text %in% c(
            "Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson",
            "Kent Brockman", "Ralph Wiggum" # Key outliers
        )
    )

# P2. US Viewership Distribution by Season
episodes_filtered <- episodes |>
    filter(season != 28)

# P3. Top 10 Most Talkative Characters
talkative_chars <- script_lines |>
    filter(!is.na(raw_character_text)) |>  
    group_by(raw_character_text) |>
    summarise(
        total_words = sum(word_count, na.rm = TRUE)
    ) |>
    ungroup() |> 
    # Get top 10
    arrange(desc(total_words)) |>
    head(10) |>
    # Add character type and reverse the order
    mutate(
        character_type = case_when(
            raw_character_text %in% c("Homer Simpson", "Marge Simpson", 
                                      "Bart Simpson", "Lisa Simpson") ~ "Simpson Family",
            TRUE ~ "Supporting Characters"
        ),
        # Create factor for reversed ordering
        raw_character_text = factor(raw_character_text, levels = rev(raw_character_text))
    )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
    "Simpson Family"        = "#FED41D", 
    "Supporting Characters" = "grey50",
    " " = "#009DDC"
    )
)

### |-  titles and caption ----
title_text <- str_glue("The Simpsons: Character Dialogue Analysis (2010-2016)")
subtitle_text <- str_glue("Exploring speaking patterns, viewership trends, and character contributions across seasons")

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 05,
    source_text = "The Simpsons Dataset"
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
        # Axis elements
        axis.title = element_text(color = colors$text, size = rel(0.8)),
        axis.text = element_text(color = colors$text, size = rel(0.7)),
        
        # Grid elements
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.1),
        
        # Legend elements
        legend.position = "right",
        legend.title = element_text(family = fonts$text, size = rel(0.8)),
        legend.text = element_text(family = fonts$text, size = rel(0.7)),
        
        # Plot margins 
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),

    )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----

# P1. Character Speaking Pattern
p1 <- ggplot(speaking_patterns, aes(x = total_lines, y = avg_words)) +
    
    # Geoms
    geom_point(aes(size = total_lines, color = character_type, alpha = character_type)) +
    geom_text_repel(
        data = filter(speaking_patterns, show_label),
        aes(label = raw_character_text),
        family = fonts$text,
        size = 4,
        color = "grey30",
        min.segment.length = 0,
        max.overlaps = Inf,
        segment.size = 0.2,
        segment.color = "grey50",
        segment.alpha = 0.5,
        box.padding = 0.5,
        point.padding = 0.3,
        force = 3,
        direction = "both",
        seed = 123
    ) +
    # Control legend order and appearance
    guides(
        color = guide_legend(
            override.aes = list(size = 4),
            order = 1
        ),
        size = guide_legend(
            nrow = 1,
            order = 2,
            override.aes = list(color = "grey70")  
        )
    ) +
    
    # Scales
    scale_x_continuous(
        breaks = seq(0, 6000, 1000),
        labels = scales::label_number(scale = 1e-3, suffix = " K"),  
        limits = c(-100, 6100)
    ) +
    scale_y_continuous(
        breaks = seq(6, 18, 3),
        limits = c(5.5, 18)
    ) +
    scale_color_manual(
        values = colors$palette
    ) +
    scale_alpha_manual(
        values = c("Simpson Family" = 0.9,
                   "Supporting Characters" = 0.5),
        guide = "none"
    ) +
    scale_size_continuous(
        range = c(1, 8),
        breaks = c(100, 500, 1000, 2000),
        labels = scales::comma
    ) +
    coord_cartesian(clip = 'off') +
    
    # Labs
    labs(
        title = "Character Speaking Patterns",
        x = "Total Lines (Thousands)",
        y = "Average Words per Line",
        color = "Character Type",
        size = "Total Lines"
    ) +
    
    # Theme
    theme(
        panel.grid = element_blank(),
        legend.position = c(0.8, 0.84),
        legend.box = "vertical",
        legend.background = element_rect(fill = colors$background, color = NA),
        legend.title = element_text(family = fonts$text, size = 10),
        legend.text = element_text(family = fonts$text, size = 9),
        legend.margin = margin(5, 5, 5, 5),
        
        plot.title = element_text(
            size   = rel(1.3),
            family = fonts$title,
            face   = "bold",
            color  = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
    ) 
   
# P2. US Viewership Distribution by Season
p2 <- ggplot(episodes_filtered, aes(x = factor(season), y = us_viewers_in_millions)) +
    
    # Geoms
    geom_point(
        position = position_jitter(width = 0.2, seed = 123),
        color = colors$palette[3],
        alpha = 0.5,
        size = 2
    ) +
    geom_boxplot(
        fill = colors$palette[1],
        alpha = 0.25, 
        outlier.shape = NA,
        width = 0.5
    ) +
    
    # Scales
    scale_y_continuous(
        breaks = seq(0, 15, 3),
        limits = c(0, 15),
        labels = scales::label_number(scale = 1, suffix = " M"),
    ) +
    
    # Labs
    labs(
        title = "US Viewership Distribution by Season",
        x = "Season",
        y = "US Viewers (Millions)"
    ) +
    
    # Theme
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        
        plot.title = element_text(
            size   = rel(1.3),
            family = fonts$title,
            face   = "bold",
            color  = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
    ) 
    
# P3. Top 10 Most Talkative Characters
p3 <- ggplot(talkative_chars,
             aes(x = raw_character_text, 
                 y = total_words,
                 fill = character_type)) +
    
    # Geoms
    geom_col(
        width = 0.7,
        alpha = 0.9, 
        show.legend = FALSE
    ) +
    geom_text(
        aes(label = scales::comma(total_words)),
        hjust = -0.2,
        family = fonts$text,
        size = 3,
        color = colors$text
    ) +
    
    # Scales
    scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = " K"),  
        expand = expansion(mult = c(0, 0.15))  
    ) +
    scale_fill_manual(
        values = colors$palette
    ) +
    coord_flip() +
    
    # Labs
    labs(
        title = "Top 10 Most Talkative Characters",
        x = NULL,
        y = "Total Words Spoken ((Thousands)"
    ) +
    
    # Theme
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),

        plot.title = element_text(
            size   = rel(1.3),
            family = fonts$title,
            face   = "bold",
            color  = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
    ) 
    
# Combine plots 
combined_plot <- (p1 / (p2 + p3)) +
    
    plot_layout(
        heights = c(1.2, 1),  
        widths = c(1, 1)    
        ) 

combined_plot <- combined_plot +
    # Add overall title, subtitle, and caption
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_text(
                size   = rel(1.8),
                family = fonts$title,
                face   = "bold",
                color  = colors$title,
                lineheight = 1.1,
                margin = margin(t = 5, b = 5)
            ),
            plot.subtitle = element_text(
                size   = rel(1),
                family = fonts$subtitle,
                color  = colors$subtitle,
                lineheight = 1.2,
                margin = margin(t = 5, b = 5)
            ),
            plot.caption = element_markdown(
                size   = rel(0.6),
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

# ─ Session info ─────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-02-02
# rstudio  2024.12.0+467 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────
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
# P ggrepel      * 0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
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
# ────────────────────────────────────────────────────────────────────────────
# > 