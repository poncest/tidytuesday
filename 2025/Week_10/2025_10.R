
## Challenge: #TidyTuesday 2025 week 10
## Data:      Pixar Films
## Author:    Steven Ponce
## Date:      2025-03-10       


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
    patchwork,      # The Composer of Plots # The Composer of Plots # The Composer of Plots
    ggrepel         # Position Non-Overlapping Text Labels with 'ggplot2'
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  8,
    height =  10,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 10) 

pixar_films <- tt$pixar_films |> clean_names()
public_response <- tt$public_response |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(pixar_films)
skim(pixar_films)

glimpse(public_response)
skim(public_response)



## 4. TIDYDATA ----

### |-  tidy data ----
# Combine the data for easier analysis
pixar_combined <- pixar_films |>
    left_join(public_response, by = "film")

# P1. Film Duration's ----

# Define eras for P1
p1_data <- pixar_combined |>
    mutate(
        era = case_when(
            release_date < as.Date("2001-01-01") ~ "Early Years (1995-2000)",
            release_date < as.Date("2006-01-01") ~ "Finding Nemo Era (2001-2005)",
            release_date < as.Date("2011-01-01") ~ "Golden Age (2006-2010)",
            release_date < as.Date("2016-01-01") ~ "Brave New World (2011-2015)",
            TRUE ~ "Modern Era (2016+)"
        )
    ) |> 
    filter(!is.na(film))

# Define era boundary lines
era_boundaries <- data.frame(
    x = as.Date(c("2001-01-01", "2006-01-01", "2011-01-01", "2016-01-01")),
    ymin = 70,
    ymax = 160
)

# Era labels coordinates
era_labels <- data.frame(
    x = as.Date(c("1997-06-01", "2003-06-01", "2008-06-01", "2013-06-01", "2020-06-01")),
    y = c(rep(160, 5)),
    label = c("Early Years", "Finding\nNemo Era", "Golden Age", "Brave New\nWorld", "Modern Era")
)

# P2. Audience vs. Critic Reception ----
p2_data <- pixar_combined |>  
    filter(!is.na(rotten_tomatoes) & !is.na(cinema_score)) |>
    mutate(
        # Convert Cinema Score to numeric
        cinema_score_numeric = case_when(
            cinema_score == "A+" ~ 10,
            cinema_score == "A" ~ 9,
            cinema_score == "A-" ~ 8.5,
            cinema_score == "B+" ~ 8,
            cinema_score == "B" ~ 7,
            cinema_score == "B-" ~ 6.5,
            TRUE ~ 5
        ),
        # Create release year groups
        release_decade = case_when(
            release_date < as.Date("2000-01-01") ~ "1995-1999",
            release_date < as.Date("2010-01-01") ~ "2000-2009",
            release_date < as.Date("2020-01-01") ~ "2010-2019",
            TRUE ~ "2020+"
        ),
        # Create selective labeling flag
        label_film = case_when(
            cinema_score_numeric >= 9 & rotten_tomatoes < 90 ~ TRUE,
            cinema_score_numeric < 10 & rotten_tomatoes >= 99 ~ TRUE,
            rotten_tomatoes < 50 ~ TRUE,
            film %in% c("Toy Story", "Inside Out", "Coco", "Soul") ~ TRUE,
            TRUE ~ FALSE
        )
    )

# Create a filtered dataframe for labels
p2_labels <- p2_data |> filter(label_film == TRUE)

# Define reception area dividing lines
reception_lines <- data.frame(
    type = c("vertical_1",  "horizontal"),
    x1 = c(90, 40),
    y1 = c(5, 9),
    x2 = c(90, 105),
    y2 = c(10.5, 9)
)



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Color for P1
colors <- get_theme_colors(palette = c(
    "gray40", "gray60", "gray70", "gray80", "#87BBA2")
    )

### |-  titles and caption ----
title_text <- str_glue("A Tale of Pixar's Evolution: Duration and Reception")

subtitle_text <- str_glue("How Pixar films have evolved in length and how audiences vs critics perceive them.")

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 10,
    source_text =  "pixarfilms R package" 
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
        legend.position = "plot",
        legend.title = element_text(family = fonts$text, size = rel(0.8)),
        legend.text = element_text(family = fonts$text, size = rel(0.7)),
        
        # Plot margins 
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    )
)

# Set theme
theme_set(weekly_theme)

# P1. Film Duration's ----
p1 <- ggplot(p1_data, aes(x = release_date, y = run_time)) +
  # Geoms
  geom_vline(
    data = era_boundaries,
    aes(xintercept = x),
    linetype = "dotted",
    color = colors$palette[2],
    alpha = 0.4
  ) +
  geom_smooth(
    method = "loess", se = FALSE, color = colors$palette[1],
    linewidth = 0.5, alpha = 0.6, linetype = "dashed", na.rm = TRUE
  ) +
  geom_text_repel(
    aes(label = film),
    size = 3,
    max.overlaps = 10,
    box.padding = 0.5,
    colour = colors$palette[1],
    segment.color = colors$palette[3],
    min.segment.length = 0.2,
    seed = 123
  ) +
  geom_point(size = 3, color = colors$palette[5], alpha = 0.9, na.rm = TRUE) +
  # Annotate
  annotate(
    "text",
    x = era_labels$x,
    y = era_labels$y,
    label = era_labels$label,
    alpha = 0.8,
    fontface = "bold",
    size = 3
  ) +
  # Scales
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    limits = c(as.Date("1994-01-01"), NA)
  ) +
  scale_y_continuous(
    breaks = seq(80, 180, by = 10),
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = "The Evolution of Pixar Film Durations (1995-2023)",
    subtitle = "From 80-minute early films to 100+ minute modern features",
    x = "Release Date",
    y = "Run Time (minutes)"
  )

# P2. Audience vs. Critic Reception ----
p2 <- ggplot(p2_data, aes(x = rotten_tomatoes, y = cinema_score_numeric)) +
  # Geoms
  geom_segment(
    data = reception_lines,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    linetype = "dotted", color = colors$palette[1], alpha = 0.8
  ) +
  geom_text_repel(
    data = p2_labels,
    aes(label = film),
    size = 3,
    box.padding = 0.8,
    point.padding = 0.5,
    colour = colors$palette[1],
    segment.color = colors$palette[3],
    min.segment.length = 0.2,
    max.overlaps = 30,
    force = 3,
    seed = 145
  ) +
  geom_point(
    aes(size = 3),
    color = colors$palette[5], alpha = 0.8
  ) +
  # Annotate
  annotate(
    "text",
    x = 105, y = 10.8,
    label = "Universal Acclaim",
    size = 3, fontface = "bold", alpha = 0.7,
    color = colors$palette[1],
    hjust = 1
  ) +
  annotate(
    "text",
    x = 105, y = 7,
    label = "Critic Favorite",
    size = 3, fontface = "bold", alpha = 0.7,
    color = colors$palette[1],
    hjust = 1
  ) +
  annotate(
    "text",
    x = 75, y = 10.8,
    label = "Audience Favorite",
    size = 3, fontface = "bold", alpha = 0.7,
    color = colors$palette[1],
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = 75, y = 7,
    label = "Mixed Reception",
    size = 3, fontface = "bold", alpha = 0.7,
    color = colors$palette[1],
    hjust = 0.5
  ) +
  # Scales
  scale_color_brewer(
    palette = "Set1",
    name = "Film Era"
  ) +
  scale_size_continuous(
    range = c(2, 5),
    name = "Runtime (min)"
  ) +
  scale_y_continuous(
    breaks = c(10, 9, 8.5, 8, 7, 6.5),
    labels = c("A+", "A", "A-", "B+", "B", "B-"),
    limits = c(6, 10.8),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_x_continuous(
    breaks = seq(40, 100, by = 10),
    limits = c(40, 105),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = "Audience vs. Critic Reception of Pixar Films",
    subtitle = "Comparing Cinema Score (audience ratings) with Rotten Tomatoes (critic consensus)",
    y = "Cinema Score (audience rating)",
    x = "Rotten Tomatoes Score (critic rating)"
  ) 

# Combined Plot -----
combined_plot <- (p1 / plot_spacer() / p2) +
  plot_layout(
    height = c(1, 0.005, 1),
    ncol = 1
  )

combined_plot <- combined_plot +
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
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_text(
        size = rel(0.85),
        family = fonts$subtitle,
        color = colors$subtitle,
        lineheight = 1.2,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size   = rel(0.65),
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

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-10
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# P annotater      0.2.3    2024-01-26 [?] CRAN (R 4.4.0)
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
# P R.cache        0.16.0   2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3    1.8.2    2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo           1.26.0   2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils        2.12.3   2023-11-18 [?] CRAN (R 4.4.0)
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
# P styler         1.10.3   2024-04-07 [?] CRAN (R 4.4.0)
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
# ──────────────────────────────────────────────────────────────────────────────────────────────────
# > 
