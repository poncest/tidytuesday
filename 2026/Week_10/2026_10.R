## Challenge: #TidyTuesday 2026 week 10
## Data:      How likely is 'likely'?
## Author:    Steven Ponce
## Date:      2026-03-08

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor,      
    scales, glue, skimr 
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 10,
    height = 7,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
# tt <- tidytuesdayR::tt_load(2026, week = 10)
# absolute_judgements <- tt$absolute_judgements |> clean_names()
# pairwise_comparisons <- tt$pairwise_comparisons |> clean_names()
# respondent_metadata <- tt$respondent_metadata |> clean_names() 
# rm(tt)

# Option 2: Read directly from GitHub
base_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/"

absolute_judgements  <- read_csv(paste0(base_url, "absolute_judgements.csv"))  |> clean_names()
pairwise_comparisons <- read_csv(paste0(base_url, "pairwise_comparisons.csv")) |> clean_names()
respondent_metadata  <- read_csv(paste0(base_url, "respondent_metadata.csv"))  |> clean_names()


## 3. EXAMINING THE DATA ----
glimpse(absolute_judgements)
glimpse(pairwise_comparisons)
glimpse(respondent_metadata)

skim(absolute_judgements)
skim(pairwise_comparisons)
skim(respondent_metadata)


## 4. TIDY DATA ----
### |- term-level summary statistics ----
term_summary <- absolute_judgements |>
    group_by(term) |>
    summarise(
        n = n(),
        mean = mean(probability),
        median = median(probability),
        iqr = IQR(probability),
        p10 = quantile(probability, 0.10),
        p90 = quantile(probability, 0.90),
        q25 = quantile(probability, 0.25),
        q75 = quantile(probability, 0.75),
        .groups = "drop"
    ) |>
    arrange(median, term)

### |- define highlight hierarchy ----
term_summary <- term_summary |>
    mutate(
        highlight = case_when(
            term == "Realistic Possibility" ~ "standout",
            term %in% c("Could Happen", "Might Happen", "May Happen") ~ "cluster",
            TRUE ~ "other"
        ),
        term = fct_reorder(term, median)
    )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = list(
        "standout" = "#722F37",  
        "cluster"  = "#C17B82",  
        "other"    = "gray72",   
        # "bg"       = "#FAFAF8",
        "text"     = "#2C2C2C"
    )
)

### |- titles and caption ----
title_text    <- str_glue("Lost in the Middle")

subtitle_text <- str_glue(
    "People agree at the extremes — but **disagree widely** on what *\"possible\"* means.<br>",
    "Each bar shows the middle 50% range (IQR) of probability estimates across 5,174 respondents."
)

caption_text  <- create_social_caption(
    tt_year     = 2026,
    tt_week     = 10,
    source_text = "Adam Kucharski · CAPphrase Quiz"
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
      face = "bold", family = fonts$title, size = rel(1.1),
      color = colors$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_markdown(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.7), margin = margin(b = 20), hjust = 0
    ),

    # Grid
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    
    # Axes
    axis.title = element_text(size = rel(0.6), color = "gray30"),
    axis.text = element_text(color = "gray30"),
    axis.text.y = element_text(size = rel(0.6)),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),

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
    plot.margin = margin(10, 20, 10, 20),
  )
)

# Set theme
theme_set(weekly_theme)

### |- build plot ----
p <- ggplot(term_summary, aes(y = term)) +
  # Geoms
  geom_segment(
    aes(x = p10, xend = p90, yend = term),
    color = "gray78",
    linewidth = 0.75,
    lineend = "round"
  ) +
  geom_segment(
    aes(x = q25, xend = q75, yend = term, color = highlight),
    linewidth = 3.5,
    lineend = "round"
  ) +
  geom_point(
    aes(x = median, fill = highlight),
    shape = 21,
    size = 2.3,
    color = "white",
    stroke = 0.4
  ) +
  # Annotate
  annotate(
    "text",
    x          = 83,
    y          = "Realistic Possibility",
    label      = "Spans 25–80%\nfor the middle 50%",
    hjust      = 0,
    size       = 3,
    color      = colors$palette$standout,
    family     = fonts$body,
    fontface   = "italic",
    lineheight = 1.3
  ) +
  # Scales
  scale_color_manual(
    values = c(
      "standout" = colors$palette$standout,
      "cluster"  = colors$palette$cluster,
      "other"    = colors$palette$other
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "standout" = colors$palette$standout,
      "cluster"  = colors$palette$cluster,
      "other"    = colors$palette$other
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 102),
    breaks = seq(0, 100, 25),
    labels = label_percent(scale = 1),
    expand = c(0, 0)
  ) +
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = "Probability estimate (0–100%)"
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.8),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.15,
      margin = margin(t = 0, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8),
      family = fonts$subtitle,
      face = "italic",
      color = alpha(colors$subtitle, 0.88),
      lineheight = 1.5,
      margin = margin(t = 5, b = 25)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
      family = fonts$subtitle,
      color = colors$caption,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(t = 20, b = 5)
    )
  )

snap(p)


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

# ─ Session info ─────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-03
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# binom        * 1.1-1.1  2022-05-02 [1] CRAN (R 4.4.3)
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
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
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
# P httr2          1.0.1    2024-04-01 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
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
# P tidytuesdayR   1.2.1    2025-04-29 [?] CRAN (R 4.4.3)
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
# ─────────────────────────