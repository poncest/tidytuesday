## Challenge: #TidyTuesday 2025 week 43
## Data:      Selected British Literary Prizes (1990-2022)
## Author:    Steven Ponce
## Date:      2025-10-26

## NOTE: This script uses custom helper functions for theming and formatting.
## See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details,
## or view source code at: https://github.com/poncest/tidytuesday/tree/main


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, # Easily Install and Load the 'Tidyverse'
  ggtext, # Improved Text Rendering Support for 'ggplot2'
  showtext, # Using Fonts More Easily in R Graphs
  janitor, # Simple Tools for Examining and Cleaning Dirty Data
  scales, # Scale Functions for Visualization
  glue, # Interpreted String Literals
  patchwork, # The Composer of Plots
  binom # Binomial Confidence Intervals for Several Parameterizations
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 14,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 43)

prizes <- tt$prizes |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(prizes)
glimpse(prizes)


## 4. TIDY DATA ----
# data prep
prizes_clean <- prizes |>
  filter(
    !is.na(gender),
    !is.na(prize_year),
    prize_year >= 1990,
    prize_year <= 2022,
    gender %in% c("man", "woman")
  )

# P1: pyramid data ----
pyramid_data <- prizes_clean |>
  filter(person_role == "winner") |>
  count(prize_alias, gender) |>
  group_by(prize_alias) |>
  mutate(
    total = sum(n),
    pct = 100 * n / total
  ) |>
  ungroup() |>
  filter(total >= 15) |>
  select(prize_alias, gender, n, pct, total) |>
  pivot_wider(
    names_from = gender,
    values_from = c(n, pct),
    values_fill = 0
  ) |>
  mutate(
    prize_label = str_wrap(prize_alias, width = 28),
    women_pct = pct_woman,
    men_pct = pct_man,
    women_n = n_woman,
    men_n = n_man,
    men_x = -men_pct,
    women_x = women_pct,
    total = total,
    dist50 = abs(women_pct - 50)
  ) |>
  select(
    prize_label, women_pct, men_pct, women_n, men_n, men_x,
    women_x, total, dist50
  ) |>
  arrange(across(all_of("women_pct"), desc)) |>
  mutate(
    y_fac = factor(prize_label, levels = rev(prize_label))
  )

# Summary counts
balanced <- sum(abs(pyramid_data$women_pct - 50) <= 10)
total_prize <- nrow(pyramid_data)
male_dom <- sum(pyramid_data$women_pct < 40)
female_dom <- sum(pyramid_data$women_pct > 60)

# Outside labels with counts
pad <- 4.5
pad <- 5.5 # Slightly more padding for breathing room
fmt_lab <- function(pct, n) {
  paste0(
    "<span style='font-size:11pt;'><b>", round(pct), "%</b></span><br>",
    "<span style='font-size:9pt; color:gray60;'>(n=", n, ")</span>"
  )
}

men_lab <- pyramid_data |>
  mutate(
    x = men_x - pad,
    txt = fmt_lab(abs(men_x), men_n)
  )
wom_lab <- pyramid_data |>
  mutate(
    x = women_x + pad,
    txt = fmt_lab(women_x, women_n)
  )

# P2: timeline data ----
timeline <- prizes_clean |>
  filter(person_role == "winner") |>
  count(prize_year, gender) |>
  pivot_wider(names_from = gender, values_from = n, values_fill = 0) |>
  mutate(
    total = man + woman,
    p_w = woman / total
  ) |>
  mutate({
    binom::binom.wilson(x = woman, n = total)[, c("lower", "upper")]
  } |> as_tibble()) |>
  rename(ci_lo = lower, ci_hi = upper) |>
  mutate(
    pct_w = 100 * p_w,
    lo = 100 * ci_lo,
    hi = 100 * ci_hi
  )

# Headline stats
early_avg <- timeline |>
  filter(prize_year <= 1995) |>
  summarise(avg = mean(pct_w, na.rm = TRUE)) |>
  pull()

late_avg <- timeline |>
  filter(prize_year >= 2018) |>
  summarise(avg = mean(pct_w, na.rm = TRUE)) |>
  pull()

improvement <- late_avg - early_avg

parity_year <- timeline |>
  filter(pct_w >= 50) |>
  arrange(prize_year) |>
  slice(1) |>
  pull(prize_year)

# milestones
milestones <- tibble(
  year = c(1993, 2008, 2018),
  label = c(
    glue("Early 1990s:\n{round(early_avg)}% women"),
    "2000s:\nSlow growth",
    glue("Recent years:\n{round(late_avg)}% women")
  )
) |>
  left_join(timeline |> select(prize_year, pct_w), by = c("year" = "prize_year")) |>
  mutate(
    y = case_when(
      year == 1993 ~ pct_w - 8,
      year == 2008 ~ pct_w + 8,
      year == 2018 ~ pct_w + 8
    )
  )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get basic theme colors
colors <- get_theme_colors(
  palette = list(
    col_women = "#7B4ABF",
    col_men = "#17A0A3",
    col_gray1 = "#2C3E50",
    col_gray2 = "#7F8C8D",
    col_grid = "#ECF0F1"
  )
)

### |- titles and caption ----
title_text <- str_glue(
  "Gender Equity in British Literary Prizes: Progress with Persistent Disparities"
)

subtitle_text <- str_glue(
    "Overall representation improved (+{round(improvement)} pp), ",
    "yet only {balanced} of {total_prize} major prizes achieved gender balance."
)

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 43,
  source_text = str_glue(
      "Post45 Data Collective (post45.org) | Analysis: British Literary Prizes (1990–2022)"
  )
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
    plot.title = element_markdown(
      face = "bold", family = fonts$title, size = rel(1.4),
      color = colors$title, margin = margin(b = 10), hjust = 0.5
    ),
    plot.subtitle = element_text(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.9), margin = margin(b = 20), hjust = 0.5
    ),

    ## Grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),

    # Axes
    axis.title = element_text(size = rel(0.9), color = "gray30"),
    axis.text = element_text(color = "gray30"),
    axis.text.y = element_text(size = rel(0.95)),
    axis.ticks = element_blank(),

    # Facets
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(
      face = "bold",
      color = "gray20",
      size = rel(1),
      margin = margin(t = 8, b = 8)
    ),
    panel.spacing = unit(2, "lines"),

    # Legend elements
    legend.position = "plot",
    legend.title = element_text(
      family = fonts$tsubtitle,
      color = colors$text, size = rel(0.8), face = "bold"
    ),
    legend.text = element_text(
      family = fonts$tsubtitle,
      color = colors$text, size = rel(0.7)
    ),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- P1: pyramid plot ----
p1 <-
  ggplot(pyramid_data, aes(y = y_fac)) +

  # Geom
  geom_col(aes(x = men_x),
    fill = colors$palette$col_men,
    width = 0.7, color = "white", linewidth = 0.25
  ) +
  geom_col(aes(x = women_x),
    fill = colors$palette$col_women,
    width = 0.7, color = "white", linewidth = 0.25
  ) +
  geom_vline(
    xintercept = 0, color = colors$palette$col_gray1,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = c(-50, 50), color = "#E67E22",
    linewidth = 0.8, linetype = "dashed", alpha = 0.7
  ) +
  geom_richtext(
    data = men_lab,
    aes(y = y_fac, x = x, label = txt),
    hjust = 1, color = "gray40", size = 3,
    fill = NA, label.color = NA,
    inherit.aes = FALSE
  ) +
  geom_richtext(
    data = wom_lab,
    aes(y = y_fac, x = x, label = txt),
    hjust = 0, color = "gray40", size = 3,
    fill = NA, label.color = NA,
    inherit.aes = FALSE
  ) +
  # Scales
  coord_cartesian(xlim = c(-110, 110), clip = "off") +
  scale_x_continuous(
    breaks = seq(-100, 100, 25),
    labels = \(x) paste0(abs(x), "%"),
    expand = c(0, 0)
  ) +
  # Labs
  labs(
    title = glue(
      "<span style='color:{colors$palette$col_men}; font-family:{get_font_families()$title};'>**Men**</span> ",
      "<span style='font-family:sans;'>←</span> ",
      "Winners by Prize (Share of Winners) ",
      "<span style='font-family:sans;'>→</span> ",
      "<span style='color:{colors$palette$col_women}; font-family:{get_font_families()$title};'>**Women**</span>"
    ),
    subtitle = glue(
      "{balanced} balanced (40–60%) • ",
      "{male_dom} male-dominant (<40% women) • ",
      "{female_dom} female-dominant (>60% women)"
    ),
    x = NULL,
    y = NULL,
    caption = "Winners only • Prizes with ≥15 total winners • Parity guides at ±50%"
  ) +
  # Theme
  theme(
    plot.caption = element_text(
      color = "#95A5A6", size = 8.5,
      margin = margin(t = 8, b = 5),
      hjust = 0.5
    ),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 8.8, lineheight = 0.95),
    plot.margin = margin(20, 45, 20, 45) 
  )

### |- P2: timeline plot ----
p2 <-
ggplot(timeline, aes(prize_year, pct_w)) +
  # Annotations
  annotate("rect",
    xmin = 1990, xmax = 2022, ymin = 0, ymax = 50,
    fill = colors$palette$col_men, alpha = 0.03
  ) +
  annotate("rect",
    xmin = 1990, xmax = 2022, ymin = 50, ymax = 75,
    fill = colors$palette$col_women, alpha = 0.04
  ) +
  # Geoms
  geom_ribbon(aes(ymin = lo, ymax = hi),
    fill = colors$palette$col_women, alpha = 0.15
  ) +
  geom_line(color = colors$palette$col_women, linewidth = 1.5) +
  geom_point(color = colors$palette$col_women, size = 2.8, alpha = 0.9) +
  geom_hline(
    yintercept = 50,
    linetype = "dashed",
    color = alpha(colors$palette$col_gray1, 0.3),
    linewidth = 0.5
  ) +
  geom_point(
    data = timeline |> filter(prize_year == parity_year),
    aes(prize_year, pct_w),
    color = "#2C3E50", size = 4.5, shape = 21,
    fill = "white", stroke = 1.5
  ) +
  geom_segment(
    data = milestones,
    aes(
      x = year, xend = year,
      y = ifelse(y > pct_w, pct_w, 0),
      yend = y
    ),
    color = colors$palette$col_gray2,
    linetype = "dotted", linewidth = 0.7
  ) +
  geom_label(
    data = milestones,
    aes(x = year, y = y, label = label),
    size = 3, fontface = "plain",
    fill = alpha("#F9FAFB", 0.75),  
    color = "#34495E",
    label.padding = unit(0.28, "lines"),
    label.size = 0.2,
    label.r = unit(0.15, "lines")
  ) +
  annotate(
    "text",
    x = parity_year,
    y = (timeline |> 
        filter(prize_year == parity_year) |> 
        pull(pct_w) + 6.5),
    label = glue("First ≥50%\n({parity_year})"),
    color = "#2C3E50", size = 3.2, fontface = "bold",
    lineheight = 0.9
  ) +
  annotate("text",
    x = 2021, y = 46.5,
    label = "50% parity",
    color = colors$palette$col_gray1,
    size = 3, fontface = "italic", hjust = 1
  ) +
  # Scales
  scale_y_continuous(
    labels = label_percent(scale = 1),
    breaks = seq(0, 75, 25)
  ) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  coord_cartesian(ylim = c(0, 75), expand = FALSE) +
  # Labs
  labs(
    title = glue(
      "Women's share of literary prize winners increased from ",
      "{round(early_avg)}% to {round(late_avg)}% (+{round(improvement)} pp)"
    ),
    subtitle = glue(
      "British Literary Prizes, 1990–2022 • ",
      "95% Wilson confidence interval • ",
      "Parity first achieved in {parity_year}"
    ),
    x = "Year",
    y = "Women winners (%)",
    caption = "95% Wilson confidence intervals shown • 158 winners total"
  ) +
  # Theme
  theme(
    plot.caption = element_text(
      color = "#95A5A6", size = 8.5,
      margin = margin(t = 8, b = 5),
      hjust = 0.5
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "gray92", linewidth = 0.4
    )
  )

### |- Combined plot ----
combined_plots <- p2 / p1 +
    plot_layout(heights = c(0.85, 1.15))

combined_plots +
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_markdown(
                size = rel(1.55),
                family = fonts$title,
                face = "bold",
                color = colors$title,
                lineheight = 1.15,
                margin = margin(t = 8, b = 5)
            ),
            plot.subtitle = element_text(
                size = rel(0.87),
                family = fonts$subtitle,
                color = alpha(colors$subtitle, 0.88),
                lineheight = 1.2,
                margin = margin(t = 2, b = 15)
            ),
            plot.caption = element_markdown(
                size = rel(0.73),
                family = fonts$caption,
                color = colors$caption,
                hjust = 0.5,
                lineheight = 1.3,
                margin = margin(t = 12, b = 5),
            )
        )
    )


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

# ─ Session info ─────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-10-25
# rstudio  2025.09.1+401 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────
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
# P yaml           2.3.8    2023-12-11 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────
# > 