## Challenge: #TidyTuesday 2026 week 14
## Data:      Repair Cafes Worldwide
## Author:    Steven Ponce
## Date:      2026-04-05

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, patchwork, binom    
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 12,
    height = 8,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 14)
repairs <- tt$repairs |> clean_names()
repairs_text <- tt$repairs_text|> clean_names()
rm(tt)

## 3. EXAMINING THE DATA ----
glimpse(repairs)
glimpse(repairs_text)


## 4. TIDY DATA ----

### |- Panel A: potential vs. outcome ----

# Binary outcome: repaired = "yes" only; everything else = not repaired
panel_a_data <- repairs |>
    filter(!is.na(repairability)) |>
    mutate(fixed = repaired == "yes") |>
    group_by(category) |>
    summarise(
        n              = n(),
        median_repair  = median(repairability, na.rm = TRUE),
        n_fixed        = sum(fixed, na.rm = TRUE),
        .groups        = "drop"
    ) |>
    # Wilson CI on success rate — more reliable than raw proportion at small n
    mutate(
        ci             = map2(n_fixed, n, ~ binom.wilson(.x, .y)),
        pct_fixed      = map_dbl(ci, ~ .x$mean),
        pct_lower      = map_dbl(ci, ~ .x$lower),
        pct_upper      = map_dbl(ci, ~ .x$upper)
    ) |>
    filter(n >= 200) |>
    # Quadrant flag: high repairability (≥ 6) but low fix rate (< 50%)
    mutate(
        quadrant = case_when(
            median_repair >= 6 & pct_fixed < 0.50 ~ "system_failure",
            median_repair >= 6 & pct_fixed >= 0.50 ~ "system_works",
            median_repair < 6 & pct_fixed < 0.50 ~ "expected",
            median_repair < 6 & pct_fixed >= 0.50 ~ "skilled_volunteers",
            TRUE ~ "other"
        ),
        category_clean = str_to_title(
            str_wrap(str_replace_all(category, "_", " "), width = 22)
        )
    )

# Top "system failure" categories to label
label_cats <- panel_a_data |>
    filter(quadrant == "system_failure") |>
    slice_max(n, n = 6)

# Also label the top "system works" anchor point
label_works <- panel_a_data |>
    filter(quadrant == "system_works") |>
    slice_max(pct_fixed, n = 2)

panel_a_labels <- bind_rows(label_cats, label_works)


### |- Panel B: failure reasons ----
panel_b_data <- repairs_text |>
    filter(!is.na(failure_reasons), failure_reasons != "") |>
    # failure_reasons is a comma-delimited list
    separate_rows(failure_reasons, sep = ",") |>
    mutate(
        failure_reasons = str_squish(str_to_lower(failure_reasons))
    ) |>
    filter(failure_reasons != "", failure_reasons != "na") |>
    # Group semantically similar reasons
    mutate(
        reason_group = case_when(
            str_detect(failure_reasons, "spare part|parts not|no parts|onderdeel") ~
                "Spare parts unavailable",
            str_detect(failure_reasons, "unidentified|unknown|not found|unclear") ~
                "Failure unidentified",
            str_detect(failure_reasons, "cost|expensive|not worth|economisch") ~
                "Not cost-effective",
            str_detect(failure_reasons, "time|too long|no time") ~
                "Insufficient time",
            str_detect(failure_reasons, "skill|expertise|knowledge|competence") ~
                "Skill / expertise gap",
            str_detect(failure_reasons, "owner|customer|taken away|meegenomen") ~
                "Owner decision",
            str_detect(failure_reasons, "safety|gevaar|dangerous") ~
                "Safety concern",
            str_detect(failure_reasons, "irreparable|beyond repair|too damaged") ~
                "Irreparable damage",
            TRUE ~ "Other"
        )
    ) |>
    filter(reason_group != "Other") |>
    count(reason_group, sort = TRUE) |>
    mutate(
        pct = n / sum(n),
        reason_group = fct_reorder(reason_group, pct),
        bar_fill = if_else(
            reason_group == fct_reorder(reason_group, pct) |>
                levels() |>
                last(),
            "accent", "neutral"
        )
    )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = c(
        "system_failure"     = "#C26A3D",   
        "system_works"       = "#3B4252",   
        "expected"           = "#9BA8B5",   
        "skilled_volunteers" = "#5F7A8A",   
        "ref_line"           = "#4A4A4A",   
        "bar_accent"         = "#C26A3D",   
        "bar_neutral"        = "#B0B7BF"    
    )
)

### |- titles and caption ----
title_text    <- str_glue("Repair Isn't the Problem — The System Is")

subtitle_text <- str_glue(
    'Even **"easy-to-fix"** products fail in Repair Cafes — not due to **skill**,',
    " but due to **missing parts**, **unclear diagnostics**, and **cost barriers**"
)

caption_text <- create_social_caption(
    tt_year     = 2026,
    tt_week     = 14,
    source_text = "Repair Monitor (repaircafes.org)  ·  Bubble size = repair attempts"
)

panel_a_title <- "Potential vs. Outcome"
panel_a_sub   <- "High repairability scores don't guarantee a fix.\nAmber points identify categories where the system falls short."

panel_b_title <- 'Why **"Repairable"** Items Still Fail'
panel_b_sub   <- "When volunteers can't fix it, the cause is rarely skill.\nAmber bars reflect systemic constraints beyond the cafe."

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

#### |- plot theme ----
base_theme   <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        # Panel
        panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
        panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.2, "lines"),
        
        # Axes
        axis.title = element_text(
            size = 9, color = "gray30",
            family = fonts$text
        ),
        axis.text = element_text(
            size = 8, color = "gray40",
            family = fonts$text
        ),
        axis.ticks = element_blank(),
        
        # Legend
        legend.position = "none",
        
        # Strip (if used)
        strip.text = element_text(
            size = 9, face = "bold",
            family = fonts$title
        )
    )
)

theme_set(weekly_theme)

### |- Panel A: scatter — repairability vs. fix rate ----
p_a <- panel_a_data |>
    ggplot(aes(x = median_repair, y = pct_fixed)) +
    
    # Geoms
    annotate(
        "rect",
        xmin = 6, xmax = Inf, ymin = -Inf, ymax = 0.50,
        fill = "#C26A3D", alpha = 0.04
    ) +
    geom_vline(
        xintercept = 6, color = colors$palette["ref_line"],
        linetype = "dashed", linewidth = 0.4, alpha = 0.6
    ) +
    geom_hline(
        yintercept = 0.50, color = colors$palette["ref_line"],
        linetype = "dashed", linewidth = 0.4, alpha = 0.6
    ) +
    geom_linerange(
        aes(
            ymin = pct_lower, ymax = pct_upper,
            color = quadrant
        ),
        linewidth = 0.5, alpha = 0.4
    ) +
    geom_point(
        aes(size = n, color = quadrant, fill = quadrant),
        shape = 21, stroke = 0.5, alpha = 0.75
    ) +
    geom_text_repel(
        data = panel_a_labels,
        aes(label = category_clean, color = quadrant),
        size = 2.2,
        family = fonts$text,
        lineheight = 0.9,
        max.overlaps = 20,
        box.padding = 0.4,
        point.padding = 0.3,
        segment.color = "gray75",
        segment.size = 0.25,
        seed = 123
    ) +
    
    # Annotate
    annotate("text",
             x = 1.1, y = 0.97,
             label = "Expert-driven\nsuccess",
             hjust = 0, vjust = 1, size = 2.2, color = "gray65",
             fontface = "italic", lineheight = 0.9, family = fonts$text
    ) +
    annotate("text",
             x = 6.1, y = 0.97,
             label = "System works",
             hjust = 0, vjust = 1, size = 2.2, color = "gray65",
             fontface = "italic", family = fonts$text
    ) +
    annotate("text",
             x = 1.1, y = 0.03,
             label = "Expected\ndifficulty",
             hjust = 0, vjust = 0, size = 2.2, color = "gray65",
             fontface = "italic", lineheight = 0.9, family = fonts$text
    ) +
    annotate("text",
             x = 7.5, y = 0.30,
             label = "System\nconstraint",
             hjust = 0.5, vjust = 0.5, size = 2.3, color = "#C26A3D",
             fontface = "bold.italic", lineheight = 0.9, family = fonts$text
    ) +
    annotate("text",
             x = 6.08, y = 0.53,
             label = "≥6 = repairable",
             hjust = 0, vjust = 0, size = 2.0, color = "gray45",
             fontface = "italic", family = fonts$text
    ) +
    annotate("text",
             x = 1.1, y = 0.52,
             label = "50% success threshold",
             hjust = 0, vjust = 0, size = 2.0, color = "gray45",
             fontface = "italic", family = fonts$text
    ) +
    annotate("text",
             x = 9.5, y = 0.44,
             label = "← Amber zone:\nsee breakdown →",
             hjust = 0.5, vjust = 1, size = 2.1, color = "#C26A3D",
             fontface = "italic", lineheight = 0.95, family = fonts$text
    ) +
    
    # Scales
    scale_color_manual(values = c(
        "system_failure"     = unname(colors$palette["system_failure"]),
        "system_works"       = unname(colors$palette["system_works"]),
        "expected"           = unname(colors$palette["expected"]),
        "skilled_volunteers" = unname(colors$palette["skilled_volunteers"])
    )) +
    scale_fill_manual(values = c(
        "system_failure"     = unname(colors$palette["system_failure"]),
        "system_works"       = unname(colors$palette["system_works"]),
        "expected"           = unname(colors$palette["expected"]),
        "skilled_volunteers" = unname(colors$palette["skilled_volunteers"])
    )) +
    scale_size_continuous(range = c(2, 9)) +
    scale_x_continuous(
        name   = "Median Repairability Score  (≥6 = generally considered repairable)",
        limits = c(1, 10),
        breaks = 1:10
    ) +
    scale_y_continuous(
        name   = "% Successfully Repaired",
        labels = percent_format(accuracy = 1),
        limits = c(0, 1)
    ) +
    
    # Labs
    labs(
        title    = panel_a_title,
        subtitle = panel_a_sub
    ) +
    
    # Theme
    theme(
        plot.title = element_text(
            size = 11, face = "bold",
            family = fonts$title, color = "gray15"
        ),
        plot.subtitle = element_text(
            size = 8, color = "gray40",
            family = fonts$text, lineheight = 1.3,
            margin = margin(b = 8)
        )
    )

### |- Panel B: failure reasons bar chart ----
p_b <- panel_b_data |>
    ggplot(aes(x = pct, y = reason_group, fill = bar_fill)) +
    # Geoms
    geom_col(width = 0.7) +
    geom_text(
        aes(label = percent(pct, accuracy = 1)),
        hjust = -0.15,
        size = 2.8,
        color = "gray30",
        family = fonts$text
    ) +
    # Scales
    scale_fill_manual(
        values = c(
            "accent"  = unname(colors$palette["bar_accent"]),
            "neutral" = unname(colors$palette["bar_neutral"])
        )
    ) +
    scale_x_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.15))
    ) +
    # Labs
    labs(
        x        = "Share of recorded failure reasons",
        y        = NULL,
        title    = panel_b_title,
        subtitle = panel_b_sub
    ) +
    # Theme
    theme(
        panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
        panel.grid.major.y = element_blank(),
        plot.title = element_markdown(
            size = 11, face = "bold",
            family = fonts$title, color = "gray15"
        ),
        plot.subtitle = element_text(
            size = 8, color = "gray40",
            family = fonts$text, lineheight = 1.3,
            margin = margin(b = 8)
        ),
        axis.text.y = element_text(
            size = 8.5, color = "gray25",
            family = fonts$text
        )
    )

### |- Combined layout ----
p_final <- p_a + p_b +
    plot_layout(widths = c(1.4, 1)) +
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_text(
                size         = 20,
                face         = "bold",
                family       = fonts$title,
                color        = "gray10",
                margin       = margin(b = 6)
            ),
            plot.subtitle = element_markdown(
                size         = 12,
                family       = fonts$text,
                color        = "gray35",
                lineheight   = 1.35,
                margin       = margin(b = 14)
            ),
            plot.caption = element_markdown(
                size         = 7,
                family       = fonts$text,
                color        = "gray55",
                hjust        = 0.5,
                margin       = margin(t = 12)
            ),
            plot.background = element_rect(fill = "gray98", color = NA),
            plot.margin = margin(20, 20, 12, 20)
        )
    )

snap(p_final)


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

# ─ Session info ───────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-04-03
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.3.1)
# binom        * 1.1-1.1  2022-05-02 [1] CRAN (R 4.3.3)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.5    2025-04-23 [1] CRAN (R 4.3.1)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.3.1)
# dplyr        * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.3.1)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.3.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.3.1)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.3.1)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.3.1)
# ggrepel      * 0.9.8    2026-03-17 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.3)
# gh             1.4.1    2024-03-28 [1] CRAN (R 4.3.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.3.3)
# gitcreds       0.1.2    2022-09-08 [1] CRAN (R 4.3.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.3.3)
# P graphics     * 4.3.1    2023-06-16 [2] local
# P grDevices    * 4.3.1    2023-06-16 [2] local
# P grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.3.1)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.3.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.3.1)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.3.1)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.3.1)
# httr2          1.2.2    2025-12-08 [1] CRAN (R 4.3.1)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.3.1)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# P methods      * 4.3.1    2023-06-16 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.3.1)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# P parallel       4.3.1    2023-06-16 [2] local
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.3.1)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# rappdirs       0.3.4    2026-01-17 [1] CRAN (R 4.3.1)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.3.3)
# rlang          1.1.7    2026-01-09 [1] CRAN (R 4.3.1)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.3.1)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.3.1)
# rsvg           2.6.2    2025-03-23 [1] CRAN (R 4.3.3)
# S7             0.2.0    2024-11-07 [1] CRAN (R 4.3.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.3.1)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# skimr        * 2.2.2    2026-01-10 [1] CRAN (R 4.3.1)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.3)
# P stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.3.3)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.3.1)
# svglite        2.1.3    2023-12-08 [1] CRAN (R 4.3.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.3.1)
# textshaping    1.0.4    2025-10-10 [1] CRAN (R 4.3.1)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.3.1)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# tidytuesdayR   1.2.1    2025-04-29 [1] CRAN (R 4.3.1)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.3.1)
# P tools          4.3.1    2023-06-16 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.3.3)
# P utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.7.1    2026-01-23 [1] CRAN (R 4.3.1)
# vroom          1.7.0    2026-01-27 [1] CRAN (R 4.3.1)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.3.3)
# xfun           0.56     2026-01-18 [1] CRAN (R 4.3.1)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.1/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────