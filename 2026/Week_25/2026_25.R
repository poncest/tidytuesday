
## Challenge: #TidyTuesday 2026 week 25
## Data:      Papal Encyclicals: Industrial Revolution vs. AI Revolution
## Author:    Steven Ponce
## Date:      2026-06-23


## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, ggrepel,      
    scales, glue, skimr, tidytext
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 9,
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
tt <- tidytuesdayR::tt_load(2026, week = 25)
encyclicals <- tt$encyclicals |> clean_names()
# papal_encyclicals <- tt$papal_encyclicals |> clean_names()
# scripture_references <- tt$scripture_references |> clean_names()

rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(encyclicals)
# glimpse(papal_encyclicals)
# glimpse(scripture_references)


## 4. TIDY DATA ----
### |- tokenize ----
tokens <- encyclicals |>
    select(encyclical, paragraph, text) |>
    unnest_tokens(word, text) |>
    filter(str_detect(word, "^[a-z]+$")) |>  
    anti_join(stop_words, by = "word")

### |- weighted log-odds (signed distinctiveness) ----
word_counts <- tokens |>
    count(encyclical, word, name = "n") |>
    filter(n >= 3) |>                          
    pivot_wider(names_from = encyclical, values_from = n, values_fill = 0) |>
    rename(mh = `Magnifica Humanitas`, rn = `Rerum Novarum`)

n_mh  <- sum(word_counts$mh)
n_rn  <- sum(word_counts$rn)
vocab <- nrow(word_counts)

keyness <- word_counts |>
    mutate(
        a_mh = mh + 1,                           # + uninformative prior
        a_rn = rn + 1,
        log_odds = log(a_mh / (n_mh + vocab - a_mh)) -
            log(a_rn / (n_rn + vocab - a_rn)),
        se = sqrt(1 / a_mh + 1 / a_rn),
        z  = log_odds / se                       # weighted log-odds (signed)
    )

### |- word selection: thesis vocabulary, log-odds ranked ----
# Pure log-odds buries the AI signal under high-count CST register
# (social, economic, justice, power...). So we force-include each side's
# vocabulary of CONCERN and suppress the institutional register -- same logic
# as the shared-spine exclusion, just extended. Every shown word is still
# verified distinctive (correct sign of z); sizing stays log-odds.

# words common to both / structural -- never shown ("human" returns as anchor)
shared_spine <- c(
    "human", "god", "church", "society", "life",
    "poor", "common", "world", "people", "time"
)

# modern-CST institutional register -- distinctive to MH but off-thesis
register_terms <- c(
    "social", "economic", "responsibility", "development",
    "doctrine", "political", "forms", "justice", "power",
    "humanity"
)

excluded <- c(shared_spine, register_terms)

# force-included concern vocabularies (verified by sign of z below)
rn_focus <- c(
    "labor", "property", "rights", "classes", "rich", "wages",
    "employer", "workmen", "associations", "private", "law"
)

mh_focus <- c(
    "dignity", "person", "ai", "digital", "artificial",
    "intelligence", "technology", "technological", "data",
    "algorithms", "machines"
)

n_each <- 11

# force-include only words that genuinely lean the intended way, then
# top up to n_each from the remaining distinctive (non-register) words.
pick_side <- function(focus, sign_dir) {
    forced <- keyness |>
        filter(word %in% focus, sign(z) == sign_dir)
    fill <- keyness |>
        filter(sign(z) == sign_dir, !word %in% excluded, !word %in% forced$word) |>
        slice_max(abs(z), n = n_each, with_ties = FALSE)
    bind_rows(forced, fill) |>
        distinct(word, .keep_all = TRUE) |>
        slice_head(n = n_each)
}

plot_words <- bind_rows(
    pick_side(mh_focus, 1L),
    pick_side(rn_focus, -1L)
) |>
    mutate(side = if_else(z > 0, "Magnifica Humanitas", "Rerum Novarum")) |>
    arrange(z) |>
    mutate(word = fct_inorder(word))

### |- shared anchor ----
anchor <- keyness |>
    filter(word == "human") |>
    mutate(
        mh_per1k = mh / n_mh * 1000,
        rn_per1k = rn / n_rn * 1000
    )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
    palette = list(
        mh  = "#722F37",
        rn  = "#7A7068",
        ink = "#2C2825"
    )
)
col_mh <- clrs$palette$mh
col_rn <- clrs$palette$rn
col_ink <- clrs$palette$ink

### |- titles and caption ----
title_text <- str_glue("From what workers are owed to what humans are")

subtitle_text <- str_glue(
    "Across two encyclicals by two popes named Leo, 135 years apart, the ",
    "vocabulary shifts from **labor, wages, and property** to ",
    "**dignity, personhood, and artificial intelligence**."
)

method_note <- str_glue(
    "Each encyclical's distinctive vocabulary of concern (shared and ",
    "general-register terms removed);<br>bar length = weighted log-odds ",
    "(Monroe et al.). Words appear at least 3 times."
)

caption_text <- str_glue(
    "{create_social_caption(tt_year = 2026, tt_week = 25, ",
    "source_text = 'Vatican.va (Holy See)')}<br>{method_note}"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        plot.title = element_markdown(
            face = "bold", family = fonts$title_1, size = rel(1.85),
            color = col_ink, margin = margin(b = 6)
        ),
        plot.subtitle = element_textbox_simple(
            family = fonts$subtitle, size = rel(0.8), color = col_ink,
            width = unit(1, "npc"), lineheight = 1.25, margin = margin(b = 30)
        ),
        plot.caption = element_markdown(
            family = fonts$caption, size = rel(0.55), color = col_rn,
            hjust = 0, lineheight = 1.3, margin = margin(t = 20, b = 10)
        ),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank()
    )
)

theme_set(weekly_theme)

### |- annotation positions ----
x_max <- max(abs(plot_words$z)) * 1.05
y_header <- n_each * 2 + 1.4
y_anchor <- -1.3          

### |- plot ----
p <- plot_words |>
    ggplot(aes(x = z, y = word, fill = side)) +
    
    # Geoms
    geom_col(width = 0.72) +
    geom_text(
        data = \(d) filter(d, z > 0),
        aes(label = word), hjust = -0.08,
        family = fonts$text, size = 3.5, color = col_ink
    ) +
    geom_text(
        data  = \(d) filter(d, z < 0),
        aes(label = word), hjust = 1.08,
        family = fonts$text, size = 3.5, color = col_ink
    ) +
    geom_vline(xintercept = 0, color = col_ink, linewidth = 0.4) +
    # Annotations
    annotate(
        "richtext",
        x = -x_max, y = y_header, hjust = 0, vjust = 0,
        label.color = NA, fill = NA, family = fonts$text, size = 3.4,
        color = col_rn,
        label = "**Rerum Novarum** \u00b7 1891 \u00b7 Leo XIII<br>
             <span style='font-size:8pt'>The Industrial Revolution</span>"
    ) +
    annotate(
        "richtext",
        x = x_max, y = y_header, hjust = 1, vjust = 0,
        label.color = NA, fill = NA, family = fonts$text, size = 3.4,
        color = col_mh,
        label = "**Magnifica Humanitas** \u00b7 2026 \u00b7 Leo XIV<br>
             <span style='font-size:8pt'>The AI Revolution</span>"
    ) +
    annotate(
        "richtext",
        x = 0, y = y_anchor, hjust = 0.5, vjust = 0.5,
        label.color = NA, fill = NA, family = fonts$text, size = 3.1,
        color = col_rn, lineheight = 1.3,
        label = "**The constant is the human** -- both encyclicals lean on that one word.<br>
             What changes is the *threat* to it."
    ) +
    # Scales
    scale_fill_manual(
        values = c("Magnifica Humanitas" = col_mh, "Rerum Novarum" = col_rn),
        guide  = "none"
    ) +
    scale_x_continuous(
        limits = c(-x_max, x_max),
        expand = expansion(mult = c(0.22, 0.22))
    ) +
    scale_y_discrete(expand = expansion(add = c(3, 2.2))) +
    coord_cartesian(clip = "off") +
    # Labs
    labs(
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text,
        x        = NULL,
        y        = NULL
    )

### |- preview ----
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

# ─ Session info ──────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-06-22
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpG8fWdE/file192c77df7f8a". Did you mean command "install"? @ C:\\Users\\poncest\\AppData\\Local\\Programs\\Quarto\\bin\\quarto.exe
# 
# ─ Packages ──────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.5.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.5.3)
# P datasets     * 4.5.3    2026-03-11 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.5.3)
# dplyr        * 1.2.1    2026-04-03 [1] CRAN (R 4.5.3)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.5.3)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.5.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.5.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.5.3)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.5.3)
# ggplot2      * 4.0.3    2026-04-22 [1] CRAN (R 4.5.3)
# ggrepel      * 0.9.8    2026-03-17 [1] CRAN (R 4.5.3)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.5.3)
# gh             1.5.0    2025-05-26 [1] CRAN (R 4.5.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.5.3)
# gitcreds       0.1.2    2022-09-08 [1] CRAN (R 4.5.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.5.3)
# P graphics     * 4.5.3    2026-03-11 [2] local
# P grDevices    * 4.5.3    2026-03-11 [2] local
# P grid           4.5.3    2026-03-11 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.5.3)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.5.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.5.3)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.5.3)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.5.3)
# httr2          1.2.2    2025-12-08 [1] CRAN (R 4.5.3)
# janeaustenr    1.0.0    2022-08-26 [1] CRAN (R 4.5.3)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.5.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.5.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.5.3)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.5.2)
# lattice        0.22-9   2026-02-09 [2] CRAN (R 4.5.3)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.5.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.5.3)
# Matrix         1.7-4    2025-08-28 [2] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.5.3)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# P parallel       4.5.3    2026-03-11 [2] local
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# rappdirs       0.3.4    2026-01-17 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.5.3)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.5.3)
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.5.3)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.5.3)
# rsvg           2.7.0    2025-09-08 [1] CRAN (R 4.5.3)
# S7             0.2.1    2025-11-14 [1] CRAN (R 4.5.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.5.3)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.5.3)
# showtext     * 0.9-8    2026-03-21 [1] CRAN (R 4.5.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.5.3)
# skimr        * 2.2.2    2026-01-10 [1] CRAN (R 4.5.3)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.5.3)
# SnowballC      0.7.1    2023-04-25 [1] CRAN (R 4.5.2)
# P stats        * 4.5.3    2026-03-11 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.5.2)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.5.3)
# svglite        2.2.2    2025-10-21 [1] CRAN (R 4.5.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.5.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.5.3)
# textshaping    1.0.5    2026-03-06 [1] CRAN (R 4.5.3)
# tibble       * 3.3.1    2026-01-11 [1] CRAN (R 4.5.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.5.3)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.5.3)
# tidytext     * 0.4.3    2025-07-25 [1] CRAN (R 4.5.3)
# tidytuesdayR   1.3.2    2026-04-12 [1] CRAN (R 4.5.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.5.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.5.3)
# tokenizers     0.3.0    2022-12-22 [1] CRAN (R 4.5.3)
# P tools          4.5.3    2026-03-11 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.5.3)
# P utils        * 4.5.3    2026-03-11 [2] local
# vctrs          0.7.3    2026-04-11 [1] CRAN (R 4.5.3)
# vroom          1.7.1    2026-03-31 [1] CRAN (R 4.5.3)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.5.3)
# xfun           0.57     2026-03-20 [1] CRAN (R 4.5.3)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.5.3)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.5
# [2] C:/Program Files/R/R-4.5.3/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────