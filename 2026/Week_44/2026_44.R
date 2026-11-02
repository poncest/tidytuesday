## Challenge: #TidyTuesday 2026 week 44
## Data:      Grambank
## Author:    Steven Ponce
## Date:      2026-11-02

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, scales, glue, ggview,
    ggbeeswarm, patchwork, ragg
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure settings ----
fig_w   <- 10
fig_h   <- 9
fig_dpi <- 320
out_file <- "2026/Week_44/2026_44.png"

### |- fonts ----
setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1


## 2. READ IN THE DATA ----
## Read every field as character with no NA tokens: Grambank writes isolates' family as
## the literal "NA", and tt_load() cannot be told otherwise.
tt_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-03/"
read_chr <- function(url) {
  read_csv(url, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE)
}

languages_raw <- read_chr(paste0(tt_url, "grambank_languages.csv"))
parameters_raw <- read_chr(paste0(tt_url, "grambank_parameters.csv"))
values_raw <- read_chr(paste0(tt_url, "grambank_values.csv"))

# Grambank's own languages table (v1.0.3, same release as the TT extract): variety level and
# language-level ID, which the TT extract drops
gb_languages_raw <- read_chr("https://raw.githubusercontent.com/grambank/grambank/v1.0.3/cldf/languages.csv")


## 3. EXAMINING THE DATA ----
glimpse(languages_raw)
glimpse(parameters_raw)
glimpse(values_raw)


## 4. TIDY DATA ----

### |- analysis frame: one variety per language, no proto-languages ----
multistate_ids <- c("GB024", "GB025", "GB065", "GB130", "GB193", "GB203")

coded_counts <- values_raw |> count(language_id, name = "n_coded")

analysis_ids <- gb_languages_raw |>
  filter(level != "family") |>
  left_join(coded_counts, by = c("ID" = "language_id")) |>
  slice_max(n_coded, n = 1, with_ties = FALSE, by = Language_level_ID) |>
  pull(ID)

languages <- languages_raw |>
  filter(language_id %in% analysis_ids) |>
  mutate(family_unit = if_else(family_name == "NA", paste0("isolate_", language_id), family_name))

binary_values <- values_raw |>
  filter(!parameter_id %in% multistate_ids, language_id %in% analysis_ids) |>
  mutate(present = as.integer(value)) |>
  left_join(languages |> select(language_id, family_unit), by = "language_id")

### |- family-balanced share for each feature ----
## within each family: share of its recorded languages with the feature;
## then the mean over families with a recorded answer (each isolate is one family)
feature_share <- binary_values |>
  summarise(family_rate = mean(present), .by = c(parameter_id, family_unit)) |>
  summarise(family_share = mean(family_rate), n_families = n(), .by = parameter_id)

### |- English's present features (panel A) ----
english_id <- "stan1293"
english_features <- binary_values |>
  filter(language_id == english_id, present == 1) |>
  left_join(feature_share, by = "parameter_id")

### |- each language's average (panel B) ----
min_recorded <- 150
language_avg <- binary_values |>
  left_join(feature_share, by = "parameter_id") |>
  summarise(n_recorded = n(), avg_share = mean(family_share[present == 1]), .by = language_id) |>
  filter(n_recorded >= min_recorded)

english_avg <- language_avg$avg_share[language_avg$language_id == english_id]
median_avg <- median(language_avg$avg_share)
share_more_common <- mean(language_avg$avg_share > english_avg)

### |- named features: four rarest English features + two widely shared controls ----
named_features <- tribble(
  ~parameter_id, ~example,          ~device,                          ~side,
  "GB260",       "Is it raining?",  "word order can mark a question", "rare",
  "GB302",       "was sold",        "free passive auxiliary",         "rare",
  "GB021",       "a dog",           "indefinite article",             "rare",
  "GB400",       "I / she walked",  "one past form for every person", "rare",
  "GB083",       "walk-ed",         "past tense on the verb",         "control",
  "GB080",       "bark-ing",        "suffix on the verb",             "control"
)


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    accent = "#722F37",
    ink    = "#2C2825",
    dot    = "#8A8178",
    cloud  = "#C9C1B6",
    ref    = "#D9D3C9",
    muted  = "#7A7068"
  )
)
col_accent <- clrs$palette$accent
col_ink <- clrs$palette$ink
col_dot <- clrs$palette$dot
col_cloud <- clrs$palette$cloud
col_ref <- clrs$palette$ref
col_muted <- clrs$palette$muted

### |- titles and caption ----
title_text <- "English's grammatical features are less widely shared\nthan those of most languages studied"

subtitle_text <- glue(
  "<span style='color:{col_accent}'>**English**</span>'s grammatical features in Grambank average a family share of ",
  "**{round(100 * english_avg)}%**, against **{round(100 * median_avg)}%** for the<br>median of the {comma(nrow(language_avg))} plotted languages."
)

cue_a <- "Each dot is one feature, placed by its family share: the share of languages with it, each language family weighted equally"

caption_lines <- c(
  "Family share: within each language family, the share of its recorded languages with the feature, averaged over families with a recorded answer.",
  glue(
    "Isolates count as one family each; multi-value word-order features and '?' answers excluded. ",
    "Panel B: languages with {min_recorded}+ of 189 features recorded."
  )
)

disclosure_text <- glue(
  "{caption_lines[1]}<br>{caption_lines[2]}"
)

caption_text <- paste0(
  disclosure_text, "<br>",
  create_social_caption(
    tt_year = 2026,
    tt_week = 44,
    source_text = "Grambank v1.0.3 (Skirgård et al. 2023) via TidyTuesday, curated by Ntobeko Sosibo"
  )
)

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_text(
      face = "bold", family = title_family, size = 26,
      color = col_ink, lineheight = 1.0, margin = margin(b = 10)
    ),
    plot.subtitle = element_markdown(
      family = fonts$subtitle, size = 12, color = col_ink,
      lineheight = 1.25, margin = margin(b = 6)
    ),
    plot.caption = element_markdown(
      family = fonts$caption, size = 6.8, color = col_muted,
      hjust = 0, lineheight = 1.35, margin = margin(t = 12)
    ),
    axis.text.x = element_text(family = fonts$text, size = 9, color = col_muted),
    axis.title.x = element_text(
      family = fonts$text, size = 9, color = col_muted,
      face = "plain", margin = margin(t = 6)
    ),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(14, 24, 8, 20)
  )
)

theme_set(weekly_theme)

### |- shared x scale ----
x_scale <- scale_x_continuous(
  limits = c(0, 1), breaks = seq(0, 1, 0.25),
  labels = label_percent(), expand = expansion(add = c(0.01, 0.01))
)

### |- panel A: English's 63 features ----
set.seed(2026)
swarm_a <- layer_data(
  ggplot(english_features, aes(family_share, 0)) +
    geom_quasirandom(orientation = "y", width = 0.32, method = "pseudorandom", )
) |>
  select(x, y) |>
  bind_cols(english_features |> select(parameter_id, family_share))

labels_a <- named_features |>
  left_join(swarm_a, by = "parameter_id") |>
  mutate(
    pct = round(100 * family_share),
    label = glue("<b>{example}</b> · {device} · {pct}%"),
    label_y = c(0.92, -0.88, 0.66, -0.52, 0.80, -0.80),
    hjust = case_when(parameter_id == "GB080" ~ 1, TRUE ~ 0),
    colour = if_else(side == "rare", col_accent, col_ink)
  )

p_a <- ggplot(swarm_a, aes(x, y)) +
  annotate("segment", x = 0.5, xend = 0.5, y = -1.2, yend = 1.08, color = col_ref, linewidth = 0.35) +
  annotate("text",
    x = 0.505, y = 1.06, label = "50% reference",
    family = fonts$text, size = 2.7, color = col_muted, hjust = 0
  ) +
  geom_segment(
    data = labels_a, aes(x = x, xend = x, y = y, yend = label_y),
    color = col_ref, linewidth = 0.3
  ) +
  geom_point(aes(color = family_share < 0.10), size = 2, stroke = 0) +
  geom_richtext(
    data = labels_a,
    aes(x = x, y = label_y, label = label, hjust = hjust),
    color = labels_a$colour, family = fonts$text, size = 3, fill = NA, label.colour = NA,
    label.padding = unit(c(1, 2, 1, 2), "pt"), vjust = if_else(labels_a$label_y > 0, 0, 1)
  ) +
  annotate("segment",
    x = english_avg, xend = english_avg, y = -1.20, yend = -0.96,
    color = col_accent, linewidth = 1.3, lineend = "butt"
  ) +
  annotate("text",
    x = english_avg - 0.006, y = -1.08, label = "English's average",
    family = fonts$text, size = 3.1, color = col_accent, fontface = "bold", hjust = 1
  ) +
  annotate("text",
    x = 0, y = 1.42, hjust = 0, vjust = 1, family = fonts$text, size = 3.3,
    color = col_ink, fontface = "bold", label = "A   The 63 grammatical features English has"
  ) +
  annotate("text",
    x = 0, y = 1.29, hjust = 0, vjust = 1, family = fonts$text, size = 2.9,
    color = col_muted,
    label = cue_a
  ) +
  scale_color_manual(values = c(`TRUE` = col_accent, `FALSE` = col_dot)) +
  x_scale +
  scale_y_continuous(limits = c(-1.2, 1.45), expand = expansion(0)) +
  labs(
    title = title_text, subtitle = subtitle_text,
    x = "Family share of each feature (each family weighted equally)"
  )

### |- panel B: 1,320 languages at their averages ----
set.seed(2026)
swarm_b <- layer_data(
  ggplot(language_avg, aes(avg_share, 0)) +
    geom_quasirandom(orientation = "y", width = 0.42, method = "pseudorandom")
) |>
  select(x, y)

p_b <- ggplot(language_avg, aes(avg_share, 0)) +
  annotate("segment", x = 0.5, xend = 0.5, y = -0.6, yend = 0.40, color = col_ref, linewidth = 0.35) +
  geom_quasirandom(
    orientation = "y", width = 0.42, method = "pseudorandom",
    size = 0.6, stroke = 0, color = col_cloud
  ) +
  annotate("segment", x = median_avg, xend = median_avg, y = -0.6, yend = 0.42, color = col_muted, linewidth = 0.35) +
  annotate("text",
    x = median_avg + 0.008, y = 0.5, label = "median of plotted languages",
    family = fonts$text, size = 2.8, color = col_muted, hjust = 0
  ) +
  annotate("point", x = english_avg, y = 0, color = col_accent, size = 2.8) +
  annotate("segment",
    x = english_avg, xend = english_avg, y = 0.06, yend = 0.42,
    color = col_accent, linewidth = 0.3
  ) +
  annotate("text",
    x = english_avg - 0.008, y = 0.5, label = "English",
    family = fonts$text, size = 3, color = col_accent, fontface = "bold", hjust = 1
  ) +
  annotate("text",
    x = 0, y = 0.98, hjust = 0, vjust = 1, family = fonts$text, size = 3.3,
    color = col_ink, fontface = "bold",
    label = glue("B   English among {comma(nrow(language_avg))} languages")
  ) +
  annotate("text",
    x = 0, y = 0.85, hjust = 0, vjust = 1, family = fonts$text, size = 2.9,
    color = col_muted,
    label = "Each dot is one language, placed at the average position of its own features"
  ) +
  x_scale +
  scale_y_continuous(limits = c(-0.6, 1.0), expand = expansion(0)) +
  labs(
    x = "Average family share across the language's own features",
    caption = caption_text
  )

### |- combine ----
p <- p_a / p_b + plot_layout(heights = c(1.45, 1))

### |-  preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in", dpi = fig_dpi)


## 6. SAVE ----
save_ggplot(plot = p, file = out_file, width = fig_w, height = fig_h,
            units = "in", dpi = fig_dpi)


# 7. HELPER FUNCTIONS DOCUMENTATION ----

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


# 8. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-11-02
# rstudio  2026.10.1+239 Blue Mistflower (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ───────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# beeswarm       0.4.0   2021-06-01 [1] CRAN (R 4.6.0)
# bit            4.6.0   2025-03-06 [1] CRAN (R 4.6.0)
# bit64          4.8.2   2026-05-19 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# crayon         1.5.3   2024-06-20 [1] CRAN (R 4.6.0)
# curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1   2026-06-25 [1] local
# digest         0.6.39  2025-11-19 [1] CRAN (R 4.6.0)
# dplyr        * 1.2.1   2026-04-03 [1] CRAN (R 4.6.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
# forcats      * 1.0.1   2025-09-25 [1] CRAN (R 4.6.0)
# generics       0.1.4   2025-05-09 [1] CRAN (R 4.6.0)
# ggbeeswarm   * 0.7.3   2025-11-29 [1] CRAN (R 4.6.0)
# ggplot2      * 4.0.3   2026-04-22 [1] CRAN (R 4.6.0)
# ggtext       * 0.2.0   2026-08-28 [1] CRAN (R 4.6.1)
# ggview       * 0.2.2   2025-07-05 [1] CRAN (R 4.6.0)
# glue         * 1.8.1   2026-04-17 [1] CRAN (R 4.6.0)
# P graphics     * 4.6.1   2026-06-25 [1] local
# P grDevices    * 4.6.1   2026-06-25 [1] local
# P grid           4.6.1   2026-06-25 [1] local
# gridtext       0.1.6   2026-02-19 [1] CRAN (R 4.6.0)
# gtable         0.3.6   2024-10-25 [1] CRAN (R 4.6.0)
# here         * 1.0.2   2025-09-15 [1] CRAN (R 4.6.0)
# hms            1.1.4   2025-10-17 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# P parallel       4.6.1   2026-06-25 [1] local
# patchwork    * 1.3.2   2025-08-25 [1] CRAN (R 4.6.0)
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R.cache        0.17.0  2025-05-02 [1] CRAN (R 4.6.0)
# R.methodsS3    1.8.2   2022-06-13 [1] CRAN (R 4.6.0)
# R.oo           1.27.1  2025-05-02 [1] CRAN (R 4.6.0)
# R.utils        2.13.0  2025-02-24 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg         * 1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
# rlang          1.3.0   2026-07-05 [1] CRAN (R 4.6.1)
# rprojroot      2.1.1   2025-08-26 [1] CRAN (R 4.6.0)
# rstudioapi     0.19.0  2026-06-11 [1] CRAN (R 4.6.0)
# S7             0.2.2   2026-04-22 [1] CRAN (R 4.6.0)
# scales       * 1.4.0   2025-04-24 [1] CRAN (R 4.6.0)
# sessioninfo    1.2.4   2026-06-04 [1] CRAN (R 4.6.0)
# showtext     * 0.9-8   2026-03-21 [1] CRAN (R 4.6.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.6.0)
# P stats        * 4.6.1   2026-06-25 [1] local
# stringi        1.8.7   2025-03-27 [1] CRAN (R 4.6.0)
# stringr      * 1.6.0   2025-11-04 [1] CRAN (R 4.6.0)
# styler         1.11.0  2025-10-13 [1] CRAN (R 4.6.0)
# sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.6.0)
# systemfonts    1.3.2   2026-03-05 [1] CRAN (R 4.6.0)
# textshaping    1.0.5   2026-03-06 [1] CRAN (R 4.6.0)
# tibble       * 3.3.1   2026-01-11 [1] CRAN (R 4.6.0)
# tidyr        * 1.3.2   2025-12-19 [1] CRAN (R 4.6.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.6.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.6.0)
# timechange     0.4.0   2026-01-29 [1] CRAN (R 4.6.0)
# P tools          4.6.1   2026-06-25 [1] local
# tzdb           0.5.0   2025-03-15 [1] CRAN (R 4.6.0)
# P utils        * 4.6.1   2026-06-25 [1] local
# vctrs          0.7.3   2026-04-11 [1] CRAN (R 4.6.0)
# vipor          0.4.7   2023-12-18 [1] CRAN (R 4.6.0)
# vroom          1.7.1   2026-03-31 [1] CRAN (R 4.6.0)
# withr          3.0.3   2026-06-19 [1] CRAN (R 4.6.0)
# xfun           0.60    2026-07-09 [1] CRAN (R 4.6.1)
# xml2           1.6.0   2026-06-22 [1] CRAN (R 4.6.1)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────
# > 
