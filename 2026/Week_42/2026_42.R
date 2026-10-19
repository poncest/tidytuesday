
## Challenge: #TidyTuesday 2026 week 42
## Data:      Which College Majors Are Safest From AI (and Do They Pay)?
## Author:    Steven Ponce
## Date:      2026-10-19

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, scales, 
    glue, ggview
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure settings ----
fig_w   <- 10
fig_h   <- 6.5
fig_dpi <- 320
out_file   <- "2026/Week_42/2026_42.png"

setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1 %||% fonts$title
text_family  <- fonts$text


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 42)
majors_raw <- tt$ai_salary_majors |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(majors_raw)
skimr::skim(majors_raw)


## 4. TIDY DATA ----

### |- majors ----
majors <- majors_raw |>
  mutate(is_rn = str_detect(field_of_study, "^Registered Nursing"))

### |- weighted ECDF: aggregate tied scores, then one step per distinct score ----
ecdf_df <- majors |>
  summarise(
    grads   = sum(n_graduates),
    n_maj   = n(),
    has_rn  = any(is_rn),
    .by     = ai_exposure
  ) |>
  arrange(ai_exposure) |>
  mutate(
    cum_share  = cumsum(grads) / sum(grads),
    prev_share = lag(cum_share, default = 0)
  )

x_min <- -1.3
x_max <- 1.5

step_df <- bind_rows(
  tibble(ai_exposure = x_min, cum_share = 0),
  ecdf_df |> select(ai_exposure, cum_share),
  tibble(ai_exposure = x_max, cum_share = 1)
) |>
  mutate(side = if_else(ai_exposure < 0, "below", "above"))

# Split the step path at zero so the left segment can carry the accent
share_at_zero <- max(ecdf_df$cum_share[ecdf_df$ai_exposure < 0])
step_below <- bind_rows(
  step_df |> filter(ai_exposure < 0),
  tibble(ai_exposure = 0, cum_share = share_at_zero)
)
step_above <- bind_rows(
  tibble(ai_exposure = 0, cum_share = share_at_zero),
  step_df |> filter(ai_exposure >= 0)
)

### |- reader-facing numbers (computed, guarded next to the copy they feed) ----
total_grads <- sum(majors$n_graduates)
n_majors <- nrow(majors)
n_below_zero <- sum(majors$ai_exposure < 0)
share_below <- sum(majors$n_graduates[majors$ai_exposure < 0]) / total_grads

rn_row <- ecdf_df |> filter(has_rn)
rn_exp <- rn_row$ai_exposure
rn_share <- majors$n_graduates[majors$is_rn] / total_grads

# Graduate-weighted median: first exposure score where cumulative share >= 50%
half_row <- ecdf_df |>
  filter(cum_share >= 0.5) |>
  slice_head(n = 1)
x_half <- half_row$ai_exposure

med_major <- median(majors$ai_exposure) # equal-weight median


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    accent    = "#722F37",
    curve     = "#5c5f5b",
    ink       = "#22282b",
    muted     = "#7A7068",
    paper     = "#f4f1ea"
  )
)
clrs <- colors$palette
if (is.null(clrs$accent)) {
  clrs <- list(
    accent = "#722F37", curve = "#5c5f5b",
    ink = "#22282b", muted = "#7A7068", paper = "#f4f1ea"
  )
}
col_accent <- clrs$accent
col_curve <- clrs$curve
col_ink <- clrs$ink
col_muted <- clrs$muted
col_paper <- clrs$paper

### |- titles and caption ----
title_text <- "Hardly any graduates studied a major less AI-exposed than the average occupation"

subtitle_text <- str_glue(
  "{comma(total_grads)} graduates with measured earnings across {n_majors} bachelor's majors.\n",
  "Each step adds the share of graduates from majors with that exposure score."
)

# Provenance constant (not computed from this dataset): AIOE Data Appendix A
n_aioe_occupations <- 774

note_text <- paste(
  "Exposure is the unweighted mean of linked occupational AIOE scores.",
  glue("Index zero is the unweighted average across {n_aioe_occupations} occupations."),
  "The supplied CIP-SOC crosswalk mixes SOC 2018 mappings with SOC 2010 exposure codes,",
  "potentially omitting occupations whose codes changed.",
  "Exposure measures task overlap, not job displacement.",
  "Graduates = those with measured earnings one year after graduation.<br>"
)

caption_text <- create_social_caption(
  tt_year     = 2026,
  tt_week     = 42,
  source_text = "College Scorecard (US Dept. of Education) · AIOE (Felten, Raj & Seamans 2021) · NCES/BLS CIP-SOC crosswalk"
)
caption_full <- paste0(note_text, "<br>", caption_text)

### |- annotation text (built from guarded values) ----
lab_below_big <- percent(share_below, accuracy = 0.1)
lab_below_small <- glue("of measured graduates studied one of the\n{n_below_zero} majors scoring below the index's zero reference")
lab_rn <- glue("Registered Nursing alone:\n{percent(rn_share, accuracy = 0.1)} of measured graduates")
lab_half <- "Half of measured\ngraduates"
lab_zero <- "Index average (zero)"

### |- copy guards ----
all_copy <- c(title_text, subtitle_text, note_text, lab_below_small, lab_rn, lab_half, lab_zero)

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_paper, color = NA),
    panel.background = element_rect(fill = col_paper, color = NA),
    plot.title = element_text(
      family = title_family, face = "bold", size = rel(1.42),
      color = col_ink, lineheight = 1.05, margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      family = text_family, size = rel(0.92), color = col_muted,
      lineheight = 1.2, margin = margin(b = 16)
    ),
    plot.caption = element_textbox_simple(
      family = text_family, size = rel(0.62), color = col_muted,
      lineheight = 1.3, width = unit(1, "npc"), margin = margin(t = 14)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.title.x = element_text(
      family = text_family, color = col_muted,
      size = rel(0.85), margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      family = text_family, color = col_muted,
      size = rel(0.85), margin = margin(r = 10)
    ),
    axis.text = element_text(family = text_family, color = col_muted, size = rel(0.8)),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_line(color = "#ddd5c6", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(20, 28, 14, 20)
  )
)

theme_set(weekly_theme)

### |- annotation positions ----
pos <- list(
  below_x = -1.22, below_big_y = 0.30, below_small_y = 0.215,
  below_pt_x = 0, below_pt_y = share_below,
  rn_x = rn_exp + 0.04, rn_y = (rn_row$prev_share + rn_row$cum_share) / 2, # right of its riser, below the plateau
  half_x = x_half, half_lab_x = x_half - 0.22, half_lab_y = 0.62,
  zero_lab_y = 0.98
)

### |-  main plot ----
p <- ggplot() +
  geom_vline(xintercept = 0, color = col_ink, linewidth = 0.6) +
  geom_step(
    data = step_above, aes(ai_exposure, cum_share),
    direction = "hv", color = col_curve, linewidth = 0.75
  ) +
  geom_step(
    data = step_below, aes(ai_exposure, cum_share),
    direction = "hv", color = col_accent, linewidth = 1.3
  ) +
  annotate("point",
    x = pos$below_pt_x, y = pos$below_pt_y,
    color = col_accent, size = 3
  ) +
  annotate("text",
    x = pos$below_x, y = pos$below_big_y, label = lab_below_big,
    family = title_family, fontface = "bold", size = 11,
    color = col_accent, hjust = 0, vjust = 0
  ) +
  annotate("text",
    x = pos$below_x, y = pos$below_small_y, label = lab_below_small,
    family = text_family, size = 3.6, color = col_ink,
    hjust = 0, vjust = 1, lineheight = 1.15
  ) +
  annotate("text",
    x = 0.03, y = pos$zero_lab_y, label = lab_zero,
    family = text_family, size = 3.1, color = col_ink, hjust = 0
  ) +
  annotate("text",
    x = pos$rn_x, y = pos$rn_y, label = lab_rn,
    family = text_family, size = 3.1, color = col_muted,
    hjust = 0, lineheight = 1.15
  ) +
  annotate("point", x = pos$half_x, y = 0.5, color = col_ink, size = 2) +
  annotate("segment",
    x = pos$half_lab_x + 0.02, xend = pos$half_x - 0.02,
    y = pos$half_lab_y - 0.03, yend = 0.51,
    color = col_muted, linewidth = 0.3
  ) +
  annotate("text",
    x = pos$half_lab_x, y = pos$half_lab_y, label = lab_half,
    family = text_family, size = 3.1, color = col_muted,
    hjust = 1, vjust = 0, lineheight = 1.1
  ) +
  scale_x_continuous(
    limits = c(x_min, x_max), breaks = seq(-1, 1.5, 0.5),
    labels = label_number(accuracy = 0.1, style_positive = "plus"),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = percent,
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_full,
    x = "AI exposure of the occupations linked to a major (AIOE score)   more exposed \u2192",
    y = "Cumulative share of measured graduates"
  )

### |-  preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in", dpi = 320)


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

# ─ Session info ─────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-10-19
# rstudio  2026.10.0+233 Blue Mistflower (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
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
# ggplot2      * 4.0.3   2026-04-22 [1] CRAN (R 4.6.0)
# ggtext       * 0.2.0   2026-08-28 [1] CRAN (R 4.6.1)
# ggview       * 0.2.2   2025-07-05 [1] CRAN (R 4.6.0)
# gh             1.6.1   2026-07-20 [1] CRAN (R 4.6.1)
# gitcreds       0.1.2   2022-09-08 [1] CRAN (R 4.6.0)
# glue         * 1.8.1   2026-04-17 [1] CRAN (R 4.6.0)
# P graphics     * 4.6.1   2026-06-25 [1] local
# P grDevices    * 4.6.1   2026-06-25 [1] local
# P grid           4.6.1   2026-06-25 [1] local
# gridtext       0.1.6   2026-02-19 [1] CRAN (R 4.6.0)
# gtable         0.3.6   2024-10-25 [1] CRAN (R 4.6.0)
# here         * 1.0.2   2025-09-15 [1] CRAN (R 4.6.0)
# hms            1.1.4   2025-10-17 [1] CRAN (R 4.6.0)
# httr2          1.3.0   2026-07-13 [1] CRAN (R 4.6.1)
# janitor      * 2.2.1   2024-12-22 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# otel           0.2.0   2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R.cache        0.17.0  2025-05-02 [1] CRAN (R 4.6.0)
# R.methodsS3    1.8.2   2022-06-13 [1] CRAN (R 4.6.0)
# R.oo           1.27.1  2025-05-02 [1] CRAN (R 4.6.0)
# R.utils        2.13.0  2025-02-24 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
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
# snakecase      0.11.1  2023-08-27 [1] CRAN (R 4.6.0)
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
# tidytuesdayR   1.3.2   2026-04-12 [1] CRAN (R 4.6.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.6.0)
# timechange     0.4.0   2026-01-29 [1] CRAN (R 4.6.0)
# P tools          4.6.1   2026-06-25 [1] local
# tzdb           0.5.0   2025-03-15 [1] CRAN (R 4.6.0)
# P utils        * 4.6.1   2026-06-25 [1] local
# vctrs          0.7.3   2026-04-11 [1] CRAN (R 4.6.0)
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
# ────────────────────────────────────────────────────────────────────────────────
# > 
