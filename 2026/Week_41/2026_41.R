
## Challenge: #TidyTuesday 2026 week 41
## Data:      AmphibiaWeb
## Author:    Steven Ponce
## Date:      2026-10-12

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, janitor, scales, 
    glue, ggview, magick, countrycode
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure settings ----
fig_w      <- 10
fig_h      <- 10
out_file   <- "2026/Week_41/2026_41.png"

## 2. READ IN THE DATA ----
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-13/frogs_and_toads_amphibiaweb.csv"

frogs_raw <- readr::read_csv(
  url,
  col_types = cols(.default = col_character()),
  na = character()
) |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(frogs_raw)
skimr::skim(frogs_raw)


## 4. TIDY DATA ----
frogs <- frogs_raw |>
  mutate(
    across(-c(isocc, intro_isocc), \(x) na_if(x, "NA")),
    intro_isocc = na_if(intro_isocc, "NA"),
    has_account = !is.na(description) | !is.na(life_history) | !is.na(distribution)
  )

intro <- frogs |>
  filter(!is.na(intro_isocc)) |>
  transmute(amphib_id, cc = str_split(str_remove_all(intro_isocc, " "), ",")) |>
  unnest(cc) |>
  filter(cc != "") |>
  mutate(introduced = TRUE)

native <- frogs |>
  filter(isocc != "NA") |>
  select(amphib_id, has_account, isocc) |>
  mutate(cc = str_split(str_remove_all(isocc, " "), ",")) |>
  unnest(cc) |>
  filter(cc != "") |>
  left_join(intro, by = c("amphib_id", "cc")) |>
  filter(is.na(introduced)) |>
  distinct(amphib_id, cc, has_account)

country <- native |>
  summarise(n = n_distinct(amphib_id), written = sum(has_account), .by = cc) |>
  mutate(
    share = written / n,
    pct = round(100 * share),
    name = countrycode(cc, "iso2c", "country.name", warn = FALSE)
  ) |>
  arrange(desc(n))

min_species <- 250
grid15 <- country |> filter(n >= min_species)

n_ambiguous <- sum(frogs$isocc == "NA")
br <- grid15 |> filter(cc == "BR")

## the 13-country band and the two exceptions
band <- grid15 |> filter(pct <= 30)
exceptions <- grid15 |>
  filter(pct > 30) |>
  arrange(desc(n))
band_rng <- range(band$pct)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
col_paper <- "#f4f1ea"
col_ink <- "#22282b"
col_soft <- "#5c5f5b"
col_rest <- "#e4ddd0"
col_accent <- "#722F37"

clrs <- get_theme_colors(
  palette = list(paper = col_paper, ink = col_ink, soft = col_soft, accent = col_accent)
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()
if (!"IBM Plex Mono" %in% sysfonts::font_families()) {
    sysfonts::font_add_google("IBM Plex Mono", "IBM Plex Mono")
}
title_family <- fonts$title_1          
text_family  <- fonts$text             
mono_family  <- "IBM Plex Mono"

### |- titles and caption ----
title_text <- glue(
  "AmphibiaWeb lists {comma(br$n)} native frog species in {br$name}.<br>",
  "It has written accounts for {comma(br$written)}."
)

subtitle_text <- glue(
  "In {nrow(band)} of the {nrow(grid15)} largest national lists, ",
  "{band_rng[1]}–{band_rng[2]}% of recorded native species have an ",
  "AmphibiaWeb account; {exceptions$name[1]} and {exceptions$name[2]} stand apart.",
  "<br><br>",
  "<span style='font-size:11pt; color:{col_soft}'>Each square is one country's list ",
  "of native frog species, largest first. Area shows how many are listed; the ",
  "height of the <b style='color:{col_accent}'>burgundy fill</b> shows the share ",
  "with a written account.</span>"
)

method_note <- glue(
  "Account = description, life history or distribution text on AmphibiaWeb; ",
  "no account does not mean no published research. Each country counts its ",
  "recorded native species (introduced countries removed); a species can appear ",
  "in several lists, so lists do not add up. Excludes {n_ambiguous} species whose ",
  "country code reads \"NA\" (Namibia's code, or missing). Countries with ",
  "≥ {min_species} native species."
)

social <- create_social_caption(
  tt_year = 2026,
  tt_week = 41,
  source_text = "AmphibiaWeb (amphibiaweb.org)"
)
caption_text <- glue(
  "<span style='font-family:\"{mono_family}\"'>{method_note}</span><br><br>{social}"
)

## Layout: one cell per country, 5 x 3.
grid_cols <- 5
row_step <- 1.25
max_side <- 0.82

squares <- grid15 |>
  arrange(desc(n)) |>
  mutate(
    i = row_number() - 1,
    x0 = i %% grid_cols,
    y0 = -(i %/% grid_cols) * row_step,
    name_lab = if_else(nchar(name) > 13, str_replace(name, " (?=\\S+$)", "<br>"), name),
    side = sqrt(n / max(n)) * max_side, # outer AREA proportional to n
    level = side * share, # band HEIGHT proportional to share
    label = glue(
      "<b>{name_lab}</b><br>",
      "<span style='color:{col_soft}'>{written} of {comma(n)} · {pct}%</span>"
    )
  )

# Brazil teaches the encoding once, in words only (its numbers sit below).
br_sq <- squares |> filter(cc == "BR")

key_labels <- tibble(
  x      = br_sq$x0 + 0.025,
  y      = c(br_sq$y0 + br_sq$side - 0.03, br_sq$y0 + br_sq$level / 2),
  label  = c("without an account", "with an account"),
  color = c(col_soft, col_paper),
  vjust  = c(1, 0.5)
)

### |-  plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_paper, color = NA),
    panel.background = element_rect(fill = col_paper, color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_textbox_simple(
      family = title_family, color = col_ink, size = rel(1.55),
      lineheight = 1.1, margin = margin(b = 18)
    ),
    plot.subtitle = element_textbox_simple(
      family = text_family, color = col_ink, size = rel(1.0),
      lineheight = 1.3, margin = margin(b = 22)
    ),
    plot.caption = element_textbox_simple(
      family = text_family, color = col_soft, size = rel(0.62),
      lineheight = 1.3, margin = margin(t = 34)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(28, 32, 20, 32)
  )
)

theme_set(weekly_theme)

### |-  main plot ----
p <- ggplot(squares) +
    geom_rect(aes(xmin = x0, xmax = x0 + side, ymin = y0, ymax = y0 + side),
      fill = col_rest, color = col_soft, linewidth = 0.3
    ) +
      geom_rect(aes(xmin = x0, xmax = x0 + side, ymin = y0, ymax = y0 + level),
        fill = col_accent, color = NA
      ) +
      geom_text(
        data = key_labels,
        aes(x = x, y = y, label = label, color = color, vjust = vjust),
        hjust = 0, size = 2.9, family = text_family
      ) + 
      scale_color_identity() +
      geom_richtext(aes(x = x0, y = y0 - 0.06, label = label),
        hjust = 0, vjust = 1, size = 3.4, lineheight = 1.15,
        family = text_family, color = col_ink,
        fill = NA, label.color = NA, label.padding = unit(0, "pt")
      ) +
      coord_equal(clip = "off") +
      labs(title = title_text, subtitle = subtitle_text, caption = caption_text)

### |-  preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in", dpi = 320)


## 6. SAVE ----
ggview::save_ggplot(
    plot   = p,
    file   = out_file,
    width  = fig_w,
    height = fig_h,
    units  = "in",
    dpi    = 320
)

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

# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-10-12
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# base64enc      0.1-6   2026-02-02 [1] CRAN (R 4.6.0)
# bit            4.6.0   2025-03-06 [1] CRAN (R 4.6.0)
# bit64          4.8.2   2026-05-19 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# countrycode  * 1.9.0   2026-08-20 [1] CRAN (R 4.6.1)
# crayon         1.5.3   2024-06-20 [1] CRAN (R 4.6.0)
# curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1   2026-06-25 [1] local
# digest         0.6.39  2025-11-19 [1] CRAN (R 4.6.0)
# dplyr        * 1.2.1   2026-04-03 [1] CRAN (R 4.6.0)
# evaluate       1.0.5   2025-08-27 [1] CRAN (R 4.6.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
# fastmap        1.2.0   2024-05-15 [1] CRAN (R 4.6.0)
# forcats      * 1.0.1   2025-09-25 [1] CRAN (R 4.6.0)
# generics       0.1.4   2025-05-09 [1] CRAN (R 4.6.0)
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
# htmltools      0.5.9   2025-12-04 [1] CRAN (R 4.6.0)
# janitor      * 2.2.1   2024-12-22 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
# knitr          1.51    2025-12-20 [1] CRAN (R 4.6.0)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magick       * 2.9.1   2026-02-28 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# otel           0.2.0   2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
# repr           1.1.7   2024-03-22 [1] CRAN (R 4.6.0)
# rlang          1.3.0   2026-07-05 [1] CRAN (R 4.6.1)
# rprojroot      2.1.1   2025-08-26 [1] CRAN (R 4.6.0)
# rstudioapi     0.19.0  2026-06-11 [1] CRAN (R 4.6.0)
# S7             0.2.2   2026-04-22 [1] CRAN (R 4.6.0)
# scales       * 1.4.0   2025-04-24 [1] CRAN (R 4.6.0)
# sessioninfo    1.2.4   2026-06-04 [1] CRAN (R 4.6.0)
# showtext     * 0.9-8   2026-03-21 [1] CRAN (R 4.6.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.6.0)
# skimr          2.2.2   2026-01-10 [1] CRAN (R 4.6.0)
# snakecase      0.11.1  2023-08-27 [1] CRAN (R 4.6.0)
# P stats        * 4.6.1   2026-06-25 [1] local
# stringi        1.8.7   2025-03-27 [1] CRAN (R 4.6.0)
# stringr      * 1.6.0   2025-11-04 [1] CRAN (R 4.6.0)
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
# utf8           1.2.6   2025-06-08 [1] CRAN (R 4.6.0)
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
# ─────────────────────────────────────────────────────────────────────────
# > 
