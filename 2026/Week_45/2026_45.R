## Challenge: #TidyTuesday 2026 week 45
## Data:      evidence-graded power rankings of fictional characters
## Author:    Steven Ponce
## Date:      2026-11-09

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, scales, glue, ggview,
    patchwork, ragg, magick
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure settings ----
fig_w   <- 12
fig_h   <- 7.5
fig_dpi <- 320
out_file <- "2026/Week_45/2026_45.png"

### |- fonts ----
setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1


## 2. READ IN THE DATA ----
## Read every field as character with no NA tokens; restore numbers column by column.
tt_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-10/"
read_chr <- function(url) {
    read_csv(url, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE)
}

citations_raw <- read_chr(paste0(tt_url, "fiction_citations.csv"))
rankings_raw  <- read_chr(paste0(tt_url, "fiction_rankings.csv"))

## Hand-reviewed pool, written by tt_2026_45_match_candidates.R: every word-overlap candidate
## pair (prose original vs adaptation) with its decision -- clean, partial or rejected.
matches_path <- here::here("2026/Week_45/tt_2026_45_matched_events.csv")
matches_raw  <- read_chr(matches_path)

## 3. EXAMINING THE DATA ----
glimpse(citations_raw)
glimpse(matches_raw)
citations_raw |> count(type, confidence) |> print(n = Inf)


## 4. TIDY DATA ----

### |- base rates quoted in copy ----
n_citations <- nrow(citations_raw)
n_scaled <- sum(citations_raw$confidence == "scaled")

### |- look up each side of every matched pair in the citations ----
## A side matches when franchise, character, type and locator agree exactly and the
## citation's claim starts with the recorded opening; exactly one citation must match.
lookup_side <- function(slug, entry, type_wanted, loc, claim_start) {
  hit <- citations_raw |>
    filter(
      franchise_slug == slug, subject_id == entry, subject_kind == "entry",
      type == type_wanted, locator == loc, str_starts(claim, fixed(claim_start))
    )
  if (nrow(hit) != 1) stop(glue("{entry} / {loc}: {nrow(hit)} citations matched, expected 1"))
  hit |> select(work, confidence, claim)
}

n_reviewed <- nrow(matches_raw)
n_rejected <- sum(matches_raw$match == "rejected")

pairs <- matches_raw |>
  filter(match != "rejected") |>
  mutate(
    prose = pmap(list(franchise_slug, entry_id, "novel", prose_locator, prose_claim_start), lookup_side),
    adapt = pmap(list(franchise_slug, entry_id, adapt_type, adapt_locator, adapt_claim_start), lookup_side)
  ) |>
  unnest_wider(prose, names_sep = "_") |>
  unnest_wider(adapt, names_sep = "_") |>
  left_join(
    rankings_raw |> select(franchise_slug, entry_id, name, franchise),
    by = c("franchise_slug", "entry_id")
  ) |>
  mutate(changed = prose_confidence != adapt_confidence)

clean_pairs <- pairs |> filter(match == "clean")
n_clean <- nrow(clean_pairs)
n_changed <- sum(clean_pairs$changed)
n_same <- n_clean - n_changed
n_partial <- sum(pairs$match == "partial")

### |- Panel A: the one clean pair whose grade changes ----
hero <- clean_pairs |> filter(changed)
stopifnot(nrow(hero) == 1, hero$entry_id == "arthur-dayne", hero$event_label == "Tower of Joy")

### |- Panel B: row layout, franchise groups with a header row each ----
franchise_order <- c("westeros", "overlord", "solo-leveling")
franchise_head <- c(
  westeros        = "A Song of Ice and Fire  /  Game of Thrones",
  overlord        = "Overlord light novels  /  anime",
  `solo-leveling` = "Solo Leveling web novel  /  webtoon"
)

ledger <- clean_pairs |>
  mutate(franchise_slug = factor(franchise_slug, levels = franchise_order)) |>
  arrange(franchise_slug, name, event_label) |>
  mutate(row = row_number())

## y positions: one slot per row, plus one header slot (and a small gap) per franchise
row_step <- 1
ledger <- ledger |>
  mutate(
    group_idx = as.integer(franchise_slug),
    y = -(row + group_idx * 1.25) * row_step
  )

group_heads <- ledger |>
  summarise(y = max(y) + 1.05 * row_step, .by = franchise_slug) |>
  mutate(label = franchise_head[as.character(franchise_slug)])

partial_row <- pairs |>
  filter(match == "partial") |>
  mutate(y = min(ledger$y) - 2.1 * row_step)


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    accent = "#722F37",
    ink    = "#2C2825",
    shown  = "#5E5650",
    stated = "#FFFFFF",
    rule   = "#D9D3C9",
    muted  = "#7A7068",
    card   = "#FFFFFF"
  )
)
col_accent <- clrs$palette$accent
col_ink <- clrs$palette$ink
col_shown <- clrs$palette$shown
col_stated <- clrs$palette$stated
col_rule <- clrs$palette$rule
col_muted <- clrs$palette$muted
col_card <- clrs$palette$card

### |- titles and caption ----
title_text <- "One fight, two grades"

subtitle_text <- glue(
    "Cited Feats grades each citation **shown** if the cited passage depicts the feat, ",
    "**stated** if a character, narrator or guidebook asserts it.<br>",
    "A grade describes the evidence cited for a claim, not the event itself."
)

cue_a <- "The event is the same; the cited passages are not. The book passage is Eddard's\naccount, told to Bran; the show's is Bran's vision of the fight itself."

head_b <- glue("B   {n_same} of {n_clean} hand-matched events keep the same grade")
cue_b  <- glue(
    "Events cited from both a prose original and its adaptation, found by word overlap within each\n",
    "character's citations and checked by hand; not every event the site cites twice."
)

caption_lines <- c(
    glue(
        "Claim text is Cited Feats' own summary of each cited passage. {n_reviewed} word-overlap candidates from the three franchises cited in both ",
        "a prose original and its adaptation were read by hand: {n_clean} same event, {n_partial} partial, {n_rejected} different events."
    ),
    glue(
        "The site's third grade, scaled (inferred by comparison), appears in {n_scaled} of {comma(n_citations)} citations. ",
        "Data snapshot 2026-10-10; the live site has changed since."
    )
)

caption_text <- paste0(
    caption_lines[1], "<br>", caption_lines[2], "<br>",
    create_social_caption(
        tt_year = 2026,
        tt_week = 45,
        source_text = "Cited Feats open data (CC BY 4.0) via TidyTuesday"
    )
)

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        plot.title = element_textbox_simple(
            face = "bold", family = title_family, size = 30,
            color = col_ink, lineheight = 1.0, margin = margin(b = 12)
        ),
        plot.subtitle = element_textbox_simple(
            family = fonts$subtitle, size = 11.5, color = col_ink,
            lineheight = 1.3, margin = margin(b = 10)
        ),
        plot.caption = element_textbox_simple(
            family = fonts$caption, size = 6.6, color = col_muted,
            lineheight = 1.35, margin = margin(t = 10)
        ),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(14, 20, 8, 20)
    )
)

theme_set(weekly_theme)

### |- grade chip helper (shared by both panels) ----
chip_layer <- function(df, x, y, grade, w, h, size, border, lw) {
    list(
        geom_tile(
            data = df, aes(x = {{ x }}, y = {{ y }}),
            width = w, height = h,
            fill = if_else(pull(df, {{ grade }}) == "shown", col_shown, col_stated),
            color = border, linewidth = lw
        ),
        geom_text(
            data = df, aes(x = {{ x }}, y = {{ y }}, label = str_to_upper({{ grade }})),
            family = fonts$text, fontface = "bold", size = size,
            color = if_else(pull(df, {{ grade }}) == "shown", "white", col_ink)
        )
    )
}

### |- panel A: one event, two cited passages ----
cards <- tibble(
    side   = c("prose", "adapt"),
    x      = c(0.245, 0.755),
    head   = c("PROSE ORIGINAL", "ADAPTATION"),
    source = c(
        glue("{hero$prose_work}\n{hero$prose_locator}"),
        glue("{hero$adapt_work}\n{hero$adapt_locator}")
    ),
    claim  = c(str_wrap(hero$prose_claim, 30), str_wrap(hero$adapt_claim, 30)),
    grade  = c(hero$prose_confidence, hero$adapt_confidence)
)
card_w <- 0.47
card_bottom <- 0.02
card_top <- 0.66

p_a <- ggplot() +
    # panel heading and decoding cue
    annotate("text",
             x = 0, y = 1.00, hjust = 0, vjust = 1, family = fonts$text, size = 3.6,
             fontface = "bold", color = col_ink, label = "A   One event, two cited passages"
    ) +
    annotate("text",
             x = 0, y = 0.945, hjust = 0, vjust = 1, family = fonts$text, size = 2.9,
             color = col_muted, lineheight = 1.15, label = cue_a
    ) +
    # the event
    annotate("rect",
             xmin = 0.22, xmax = 0.78, ymin = 0.765, ymax = 0.835,
             fill = col_card, color = col_ink, linewidth = 0.4
    ) +
    annotate("text",
             x = 0.5, y = 0.80, family = fonts$text, size = 3.6, fontface = "bold",
             color = col_ink, label = glue("{hero$name} at the {hero$event_label}")
    ) +
    # the branch: the accent marks the change, not a grade
    annotate("segment", x = 0.5, xend = 0.5, y = 0.765, yend = 0.715, color = col_accent, linewidth = 0.9) +
    annotate("segment", x = 0.245, xend = 0.755, y = 0.715, yend = 0.715, color = col_accent, linewidth = 0.9) +
    annotate("segment", x = 0.245, xend = 0.245, y = 0.715, yend = card_top, color = col_accent, linewidth = 0.9) +
    annotate("segment", x = 0.755, xend = 0.755, y = 0.715, yend = card_top, color = col_accent, linewidth = 0.9) +
    # the two cards
    geom_rect(
        data = cards,
        aes(xmin = x - card_w / 2, xmax = x + card_w / 2, ymin = card_bottom, ymax = card_top),
        fill = col_card, color = col_rule, linewidth = 0.5
    ) +
    geom_text(
        data = cards, aes(x = x - card_w / 2 + 0.03, y = card_top - 0.035, label = head),
        hjust = 0, vjust = 1, family = fonts$text, size = 2.6, fontface = "bold", color = col_muted
    ) +
    geom_text(
        data = cards, aes(x = x - card_w / 2 + 0.03, y = card_top - 0.085, label = source),
        hjust = 0, vjust = 1, family = fonts$text, size = 3.1, color = col_ink, lineheight = 1.1
    ) +
    geom_text(
        data = cards, aes(x = x - card_w / 2 + 0.03, y = card_top - 0.215, label = claim),
        hjust = 0, vjust = 1, family = fonts$text, size = 2.75, fontface = "italic",
        color = col_muted, lineheight = 1.2
    ) +
    chip_layer(
        cards |> mutate(cy = card_bottom + 0.075), x, cy, grade,
        w = 0.30, h = 0.075, size = 4.2, border = col_ink, lw = 0.5
    ) +
    scale_x_continuous(limits = c(0, 1), expand = expansion(0)) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(0)) +
    coord_cartesian(clip = "off")

### |- panel B: the whole reviewed pool ----
x_name  <- 0.025
x_event <- 0.27
x_prose <- 0.70
x_adapt <- 0.89
chip_w  <- 0.15
chip_h  <- 0.72
y_top   <- max(group_heads$y) + 1.9

p_b <- ggplot() +
    # panel heading and decoding cue
    annotate("text",
             x = 0, y = y_top + 2.5, hjust = 0, vjust = 1, family = fonts$text, size = 3.6,
             fontface = "bold", color = col_ink, label = head_b
    ) +
    annotate("text",
             x = 0, y = y_top + 1.5, hjust = 0, vjust = 1, family = fonts$text, size = 2.9,
             color = col_muted, lineheight = 1.15, label = cue_b
    ) +
    # column headers
    annotate("text",
             x = c(x_prose, x_adapt), y = y_top - 0.55, label = c("PROSE ORIGINAL", "ADAPTATION"),
             family = fonts$text, size = 2.5, fontface = "bold", color = col_muted
    ) +
    # franchise group headers
    geom_text(
        data = group_heads, aes(x = 0, y = y, label = label),
        hjust = 0, family = fonts$caption, size = 2.4, color = col_muted
    ) +
    # rows: name and event
    geom_text(
        data = ledger, aes(x = x_name, y = y, label = name),
        hjust = 0, family = fonts$text, size = 2.9,
        fontface = if_else(ledger$changed, "bold", "plain"),
        color = if_else(ledger$changed, col_accent, col_ink)
    ) +
    geom_text(
        data = ledger, aes(x = x_event, y = y, label = event_label),
        hjust = 0, family = fonts$text, size = 2.9,
        fontface = if_else(ledger$changed, "bold", "plain"),
        color = if_else(ledger$changed, col_accent, col_muted)
    ) +
    # connector between the two grades
    geom_segment(
        data = ledger,
        aes(x = x_prose + chip_w / 2, xend = x_adapt - chip_w / 2, y = y, yend = y),
        color = if_else(ledger$changed, col_accent, col_rule),
        linewidth = if_else(ledger$changed, 0.9, 0.35)
    ) +
    chip_layer(
        ledger, x_prose, y, prose_confidence,
        w = chip_w, h = chip_h, size = 2.3,
        border = if_else(ledger$changed, col_accent, col_shown),
        lw = if_else(ledger$changed, 0.9, 0.3)
    ) +
    chip_layer(
        ledger, x_adapt, y, adapt_confidence,
        w = chip_w, h = chip_h, size = 2.3,
        border = if_else(ledger$changed, col_accent, col_shown),
        lw = if_else(ledger$changed, 0.9, 0.3)
    ) +
    # partial match, outside the count
    annotate("segment",
             x = 0, xend = 1, y = partial_row$y + 1.15, yend = partial_row$y + 1.15,
             color = col_rule, linewidth = 0.4, linetype = "22"
    ) +
    annotate("text",
             x = 0, y = partial_row$y + 0.6, hjust = 0, family = fonts$caption, size = 2.4,
             color = col_muted, label = "Partial match, not counted"
    ) +
    geom_text(
        data = partial_row, aes(x = x_name, y = y - 0.25, label = name),
        hjust = 0, family = fonts$text, size = 2.9, fontface = "italic", color = col_muted
    ) +
    geom_text(
        data = partial_row, aes(x = x_event, y = y - 0.25, label = event_label),
        hjust = 0, family = fonts$text, size = 2.9, fontface = "italic", color = col_muted
    ) +
    chip_layer(
        partial_row |> mutate(y = y - 0.25), x_prose, y, prose_confidence,
        w = chip_w, h = chip_h, size = 2.3, border = col_shown, lw = 0.3
    ) +
    chip_layer(
        partial_row |> mutate(y = y - 0.25), x_adapt, y, adapt_confidence,
        w = chip_w, h = chip_h, size = 2.3, border = col_shown, lw = 0.3
    ) +
    scale_x_continuous(limits = c(0, 1), expand = expansion(0)) +
    scale_y_continuous(limits = c(partial_row$y - 1, y_top + 2.6), expand = expansion(0)) +
    coord_cartesian(clip = "off")

### |- combine ----
p <- p_a + p_b +
    plot_layout(widths = c(0.82, 1.18)) +
    plot_annotation(
        title = title_text, subtitle = subtitle_text, caption = caption_text,
        theme = weekly_theme
    )

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

# ─ Session info ───────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.7.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-11-09
# rstudio  2026.10.1+239 Blue Mistflower (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ───────────────────────────────────────────────
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
# magick       * 2.9.1   2026-02-28 [1] CRAN (R 4.6.0)
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
# ──────────────────────────────────────────────────────────
# > 
