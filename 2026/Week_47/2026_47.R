## Challenge: #TidyTuesday 2026 week 47
## Data:      Filming Locations (Movie Scene Map, Wikidata P915)
## Author:    Steven Ponce
## Date:      2026-11-24

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


# 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, ggtext, showtext, scales, glue, ggview,
    ragg, magick, here
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure settings ----
fig_w    <- 12
fig_h    <- 7.5
fig_dpi  <- 320
out_file <- "2026/Week_47/2026_47.png"

### |- fonts ----
setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1
title_size   <- 28


## 2. READ IN THE DATA ----
## Read every field as character with no NA tokens; restore numbers column by column.
read_chr <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE)
}

tt_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-24/"
locations_raw <- read_chr(paste0(tt_url, "locations.csv"))
production_locations_raw <- read_chr(paste0(tt_url, "production_locations.csv"))


## 3. EXAMINING THE DATA ----
glimpse(locations_raw)
glimpse(production_locations_raw)


## 4. TIDY DATA ----

### |- places with their link counts ----
locations <- locations_raw |>
  mutate(n_productions = as.numeric(n_productions))


### |- reuse classes: powers of two ----
class_levels <- c("1", "2–3", "4–7", "8–15", "16–31", "32–63", "64–127", "128+")

reuse <- locations |>
  mutate(class = cut(n_productions,
    breaks = c(1, 2, 4, 8, 16, 32, 64, 128, Inf),
    labels = class_levels, right = FALSE
  )) |>
  summarise(places = n(), links = sum(n_productions), .by = class) |>
  arrange(class) |>
  mutate(
    place_share = places / sum(places),
    link_share = links / sum(links),
    y = rev(seq_along(class)), # "1" on top, "128+" at the bottom
    tone = case_when(
      class == "128+" ~ "subject",
      class == "1" ~ "comparator",
      TRUE ~ "context"
    )
  )

### |- numbers quoted in copy (all from the data) ----
n_places <- sum(reuse$places)
n_links <- sum(reuse$links)
once <- filter(reuse, class == "1")
top <- filter(reuse, class == "128+")


### |- bar geometry ----
gap <- 0.10 # half-width of the centre column
bar_h <- 0.36
inside_cut <- 0.5 # labels move inside bars this long

bars <- reuse |>
  mutate(
    l_xmin = -gap - place_share, l_xmax = -gap,
    r_xmin = gap, r_xmax = gap + link_share,
    lab_place = glue("{sprintf('%.1f', 100 * place_share)}% ({comma(places)})"),
    lab_link = glue("{sprintf('%.1f', 100 * link_share)}% ({comma(links)})")
  )

grid_at <- c(0, 0.2, 0.4, 0.6, 0.8)
grid_df <- tibble(x = c(-gap - grid_at, gap + grid_at))
y_top <- max(bars$y) + 0.55 # gridlines stop below the headings
y_head <- max(bars$y) + 0.95


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    subject    = "#722F37",
    comparator = "#A9A097",
    edge       = "#8C847C",
    context    = "#D3CDC4",
    ink        = "#2C2825",
    label      = "#655C55",
    grid       = "#E4DED4"
  )
)
col_subject <- clrs$palette$subject
col_comparator <- clrs$palette$comparator
col_edge <- clrs$palette$edge
col_context <- clrs$palette$context
col_ink <- clrs$palette$ink
col_label <- clrs$palette$label
col_grid <- clrs$palette$grid
col_bg <- clrs$background

### |- titles and caption ----
title_text <- glue(
  "<span style='color:{col_subject}'>{top$places} places</span> account for more ",
  "filming-location links<br>than the {comma(once$places)} places linked once"
)

subtitle_text <- glue(
  "A **place** is one distinct recorded location. A **link** is one film or TV series ",
  "that Wikidata records as filmed there.<br>",
  "Places are grouped by their number of links; both sides share one percentage scale."
)

caption_note <- glue(
  "Wikidata filming-location (P915) records via Movie Scene Map. ",
  "Links are production–place associations, not scenes or shooting days.<br>",
  "The source lists 22,110 Wikidata links; this TidyTuesday extract has {comma(n_links)}."
)

caption_text <- paste0(
  caption_note, "<br>",
  create_social_caption(
    tt_year = 2026,
    tt_week = 47,
    source_text = "Movie Scene Map (Wikidata) via TidyTuesday"
  )
)

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_textbox_simple(
      face = "bold", family = title_family, size = title_size,
      color = col_ink, lineheight = 1.0, margin = margin(b = 8)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$subtitle, size = 12, color = col_ink,
      lineheight = 1.3, margin = margin(b = 4)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$caption, size = 6.6, color = col_label,
      lineheight = 1.35, margin = margin(t = 8)
    ),
    axis.text.x = element_text(family = fonts$text, size = 9, color = col_label),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(14, 20, 8, 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot(bars) +
  geom_segment(
    data = grid_df, aes(x = x, xend = x, y = 0.5, yend = y_top),
    color = col_grid, linewidth = 0.3
  ) +
  geom_rect(aes(xmin = l_xmin, xmax = l_xmax, ymin = y - bar_h, ymax = y + bar_h, fill = tone)) +
  geom_rect(aes(xmin = r_xmin, xmax = r_xmax, ymin = y - bar_h, ymax = y + bar_h, fill = tone)) +
  geom_rect(
    data = filter(bars, tone == "comparator"),
    aes(xmin = l_xmin, xmax = l_xmax, ymin = y - bar_h, ymax = y + bar_h),
    fill = NA, color = col_edge, linewidth = 0.4
  ) +
  geom_rect(
    data = filter(bars, tone == "comparator"),
    aes(xmin = r_xmin, xmax = r_xmax, ymin = y - bar_h, ymax = y + bar_h),
    fill = NA, color = col_edge, linewidth = 0.4
  ) +
  geom_text(
    data = filter(bars, place_share < inside_cut),
    aes(x = l_xmin - 0.01, y = y, label = lab_place),
    hjust = 1, family = fonts$text, size = 3.7, color = col_label
  ) +
  geom_text(
    data = filter(bars, place_share >= inside_cut),
    aes(x = l_xmin + 0.015, y = y, label = lab_place),
    hjust = 0, family = fonts$text, fontface = "bold", size = 3.7, color = col_ink
  ) +
  geom_text(
    aes(
      x = r_xmax + 0.01, y = y, label = lab_link,
      fontface = if_else(tone == "subject", "bold", "plain"),
      color = if_else(tone == "subject", "subject", "label")
    ),
    hjust = 0, family = fonts$text, size = 3.7
  ) +
  geom_text(
    aes(x = 0, y = y, label = class),
    family = fonts$text, size = 4.3, color = col_ink
  ) +
  annotate("text",
    x = -gap, y = y_head, hjust = 1, label = glue("Share of the {comma(n_places)} distinct places"),
    family = fonts$text, fontface = "bold", size = 4.3, color = col_ink
  ) +
  annotate("text",
    x = gap, y = y_head, hjust = 0, label = glue("Share of the {comma(n_links)} production–place links"),
    family = fonts$text, fontface = "bold", size = 4.3, color = col_ink
  ) +
  annotate("text",
    x = 0, y = y_head, label = "Links per place",
    family = fonts$text, size = 3.3, color = col_label
  ) +
  # Scales
  scale_fill_manual(
    values = c(subject = col_subject, comparator = col_comparator, context = col_context),
    guide = "none"
  ) +
  scale_color_manual(values = c(subject = col_subject, label = col_label), guide = "none") +
  scale_x_continuous(
    limits = c(-gap - 0.86, gap + 0.86),
    breaks = grid_df$x,
    labels = percent(c(grid_at, grid_at)),
    expand = expansion(0)
  ) +
  scale_y_continuous(limits = c(0.45, y_head + 0.35), expand = expansion(0)) +
  # Labs
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)

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