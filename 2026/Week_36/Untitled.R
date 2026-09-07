## Challenge: #TidyTuesday 2026 week 36
## Data:      Cappuccino Index (James Hoffmann / Filip Reierson)
## Author:    Steven Ponce
## Date:      2026-09-07

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,     # Easily Install and Load the 'Tidyverse'
    ggtext,        # Improved Text Rendering Support for 'ggplot2'
    showtext,      # Using Fonts More Easily in R Graphs
    janitor,       # Simple Tools for Examining and Cleaning Dirty Data
    scales,        # Scale Functions for Visualization
    glue,          # Interpreted String Literals
    geomtextpath,  # Text that follows a path (iso-index contour labels)
    ggrepel        # Non-overlapping labels
)

## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2026, week = 36)
cafe <- tt$cafe
cappuccino_index <- tt$cappuccino_index

## 3. EXAMINING THE DATA ----
## (see Phase 0 diagnostics — index formula confirmed as
##  60 * mean(price_gbp) / mean(hourly_wage_gbp); wage dispersion
##  2.5x price dispersion on log scale)

## 4. TIDY DATA ----

# Country-level means — the exact quantities the published index is built from
driver_diag <- cafe |>
    group_by(country) |>
    summarise(
        n = n(),
        mean_price = mean(price_gbp),
        mean_wage  = mean(hourly_wage_gbp),
        .groups = "drop"
    ) |>
    left_join(
        cappuccino_index |> select(country, published_index = index),
        by = "country"
    )

# Primary contrast: same mechanism (the index ratio), opposite ingredients
# Saudi Arabia/Qatar dropped — analytically useful during exploration, but
# the hero has converged on a clean two-pole contrast and a third mechanism
# (price-driven rather than wage-driven) dilutes rather than sharpens it.
primary_countries <- c("India", "Pakistan", "Switzerland", "Denmark")

# Winning contour set from cheap-render testing: 30 / 60 / 120
# (½ hour / 1 hour / 2 hours — a coherent affordability vocabulary,
#  cleaner than adding 15 min, which mostly sat in empty space)
iso_lines <- tibble(
    index_level = c(30, 60, 120),
    intercept   = log(index_level / 60),
    contour_label = paste0(index_level, " min")
)

## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
    palette = c(highlight = "#722F37", secondary = "gray70")
)

### |- titles and caption ----
title_text <- str_glue("Cheap coffee isn't necessarily affordable coffee")

# element_markdown() does NOT auto-wrap — a subtitle that visually fits at
# one canvas size will still clip at another unless explicitly wrapped and
# converted to <br>. This is the actual fix for the recurring clipping,
# not the canvas resize alone (that only fixed part of it last time).
subtitle_line1 <- "The **Cappuccino Index** measures the minutes a barista must work to afford a small cappuccino." |>
    str_wrap(width = 70) |>
    str_replace_all("\n", "<br>")

subtitle_line2 <- "Across these countries, barista wages vary far more than cappuccino prices." |>
    str_wrap(width = 70) |>
    str_replace_all("\n", "<br>")

subtitle_text <- str_glue("{subtitle_line1}<br>{subtitle_line2}")
# NOTE: verify the wrap doesn't split inside "**Cappuccino Index**" at your
# final font size — width=70 is a starting estimate, not calibrated yet.

# Same underlying issue as the subtitle: plot.caption also uses
# element_markdown(), which does not auto-wrap. The previous caption
# clipped because it was one long unwrapped string, not because the
# font was too large — wrap deliberately rather than shrink.
caption_source <- "James Hoffmann via Filip Reierson. Index = 60 x mean price / mean wage per country. Cross-country dispersion (log scale): barista wages vary 2.5x more than cappuccino prices." |>
    str_wrap(width = 100) |>
    str_replace_all("\n", "<br>")

caption_text <- create_social_caption(
    tt_year = 2026,
    tt_week = 36,
    source_text = caption_source
)
# NOTE: width=100 is a starting guess for the caption's smaller type size —
# not calibrated yet, same caveat as the subtitle wrap.

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
    base_theme,
    theme(
        plot.title = element_markdown(
            face = "bold", size = rel(1.6), family = fonts$title,
            margin = margin(b = 6)
        ),
        plot.subtitle = element_markdown(
            size = rel(1.05), family = fonts$text, lineheight = 1.2,
            margin = margin(b = 16)
        ),
        plot.caption = element_markdown(
            size = rel(0.7), family = fonts$text, hjust = 0
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray92", linewidth = 0.25)
    )
)

theme_set(weekly_theme)

### |- plot ----
p_hero <- driver_diag |>
    ggplot(aes(x = mean_wage, y = mean_price)) +
    
    # Iso-index contours — supporting structure, not the subject
    geom_textabline(
        data = iso_lines,
        aes(slope = 1, intercept = intercept, label = contour_label),
        color = "gray55", linetype = "dashed", linewidth = 0.35,
        size = 2.8, hjust = 0.88, text_smoothing = 0
    ) +
    
    # Contour explainer removed — direct "30 min/60 min/120 min" labels on the
    # lines plus the subtitle's definition of the index are sufficient; the
    # bottom-left was getting squeezed against the axis anyway. This opens up
    # real Ma in the lower-left rather than crowded micro-copy.
    
    # Context points
    geom_point(alpha = 0.45, color = "gray55", size = 1.8) +
    
    # Primary highlight: the core contrast
    geom_point(
        data = driver_diag |> filter(country %in% primary_countries),
        color = "#722F37", size = 3
    ) +
    geom_text_repel(
        data = driver_diag |> filter(country %in% primary_countries),
        aes(label = country),
        size = 3.4, fontface = "plain", color = "#2C2825",
        min.segment.length = 0, seed = 1234,
        box.padding = 0.5, point.padding = 0.3, force = 2, max.overlaps = Inf
    ) +
    
    # Contrast annotations — floating text, no border/fill, no leader lines.
    # Positions split the difference between the earlier "detached" pass and
    # the last "overlapping the point cloud" pass. Still a guess — verify
    # against the LOCKED CANVAS render (see seed note below), not an
    # interactive preview, since repel/label positions are device-size
    # dependent.
    annotate(
        "richtext",
        x = 0.6, y = 1.5,
        label = "**Cheap, but not affordable**<br>India \u00b7 £1.96 \u2192 172 min<br>Pakistan \u00b7 £1.87 \u2192 277 min",
        hjust = 0, size = 3.1, color = "#2C2825",
        fill = NA, label.color = NA
    ) +
    annotate(
        "richtext",
        x = 7, y = 4.3,
        label = "**Expensive, but relatively affordable**<br>Switzerland \u00b7 £5.41 \u2192 14 min<br>Denmark \u00b7 £5.41 \u2192 19 min",
        hjust = 0, size = 3.1, color = "#2C2825",
        fill = NA, label.color = NA
    ) +
    
    scale_x_log10(labels = scales::label_currency(prefix = "£")) +
    scale_y_log10(labels = scales::label_currency(prefix = "£")) +
    # clip = "off" reverted: it let the iso-index contours bleed into the
    # title/subtitle region. The actual prior clipping incidents (subtitle,
    # caption, the dropped 15-min contour) were each already solved at their
    # own layer — there was no open problem left for a blanket clip fix to
    # solve, and it introduced a worse one. Default clip = "on" is correct
    # here; contours should terminate cleanly at the panel edge.
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = "Mean hourly wage (log scale)",
        y = "Mean cappuccino price (log scale)"
    )

p_hero +
    canvas(width = 10, height = 7.5, units = "in", dpi = 300)


