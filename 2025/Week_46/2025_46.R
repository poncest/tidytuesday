## Challenge: #TidyTuesday 2025 week 46
## Data:      The Complete Sherlock Holmes
## Author:    Steven Ponce
## Date:      2025-11-16

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
    tidygraph,     # A Tidy API for Graph Manipulation
    igraph,        # Network Analysis and Visualization
    ggraph,        # An Implementation of Grammar of Graphics for Graphs and Networks
    ggrepel,       # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
    tidytext       # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 46)

holmes <- tt$holmes |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(holmes)
skimr::skim(holmes) |> summary()


## 4. TIDY DATA ----
holmes_clean <- holmes |>
  filter(!is.na(text), nchar(text) > 5) |>
  filter(
    !str_detect(text, "^[A-Z ]+$"),
    !str_detect(text, "^(Table of|CHAPTER|Part [IVX]+|\\d+$)")
  ) |>
  mutate(
    has_quotes = str_detect(text, '"'),
    speaker = case_when(
      str_detect(text, "(?i)said Holmes|Holmes said|Holmes replied|Holmes answered|Holmes asked") ~ "Holmes",
      str_detect(text, "(?i)said I|I said|I replied|I answered|I asked") ~ "Watson",
      str_detect(text, "(?i)he said|she said|he replied|she replied") ~ "Other",
      has_quotes ~ "Unknown",
      TRUE ~ "Narrative"
    ),
    dialogue_text = str_extract_all(text, '"([^"]*)"') |>
      map_chr(~ {
        if (length(.x) > 0) {
          str_remove_all(.x, '"') |>
            str_trim() |>
            str_c(collapse = " ")
        } else {
          NA_character_
        }
      })
  )

# Word counts & TF–IDF
book_words <- holmes_clean |>
  filter(speaker %in% c("Holmes", "Watson")) |>
  filter(!is.na(dialogue_text)) |>
  select(book, dialogue_text) |>
  unnest_tokens(word, dialogue_text) |>
  anti_join(stop_words, by = "word") |>
  filter(nchar(word) >= 4) |>
  count(book, word, sort = TRUE)

book_tfidf <- book_words |>
  bind_tf_idf(word, book, n) |>
  arrange(desc(tf_idf))

top_books <- book_words |>
  count(book, sort = TRUE) |>
  slice_head(n = 15) |>
  pull(book)

distinctive_words <- book_tfidf |>
  filter(book %in% top_books) |>
  group_by(book) |>
  slice_max(tf_idf, n = 5) |>
  ungroup()

edges <- distinctive_words |>
  select(from = word, to = book, weight = tf_idf)

graph <- graph_from_data_frame(edges, directed = FALSE)
V(graph)$degree <- degree(graph)

tbl_graph <- as_tbl_graph(graph)

# Short book labels and node attributes
book_labels <- tibble(book = unique(distinctive_words$book)) |>
  mutate(
    book_label = book |>
      str_remove("^The Adventure of the ") |>
      str_remove("^The Adventure of ") |>
      str_remove("^The ") |>
      str_remove("^A ")
  )

tbl_graph <- tbl_graph |>
  activate(nodes) |>
  left_join(book_labels, by = c("name" = "book")) |>
  mutate(
    node_type = if_else(name %in% top_books, "Book", "Word"),
    size_metric = if_else(node_type == "Book", degree, 1),
    node_type_fct = factor(node_type, levels = c("Book", "Word")),
    display_label = if_else(node_type == "Book", book_label, NA_character_),
    # Highlight Sign of the Four cluster
    is_sign_cluster = name == "The Sign of the Four" |
      str_detect(name, "Sign of the Four")
  )

# Mark edges connected to "Sign of the Four"
tbl_graph <- tbl_graph |>
  activate(edges) |>
  mutate(
    from_node = .N()$name[from],
    to_node = .N()$name[to],
    is_sign_edge = from_node == "The Sign of the Four" |
      to_node == "The Sign of the Four"
  )

# Create layout
set.seed(221)
layout_tbl <- ggraph::create_layout(tbl_graph, layout = "fr")

# Position for "Sign of the Four"
sign_pos <- layout_tbl |>
  as_tibble() |>
  filter(node_type == "Book", str_detect(name, "Sign of the Four")) |>
  slice(1)

annotation_df <- tibble(
  x = sign_pos$x + 0.5,
  y = sign_pos$y + 3,
  xend = sign_pos$x,
  yend = sign_pos$y,
  label = "'Sign of the Four' features<br>the most distinctive vocabulary."
)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = list(
        book_col = "#0f9a8a",      
        word_col = "#b5beca",      
        highlight_col = "#9b59b6"  
    )
)

### |- titles and caption ----
title_text <- str_glue("The Sherlock Holmes Canon<br>Thematic Word Networks")

subtitle_text <- str_glue(
    "Each of the 15 most-discussed stories is linked to its five most distinctive dialogue words.<br>",
    "Cluster size reflects vocabulary uniqueness. Purple cluster highlights Sign of the Four's distinctive vocabulary."
)

caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 46,
    source_text = "{ sherlock R package }"
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
      color = colors$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_text(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.9), margin = margin(b = 20), hjust = 0
    ),

    ## Grid
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),

    # Axes
    axis.title = element_text(size = rel(0.8), color = "gray30"),
    axis.text = element_text(color = "gray30"),
    axis.text.y = element_text(size = rel(0.85)),
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

### |-  main plot ----
ggraph(layout_tbl) +

  # Geoms
  geom_edge_link(
    aes(
      color = is_sign_edge,
      alpha = is_sign_edge,
      # width = is_sign_edge
    )
  ) +
  scale_edge_color_manual(
    values = c("TRUE" = colors$palette$highlight_col, "FALSE" = "grey70"),
    guide = "none"
  ) +
  scale_edge_alpha_manual(
    values = c("TRUE" = 0.28, "FALSE" = 0.25),
    guide = "none"
  ) +
  scale_edge_width_manual(
    values = c("TRUE" = 0.28, "FALSE" = 0.30),
    guide = "none"
  ) +
  geom_node_point(
    data = ~ filter(., node_type == "Book"),
    aes(size = size_metric * 1.35),
    color = if_else(
      layout_tbl |> filter(node_type == "Book") |> pull(is_sign_cluster),
      colors$palette$highlight_col,
      colors$palette$book_col
    ),
    alpha = 0.25,
    show.legend = FALSE
  ) +
  geom_node_point(
    aes(size = size_metric),
    shape = 21,
    fill = case_when(
      layout_tbl$is_sign_cluster & layout_tbl$node_type == "Book" ~ colors$palette$highlight_col, # Purple Sign book
      layout_tbl$node_type == "Book" ~ colors$palette$book_col,
      layout_tbl$is_sign_cluster & layout_tbl$node_type == "Word" ~ colors$palette$highlight_col, # Purple Sign words
      TRUE ~ colors$palette$word_col
    ),
    color = if_else(
      layout_tbl$is_sign_cluster,
      colors$palette$highlight_col,
      colors$background
    ),
    stroke = if_else(layout_tbl$is_sign_cluster, 0.8, 0.30),
    alpha = 0.70,
    show.legend = FALSE
  ) +
  geom_node_text(
    data = ~ filter(., node_type == "Book"),
    aes(label = display_label),
    repel = TRUE,
    size = 3.0,
    fontface = "bold",
    family = "text",
    color = "grey10",
    box.padding = unit(0.3, "lines"),
    point.padding = unit(0.3, "lines"),
    segment.size = 0.25,
    segment.color = "grey65"
  ) +
  geom_richtext(
    data = annotation_df,
    aes(x = x, y = y, label = label),
    family = "text",
    size = 3.0,
    color = "grey20",
    fill = alpha(colors$background, 0.5),
    label.colour = NA,
    lineheight = 1.05,
    label.padding = unit(0.1, "lines")
  ) +
  # Scales
  scale_size_continuous(range = c(2.4, 10), guide = "none") +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.85),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.15,
      margin = margin(t = 8, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.85),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.88),
      lineheight = 1.2,
      margin = margin(t = 2, b = 15)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
      family = "Arial",
      color = colors$caption,
      hjust = 0,
      lineheight = 1.3,
      margin = margin(t = 12, b = 5)
    ),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
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

# ─ Session info ────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-11-15
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P cachem         1.0.8    2023-05-01 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# P colorspace     2.1-1    2024-07-26 [?] CRAN (R 4.4.1)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# P digest         0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P evaluate       0.23     2023-11-01 [?] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# P ggforce        0.4.2    2024-02-19 [?] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggraph       * 2.2.1    2024-03-07 [?] CRAN (R 4.4.0)
# P ggrepel      * 0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gh             1.4.1    2024-03-28 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P gitcreds       0.1.2    2022-09-08 [?] CRAN (R 4.4.0)
# glue           1.8.0    2024-09-30 [1] CRAN (R 4.4.2)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P graphlayouts   1.2.2    2025-01-23 [?] CRAN (R 4.4.3)
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridExtra      2.3      2017-09-09 [?] CRAN (R 4.4.0)
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here         * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P httr2          1.0.1    2024-04-01 [?] CRAN (R 4.4.0)
# P igraph       * 2.0.3    2024-03-13 [?] CRAN (R 4.4.0)
# P janeaustenr    1.0.0    2022-08-26 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.50     2025-03-16 [?] RSPM
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lattice        0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P MASS           7.3-60.2 2024-04-24 [?] local
# P Matrix         1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P memoise        2.0.1    2021-11-26 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P polyclip       1.10-6   2023-09-27 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.3    2024-09-11 [?] CRAN (R 4.4.2)
# P rappdirs       0.3.3    2021-01-31 [?] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr          2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P SnowballC      0.7.1    2023-04-25 [?] CRAN (R 4.4.0)
# P stats        * 4.4.0    2024-04-24 [?] local
# stringi        1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite        2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts    1.1.0    2024-05-15 [1] CRAN (R 4.4.0)
# textshaping    0.4.0    2024-05-24 [1] CRAN (R 4.4.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# P tidygraph    * 1.3.1    2024-01-30 [?] CRAN (R 4.4.0)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidytext     * 0.4.2    2024-04-10 [?] CRAN (R 4.4.0)
# P tidytuesdayR   1.2.1    2025-04-29 [?] CRAN (R 4.4.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.4.3)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tokenizers     0.3.0    2022-12-22 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tweenr         2.0.3    2024-02-26 [?] CRAN (R 4.4.0)
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P viridis        0.6.5    2024-01-29 [?] CRAN (R 4.4.0)
# viridisLite    0.4.2    2023-05-02 [1] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr          3.0.2    2024-10-28 [?] CRAN (R 4.4.2)
# P xfun           0.52     2025-04-02 [?] CRAN (R 4.4.3)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────
# > 