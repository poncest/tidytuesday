
## Challenge: #TidyTuesday 2025 week 17
## Data:      useR! 2025 program
## Author:    Steven Ponce
## Date:      2025-04-29  


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,      # Easily Install and Load the 'Tidyverse'
    ggtext,         # Improved Text Rendering Support for 'ggplot2'
    showtext,       # Using Fonts More Easily in R Graphs
    janitor,        # Simple Tools for Examining and Cleaning Dirty Data
    skimr,          # Compact and Flexible Summaries of Data
    scales,         # Scale Functions for Visualization
    glue,           # Interpreted String Literals
    here,           # A Simpler Way to Find Your Files
    igraph,         # Network Analysis and Visualization
    ggrepel,        # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
    camcorder       # Record Your Plot History 
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  8,
    height =  8,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 17) 

user2025_raw <- tt$user2025 |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(user2025_raw)
skim(user2025_raw)


## 4. TIDYDATA ----

### |-  tidy data ----      
user2025 <- user2025_raw |>
    mutate(
        keywords_list = str_split(keywords, ", "),
        co_authors = replace_na(co_authors, "")
    )

# Define Topic Mapping 
topic_definitions <- tribble(
    ~pattern, ~topic,
    "visualization|ggplot2|shiny|dashboard|interactive", "Visualization",
    "machine learning|deep learning|statistics|modeling|neural", "Machine Learning",
    "data wrangling|tidyverse|data.table|cleaning|manipulation", "Data Wrangling",
    "reproducibility|rmarkdown|workflow|version control|git", "Reproducibility",
    "package|cran|bioconductor|github", "Package Development"
)

# Expand to keyword-topic map
keyword_topics <- map_dfr(seq_len(nrow(topic_definitions)), function(i) {
    pattern <- topic_definitions$pattern[i]
    topic <- topic_definitions$topic[i]
    matched <- unique(unlist(user2025$keywords_list))[str_detect(unique(unlist(user2025$keywords_list)), regex(pattern, ignore_case = TRUE))]
    tibble(keyword = matched, topic = topic)
    }) |> distinct()

# Create Edges ----
speaker_topic_edges <- user2025 |>
    unnest(keywords_list) |>
    inner_join(keyword_topics, by = c("keywords_list" = "keyword")) |>
    select(speakers, topic) |>
    distinct()

# Build Graph ----
g <- graph_from_data_frame(speaker_topic_edges |> rename(from = speakers, to = topic), directed = FALSE)

# Set node attributes
V(g)$type <- ifelse(V(g)$name %in% speaker_topic_edges$speakers, "speaker", "topic")
V(g)$label <- ifelse(V(g)$type == "speaker", str_trunc(str_extract(V(g)$name, "^[^(]+"), 25), V(g)$name)
deg <- degree(g)
V(g)$size <- ifelse(V(g)$type == "speaker", rescale(deg, to = c(3.5, 6)), 10)
V(g)$color <- ifelse(V(g)$type == "speaker", "#F4978E", "#99C1DE")
V(g)$shape <- 21

# Compute Layout ----
set.seed(42)
layout_coords <- layout_with_fr(g) + matrix(rnorm(length(V(g)) * 2, sd = 0.15), ncol = 2)

# Build Plot Data ----
nodes_df <- data.frame(
    name = V(g)$name,
    type = V(g)$type,
    color = V(g)$color,
    shape = V(g)$shape,
    size = V(g)$size,
    label = V(g)$label,
    x = layout_coords[, 1],
    y = layout_coords[, 2]
)

edges_df <- as_data_frame(g) |>
    left_join(nodes_df |> select(name, x_from = x, y_from = y), by = c("from" = "name")) |>
    left_join(nodes_df |> select(name, x_to = x, y_to = y), by = c("to" = "name"))

# Label high-degree speakers only
high_deg_names <- names(deg[deg >= 2 & V(g)$type == "speaker"])
speaker_labels_df <- nodes_df |> filter(name %in% high_deg_names)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
    palette = c(
        "speaker" = "#F4978E", "topic" = "#99C1DE"
    )
)

### |-  titles and caption ----
title_text <- str_glue("Topic-Speaker Bipartite Network for useR! 2025 Conference")
subtitle_text <- str_glue("Connections between speakers and topics")

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 17,
    source_text =  "Program for the useR! 2025 conference" 
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
        plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.14), margin = margin(b = 10)),
        plot.subtitle = element_text(family = fonts$subtitle, color = colors$text, size = rel(0.78), margin = margin(b = 20)),
        
        # Axis elements
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        # Grid elements
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(),
        
        # Legend elements
        legend.position = "plot",
        legend.direction = "horizontal",
        legend.title = element_text(family = fonts$text, size = rel(0.8), face = "bold"),
        legend.text = element_text(family = fonts$text, size = rel(0.7)),
        
         # Plot margins 
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    )
)

# Set theme
theme_set(weekly_theme)

### |-  plot  ----
ggplot() +
    # Geoms
    geom_segment(
        data = edges_df, aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
        color = "#CCCCCC", alpha = 0.5, size = 0.3
        ) +
    geom_point(
        data = nodes_df,
        aes(x = x, y = y, fill = type, size = size),
        shape = 21, stroke = 0.4, color = "#333333", show.legend = FALSE
        ) +
    geom_text(
        data = nodes_df |> filter(type == "topic"),
        aes(x = x, y = y, label = label),
        color = "#1A1A1A", size = 5, fontface = "bold"
        ) +
    geom_text_repel(
        data = speaker_labels_df,
        aes(x = x, y = y, label = label),
        color = "#333333", size = 3.2,
        max.overlaps = 20, point.padding = 0.3, box.padding = 0.4,
        segment.color = "#AAAAAA", segment.alpha = 0.6,
        force = 0.5, seed = 42
        ) +
    # Scales
    scale_fill_manual(values = colors$palette) +
    scale_size_identity() +
    coord_equal(clip = "off") +
    # Labs
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = NULL,
        y = NULL,
    ) +
    # Theme
    theme(
        plot.title = element_text(
            size = rel(1.4),
            family = fonts$title,
            face = "bold",
            color = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.subtitle = element_text(
            size = rel(0.85),
            family = fonts$subtitle,
            color = alpha(colors$subtitle, 0.9),
            lineheight = 1.2,
            margin = margin(t = 5, b = 10)
        ),
        plot.caption = element_markdown(
            size = rel(0.65),
            family = fonts$caption,
            color = colors$caption,
            hjust = 0.5,
            margin = margin(t = 10)
        )
    ) 


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-29
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder    * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
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
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggrepel      * 0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
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
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P httr2          1.0.1    2024-04-01 [?] CRAN (R 4.4.0)
# P igraph       * 2.0.3    2024-03-13 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.50     2025-03-16 [?] RSPM
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
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
# P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
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
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
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
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────
# > 