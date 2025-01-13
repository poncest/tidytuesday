
## Challenge: #TidyTuesday 2025 week 02
## Data:      posit::conf talks
## Author:    Steven Ponce
## Date:      2025-01-13


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
    camcorder,      # Record Your Plot History 
    tidytext,       # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools 
    ggraph,         # An Implementation of Grammar of Graphics for Graphs and Networks 
    igraph,         # Network Analysis and Visualization 
    tidygraph       # A Tidy API for Graph Manipulation
    #withr           # Run Code 'With' Temporarily Modified Global State
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  14,
    height =  10,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))



## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 02) 

conf2023 <- tt$conf2023 |> clean_names()
conf2024 <- tt$conf2024 |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(conf2023)
skim(conf2023)

glimpse(conf2024)
skim(conf2024)


## 4. TIDYDATA ----

### |-  tidy ----

# Prepare 2023 data
conf2023_clean <- conf2023 |>
    select(
        speaker_name,
        title = session_title,
        description = session_abstract,
        track = block_track_title,
        session_type,
        speaker_affiliation,
        session_date,
        session_start,
        session_length
    ) |>
    mutate(
        year = 2023,
        has_video = FALSE
    )

# Prepare 2024 data
conf2024_clean <- conf2024 |>
    select(
        speaker_name,
        title = talk_title,
        description,
        track,
        yt_url
    ) |>
    mutate(
        year = 2024,
        has_video = TRUE,
        session_type = case_when(
            str_to_lower(track) == "keynote" ~ "keynote",
            TRUE ~ "regular"
        ),
        speaker_affiliation = NA_character_,
        session_date = NA,
        session_start = NA,
        session_length = NA
    )

# Combine datasets
conf_combined <- bind_rows(conf2023_clean, conf2024_clean) 

### |-  plot data ----

# Create topic similarity network
title_similarity <- conf_combined |>
    # Split titles into individual words
    unnest_tokens(word, title) |>
    
    # Remove common stop words (e.g., "the", "and", "in")
    anti_join(stop_words) |>
    
    # Clean up words:
    # Remove numbers and single-letter words
    filter(!str_detect(word, "^[0-9]+$"),
           str_length(word) > 1) |>
    
    # Count word occurrences per track
    count(track, word) |>
    
    # Focus on meaningful patterns:
    group_by(track) |>
    # Keep words that appear at least twice
    filter(n >= 2) |>
    # Take top 8 most frequent words per track
    slice_max(n, n = 8) |>
    ungroup()

# Create network edges by finding pairs of tracks that share common words
edges <- title_similarity |>
    # Group by each unique word
    group_by(word) |>
    
    # Keep only words that appear in more than one track
    filter(n() > 1) |>
    
    # For each word group, create pairs of tracks that share the word
    summarize(
        combinations = list(data.frame(
            # First track in each pair (taking first row of combinations)
            X1 = combn(track, 2)[1,],
            # Second track in each pair (taking second row of combinations) 
            X2 = combn(track, 2)[2,]
        ))
    ) |>
    
    # Convert the list of combinations into rows
    unnest(combinations) |>
    
    # Count how many words each pair of tracks have in common
    # This creates the 'weight' of the connection between tracks
    count(X1, X2, name = "weight")

# Create network nodes
nodes <- tibble(
    name = unique(c(edges$X1, edges$X2)),
    type = "track"
)

# Create network graph using tidygraph
graph <- tbl_graph(
    nodes = nodes,
    edges = edges,
    directed = FALSE
) |>
    # Add degree centrality using mutate
    mutate(
        degree = centrality_degree()
    )



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c("gray50", "#297ACC"))

### |-  titles and caption ----
title_text <- str_glue("Track Connections at posit::conf (2023-2024)")

subtitle_text <- str_glue(
    "__Kamada–Kawai__ Forced-Directed Network analysis showing how conference tracks are related<br>
    through shared words in talk titles.<br><br>
    __Node size__ corresponds to __node degree__ (the number of connections to other tracks),<br>
    __edge thickness__ shows the number of shared words between tracks."
)

# Create caption
caption_text <- create_social_caption(
    tt_year = 2025,
    tt_week = 02,
    source_text = "posit::conf attendee portal 2023-2024"
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
        # Weekly-specific modifications
        legend.box = "vertical",          # Stack legends vertically
        # legend.position = "top",
        legend.position      = c(0.95, 1.28),    # x=1, y=1 puts it in the upper-right
        legend.justification = c(1, 1),          # Anchor the legend’s top-right corner
        legend.box.margin  = margin(b = 15),
        legend.spacing     = unit(0.2, "cm"),
        legend.box.spacing = unit(0.2, "cm"),
        legend.key.size    = unit(0.8, "lines"),
        legend.text        = element_text(size = 9),
        legend.title       = element_text(size = 10, face = "bold"),
        panel.grid.major   = element_blank(),
        panel.grid.minor   = element_blank()
    )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----

# Set seed for reproducibility
set.seed(123)  

# Create layout using tidygraph functions
graph_laid_out <- graph |>
    activate(nodes) |>
    create_layout(layout = "kk")

# ggraph call
ggraph(graph_laid_out) +
    # Geom
    geom_edge_link(
        aes(edge_alpha = weight, 
            edge_width = weight),
        edge_color = colors$palette[1],
        show.legend = TRUE,
        edge_linetype = "solid",
        alpha = 0.5,    
        lineend = "round"
    ) +
    geom_node_point(
        aes(size = degree),
        color = colors$palette[2],
        alpha = 0.8
    ) +
    geom_node_label(
        aes(label = str_wrap(name, 20)),
        repel        = TRUE,
        fill = alpha("white", 0.8),  
        label.size   = 0,     # remove border
        label.padding = unit(0.15, "lines"),
        size         = 3.0,
        family       = fonts$text,
        fontface     = "bold",
        color        = colors$text,
        # Additional ggrepel arguments:
        box.padding  = 0.4,        # Increase if labels overlap too much
        point.padding = 0.3,       # Space between node and label
        force        = 1.0,        # Higher = stronger repel
        force_pull   = 0.1,        # Pull label toward or away from point
        max.overlaps = Inf
    ) +
    # Scales
    scale_edge_width(range = c(0.5, 2.5)) +
    scale_size(range = c(3, 8)) +
    scale_edge_alpha(range = c(0.2, 0.8)) +
    
    # Labs
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        edge_alpha = "Shared Words",   
        edge_width = "Connection Strength",   
        size = "Node Degree" 
    ) +
    # Combine similar legends
    guides(
        edge_alpha = guide_legend(
            title = "Connection Strength",
            override.aes = list(alpha = 0.6)
        ),
        edge_width = "none",  # Hide duplicate legend
        size = guide_legend(
            title = "Node Degree",
            override.aes = list(alpha = 0.8)
        ) 
    ) +
    # Theme
    theme(
        plot.title = element_text(
            size   = rel(2.6),
            family = fonts$title,
            face   = "bold",
            color  = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.subtitle = element_markdown(
            size   = rel(1.1),
            family = fonts$subtitle,
            color  = colors$subtitle,
            lineheight = 1.2,
            margin = margin(t = 5, b = 15)
        ),
        plot.caption = element_markdown(
            size   = rel(0.65),
            family = fonts$caption,
            color  = colors$caption,
            hjust  = 0.5,
            margin = margin(t = 10)
        )
    )  


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-01-13
# rstudio  2024.12.0+467 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P cachem         1.0.8    2023-05-01 [?] CRAN (R 4.4.0)
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
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# P ggforce        0.4.2    2024-02-19 [?] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggraph       * 2.2.1    2024-03-07 [?] CRAN (R 4.4.0)
# P ggrepel        0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gh             1.4.1    2024-03-28 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P gitcreds       0.1.2    2022-09-08 [?] CRAN (R 4.4.0)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.4.2)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P graphlayouts   1.1.1    2024-03-09 [?] CRAN (R 4.4.0)
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
# P knitr          1.46     2024-04-06 [?] CRAN (R 4.4.0)
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
# P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
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
# P tidytuesdayR   1.1.2    2024-09-09 [?] CRAN (R 4.4.2)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
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
# P xfun           0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────────
# > 