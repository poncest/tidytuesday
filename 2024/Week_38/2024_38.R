
## Challenge: #TidyTuesday 2024 week 38
## Data:      Shakespeare Dialogue
## Author:    Steven Ponce
## Date:      2024-09-16


## 1. LOAD PACKAGES & SETUP ----
pacman::p_load(
  tidyverse,         # Easily Install and Load the 'Tidyverse'
  ggtext,            # Improved Text Rendering Support for 'ggplot2'
  showtext,          # Using Fonts More Easily in R Graphs
  janitor,           # Simple Tools for Examining and Cleaning Dirty Data
  skimr,             # Compact and Flexible Summaries of Data
  scales,            # Scale Functions for Visualization
  lubridate,         # Make Dealing with Dates a Little Easier
  MetBrewer,         # Color Palettes Inspired by Works at the Metropolitan Museum of Art
  MoMAColors,        # Color Palettes Inspired by Artwork at the Museum of Modern Art in New York City
  glue,              # Interpreted String Literals
  igraph,            # Network Analysis and Visualization # Network Analysis and Visualization # Network Analysis and Visualization # Network Analysis and Visualization
  ggraph,            # An Implementation of Grammar of Graphics for Graphs and Networks # An Implementation of Grammar of Graphics for Graphs and Networks # An Implementation of Grammar of Graphics for Graphs and Networks
  patchwork,         # The Composer of Plots
  NatParksPalettes   # Color Palettes Inspired by National Parks
 )  

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  10,
  height =  8,
  units  = "in",
  dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <-tidytuesdayR::tt_load(2024, week = 38) 

hamlet <- tt$hamlet |> clean_names() |> glimpse()
macbeth <- tt$macbeth |> clean_names() |> glimpse()
romeo_juliet <- tt$romeo_juliet |> clean_names() |> glimpse()

tidytuesdayR::readme(tt)
rm(tt)



## 3. EXAMINING THE DATA ----
glimpse(hamlet)
glimpse(macbeth)
glimpse(romeo_juliet)



## 4. TIDYDATA ----

# Preprocess data for character interactions
combined_plays <- bind_rows(
    hamlet |> mutate(play = "Hamlet"),
    macbeth |> mutate(play = "Macbeth"),
    romeo_juliet |> mutate(play = "Romeo and Juliet")
)

# Filter out stage directions
dialogue_data <- combined_plays |>
    filter(character != "[stage direction]") |>
    group_by(play, act, scene, character) |>
    summarize(dialogue = n(), .groups = 'drop')

# Filter out scenes with fewer than 2 characters
filtered_dialogue_data <- dialogue_data |>
    group_by(play, act, scene) |>
    filter(n() > 1) |> 
    ungroup()

# Create edges: character exchanges within the same scene
edges <- filtered_dialogue_data |>
    group_by(play, act, scene) |>
    summarise(pairs = list(combn(character, 2, simplify = FALSE)), .groups = 'drop') |>
    unnest(pairs) |>
    unnest_wider(pairs, names_sep = "_") |>
    rename(from = pairs_1, to = pairs_2) |>
    count(play, from, to) |> 
    rename(from_char = from, to_char = to)  # Rename columns to prevent conflicts

# Filter the data by each play and create separate graphs
hamlet_edges <- edges |> filter(play == "Hamlet")
macbeth_edges <- edges |> filter(play == "Macbeth")
romeo_juliet_edges <- edges |> filter(play == "Romeo and Juliet")

# Create separate igraph objects for each play
g_hamlet <- graph_from_data_frame(hamlet_edges, directed = FALSE)
g_macbeth <- graph_from_data_frame(macbeth_edges, directed = FALSE)
g_romeo_juliet <- graph_from_data_frame(romeo_juliet_edges, directed = FALSE)



# 5. VISUALIZATION ----

### |- plot aesthetics ----
bkg_col      <- colorspace::lighten('#f7f5e9', 0.05)    
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <- NatParksPalettes::natparks.pals(name = 'CraterLake', n = 3, type = "discrete")
col_palette  <- colorspace::lighten(col_palette, 0.1) 


### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 38 } &bull; Source: shakespeare.mit.edu (via github.com/nrennie/shakespeare<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Character Interaction Networks in Shakespeare's Plays")
subtitle_text <- str_glue("Visualizing character exchanges across different scenes and acts")
caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Merriweather Sans", regular.wt = 400, family = "subtitle")
font_add_google("Merriweather Sans", regular.wt = 400, family = "text")
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
showtext_auto(enable = TRUE)

### |-  plot theme ----
theme_set(theme_void(base_size = 14, base_family = "text"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
    strip.text            = element_textbox(size     = rel(1),
                                            face     = 'bold',
                                            color    = text_col,
                                            hjust    = 0.5,
                                            halign   = 0.5,
                                            r        = unit(3, "pt"),
                                            width    = unit(6, "npc"),
                                            padding  = margin(2, 0, 2, 0),
                                            margin   = margin(3, 3, 3, 3),
                                            fill     = "transparent"),
    panel.spacing         = unit(1, 'lines')
)  

### |-  plot function ----
plot_character_network <- function(play_name, edges_data, node_color, edge_color) {
    
    # Create igraph object for the play
    g_play <- graph_from_data_frame(edges_data, directed = FALSE)
    
    # Network plot
    plot <- ggraph(g_play, layout = 'fr') +
        geom_edge_link(aes(edge_alpha = n, edge_width = n), color = edge_color, show.legend = FALSE) +  # Set edge color with alpha and width
        geom_node_point(size = 5, color = node_color) +  # Set node color
        geom_node_text(aes(label = name), color = text_col, repel = TRUE, check_overlap = TRUE) +
        scale_edge_width(range = c(0.5, 2.5)) +
        theme_void() +
        ggtitle(play_name) +  # Add top-center title for the play
        theme(
            plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5)  # Centered title with bold font
        )
    
    return(plot)
} 

### |-  individual plots ----
hamlet_plot       <- plot_character_network("Hamlet", hamlet_edges, node_color = col_palette[1], edge_color = "gray60")
macbeth_plot      <- plot_character_network("Macbeth", macbeth_edges, node_color = col_palette[2], edge_color = "gray60")
romeo_juliet_plot <- plot_character_network("Romeo and Juliet", romeo_juliet_edges, col_palette[3], edge_color = "gray60")

### |-  Combine plots using patchwork ----
combined_plot <- hamlet_plot + macbeth_plot + romeo_juliet_plot + 
    patchwork::plot_layout(ncol = 3)

### |-  final plot ----  
final_plot <- combined_plot + 
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        
        # Theme
        theme = theme(
            plot.title      = element_text(
                size          = rel(1.9),   
                hjust         = 0.5,
                family        = "title",
                face          = "bold",
                color         = title_col,
                lineheight    = 1.1,
                margin        = margin(t = 5, b = 5)
            ),
            plot.subtitle     = element_text(
                size          = rel(1.1), 
                hjust         = 0.5,
                family        = 'subtitle',
                color         = subtitle_col,
                lineheight    = 1.1, 
                margin        = margin(t = 5, b = 15)
            ),
            plot.caption      = element_markdown(
                size          = rel(.50),
                family        = "caption",
                color         = caption_col,
                lineheight    = 1.1,
                hjust         = 0.5,
                halign        = 0.5,
                margin        = margin(t = 5, b = 5)
            ),
        )
    )

# Display the combined plot
print(final_plot)  



# 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────────────────────────────────
#  setting  value
#  version  R version 4.4.1 (2024-06-14 ucrt)
#  os       Windows 10 x64 (build 19045)
#  system   x86_64, mingw32
#  ui       RStudio
#  language (EN)
#  collate  English_United States.utf8
#  ctype    English_United States.utf8
#  tz       America/New_York
#  date     2024-09-16
#  rstudio  2024.04.2+764 Chocolate Cosmos (desktop)
#  pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────
#  ! package      * version    date (UTC) lib source
#  V base         * 4.4.1      2024-04-24 [2] local (on disk 4.4.0)
#  P base64enc      0.1-3      2015-07-28 [?] CRAN (R 4.4.0)
#  P bit            4.0.5      2022-11-15 [?] CRAN (R 4.4.0)
#  P bit64          4.0.5      2020-08-30 [?] CRAN (R 4.4.0)
#  P cachem         1.0.8      2023-05-01 [?] CRAN (R 4.4.0)
#  P camcorder      0.1.0      2022-10-03 [?] CRAN (R 4.4.0)
#  P cellranger     1.1.0      2016-07-27 [?] CRAN (R 4.4.0)
#    cli            3.6.3      2024-06-21 [1] CRAN (R 4.4.1)
#    colorspace     2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
#  P commonmark     1.9.1      2024-01-30 [?] CRAN (R 4.4.0)
#  P compiler       4.4.0      2024-04-24 [?] local
#    cowplot        1.1.3      2024-01-22 [1] CRAN (R 4.4.0)
#  P crayon         1.5.2      2022-09-29 [?] CRAN (R 4.4.0)
#  P curl           5.2.1      2024-03-01 [?] CRAN (R 4.4.0)
#  P datasets     * 4.4.0      2024-04-24 [?] local
#  P digest         0.6.35     2024-03-11 [?] CRAN (R 4.4.0)
#    dplyr        * 1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
#  P fansi          1.0.6      2023-12-08 [?] CRAN (R 4.4.0)
#    farver         2.1.2      2024-05-13 [1] CRAN (R 4.4.1)
#  P fastmap        1.1.1      2023-02-24 [?] CRAN (R 4.4.0)
#    forcats      * 1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
#  P fs             1.6.4      2024-04-25 [?] CRAN (R 4.4.0)
#    generics       0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
#  P ggforce        0.4.2      2024-02-19 [?] CRAN (R 4.4.0)
#    ggplot2      * 3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
#  P ggraph       * 2.2.1      2024-03-07 [?] CRAN (R 4.4.0)
#  P ggrepel        0.9.5      2024-01-10 [?] CRAN (R 4.4.0)
#    ggstream       0.1.0      2021-05-06 [1] CRAN (R 4.4.0)
#  P ggtext       * 0.1.2      2022-09-16 [?] CRAN (R 4.4.0)
#  P gifski         1.12.0-2   2023-08-12 [?] CRAN (R 4.4.0)
#  P glue         * 1.7.0      2024-01-09 [?] CRAN (R 4.4.0)
#  P graphics     * 4.4.0      2024-04-24 [?] local
#  P graphlayouts   1.1.1      2024-03-09 [?] CRAN (R 4.4.0)
#  P grDevices    * 4.4.0      2024-04-24 [?] local
#  P grid           4.4.0      2024-04-24 [?] local
#  P gridExtra      2.3        2017-09-09 [?] CRAN (R 4.4.0)
#  P gridtext       0.1.5      2022-09-16 [?] CRAN (R 4.4.0)
#    gtable         0.3.5      2024-04-22 [1] CRAN (R 4.4.0)
#  P here           1.0.1      2020-12-13 [?] CRAN (R 4.4.0)
#  P hms            1.1.3      2023-03-21 [?] CRAN (R 4.4.0)
#  P htmltools      0.5.8.1    2024-04-04 [?] CRAN (R 4.4.0)
#  P httr           1.4.7      2023-08-15 [?] CRAN (R 4.4.0)
#  P igraph       * 2.0.3      2024-03-13 [?] CRAN (R 4.4.0)
#  P janitor      * 2.2.0      2023-02-02 [?] CRAN (R 4.4.0)
#  P jsonlite       1.8.8      2023-12-04 [?] CRAN (R 4.4.0)
#  P knitr          1.46       2024-04-06 [?] CRAN (R 4.4.0)
#    labeling       0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
#  P lifecycle      1.0.4      2023-11-07 [?] CRAN (R 4.4.0)
#  P lubridate    * 1.9.3      2023-09-27 [?] CRAN (R 4.4.0)
#    magick         2.8.3      2024-02-18 [1] CRAN (R 4.4.0)
#  P magrittr       2.0.3      2022-03-30 [?] CRAN (R 4.4.0)
#  P markdown       1.12       2023-12-06 [?] CRAN (R 4.4.0)
#  P MASS           7.3-60.2   2024-04-24 [?] local
#  P memoise        2.0.1      2021-11-26 [?] CRAN (R 4.4.0)
#  P MetBrewer    * 0.2.0      2022-03-21 [?] CRAN (R 4.4.0)
#  P methods      * 4.4.0      2024-04-24 [?] local
#    MoMAColors   * 0.0.0.9000 2024-05-02 [1] Github (BlakeRMills/MoMAColors@6f5d75d)
#    munsell        0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
#  P pacman         0.5.1      2019-03-11 [?] CRAN (R 4.4.0)
#  P parallel       4.4.0      2024-04-24 [?] local
#  P patchwork    * 1.2.0      2024-01-08 [?] CRAN (R 4.4.0)
#  P pillar         1.9.0      2023-03-22 [?] CRAN (R 4.4.0)
#  P pkgconfig      2.0.3      2019-09-22 [?] CRAN (R 4.4.0)
#  P polyclip       1.10-6     2023-09-27 [?] CRAN (R 4.4.0)
#  P purrr        * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
#  P R6             2.5.1      2021-08-19 [?] CRAN (R 4.4.0)
#  P ragg           1.3.0      2024-03-13 [?] CRAN (R 4.4.0)
#  P Rcpp           1.0.12     2024-01-09 [?] CRAN (R 4.4.0)
#  P readr        * 2.1.5      2024-01-10 [?] CRAN (R 4.4.0)
#  P readxl         1.4.3      2023-07-06 [?] CRAN (R 4.4.0)
#  P renv           1.0.7      2024-04-11 [?] CRAN (R 4.4.0)
#  P repr           1.1.7      2024-03-22 [?] CRAN (R 4.4.0)
#    rlang          1.1.4      2024-06-04 [1] CRAN (R 4.4.1)
#  P rprojroot      2.0.4      2023-11-05 [?] CRAN (R 4.4.0)
#  P rstudioapi     0.16.0     2024-03-24 [?] CRAN (R 4.4.0)
#  P rsvg           2.6.0      2023-10-08 [?] CRAN (R 4.4.0)
#  P rvest          1.0.4      2024-02-12 [?] CRAN (R 4.4.0)
#    scales       * 1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
#  P selectr        0.4-2      2019-11-20 [?] CRAN (R 4.4.0)
#  P sessioninfo    1.2.2      2021-12-06 [?] CRAN (R 4.4.0)
#  P showtext     * 0.9-7      2024-03-02 [?] CRAN (R 4.4.0)
#  P showtextdb   * 3.0        2020-06-04 [?] CRAN (R 4.4.0)
#  P skimr        * 2.1.5      2022-12-23 [?] CRAN (R 4.4.0)
#  P snakecase      0.11.1     2023-08-27 [?] CRAN (R 4.4.0)
#  P stats        * 4.4.0      2024-04-24 [?] local
#    stringi        1.8.4      2024-05-06 [1] CRAN (R 4.4.0)
#  P stringr      * 1.5.1      2023-11-14 [?] CRAN (R 4.4.0)
#  P svglite        2.1.3      2023-12-08 [?] CRAN (R 4.4.0)
#  P sysfonts     * 0.8.9      2024-03-02 [?] CRAN (R 4.4.0)
#    systemfonts    1.1.0      2024-05-15 [1] CRAN (R 4.4.0)
#    textshaping    0.4.0      2024-05-24 [1] CRAN (R 4.4.0)
#  P tibble       * 3.2.1      2023-03-20 [?] CRAN (R 4.4.0)
#  P tidygraph      1.3.1      2024-01-30 [?] CRAN (R 4.4.0)
#    tidyr        * 1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
#    tidyselect     1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
#  P tidytuesdayR   1.0.3      2023-12-13 [?] CRAN (R 4.4.0)
#  P tidyverse    * 2.0.0      2023-02-22 [?] CRAN (R 4.4.0)
#  P timechange     0.3.0      2024-01-18 [?] CRAN (R 4.4.0)
#  P tools          4.4.0      2024-04-24 [?] local
#  P tweenr         2.0.3      2024-02-26 [?] CRAN (R 4.4.0)
#  P tzdb           0.4.0      2023-05-12 [?] CRAN (R 4.4.0)
#  P usethis        2.2.3      2024-02-19 [?] CRAN (R 4.4.0)
#  P utf8           1.2.4      2023-10-22 [?] CRAN (R 4.4.0)
#  P utils        * 4.4.0      2024-04-24 [?] local
#  P vctrs          0.6.5      2023-12-01 [?] CRAN (R 4.4.0)
#  P viridis        0.6.5      2024-01-29 [?] CRAN (R 4.4.0)
#    viridisLite    0.4.2      2023-05-02 [1] CRAN (R 4.4.0)
#  P vroom          1.6.5      2023-12-05 [?] CRAN (R 4.4.0)
#    withr          3.0.1      2024-07-31 [1] CRAN (R 4.4.1)
#  P xfun           0.43       2024-03-25 [?] CRAN (R 4.4.0)
#  P xml2           1.3.6      2023-12-04 [?] CRAN (R 4.4.0)
# 
#  [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/windows/R-4.4/x86_64-w64-mingw32
#  [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/d6ee0ff8
# 
#  V ── Loaded and on-disk version mismatch.
#  P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────────────
# > 
# 


