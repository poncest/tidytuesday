
## Challenge: #TidyTuesday 2024 week 09
## Data:      Leap Day
## Author:    Steven Ponce
## Date:      2024-02-26

#' Reference: Text Mining with R: A Tidy Approach
#' https://www.tidytextmining.com/


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
pacman::p_load(tidytext, widyr, ggraph, igraph)                                        


### |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 7,
    height = 6,
    units  = "in",
    dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(x = base::as.double("2024"), 
                            week = base::as.double("09")) 

events <- tt$events |> clean_names() |> glimpse()

tidytuesdayR::readme(tt) 
rm(tt)  


## 3. EXAMINING THE DATA ----
skim(events)


## 4. TIDYDATA ----

### |- Tidy ----

# One event that's missing from Wikipedia's list: 
#     R version 1.0 was released on February 29, 2000.

events_df <- rbind(events, list(
    # add R version 1.0 
    2020, 
    'R version 1.0 was released on February 29, 2000')) |> 
    # add decade column
    mutate(decade = (year %/% 10) * 10) |> 
    select(decade, everything())

# how many events per decade
events_df |> 
    group_by(decade, word) |> 
    summarise(count = n()) |> 
    ungroup() |> 
    arrange(desc(count)) 

# stop words
data(stop_words)

# word frequencies
events_words <- events_df |>
    # tokenization
    unnest_tokens(output = word, input = event, token = 'words', 
                  format = 'text', to_lower = TRUE) |> 
    # remove stop words
    anti_join(y = stop_words)


events_words |> count(decade, word, sort = TRUE) 
events_words |> count(word, sort = TRUE) 

# count words co-occuring within decades
word_pairs <- events_words |> 
    widyr::pairwise_count(word, decade, sort = TRUE)

# Pairwise correlation
word_cors <- events_words |>
    group_by(word) |> 
    filter(n() > 1) |>
    pairwise_cor(word, decade, sort = TRUE) 

word_cors|> count(item1, sort = TRUE) 
    
# pick particular words and find the other words most associated with them 
word_cors |>
    filter(item1 %in% c("queen", "king", "president", "war")) |>
    group_by(item1) |>
    slice_max(correlation, n = 6) |>
    ungroup() |>
    mutate(
        item2 = reorder(item2, correlation),
        fill_color = ifelse(correlation > 0, "Positive", "Negative")
    ) |>
    ggplot(aes(item2, correlation, fill = fill_color)) +  # Map fill_color to fill
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    scale_fill_manual(values = c("Positive" = "#3BCEAC", "Negative" = "#540D6E")) +  # Define colors
    coord_flip() +
    theme_minimal()



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#fbf7f0', 0.35) 
title_col    <- "gray10"              
subtitle_col <- "gray20"   
caption_col  <- "gray20"  
text_col     <- "gray20" 

col_palette <- MetBrewer::met.brewer("Hokusai1", n = 32)

### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: { 2024 } Week { 09 } &bull; Source: Wikipedia: February 29<br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Pairwise Correlations in Leap Day Events") 

subtitle_text <- str_glue("Which words are more likely to occur together with other words?\n",
                          "Showing correlations > 0.5") 

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Nanum Gothic Coding", regular.wt = 700, family = "title")                            
font_add_google("Abel", family = "subtitle")   
font_add_google("Barlow Condensed", family = "text")  
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  


### |-  final plot ----  
set.seed(2024)

# graph object
graph <- word_cors  |> 
    filter(correlation > .5)  |> 
    igraph::graph_from_data_frame() 

# Lets visualize the correlations and clusters of words
ggraph(graph, layout = "fr") +
    
    # Geoms
    ggraph::geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    ggraph::geom_node_point(aes(color = as.factor(name)), size = 5, show.legend = FALSE) +
    ggraph::geom_node_text(aes(label = name), size = 3, color = "gray10", 
                           repel = TRUE, check_overlap = TRUE ) +
    
    # Scales
    scale_edge_width(range = c(0.5, 3)) +
    scale_color_manual(values = col_palette) +
    coord_cartesian(clip = "off", expand = TRUE) + 
    
    # Labs
    labs(x        = "",
         y        = "",       
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text
    ) +
    
    # Theme
    theme_void() +
    theme(
        
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.title            = element_text(
            size              = rel(2.1), 
            family            = 'title',
            face              = 'bold',
            color             = title_col,
            margin            = margin(t = 5, b = 5)), 
           
        plot.subtitle         = element_text(
            size              = rel(1.3), 
            family            = 'subtitle',
            color             = title_col,
            lineheight        = 1.1, 
            margin            = margin(t = 5, b = 10)),  
           
        plot.caption          = element_markdown(
            size              = rel(.65), 
            family            = 'caption',
            color             = caption_col,
            lineheight        = 0.65,
            hjust             = 0.5,
            halign            = 0.5,
            margin            = margin(t = 10, b = 5)),
    )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# > sessioninfo::session_info(include_base = TRUE) 
# ─ Session info ────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-02-26
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# backports      1.4.1   2021-12-13 [1] CRAN (R 4.3.0)
# base         * 4.3.2   2023-10-31 [2] local
# base64enc      0.1-3   2015-07-28 [1] CRAN (R 4.3.0)
# P bit            4.0.5   2022-11-15 [?] CRAN (R 4.3.0)
# P bit64          4.0.5   2020-08-30 [?] CRAN (R 4.3.0)
# broom          1.0.5   2023-06-09 [1] CRAN (R 4.3.2)
# camcorder      0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# P cellranger     1.1.0   2016-07-27 [?] CRAN (R 4.3.0)
# P cli            3.6.1   2023-03-23 [?] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# P compiler       4.3.2   2023-10-31 [?] local
# P crayon         1.5.2   2022-09-29 [?] CRAN (R 4.3.0)
# P curl           5.0.0   2023-01-12 [?] CRAN (R 4.3.0)
# P datasets     * 4.3.2   2023-10-31 [?] local
# P digest         0.6.34  2024-01-11 [?] CRAN (R 4.3.2)
# P dplyr        * 1.1.4   2023-11-17 [?] RSPM (R 4.3.0)
# P fansi          1.0.4   2023-01-22 [?] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# fastmap        1.1.1   2023-02-24 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# P fs             1.6.2   2023-04-25 [?] CRAN (R 4.3.0)
# P generics       0.1.3   2022-07-05 [?] CRAN (R 4.3.0)
# ggforce        0.4.2   2024-02-19 [1] CRAN (R 4.3.2)
# ggplot2      * 3.5.0   2024-02-23 [1] CRAN (R 4.3.2)
# P ggraph       * 2.1.0   2022-10-09 [?] CRAN (R 4.3.2)
# P ggrepel        0.9.5   2024-01-10 [?] CRAN (R 4.3.2)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.6.6-1 2022-04-05 [1] CRAN (R 4.3.0)
# glue           1.7.0   2024-01-09 [1] CRAN (R 4.3.2)
# P graphics     * 4.3.2   2023-10-31 [?] local
# P graphlayouts   1.0.2   2023-11-03 [?] CRAN (R 4.3.2)
# P grDevices    * 4.3.2   2023-10-31 [?] local
# P grid           4.3.2   2023-10-31 [?] local
# gridExtra      2.3     2017-09-09 [1] CRAN (R 4.3.0)
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4   2023-08-21 [1] CRAN (R 4.3.2)
# P here           1.0.1   2020-12-13 [?] CRAN (R 4.3.0)
# P hms            1.1.3   2023-03-21 [?] CRAN (R 4.3.0)
# htmltools      0.5.7   2023-11-03 [1] CRAN (R 4.3.2)
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# P igraph       * 2.0.2   2024-02-17 [?] CRAN (R 4.3.2)
# janeaustenr    1.0.0   2022-08-26 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# P jsonlite       1.8.8   2023-12-04 [?] RSPM (R 4.3.0)
# knitr          1.45    2023-10-30 [1] CRAN (R 4.3.2)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.3.1)
# P lattice        0.21-9  2023-10-01 [?] CRAN (R 4.3.2)
# P lifecycle      1.0.3   2022-10-07 [?] CRAN (R 4.3.0)
# lubridate    * 1.9.3   2023-09-27 [1] CRAN (R 4.3.2)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# P magrittr       2.0.3   2022-03-30 [?] CRAN (R 4.3.0)
# markdown       1.6     2023-04-07 [1] CRAN (R 4.3.0)
# P MASS           7.3-60  2023-05-04 [?] CRAN (R 4.3.2)
# P Matrix         1.6-1.1 2023-09-18 [?] CRAN (R 4.3.2)
# MetBrewer      0.2.0   2022-03-21 [1] CRAN (R 4.3.0)
# P methods      * 4.3.2   2023-10-31 [?] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# P pacman         0.5.1   2019-03-11 [?] CRAN (R 4.3.0)
# P parallel       4.3.2   2023-10-31 [?] local
# P pillar         1.9.0   2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig      2.0.3   2019-09-22 [?] CRAN (R 4.3.0)
# P plyr           1.8.9   2023-10-02 [?] CRAN (R 4.3.2)
# P polyclip       1.10-6  2023-09-27 [?] CRAN (R 4.3.1)
# P purrr        * 1.0.2   2023-08-10 [?] CRAN (R 4.3.1)
# P R6             2.5.1   2021-08-19 [?] CRAN (R 4.3.0)
# ragg           1.2.5   2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# readr        * 2.1.5   2024-01-10 [1] CRAN (R 4.3.2)
# readxl         1.4.3   2023-07-06 [1] CRAN (R 4.3.2)
# renv           1.0.4   2024-02-21 [1] CRAN (R 4.3.2)
# repr           1.1.6   2023-01-26 [1] CRAN (R 4.3.0)
# P reshape2       1.4.4   2020-04-09 [?] CRAN (R 4.3.2)
# rlang          1.1.3   2024-01-10 [1] CRAN (R 4.3.2)
# P rprojroot      2.0.3   2022-04-02 [?] CRAN (R 4.3.0)
# P rstudioapi     0.14    2022-08-22 [?] CRAN (R 4.3.0)
# rsvg           2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.4   2024-02-12 [1] CRAN (R 4.3.2)
# scales       * 1.3.0   2023-11-28 [1] CRAN (R 4.3.2)
# P selectr        0.4-2   2019-11-20 [?] CRAN (R 4.3.0)
# P sessioninfo    1.2.2   2021-12-06 [?] CRAN (R 4.3.0)
# showtext     * 0.9-6   2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.0)
# skimr        * 2.1.5   2022-12-23 [1] CRAN (R 4.3.0)
# snakecase      0.11.0  2019-05-25 [1] CRAN (R 4.3.0)
# SnowballC      0.7.1   2023-04-25 [1] CRAN (R 4.3.0)
# P stats        * 4.3.2   2023-10-31 [?] local
# P stringi        1.7.12  2023-01-11 [?] CRAN (R 4.3.0)
# P stringr      * 1.5.0   2022-12-02 [?] CRAN (R 4.3.0)
# svglite        2.1.1   2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8   2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.5   2023-10-09 [1] CRAN (R 4.3.2)
# textshaping    0.3.6   2021-10-13 [1] CRAN (R 4.3.0)
# P tibble       * 3.2.1   2023-03-20 [?] CRAN (R 4.3.0)
# tidygraph      1.3.1   2024-01-30 [1] CRAN (R 4.3.2)
# tidyr        * 1.3.1   2024-01-24 [1] CRAN (R 4.3.2)
# P tidyselect     1.2.0   2022-10-10 [?] CRAN (R 4.3.0)
# tidytext     * 0.4.1   2023-01-07 [1] CRAN (R 4.3.0)
# tidytuesdayR   1.0.3   2023-12-13 [1] CRAN (R 4.3.2)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.3.0)
# P timechange     0.2.0   2023-01-11 [?] CRAN (R 4.3.0)
# tokenizers     0.3.0   2022-12-22 [1] CRAN (R 4.3.0)
# P tools          4.3.2   2023-10-31 [?] local
# P tweenr         2.0.2   2022-09-06 [?] CRAN (R 4.3.2)
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# P usethis        2.1.6   2022-05-25 [?] CRAN (R 4.3.0)
# P utf8           1.2.3   2023-01-31 [?] CRAN (R 4.3.0)
# P utils        * 4.3.2   2023-10-31 [?] local
# P vctrs          0.6.5   2023-12-01 [?] CRAN (R 4.3.2)
# viridis        0.6.5   2024-01-29 [1] CRAN (R 4.3.2)
# viridisLite    0.4.2   2023-05-02 [1] CRAN (R 4.3.0)
# vroom          1.6.5   2023-12-05 [1] CRAN (R 4.3.2)
# P widyr        * 0.1.5   2022-09-13 [?] CRAN (R 4.3.2)
# withr          2.5.2   2023-10-30 [1] CRAN (R 4.3.2)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# P xml2           1.3.4   2023-04-27 [?] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/d6d4a2bb
# 
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────────
# > 
