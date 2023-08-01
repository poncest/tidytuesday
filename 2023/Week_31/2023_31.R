

## Challenge: #TidyTuesday 2023 week 31
## Data:      US State Names
## Author:    Steven Ponce
## Date:      2023-08-01


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, camcorder, scales, lubridate)
pacman::p_load(tidytext, ggraph, igraph)


# |- figure size ---- 
gg_record(
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 8,
    height = 8,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 31) 

states    <- tt$states %>% clean_names()
etymology <- tt$state_name_etymology %>% clean_names()

readme(tt)   
rm(tt)   


## 3. EXAMINING THE DATA ----
glimpse(states)  
glimpse(etymology)

states %>% count(state, sort = T)
etymology %>% count(language, sort = T)
etymology %>% count(words_in_original, sort = T)
etymology %>% count(meaning, sort = T)

## Interested in examining the etymology `meaning` column

## 4. TIDYDATA ----

# bigrams
meaning_bigrams <- etymology %>% 
    # tokkenization
    unnest_tokens(bigram, meaning, token = "ngrams", n = 2, to_lower = TRUE)

meaning_bigrams %>% count(bigram, sort = T)

# bigrams separated
bigrams_separated <- meaning_bigrams %>% 
    separate_wider_delim(cols = bigram, names = c("word1", "word2"), delim = " ")
    
# bigrams - remove stop words
bigrams_filtered <- bigrams_separated %>% 
    filter(!word1 %in% stop_words$word) %>% 
    filter(!word2 %in% stop_words$word)

# new bigram count
bigrams_filtered %>% count(word1, word2, sort = T)

# bigrams united
bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ")


# additional stop_words
my_stopwords <- tibble(word = c("font", "style:italic", "mw", "parser", "output",
                                "div.hatnote", "padding", "left", "1.6em", "margin",
                                "bottom", "0.5em", "hatnote", "link", "top", "cf",
                                "plural","nuːtʃiu", "meːˈʃiʔko", "gëdá’geh", "nutʃi̥"))

# bigrams - remove additional stop words
bigrams_filtered <- bigrams_filtered  %>% 
    filter(!word1 %in% my_stopwords$word) %>% 
    filter(!word2 %in% my_stopwords$word)

# new bigrams united
bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ")


# |- plot data ----
bigram_graph <- bigrams_filtered %>% 
    count(word1, word2, sort = T) %>% 
    drop_na() %>% 
    igraph::graph_from_data_frame()



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- '#0D1821'        
title_col    <- "#FAFAFA"          
subtitle_col <- "#FAFAFA"   
caption_col  <- "#FAFAFA" 
text_col     <- "#FAFAFA"   


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 31 &bull; Source: Wikipedia - List of states and territories of the United States<br>")  
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Network Diagram: US State Names Etymology") 

subtitle_text <- str_glue("Force-directed Algorithm: Fruchterman-Reingold") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Archivo Black", family = "title")                        
font_add_google("Space Mono", family = "subtitle")           
font_add_google("Share Tech Mono", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
font_add_google("Shadows Into Light", family = "annote")  
showtext_auto(enable = TRUE)     


### |-  final plot ----

# visualizing a network of bigrams
# REFERENCE: https://igraph.org/c/doc/igraph-Layout.html

set.seed(2023)

ggraph(bigram_graph, layout = 'fr', niter = 150) +          
    
    # geoms
    geom_edge_arc(strength = 0.2, width = 0.5, alpha = 0.15, color = "#fafafa") + 
    
    geom_edge_fan(aes(alpha = 0.2, width = n), 
                  edge_colour = "#fafafa", show.legend = FALSE, check_overlap = T) +
    
    geom_node_point(color = '#ffdb01', alpha = 0.25, size = 1.5) +   #  E2EF70
    
    geom_node_text(aes(label = name), repel = TRUE, check_overlap = T, 
                   size = 2.1, color = "#fafafa")+
    
    # scales
    scale_edge_width(range = c(0.5, 2.5)) +
    
    # labs
    labs(title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text)+
    
    # theme
    theme_void()+
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background     = element_rect(fill = bkg_col, color = bkg_col),
        panel.background    = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin         = margin(t = 10, r = 15, b = 10, l = 15),
        
        axis.text           = element_blank(),
        axis.title          = element_blank(),
        panel.grid          = element_blank(),

        plot.title          = element_markdown(
            family          = 'title',
            color           = title_col,
            face            = "bold",
            size            = 19,  
            margin          = margin(t = 10, b = 5)),
        
        plot.subtitle       = element_markdown(
            family          = 'subtitle',
            color           = title_col,
            lineheight      = 0.8, 
            size            = 16,  
            margin          = margin(t = 5, b = 10)),
        
        plot.caption        = element_markdown(
            family          = 'caption',
            color           = caption_col,
            lineheight      = 0.6,
            size            = 10,
            hjust           = 0.5,
            halign          = 0.5,
            margin          = margin(t = 10, b = 10)),
        
    )



sessioninfo::session_info(include_base = TRUE) 
## 6. SESSION INFO ---- 
# ─ Session info ───────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19044)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-08-01
# rstudio  2023.06.1+524 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────
# package      * version date (UTC) lib source
# base         * 4.3.1   2023-06-16 [2] local
# bit            4.0.5   2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5   2020-08-30 [1] CRAN (R 4.3.0)
# camcorder    * 0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1   2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# compiler       4.3.1   2023-06-16 [2] local
# crayon         1.5.2   2022-09-29 [1] CRAN (R 4.3.0)
# curl           5.0.0   2023-01-12 [1] CRAN (R 4.3.0)
# datasets     * 4.3.1   2023-06-16 [2] local
# digest         0.6.31  2022-12-11 [1] CRAN (R 4.3.0)
# dplyr        * 1.1.2   2023-04-20 [1] CRAN (R 4.3.0)
# fansi          1.0.4   2023-01-22 [1] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.2   2023-04-25 [1] CRAN (R 4.3.0)
# generics       0.1.3   2022-07-05 [1] CRAN (R 4.3.0)
# ggforce        0.4.1   2022-10-04 [1] CRAN (R 4.3.1)
# ggplot2      * 3.4.2   2023-04-03 [1] CRAN (R 4.3.0)
# ggraph       * 2.1.0   2022-10-09 [1] CRAN (R 4.3.1)
# ggrepel        0.9.3   2023-02-03 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0  2023-05-19 [1] CRAN (R 4.3.0)
# glue           1.6.2   2022-02-24 [1] CRAN (R 4.3.0)
# graphics     * 4.3.1   2023-06-16 [2] local
# graphlayouts   1.0.0   2023-05-01 [1] CRAN (R 4.3.1)
# grDevices    * 4.3.1   2023-06-16 [2] local
# grid           4.3.1   2023-06-16 [2] local
# gridExtra      2.3     2017-09-09 [1] CRAN (R 4.3.0)
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.3   2023-03-21 [1] CRAN (R 4.3.0)
# here           1.0.1   2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3   2023-03-21 [1] CRAN (R 4.3.0)
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# igraph       * 1.4.3   2023-05-22 [1] CRAN (R 4.3.0)
# janeaustenr    1.0.0   2022-08-26 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.5   2023-06-05 [1] CRAN (R 4.3.0)
# labeling       0.4.2   2020-10-20 [1] CRAN (R 4.3.0)
# lattice        0.21-8  2023-04-05 [2] CRAN (R 4.3.1)
# lifecycle      1.0.3   2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2   2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.7     2023-05-16 [1] CRAN (R 4.3.0)
# MASS           7.3-60  2023-05-04 [2] CRAN (R 4.3.1)
# Matrix         1.5-4.1 2023-05-18 [2] CRAN (R 4.3.1)
# methods      * 4.3.1   2023-06-16 [2] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1   2019-03-11 [1] CRAN (R 4.3.0)
# parallel       4.3.1   2023-06-16 [2] local
# pillar         1.9.0   2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.3.0)
# polyclip       1.10-4  2022-10-20 [1] CRAN (R 4.3.0)
# purrr        * 1.0.1   2023-01-10 [1] CRAN (R 4.3.0)
# R6             2.5.1   2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5   2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# readr        * 2.1.4   2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.2   2023-02-09 [1] CRAN (R 4.3.0)
# rlang          1.1.1   2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot      2.0.3   2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.14    2022-08-22 [1] CRAN (R 4.3.0)
# rsvg           2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.3   2022-08-19 [1] CRAN (R 4.3.0)
# scales       * 1.2.1   2022-08-20 [1] CRAN (R 4.3.0)
# selectr        0.4-2   2019-11-20 [1] CRAN (R 4.3.0)
# sessioninfo    1.2.2   2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-6   2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.0)
# snakecase      0.11.0  2019-05-25 [1] CRAN (R 4.3.0)
# SnowballC      0.7.1   2023-04-25 [1] CRAN (R 4.3.0)
# stats        * 4.3.1   2023-06-16 [2] local
# stringi        1.7.12  2023-01-11 [1] CRAN (R 4.3.0)
# stringr      * 1.5.0   2022-12-02 [1] CRAN (R 4.3.0)
# svglite        2.1.1   2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8   2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4   2022-02-11 [1] CRAN (R 4.3.0)
# textshaping    0.3.6   2021-10-13 [1] CRAN (R 4.3.0)
# tibble       * 3.2.1   2023-03-20 [1] CRAN (R 4.3.0)
# tidygraph      1.2.3   2023-02-01 [1] CRAN (R 4.3.1)
# tidyr        * 1.3.0   2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect     1.2.0   2022-10-10 [1] CRAN (R 4.3.0)
# tidytext     * 0.4.1   2023-01-07 [1] CRAN (R 4.3.0)
# tidytuesdayR * 1.0.2   2022-02-01 [1] CRAN (R 4.3.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0   2023-01-11 [1] CRAN (R 4.3.0)
# tokenizers     0.3.0   2022-12-22 [1] CRAN (R 4.3.0)
# tools          4.3.1   2023-06-16 [2] local
# tweenr         2.0.2   2022-09-06 [1] CRAN (R 4.3.1)
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# usethis        2.2.0   2023-06-06 [1] CRAN (R 4.3.0)
# utf8           1.2.3   2023-01-31 [1] CRAN (R 4.3.0)
# utils        * 4.3.1   2023-06-16 [2] local
# vctrs          0.6.2   2023-04-19 [1] CRAN (R 4.3.0)
# viridis        0.6.3   2023-05-03 [1] CRAN (R 4.3.1)
# viridisLite    0.4.2   2023-05-02 [1] CRAN (R 4.3.0)
# vroom          1.6.3   2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.0   2022-03-03 [1] CRAN (R 4.3.0)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# xml2           1.3.4   2023-04-27 [1] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ──────────────────────────────────────────────────────────────────────────────────────
# > 

  