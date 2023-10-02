

## Challenge: #TidyTuesday 2023 week 40
## Data:      US Government Grant Opportunities
## Author:    Steven Ponce
## Date:      2023-10-02


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
pacman::p_load(tidytext, igraph, ggraph)
theme_set(theme_light(base_size = 16))


# |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 8,
    height = 8,
    units  = "in",
    dpi    = 320) 

# |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 40) 

grants <- tt$grants %>% 
    clean_names() %>% 
    glimpse()


grants_opportunity_details <- tt$grant_opportunity_details %>% 
    clean_names() %>% 
    glimpse()


tidytuesdayR::readme(tt) 
rm(tt)   



## 3. EXAMINING THE DATA ----
skim(grants)
grants %>% count(agency_code, agency_name, sort = T) 
grants %>% count(opportunity_status, sort = T) 

grants %>% 
    count(agency_code, agency_name, sort = T) %>% 
    mutate(pct = n / sum(n)) %>% 
    mutate(agency_code = fct_reorder(agency_code, n)) %>% 
    head(20) %>% 
    ggplot(aes(x = agency_code, y = n))+
    geom_col()+
    coord_flip(clip = "off")+
    scale_x_discrete()+
    scale_y_continuous(labels = comma_format())

    
grants %>% 
    count(agency_code, agency_name, sort = T) %>% 
    mutate(pct = n / sum(n)) %>% 
    mutate(agency_code = fct_reorder(agency_code, pct)) %>% 
    head(20) %>% 
    ggplot(aes(x = agency_code, y = pct))+
    geom_col()+
    coord_flip(clip = "off")+
    scale_x_discrete()+
    scale_y_continuous(labels = percent_format())

colnames(grants)

colnames(grants_opportunity_details)

# Lets do a network chart using `description`


## 4. TIDYDATA ----

# |- plot data ----
# data for plot
data_tbl <- grants_opportunity_details %>% 
    select(opportunity_id, description, posted_date) 


# bigrams   
description_bigrams <- data_tbl %>% 
    # tokkenization
    unnest_tokens(bigram, description, token = "ngrams", n = 2, to_lower = TRUE) 

description_bigrams %>% count(bigram, sort = TRUE)


# bigrams separated
bigrams_separated <- description_bigrams %>% 
    # separate
    separate_wider_delim(cols = bigram, names = c("word1", "word2"), delim = " ") 


# bigrams filtered (remove stop words)
bigrams_filtered <- bigrams_separated %>% 
    # remove stop words
    filter(!word1 %in% stop_words$word) %>% 
    filter(!word2 %in% stop_words$word) 

bigrams_filtered %>% count(word1, word2, sort = T) %>% head(20) 


# additional stop_words
my_stopwords <- tibble(word = c("funding", "opportunity", "announcement", "grant"))


# bigrams - remove additional stop words
bigrams_filtered <- bigrams_filtered  %>% 
    filter(!word1 %in% my_stopwords$word) %>% 
    filter(!word2 %in% my_stopwords$word) 


bigrams_filtered %>% count(word1, word2, sort = T) %>% head(20) 


# negation words
negation_words <- c("not", "no", "never", "without", "none", "nothing", "no one", 
                    "nobody", "nowhere", "haven't", "doesn't",
                    "neither", "barely", "hardly", "rarely", "seldom")


# Sentiment Lexicon AFINN from Finn Årup Nielsen
afinn <- get_sentiments("afinn")


# negation_bigrams
negation_bigrams <- bigrams_separated %>%
    filter(word1 %in% negation_words) %>%
    inner_join(afinn, by = c(word2 = "word")) %>% 
    count(word1, word2, value, sort = TRUE) %>% 
    mutate(contribution = n * value) %>% 
    arrange(desc(abs(contribution))) %>% 
    group_by(word1) %>% 
    arrange(word1,desc(contribution)) %>%
    ungroup()


# bigram_graph
bigram_graph <- negation_bigrams %>%
    igraph::graph_from_data_frame() 



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- "#F2E6D0"      
title_col    <- "#260112"          
subtitle_col <- "#260112" 
caption_col  <- "#260112"   
text_col     <- "#260112"    
link         <- "#8C7069"
node         <- "#F25749"



### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 39 &bull; Source: Grants 101 from Grants.gov<br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Negation Bigram Network:\n2,000 US Government Grant Opportunities") 

subtitle_text <- str_glue("Simulated Annealing Algorithm: Davidson and Harels") 

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")



### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Days One", family = "title")                            
font_add_google("Sarabun", family = "subtitle")              
font_add_google("Sarabun", family = "text")                  
font_add_google("PT Sans Narrow", family = "caption")
font_add_google("Shadows Into Light", family = "annote")  
showtext_auto(enable = TRUE)     


### |-  final plot ---- 
set.seed(4321)

# arrow
arrow <- grid::arrow(type = "closed", length = unit(0.05, "inches"))

# graph
ggraph(bigram_graph, layout = 'dh') +
    
    # geoms
    geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)), 
                   arrow = arrow, alpha = 0.5, color = link) + 
    
    geom_edge_density(aes(fill = value), n = 100) +
    
    geom_node_point(color = node, size = 3) +   
    
    geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE, size = 5, color = text_col) +
    
    # scales
    scale_edge_width(range = c(0.5, 1.5)) +
    
    # labs
    labs(title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text)+
    
    # theme
    theme_graph()+
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
        
        plot.title          = element_text(
            family          = 'title',
            color           = title_col,
            face            = "bold",
            size            = 19,  
            margin          = margin(t = 10, b = 5)),
        
        plot.subtitle       = element_text(
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
# ─ Session info ──────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-10-02
# rstudio  2023.09.0+463 Desert Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────────
# package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [2] local
# base64enc      0.1-3    2015-07-28 [1] CRAN (R 4.3.0)
# bit            4.0.5    2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5    2020-08-30 [1] CRAN (R 4.3.0)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1    2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0    2023-03-17 [1] CRAN (R 4.3.0)
# compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.2    2022-09-29 [1] CRAN (R 4.3.0)
# curl           5.0.2    2023-08-14 [1] CRAN (R 4.3.1)
# datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.33   2023-07-07 [1] CRAN (R 4.3.1)
# dplyr        * 1.1.3    2023-09-03 [1] CRAN (R 4.3.1)
# fansi          1.0.4    2023-01-22 [1] CRAN (R 4.3.0)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.3.0)
# fastmap        1.1.1    2023-02-24 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.3    2023-07-20 [1] CRAN (R 4.3.1)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggforce        0.4.1    2022-10-04 [1] CRAN (R 4.3.1)
# ggplot2      * 3.4.3    2023-08-14 [1] CRAN (R 4.3.1)
# ggraph       * 2.1.0    2022-10-09 [1] CRAN (R 4.3.1)
# ggrepel        0.9.3    2023-02-03 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue           1.6.2    2022-02-24 [1] CRAN (R 4.3.0)
# graphics     * 4.3.1    2023-06-16 [2] local
# graphlayouts   1.0.0    2023-05-01 [1] CRAN (R 4.3.1)
# grDevices    * 4.3.1    2023-06-16 [2] local
# grid           4.3.1    2023-06-16 [2] local
# gridExtra      2.3      2017-09-09 [1] CRAN (R 4.3.0)
# gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# here           1.0.1    2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# htmltools      0.5.6    2023-08-10 [1] CRAN (R 4.3.1)
# httr           1.4.7    2023-08-15 [1] CRAN (R 4.3.1)
# igraph       * 1.5.1    2023-08-10 [1] CRAN (R 4.3.1)
# janeaustenr    1.0.0    2022-08-26 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.7    2023-06-29 [1] CRAN (R 4.3.1)
# knitr          1.43     2023-05-25 [1] CRAN (R 4.3.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lattice        0.21-8   2023-04-05 [2] CRAN (R 4.3.1)
# lifecycle      1.0.3    2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2    2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.5    2023-08-07 [1] CRAN (R 4.3.1)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.8      2023-08-23 [1] CRAN (R 4.3.1)
# MASS           7.3-60   2023-05-04 [2] CRAN (R 4.3.1)
# Matrix         1.6-1    2023-08-14 [2] CRAN (R 4.3.1)
# methods      * 4.3.1    2023-06-16 [2] local
# munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# parallel       4.3.1    2023-06-16 [2] local
# pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# polyclip       1.10-4   2022-10-20 [1] CRAN (R 4.3.0)
# purrr        * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5    2023-01-12 [1] CRAN (R 4.3.0)
# rappdirs       0.3.3    2021-01-31 [1] CRAN (R 4.3.0)
# Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr        * 2.1.4    2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.3    2023-07-06 [1] CRAN (R 4.3.1)
# repr           1.1.6    2023-01-26 [1] CRAN (R 4.3.1)
# rlang          1.1.1    2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot      2.0.3    2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg           2.4.0    2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.3    2022-08-19 [1] CRAN (R 4.3.0)
# scales       * 1.2.1    2022-08-20 [1] CRAN (R 4.3.1)
# selectr        0.4-2    2019-11-20 [1] CRAN (R 4.3.0)
# sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-6    2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.0)
# skimr        * 2.1.5    2022-12-23 [1] CRAN (R 4.3.1)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.1)
# SnowballC      0.7.1    2023-04-25 [1] CRAN (R 4.3.0)
# stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.7.12   2023-01-11 [1] CRAN (R 4.3.0)
# stringr      * 1.5.0    2022-12-02 [1] CRAN (R 4.3.0)
# svglite        2.1.1    2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8    2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4    2022-02-11 [1] CRAN (R 4.3.0)
# textdata       0.4.4    2022-09-02 [1] CRAN (R 4.3.0)
# textshaping    0.3.6    2021-10-13 [1] CRAN (R 4.3.0)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.0)
# tidygraph      1.2.3    2023-02-01 [1] CRAN (R 4.3.1)
# tidyr        * 1.3.0    2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.0)
# tidytext     * 0.4.1    2023-01-07 [1] CRAN (R 4.3.0)
# tidytuesdayR   1.0.2    2022-02-01 [1] CRAN (R 4.3.0)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tokenizers     0.3.0    2022-12-22 [1] CRAN (R 4.3.0)
# tools          4.3.1    2023-06-16 [2] local
# tweenr         2.0.2    2022-09-06 [1] CRAN (R 4.3.1)
# tzdb           0.4.0    2023-05-12 [1] CRAN (R 4.3.0)
# usethis        2.2.2    2023-07-06 [1] CRAN (R 4.3.1)
# utf8           1.2.3    2023-01-31 [1] CRAN (R 4.3.0)
# utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.6.3    2023-06-14 [1] CRAN (R 4.3.1)
# viridis        0.6.4    2023-07-22 [1] CRAN (R 4.3.1)
# viridisLite    0.4.2    2023-05-02 [1] CRAN (R 4.3.0)
# vroom          1.6.3    2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.0    2022-03-03 [1] CRAN (R 4.3.0)
# xfun           0.40     2023-08-09 [1] CRAN (R 4.3.1)
# xml2           1.3.5    2023-07-06 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────────

