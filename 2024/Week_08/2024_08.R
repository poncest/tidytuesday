
## Challenge: #TidyTuesday 2024 week 08
## Data:      R Consortium ISC Grants
## Author:    Steven Ponce
## Date:      2024-02-19


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
pacman::p_load(tidytext, tidygraph, ggraph)                                        


### |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 6,
    height = 5,
    units  = "in",
    dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(x = base::as.double("2024"), 
                            week = base::as.double("08")) 

isc_grants <- tt$isc_grants |> clean_names() |> glimpse()

tidytuesdayR::readme(tt) 
rm(tt)  


## 3. EXAMINING THE DATA ----
skim(isc_grants)


## 4. TIDYDATA ----

### |- Tidy ----
isc_grants <- isc_grants |> 
    mutate(cycle = case_when(
        group == 1 ~ "spring",
        group == 2 ~ "fall"
    ))

# bigrams   
summary_bigrams <- isc_grants |> 
    # tokkenization
    unnest_tokens(bigram, 
                  summary, 
                  token    = "ngrams", 
                  n        = 2, 
                  to_lower = TRUE) 

summary_bigrams |> 
    count(bigram, sort = TRUE)

# bigrams separated
bigrams_separated <- summary_bigrams |> 
    # separate
    separate_wider_delim(cols = bigram, names = c("word1", "word2"), delim = " ") 

# bigrams filtered (remove stop words)
bigrams_filtered <- bigrams_separated |> 
    # remove stop words
    filter(!word1 %in% stop_words$word) |> 
    filter(!word2 %in% stop_words$word) 

bigrams_filtered |>  
    count(word1, word2, sort = T)  

# additional stop_words
my_stopwords <- tibble(word = c("a", "to", "and", "the", "with", "of",
                                "at", "as", "has", "it", "for", "is",
                                "this"))

# bigrams - remove additional stop words
bigrams_filtered <- bigrams_filtered  |>
    filter(!word1 %in% my_stopwords$word) |>
    filter(!word2 %in% my_stopwords$word)

bigrams_filtered |>
    count(word1, word2, sort = T) 

# sentiment lexicon AFINN from Finn Årup Nielsen
afinn <- get_sentiments("afinn")

# afin bigrams separated
afin_bigrams_separated <- bigrams_filtered |>
    inner_join(afinn, by = c(word2 = "word")) |> 
    count(cycle, word1, word2, value,  sort = TRUE) |> 
    mutate(contribution = n * value) |> 
    arrange(desc(abs(contribution))) 



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- "grey15"      
title_col    <- "gray85"          
subtitle_col <- "gray85"  
caption_col  <- "gray85"    
text_col     <- "gray85"      
link         <- "#8C7069"
node         <- "#1748b6"


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: { 2024 } Week { 08 } &bull; Source: R Consortium ISC Funded Projects<br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Network Diagram: R Consortium ISC Grants Summary") 

subtitle_text <- str_glue("Force-directed Drawing Algorithm: Kamada-Kawai [KK89] uses spring forces<br>",
                          "proportional to the graph theoretic distances.<br><br>",
                          "Separated by grant cycle (Spring or Fall)") 

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Advent Pro", regular.wt = 600, family = "title")                            
font_add_google("Nanum Gothic Coding", family = "subtitle")   
font_add_google("Barlow Condensed", family = "text")  
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',

    axis.title            = element_blank(),
    axis.text             = element_text(size = rel(1), color = text_col, family   = 'text'),
    
    panel.grid            = element_blank(),
    plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
)

### |-  final plot ---- 
set.seed(4321)

# bigram_graph
bigram_graph <- igraph::graph_from_data_frame(afin_bigrams_separated) 

# arrow
arrow <- grid::arrow(type = "closed", length = unit(0.05, "inches"))

# graph
ggraph::ggraph(graph = bigram_graph, layout = 'kk') + 
    
    # Geoms
    geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)),
                   arrow = arrow, alpha = 0.5, color = link) +

    geom_edge_density(aes(fill = n), n = 50) +
    
    geom_node_point(color = node, size = 3, alpha = .75) +

    geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE, 
                   size = 2, color = text_col) +
    
    # Scales
    scale_edge_width(range = c(0.5, 1.5)) +
    
    # Labs
    labs(title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text) +
    
    # Theme
    theme(
        
        plot.title         = element_text(
            size           = rel(1.5), 
            family         = 'title',
            face           = 'bold',
            color          = title_col,
            margin         = margin(t = 5, b = 5)), 
        
        plot.subtitle      = element_markdown(
            size           = rel(.8), 
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 1.1, 
            margin         = margin(t = 5, b = 5)),  
        
        plot.caption       = element_markdown(
            size           = rel(.55), 
            family         = 'caption',
            color          = caption_col,
            lineheight     = 0.65,
            hjust          = 0.5,
            halign         = 0.5,
            margin         = margin(t = 5, b = 5)),
    )



# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-02-20
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.3.2   2023-10-31 [2] local
# base64enc      0.1-3   2015-07-28 [1] CRAN (R 4.3.0)
# camcorder      0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# P cellranger     1.1.0   2016-07-27 [?] CRAN (R 4.3.0)
# P cli            3.6.1   2023-03-23 [?] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# P compiler       4.3.2   2023-10-31 [?] local
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
# P ggforce        0.4.1   2022-10-04 [?] CRAN (R 4.3.2)
# ggplot2      * 3.4.4   2023-10-12 [1] CRAN (R 4.3.2)
# P ggraph       * 2.1.0   2022-10-09 [?] CRAN (R 4.3.2)
# P ggrepel        0.9.5   2024-01-10 [?] CRAN (R 4.3.2)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.6.6-1 2022-04-05 [1] CRAN (R 4.3.0)
# P glue           1.6.2   2022-02-24 [?] CRAN (R 4.3.0)
# P graphics     * 4.3.2   2023-10-31 [?] local
# P graphlayouts   1.0.2   2023-11-03 [?] CRAN (R 4.3.2)
# P grDevices    * 4.3.2   2023-10-31 [?] local
# P grid           4.3.2   2023-10-31 [?] local
# gridExtra      2.3     2017-09-09 [1] CRAN (R 4.3.0)
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4   2023-08-21 [1] CRAN (R 4.3.2)
# P here           1.0.1   2020-12-13 [?] CRAN (R 4.3.0)
# P hms            1.1.3   2023-03-21 [?] CRAN (R 4.3.0)
# htmltools      0.5.5   2023-03-23 [1] CRAN (R 4.3.0)
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# igraph         1.4.2   2023-04-07 [1] CRAN (R 4.3.0)
# janeaustenr    1.0.0   2022-08-26 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# P jsonlite       1.8.8   2023-12-04 [?] RSPM (R 4.3.0)
# knitr          1.42    2023-01-25 [1] CRAN (R 4.3.0)
# P lattice        0.21-9  2023-10-01 [?] CRAN (R 4.3.2)
# P lifecycle      1.0.3   2022-10-07 [?] CRAN (R 4.3.0)
# P lubridate    * 1.9.2   2023-02-10 [?] CRAN (R 4.3.0)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# P magrittr       2.0.3   2022-03-30 [?] CRAN (R 4.3.0)
# P MASS           7.3-60  2023-05-04 [?] CRAN (R 4.3.2)
# P Matrix         1.6-1.1 2023-09-18 [?] CRAN (R 4.3.2)
# P methods      * 4.3.2   2023-10-31 [?] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# P pacman         0.5.1   2019-03-11 [?] CRAN (R 4.3.0)
# P pillar         1.9.0   2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig      2.0.3   2019-09-22 [?] CRAN (R 4.3.0)
# P polyclip       1.10-6  2023-09-27 [?] CRAN (R 4.3.1)
# P purrr        * 1.0.2   2023-08-10 [?] CRAN (R 4.3.1)
# P R6             2.5.1   2021-08-19 [?] CRAN (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# P readr        * 2.1.4   2023-02-10 [?] CRAN (R 4.3.0)
# P readxl         1.4.2   2023-02-09 [?] CRAN (R 4.3.0)
# renv           1.0.3   2023-09-19 [1] CRAN (R 4.3.2)
# repr           1.1.6   2023-01-26 [1] CRAN (R 4.3.0)
# P rlang          1.1.1   2023-04-28 [?] CRAN (R 4.3.0)
# P rprojroot      2.0.3   2022-04-02 [?] CRAN (R 4.3.0)
# P rstudioapi     0.14    2022-08-22 [?] CRAN (R 4.3.0)
# rsvg           2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# P rvest          1.0.3   2022-08-19 [?] CRAN (R 4.3.0)
# scales       * 1.3.0   2023-11-28 [1] CRAN (R 4.3.2)
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
# systemfonts    1.0.4   2022-02-11 [1] CRAN (R 4.3.0)
# P tibble       * 3.2.1   2023-03-20 [?] CRAN (R 4.3.0)
# P tidygraph    * 1.2.3   2023-02-01 [?] CRAN (R 4.3.2)
# tidyr        * 1.3.0   2023-01-24 [1] CRAN (R 4.3.0)
# P tidyselect     1.2.0   2022-10-10 [?] CRAN (R 4.3.0)
# tidytext     * 0.4.1   2023-01-07 [1] CRAN (R 4.3.0)
# P tidytuesdayR   1.0.2   2022-02-01 [?] CRAN (R 4.3.0)
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
# P viridis        0.6.4   2023-07-22 [?] CRAN (R 4.3.2)
# viridisLite    0.4.2   2023-05-02 [1] CRAN (R 4.3.0)
# withr          2.5.2   2023-10-30 [1] CRAN (R 4.3.2)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# P xml2           1.3.4   2023-04-27 [?] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/d6d4a2bb
# 
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────
# > 
