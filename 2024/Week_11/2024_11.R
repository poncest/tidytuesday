
## Challenge: #TidyTuesday 2024 week 11 
## Data:      Fiscal Sponsors
## Author:    Steven Ponce
## Date:      2024-03-11


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
pacman::p_load(tidytext, stopwords, udpipe)                                       


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
                            week = base::as.double("11")) 

fiscal_sponsor_directory <- tt$fiscal_sponsor_directory |> clean_names() |> glimpse()

tidytuesdayR::readme(tt) 
rm(tt)  


## 3. EXAMINING THE DATA ----
glimpse(fiscal_sponsor_directory)
skim(fiscal_sponsor_directory)
colnames(fiscal_sponsor_directory)

fiscal_sponsor_directory |> count(n_sponsored, sort = T)


## 4. TIDYDATA ----

### |- Tidy ----
fiscal_sponsor <- 
    fiscal_sponsor_directory |> 
    # Select columns
    select(name, year_501c3, year_fiscal_sponsor, n_sponsored, project_types) |> 
    # Separate wide `project_types`
    separate_wider_delim(cols      = project_types, 
                         delim     = "|", 
                         names_sep = "-",
                         too_few   = "align_end"
                        ) |> 
    # Pivot longer
    pivot_longer(cols = starts_with(match = 'project_'),
                 names_to  = "temp_col",
                 values_to = "project_type",
                 values_drop_na = TRUE) |> 
    # Drop `temp_col`
    select(-temp_col) |> 
    # Format
    mutate(project_type = str_to_lower(project_type))

# Stopwords 
stopwords <- stopwords::stopwords(language = "en", source = "stopwords-iso")

# Tokenization
project_type_words <- fiscal_sponsor |>  
    unnest_tokens(input = project_type, output = word, token = "words") |> 
    filter(!grepl('[0-9]', word)) |>    # remove numbers
    filter(!word %in% stopwords)        # remove stop words 

# Count words 
project_type_words |> count(word, sort = TRUE)
project_type_words |> count(name, word, sort = TRUE)

# Unique words
project_type_words_unique <- unique(project_type_words$word)

# Download language model - https://bnosac.github.io/udpipe/docs/doc0.html
udmodel_en <- udpipe_download_model(language = "english-ewt")
udmodel_en <- udpipe_load_model(file = udmodel_en$file_model)

# Annotate words to identify adjectives
words_annotated <- udpipe_annotate(object = udmodel_en, x = project_type_words_unique, tokenizer = "tokenizer")
words_annotated <- as_tibble(words_annotated)
adjectives      <- subset(words_annotated, upos == "ADJ")  

# Top 10 adjectives 
top_10_adj <- project_type_words |> 
    semi_join(adjectives, by = c("word" = "token")) |>                          # return all rows from x with a match in y
    
    group_by(word) |> 
    add_count(word, name = "word_total") |> 
    ungroup() |> 
    
    nest(data = -c(word, word_total)) |> 
    arrange(desc(word_total)) |> 
    slice_max(word_total, n = 10) |>      
    unnest(cols = data) |> 
    
    mutate(
        word = str_to_title(word),
        word = as_factor(word),
    )



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#393E41', 0.05) 
title_col    <- "white"              
subtitle_col <- "#fafafa"   
caption_col  <- "#fafafa"  
text_col     <- "#fafafa"  
col_palette  <- list("#44BBA4", "#E7BB41")  


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: { 2024 } Week { 11 } &bull; Source: Fiscal Sponsor Directory<br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Fiscal Sponsor: Top 10 Adjectives in Project Type") 

subtitle_text <- str_glue("A fiscal sponsor is a non-profit organization that provides legal and tax-exempt status<br>",
                          "to other parties. The other party is often an individual, group, or organization that wants<br>",
                          "to conduct a charitable project or activity.") 

annotation_text <- str_glue("Natural Language Processing with UDPipe")

caption_text    <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Anton", regular.wt = 400, family = "title")  
font_add_google("Abel", family = "subtitle")  
font_add_google("Barlow Condensed", family = "text")  
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    
    plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
    
    axis.text             = element_text(size = rel(1), color = text_col, family = 'text'),
    
    axis.line.x           = element_line(color = "gray90", linewidth = .2),
    
    panel.grid.minor.y    = element_blank(),
    panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.05, color = 'gray'),
    
    panel.grid.minor.x    = element_blank(),
    panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray')
    )


### |-  final plot ----  
top_10_adj |> 
    ggplot(aes(x = word_total, y = word, size = word_total)) +
    
    # Geoms
    geom_point(color = col_palette[1], shape = 21, stroke = 1.5, alpha = 1) + 
    geom_point(fill = col_palette[2], shape = 21, alpha = 1) +
    geom_text(x = 100, y = "Environmental", label = annotation_text,
              color = text_col, size = rel(4.8), hjust = 0, vjust = 0, family = "text") +
    
    # Scales
    scale_x_continuous(breaks = seq(0, 300, by = 50), limits = c(0, 300)) +
    scale_y_discrete(expand = c(0.08, 0.08)) +
    scale_size_continuous(range = c(1, 8)) +
    coord_cartesian(clip = "off", expand = TRUE) +
    
    # Labs
    labs(x        = "Count",
         y        = "Adjective",       
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text
    ) + 
    
    # Theme
    theme(
        plot.title      = element_text(
            size        = rel(1.7), 
            family      = 'title',
            face        = 'bold',
            color       = title_col,
            margin      = margin(t = 5, b = 5)), 
        
        plot.subtitle   = element_markdown(
            size        = rel(.92), 
            family      = 'subtitle',
            color       = title_col,
            lineheight  = 1.1, 
            margin      = margin(t = 5, b = 10)),  
        
        plot.caption    = element_markdown(
            size        = rel(.65), 
            family      = 'caption',
            color       = caption_col,
            lineheight  = 0.65,
            hjust       = 0.5,
            halign      = 0.5,
            margin      = margin(t = 10, b = 5)),
    )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ───────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.3 (2024-02-29 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-03-11
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.3.3   2024-02-29 [2] local
# base64enc      0.1-3   2015-07-28 [1] CRAN (R 4.3.0)
# P bit            4.0.5   2022-11-15 [?] CRAN (R 4.3.0)
# P bit64          4.0.5   2020-08-30 [?] CRAN (R 4.3.0)
# camcorder      0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# P cellranger     1.1.0   2016-07-27 [?] CRAN (R 4.3.0)
# P cli            3.6.1   2023-03-23 [?] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# P compiler       4.3.3   2024-02-29 [?] local
# P crayon         1.5.2   2022-09-29 [?] CRAN (R 4.3.0)
# P curl           5.0.0   2023-01-12 [?] CRAN (R 4.3.0)
# data.table     1.15.2  2024-02-29 [1] CRAN (R 4.3.3)
# P datasets     * 4.3.3   2024-02-29 [?] local
# P digest         0.6.34  2024-01-11 [?] CRAN (R 4.3.2)
# P dplyr        * 1.1.4   2023-11-17 [?] RSPM (R 4.3.0)
# P fansi          1.0.4   2023-01-22 [?] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# fastmap        1.1.1   2023-02-24 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.3   2023-07-20 [1] CRAN (R 4.3.3)
# P generics       0.1.3   2022-07-05 [?] CRAN (R 4.3.0)
# ggplot2      * 3.5.0   2024-02-23 [1] CRAN (R 4.3.2)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.6.6-1 2022-04-05 [1] CRAN (R 4.3.0)
# glue           1.7.0   2024-01-09 [1] CRAN (R 4.3.2)
# P graphics     * 4.3.3   2024-02-29 [?] local
# P grDevices    * 4.3.3   2024-02-29 [?] local
# P grid           4.3.3   2024-02-29 [?] local
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4   2023-08-21 [1] CRAN (R 4.3.2)
# P here           1.0.1   2020-12-13 [?] CRAN (R 4.3.0)
# P hms            1.1.3   2023-03-21 [?] CRAN (R 4.3.0)
# htmltools      0.5.7   2023-11-03 [1] CRAN (R 4.3.2)
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# janeaustenr    1.0.0   2022-08-26 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# P jsonlite       1.8.8   2023-12-04 [?] RSPM (R 4.3.0)
# knitr          1.45    2023-10-30 [1] CRAN (R 4.3.2)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.3.1)
# P lattice        0.22-5  2023-10-24 [?] CRAN (R 4.3.3)
# P lifecycle      1.0.3   2022-10-07 [?] CRAN (R 4.3.0)
# lubridate    * 1.9.3   2023-09-27 [1] CRAN (R 4.3.2)
# magick         2.8.3   2024-02-18 [1] CRAN (R 4.3.3)
# P magrittr       2.0.3   2022-03-30 [?] CRAN (R 4.3.0)
# markdown       1.12    2023-12-06 [1] CRAN (R 4.3.3)
# P Matrix         1.6-5   2024-01-11 [?] CRAN (R 4.3.3)
# P methods      * 4.3.3   2024-02-29 [?] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# P pacman         0.5.1   2019-03-11 [?] CRAN (R 4.3.0)
# P parallel       4.3.3   2024-02-29 [?] local
# P pillar         1.9.0   2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig      2.0.3   2019-09-22 [?] CRAN (R 4.3.0)
# P purrr        * 1.0.2   2023-08-10 [?] CRAN (R 4.3.1)
# P R6             2.5.1   2021-08-19 [?] CRAN (R 4.3.0)
# P ragg           1.2.7   2023-12-11 [?] RSPM (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# readr        * 2.1.5   2024-01-10 [1] CRAN (R 4.3.2)
# readxl         1.4.3   2023-07-06 [1] CRAN (R 4.3.2)
# renv           1.0.5   2024-02-29 [1] CRAN (R 4.3.3)
# repr           1.1.6   2023-01-26 [1] CRAN (R 4.3.0)
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
# P stats        * 4.3.3   2024-02-29 [?] local
# P stopwords    * 2.3     2021-10-28 [?] CRAN (R 4.3.3)
# stringi        1.8.3   2023-12-11 [1] CRAN (R 4.3.2)
# stringr      * 1.5.1   2023-11-14 [1] CRAN (R 4.3.3)
# svglite        2.1.1   2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts    1.0.5   2023-10-09 [1] CRAN (R 4.3.2)
# textshaping    0.3.6   2021-10-13 [1] CRAN (R 4.3.0)
# P tibble       * 3.2.1   2023-03-20 [?] CRAN (R 4.3.0)
# tidyr        * 1.3.1   2024-01-24 [1] CRAN (R 4.3.2)
# P tidyselect     1.2.0   2022-10-10 [?] CRAN (R 4.3.0)
# P tidytext     * 0.4.1   2023-01-07 [?] CRAN (R 4.3.0)
# tidytuesdayR   1.0.3   2023-12-13 [1] CRAN (R 4.3.2)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.3.0)
# P timechange     0.2.0   2023-01-11 [?] CRAN (R 4.3.0)
# tokenizers     0.3.0   2022-12-22 [1] CRAN (R 4.3.0)
# P tools          4.3.3   2024-02-29 [?] local
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# P udpipe       * 0.8.11  2023-01-06 [?] CRAN (R 4.3.3)
# usethis        2.2.3   2024-02-19 [1] CRAN (R 4.3.3)
# P utf8           1.2.3   2023-01-31 [?] CRAN (R 4.3.0)
# P utils        * 4.3.3   2024-02-29 [?] local
# P vctrs          0.6.5   2023-12-01 [?] CRAN (R 4.3.2)
# vroom          1.6.5   2023-12-05 [1] CRAN (R 4.3.2)
# withr          2.5.2   2023-10-30 [1] CRAN (R 4.3.2)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# P xml2           1.3.4   2023-04-27 [?] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/07866f9d
# 
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────────────
# > 
    