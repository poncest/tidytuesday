 
## Challenge: #TidyTuesday 2023 week 09
## Data:      African Language Sentiment
## Author:    Steven Ponce
## Date:      2023-02-28


## 1. LOAD PACKAGES & SETUP ----  
library(pacman)   
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, glue, camcorder, scales)
p_load(tidytext, stopwords)

 
# figure size
gg_record(
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 8,
    height = 6,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 09) 

afrisenti <- tt$afrisenti %>% clean_names()

readme(tt) 
rm(tt)   
 

## 3. EXAMINING THE DATA ----
glimpse(afrisenti)  
 

## 4. TIDYDATA ----  

# Stopwords - https://github.com/quanteda/stopwords/
stopwords::stopwords_getlanguages("stopwords-iso")

# stopword
stopwords_yo <- stopwords::stopwords(language = "yo", source = "stopwords-iso") %>% 
    tibble() %>% rename("word" = ".")

stopwords_en <- stopwords::stopwords(language = "en", source = "stopwords-iso") %>% 
    tibble() %>% rename("word" = ".")

tweet_df <- afrisenti %>% 
    # select Yoruba (language)
    filter(language_iso_code == "yor") %>% 
    drop_na(tweet)

# Tokenization
tweet_word <- tweet_df %>% 
    unnest_tokens(input = tweet, output = word, token = "words") %>% 
    # remove numbers
    filter(!grepl('[0-9]', word)) %>%   
    # remove yoruba stopword
    filter(!word %in% stopwords_yo) %>% 
    # remove english stopword
    filter(!word %in% stopwords_en) 

#' Let’s clean up the text a bit using stop words to remove some of 
#' the nonsense “words” leftover from HTML or other character encoding
my_stopwords <- bind_rows(stopwords_yo,
                           stopwords_en,
                           tibble(word = c("user", "rt", "t.co", "https", "the", "http", "á"))
                           )

tweet_word <- tweet_word %>% 
    # remove stopwords using custom stopwords
    anti_join(my_stopwords) 

# word counts (top 10)
tweet_word_tbl <- tibble(tweet_word) %>% 
    count(word, sort = TRUE) %>% 
    top_n(n = 10) %>% 
    rename(yo_word = word) %>% 
    mutate(id = row_number()) %>% 
    cbind(
        # google translation yoruba -> english
        tibble(en_word = c("youruba", "come", "you", "she", "child",
                        "durable", "go", "die", "good", "youruba"))
    ) %>% 
    select(-id, yo_word, en_word, n)



# 5. VISUALIZATION ---- 
### |- plot aesthetics ---- 
bkg_col      <- "#FBF2C0"
yo_col       <- "#48392A"    
en_col       <- "#F96F5D"    

title_col    <- "gray10"              
subtitle_col <- "gray10" 
caption_col  <- "gray10" 


col_1        <- "#a14500"
col_2        <- "#edc841"

        

### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 09 &bull; Source: AfriSenti: Sentiment Analysis dataset for 14 African languages<br>")  
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text <- str_glue("Yoruba Top 10 Words from Twitter Sentiment Analysis") 

subtitle_text <- str_glue("The <span style='font-size:12pt; color:gray10'>**AfriSenti-SemEval Shared Task 12**</span> dataset involves tweets labeled with three sentiment classes<br>(positive, negative, neutral) in 14 African languages. Each tweet is annotated by three annotators<br>following the annotation guidelines in (Mohammad, Saif M, 2016).") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Mochiy Pop One", family = "title") 
font_add_google("Lato", family = "subtitle")                    
font_add_google("Roboto", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     
                

### |-  Main Plot ----
tweet_word_tbl %>% 
    ggplot(aes(x =  n,
               y = as_factor(yo_word) %>% rev(),
               size = n)) +
    
    # geoms
    geom_point(color = col_1, shape = 21, stroke = 1.5, alpha = 1)+
    geom_point(fill = col_2, shape = 21, alpha = 1) +
    
    # Yoruba words
    geom_text(aes(x = n - 300, y = as_factor(yo_word) %>% rev(),
                  label = str_glue("{yo_word}"),
                  lineheight = 0.8), 
              size = 5, family = 'text', color = yo_col)+
    
    # English translation
    geom_text(aes(x = n + 300, y = as_factor(yo_word) %>% rev(),
                 label = str_glue("{en_word}"),
                 lineheight = 0.8), 
              size = 5, family = 'text', color = en_col)+
    
    # annotations
    annotate("text", x = 800, y = 9,
             hjust    = 0.5, 
             vjust    = 0,
             color    = yo_col,
             family   = 'text',
             fontface = "bold",
             size     = 6,
             label    = str_glue("Yoruba\nWords")) +
    
    annotate("text", x = 3600, y = 2,
             hjust    = 0.5, 
             vjust    = 0,
             color    = en_col,
             family   = 'text',
             fontface = "bold",
             size     = 6,
             label    = str_glue("English\nTranslation")) +

    # scales
    scale_size_continuous(range = c(1, 10)) +               
    
    scale_x_continuous(breaks = seq(500, 4000, by = 500),   
                       limits = c(600, 4000))+
    
    scale_y_discrete(expand = c(0.06, 0.06)) +   
    
    coord_cartesian(clip = "off", expand = TRUE) +
    
    # labs
    labs(
        x        = "Word Count", 
        y        = "",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text)+
    
    # theme
    theme_minimal(base_size   = 10,
                  base_family = 'text') +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background     = element_rect(fill = bkg_col, color = bkg_col),
        panel.background    = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title          = element_text(size = 14, face = 'bold', margin = margin(t = 20, b = 5)), 
        
        axis.text.y         = element_blank(),
        axis.text.x         = element_text(family = "text", size = 12),
        
        axis.line.x         = element_line(color = "gray40", linewidth = 0.2),
        axis.ticks.x        = element_line(color = "gray40"),
        
        panel.grid.minor    = element_blank(),
        panel.grid.major.x  = element_blank(),
        panel.grid.major.y  = element_line(linetype = "dotted", linewidth = 0.015, color = 'gray'),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.title         = element_markdown(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 16,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_markdown(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.95, 
            size           = 12,
            margin         = margin(t = 10, b = 30)),  
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col,
            lineheight     = 0.6,
            size           = 10,
            hjust          = 0.5,
            halign         = 0.5,
            margin         = margin(t = 10, b = 5)),
    )

 

## 6. SESSION INFO ---- 

# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)

# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    

# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     

# other attached packages:
# [1] stopwords_2.3      tidytext_0.4.1     scales_1.2.1       camcorder_0.1.0    glue_1.6.2         here_1.0.1        
# [7] janitor_2.1.0      showtext_0.9-5     showtextdb_3.0     sysfonts_0.8.8     ggtext_0.1.2       tidytuesdayR_1.0.2
# [13] forcats_0.5.1      stringr_1.5.0      dplyr_1.1.0        purrr_1.0.1        readr_2.1.2        tidyr_1.3.0       
# [19] tibble_3.1.8       ggplot2_3.4.1      tidyverse_1.3.2    pacman_0.5.1      

# loaded via a namespace (and not attached):
# [1] httr_1.4.3          bit64_4.0.5         vroom_1.6.1         jsonlite_1.8.0      modelr_0.1.8       
# [6] assertthat_0.2.1    selectr_0.4-2       renv_0.15.5         googlesheets4_1.0.0 cellranger_1.1.0   
# [11] pillar_1.8.1        backports_1.4.1     lattice_0.20-45     gridtext_0.1.4      rvest_1.0.2        
# [16] snakecase_0.11.0    colorspace_2.1-0    Matrix_1.4-1        pkgconfig_2.0.3     broom_1.0.0        
# [21] gifski_1.6.6-1      haven_2.5.0         magick_2.7.3        svglite_2.1.1       tzdb_0.3.0         
# [26] googledrive_2.0.0   farver_2.1.1        generics_0.1.3      usethis_2.1.6       ellipsis_0.3.2     
# [31] withr_2.5.0         cli_3.6.0           magrittr_2.0.3      crayon_1.5.2        readxl_1.4.0       
# [36] tokenizers_0.3.0    janeaustenr_1.0.0   fs_1.6.0            fansi_1.0.4         SnowballC_0.7.0    
# [41] xml2_1.3.3          textshaping_0.3.6   tools_4.2.2         hms_1.1.2           gargle_1.2.0       
# [46] lifecycle_1.0.3     munsell_0.5.0       reprex_2.0.1        compiler_4.2.2      systemfonts_1.0.4  
# [51] rlang_1.0.6         grid_4.2.2          rstudioapi_0.13     rsvg_2.3.1          gtable_0.3.1       
# [56] DBI_1.1.3           curl_5.0.0          markdown_1.1        R6_2.5.1            lubridate_1.8.0    
# [61] bit_4.0.5           utf8_1.2.3          rprojroot_2.0.3     ragg_1.2.2          stringi_1.7.12     
# [66] parallel_4.2.2      Rcpp_1.0.10         vctrs_0.5.2         dbplyr_2.2.1        tidyselect_1.2.0       

# RStudio 2022.12.0+353 "Elsbeth Geranium" Release (7d165dcfc1b6d300eb247738db2c7076234f6ef0, 2022-12-03) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2022.12.0+353 Chrome/102.0.5005.167 Electron/19.1.3 Safari/537.36

