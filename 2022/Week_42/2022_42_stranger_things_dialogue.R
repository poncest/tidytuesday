 
## Challenge: #TidyTuesday 2022 week 42
## Data:      Stranger things dialogue
## Author:    Steven Ponce
## Date:      2022-10-18   
 

## 1. LOAD PACKAGES & SETUP ----  
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, skimr, glue, camcorder, scales)
p_load(tidytext, stopwords, udpipe)


gg_record(
    dir    = here("temp_plots"),
    device = "png",
    width  = 12,
    height = 10,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 42) 
episodes <- tt$episodes %>% clean_names()
dialogue <- tt$stranger_things_all_dialogue %>% clean_names()
readme(tt) 
rm(tt)  


#|- offline ----   
# write_csv(dialogue, '2022/Week_42/dialogue.csv')
# dialogue <- read_csv(file = '2022/Week_42/dialogue.csv') %>% clean_names()

## 3. EXAMINING THE DATA ----
glimpse(dialogue) 
skim(dialogue)


## 4. TIDYDATA ---- 

dialogue_df <- dialogue %>% 
    select(season:line, dialogue) %>% 
    drop_na(dialogue)    


# Stopwords - https://github.com/quanteda/stopwords/
stopwords <- stopwords(language = "en", source = "stopwords-iso")

# Tokenization - https://bookdown.org/psonkin18/berkshire/tokenize.html
dialogue_words <- dialogue_df %>% 
    unnest_tokens(input = dialogue, output = word, token = "words") %>% 
    filter(!grepl('[0-9]', word)) %>%   # remove numbers
    filter(!word %in% stopwords)        # remove stopwords
    
tibble(dialogue_words) %>% 
    count(word, sort = TRUE)

# Unique 
dialogue_words_unique <- unique(dialogue_words$word)

# Download language model - https://bnosac.github.io/udpipe/docs/doc0.html
udmodel_en <- udpipe_download_model(language = "english-ewt")
udmodel_en <- udpipe_load_model(file = udmodel_en$file_model)

# Annotate words to identify adjectives
words_annotated <- udpipe_annotate(object = udmodel_en, x = dialogue_words_unique, tokenizer = "tokenizer")
words_annotated <- as.data.frame(words_annotated)
adjectives      <- subset(words_annotated, upos == "ADJ")  # words_annotated$upos %>% unique()


# Top 15 adjectives 1702 - 851
top_15_adj <- dialogue_words %>% 
    semi_join(adjectives, by = c("word" = "token")) %>%
    filter(word != "max") %>%                       # remove character name
    
    group_by(word) %>% 
    add_count(word, name = "word_total") %>% 
    ungroup() %>% 
    
    nest(data = -c(word, word_total)) %>% 
    slice_max(word_total, n = 15) %>%      
    unnest(cols = data) %>% 
    
    mutate(
        word = str_to_title(word),
        word = as_factor(word),
    )
    

 
# 5. VISUALIZATION ---- 
# |- plot aesthetics ----  
bkg_col      <- "gray15"  
grid_col     <- "gray90"
text_col     <- "gray90"
col_1        <- "#C50505"
col_2        <- "#D2D7C3"
title_col    <- "#C50505"
subtitle_col <- "white"
caption_col  <- "white"
              

# |-  titles and caption ----
title_text    <- "Stranger Things Dialogue<br>Seasons 1 - 4"

subtitle_text <- "Top-15 adjectives count"

caption_text  <- str_glue("#TidyTuesday: 2022 Week 42 &bull; Source: 8flix.com<br>",
                          "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                          "<span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                          "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                          "<span style='font-family:fa-brands'>&#x23;</span> ggplot")

# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Poller One", family = "title")                          
font_add_google("Ubuntu Mono", family = "subtitle")                     
font_add_google("Ubuntu Mono", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |- plot ---- 

ggplot() +

   # geometries
    geom_point(data = top_15_adj,
               aes(x = word_total, y = word, size = word_total),
               color = col_1, shape = 21, stroke = 2, alpha = 1) + 
    
    geom_point(data = top_15_adj,
               aes(x = word_total, y = word, size = word_total),
               fill = col_2, shape = 21, alpha = 1) +
 
    # scales
    scale_size_continuous(range = c(1, 12)) +
    
    scale_x_continuous(breaks = seq(50, 175, by = 25),
                       limits = c(50, 175))+
    
    scale_y_discrete(expand = c(0.08, 0.08)) +
    
    coord_cartesian(clip = "off", expand = TRUE) +
    
    # labs
    labs(
        x        = "", 
        y        = "",
        title    = toupper(title_text),
        subtitle = toupper(subtitle_text),
        caption  = caption_text) +
    
    # guides
    guides(size = "none",
           color = guide_legend(override.aes = list(size = 6))) +

    # theme
    theme_minimal(base_size   = 16,
                  base_family = 'text') +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background     = element_rect(fill = bkg_col, color = bkg_col),
        panel.background    = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title          = element_blank(),
        axis.text           = element_text(size = 40),
        
        panel.grid.minor.x  =  element_blank(),
        panel.grid.minor.y  = element_blank(),

        panel.grid.major.x = element_line(linetype = "dotted", size = 0.015, color = 'gray'),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.015, color = 'gray'),
        
        axis.text.y        = element_text(color = text_col, family = "text", size = 16),
        axis.text.x        = element_text(color = text_col, family = "text", size = 16),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.title         = element_markdown(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 40,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_markdown(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.6, 
            face           = "bold",
            size           = 30,
            margin         = margin(t = 10, b = 30)),
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 12,
            hjust          = 0.5,
            margin         = margin(t = 20, b = 10)),
    )


## 6. SESSION INFO ---- 

# version.string R version 4.2.1 (2022-06-23 ucrt)
# nickname: Funny-Looking Kid 
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)

# Matrix products: default
 
# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    

# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     

# other attached packages:
# [1] udpipe_0.8.9         stopwords_2.3        tidytext_0.3.3       scales_1.2.1         camcorder_0.0.2.9000
# [6] glue_1.6.2           skimr_2.1.4          here_1.0.1           janitor_2.1.0        showtext_0.9-5      
# [11] showtextdb_3.0       sysfonts_0.8.8       ggtext_0.1.1         tidytuesdayR_1.0.2   forcats_0.5.1       
# [16] stringr_1.4.0        dplyr_1.0.9          purrr_0.3.4          readr_2.1.2          tidyr_1.2.0         
# [21] tibble_3.1.7         ggplot2_3.3.6        tidyverse_1.3.1      pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] httr_1.4.3        jsonlite_1.8.0    modelr_0.1.8      assertthat_0.2.1  pdftools_3.3.0    askpass_1.1      
# [7] renv_0.15.5       cellranger_1.1.0  qpdf_1.2.0        pillar_1.8.1      backports_1.4.1   lattice_0.20-45  
# [13] digest_0.6.29     gridtext_0.1.4    rvest_1.0.2       snakecase_0.11.0  colorspace_2.0-3  htmltools_0.5.2  
# [19] Matrix_1.4-1      pkgconfig_2.0.3   broom_1.0.0       gifski_1.6.6-1    haven_2.5.0       magick_2.7.3     
# [25] tzdb_0.3.0        generics_0.1.3    usethis_2.1.6     ellipsis_0.3.2    withr_2.5.0       repr_1.1.4       
# [31] cli_3.3.0         magrittr_2.0.3    crayon_1.5.1      readxl_1.4.0      tokenizers_0.2.1  janeaustenr_0.1.5
# [37] fs_1.5.2          fansi_1.0.3       SnowballC_0.7.0   xml2_1.3.3        data.table_1.14.2 tools_4.2.1      
# [43] hms_1.1.1         lifecycle_1.0.1   munsell_0.5.0     reprex_2.0.1      compiler_4.2.1    rlang_1.0.4      
# [49] grid_4.2.1        rstudioapi_0.13   rsvg_2.3.1        base64enc_0.1-3   gtable_0.3.0      DBI_1.1.3        
# [55] curl_4.3.2        R6_2.5.1          lubridate_1.8.0   knitr_1.39        fastmap_1.1.0     utf8_1.2.2       
# [61] rprojroot_2.0.3   stringi_1.7.8     Rcpp_1.0.9        vctrs_0.4.1       dbplyr_2.2.1      tidyselect_1.1.2 
# [67] xfun_0.31 


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36 


