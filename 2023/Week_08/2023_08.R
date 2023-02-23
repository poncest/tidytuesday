 
## Challenge: #TidyTuesday 2023 week 08
## Data:      Bob Ross Paintings
## Author:    Steven Ponce
## Date:      2023-02-20


## 1. LOAD PACKAGES & SETUP ----  
library(pacman)   
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, glue, camcorder, scales)
p_load(tidytext, textdata, patchwork)

 
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
tt <- tidytuesdayR::tt_load(2023, week = 08) 

bob_ross <- tt$bob_ross %>% clean_names()

readme(bob_ross) 
rm(tt)   
 

## 3. EXAMINING THE DATA ----
glimpse(bob_ross) 


## 4. TIDYDATA ----  

# Painting Titles
paintings_title_tbl <- bob_ross %>% 
    select(painting_title:episode) %>% 
    group_by(season) %>% 
    mutate(linenumber = row_number()) %>% 
    ungroup()

# Tidy Painting Titles
tidy_paintings_title_tbl <- paintings_title_tbl %>% 
    unnest_tokens(word, painting_title, token = "words")

# stop words
data(stop_words)

tidy_paintings_title_tbl <- tidy_paintings_title_tbl %>% 
anti_join(stop_words)
    
# word counts
word_counts <- tidy_paintings_title_tbl %>% 
    count(word,sort = TRUE) 


# a) Most commons words in Bob Ross paining titles ----
most_common_words_tbl <- tidy_paintings_title_tbl %>% 
    count(word, sort = TRUE) %>% 
    filter(n > 10) %>% 
    mutate(word = reorder(word, n)) %>% 
    mutate(bar_label = str_glue("{n}"))


# b) Sentiment Analysis ----
sentiment_analysis_word_contribution <- tidy_paintings_title_tbl  %>% 
    inner_join(get_sentiments("afinn"), by = "word") %>% 
    group_by(word) %>% 
    summarize(
        occurences = n(), 
        contribution = sum(value)
        ) %>% 
    ungroup() %>% 
    mutate(method = "AFINN") %>% 
    mutate(word = reorder(word, contribution)) %>% 
    mutate(sentiment = ifelse(contribution > 0, "positive", "negative")) %>% 
    mutate(bar_label = str_glue("{contribution}"))



# 5. VISUALIZATION ---- 
### |- plot aesthetics ---- 
bkg_col      <- "#FCFCFC"
title_col    <- "gray10"              
subtitle_col <- "gray10" 
caption_col  <- "gray10" 
palette_col  <- c("positive" = "#9BBF85", "negative" = "#B3589A")  



### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 08 &bull; Source: Bob Ross Paintings<br>")
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text <- str_glue("Text Analysis of<br>Ross Painting Titles,<br>Seasons 1-31") 

subtitle_text <- str_glue("Words Contributing to<br><span style='font-size:12pt; color:#9BBF85'>**Positive**</span> / <span style='font-size:12pt; color:#B3589A'>**Negative**</span> Scores") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social)<br>Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Fira Sans", family = "title") 
font_add_google("Fira Sans", family = "subtitle") 
font_add_google("Roboto", family = "text")  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)     
                

# |- most common words ----
word_bar_plot <- most_common_words_tbl %>% 
    ggplot(aes(word, n)) +
    
    # geoms
    geom_col(fill = "#beb5b1", show.legend = FALSE) +
    geom_text(aes(label = bar_label, hjust= -0.25), 
              family='text', 
              color = '#949494', size = 3) +

    # scale
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(0, 50, by = 10),
                       limits = c(0, 50)) +
    coord_flip(clip = "off")+
    scale_fill_manual(values = palette_col)+ 
    
    #labs
    labs(
        x = "", y = "Word Count",
        title    = "The Most Common Words",
        subtitle = "")+
    
    # theme
    theme_minimal(12, base_family = 'text') +
    theme(
        
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
        
        legend.title          = element_blank(),
        legend.text           = element_text(size = 12),
        legend.background     = element_rect(fill = bkg_col, color = bkg_col),
        
        panel.grid            = element_blank(),
        axis.line.x           = element_line(color = "gray40", linewidth = 0.2),
        axis.ticks.x          = element_line(color = "gray40"),
        
        plot.title  = element_text(
            family     = 'subtitle',
            color      = title_col,
            face       = "bold",
            size       = 12,
            lineheight = 0.5,
            margin     = margin(t = 5)),
    ) 


# |- sentiment plot ----
sentiment_bar_plot <- sentiment_analysis_word_contribution %>% 
    ggplot(aes(word, contribution, fill = sentiment)) + 
    
    # geoms
    geom_col(show.legend = FALSE, width = 0.9) + 
    
    # scale
    scale_x_discrete(expand = c(0,.85)) +
    scale_y_continuous(breaks = seq(-5, 15, by = 5),
                       limits = c(-5, 15)) +
    scale_fill_manual(values = palette_col)+
    coord_flip(clip = "off") +         
    
    #labs
    labs(
        x = "", y = "Contribution Score",
        title    = "",
        subtitle = subtitle_text)+
    
    # theme
    theme_minimal(12, base_family = 'text') +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
        
        legend.title          = element_blank(),
        legend.text           = element_text(size = 12),
        legend.background     = element_rect(fill = bkg_col, color = bkg_col),
        
        panel.grid            = element_blank(),
        axis.line.x           = element_line(color = "gray40", linewidth = 0.2),
        axis.ticks.x          = element_line(color = "gray40"),
        
        plot.subtitle  = element_markdown(
            family     = 'title',
            color      = title_col,
            face       = "bold",
            size       = 12,
            lineheight = 0.5,
            margin     = margin(b = 5)),
    ) 


# |- title plot ----
text <- ggplot()+
    theme_void()+
    
    # title
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'title',
                 label       = title_text,
                 color       = title_col,
                 size        = 7,             
                 fill        = NA,     
                 box.color   = NA,
                 width       = unit(8, "in"),
                 vjust       = 0,
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 fontface    ="bold", 
                 lineheight  = 1) +

    # caption
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'caption',
                 label       = caption_text,
                 color       = title_col,
                 size        = 3,           
                 fill        = NA, 
                 box.color   = NA,
                 vjust       = 2, 
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 fontface    = 'plain',
                 width       = unit(8, "in"),
    ) +
    
    # labs
    labs(
        title     <-  title_text,
        #subttitle <- subtitle_text,
        caption   <- caption_text 
    ) +
    
    coord_cartesian(clip = "off") +
    
    # theme
    theme(
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        
        plot.title     = element_markdown(
            family     = 'title',
            color      = title_col,
            face       = "bold",
            size       = 5,
            lineheight = 0.5,
            margin     = margin(t = 5)),
        
        plot.caption   = element_markdown(
            family     = 'caption',
            color      = title_col,
            face       = "bold",
            size       = 4,
            lineheight = 0.5,
            margin     = margin(t = 5))
    )
 


# | - putting all plots together ----
(text / word_bar_plot) |sentiment_bar_plot 




## 6. SESSION INFO ---- 

# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)

# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    

# other attached packages:
# [1] patchwork_1.1.1    textdata_0.4.4     tidytext_0.4.1     scales_1.2.1       camcorder_0.1.0    glue_1.6.2        
# [7] here_1.0.1         janitor_2.1.0      showtext_0.9-5     showtextdb_3.0     sysfonts_0.8.8     ggtext_0.1.2      
# [13] tidytuesdayR_1.0.2 forcats_0.5.1      stringr_1.5.0      dplyr_1.1.0        purrr_1.0.1        readr_2.1.2       
# [19] tidyr_1.3.0        tibble_3.1.8       ggplot2_3.4.1      tidyverse_1.3.2    pacman_0.5.1      

# loaded via a namespace (and not attached):
# [1] fs_1.6.0            usethis_2.1.6       lubridate_1.8.0     bit64_4.0.5         httr_1.4.3         
# [6] rprojroot_2.0.3     SnowballC_0.7.0     tools_4.2.2         backports_1.4.1     utf8_1.2.3         
# [11] R6_2.5.1            DBI_1.1.3           colorspace_2.1-0    withr_2.5.0         tidyselect_1.2.0   
# [16] bit_4.0.5           curl_5.0.0          compiler_4.2.2      textshaping_0.3.6   cli_3.6.0          
# [21] rvest_1.0.2         xml2_1.3.3          labeling_0.4.2      rappdirs_0.3.3      systemfonts_1.0.4  
# [26] svglite_2.1.1       pkgconfig_2.0.3     dbplyr_2.2.1        rlang_1.0.6         readxl_1.4.0       
# [31] rstudioapi_0.13     farver_2.1.1        generics_0.1.3      jsonlite_1.8.0      vroom_1.6.1        
# [36] tokenizers_0.3.0    googlesheets4_1.0.0 magrittr_2.0.3      Matrix_1.4-1        Rcpp_1.0.10        
# [41] munsell_0.5.0       fansi_1.0.4         lifecycle_1.0.3     stringi_1.7.12      snakecase_0.11.0   
# [46] grid_4.2.2          parallel_4.2.2      crayon_1.5.2        lattice_0.20-45     haven_2.5.0        
# [51] gridtext_0.1.4      hms_1.1.2           magick_2.7.3        pillar_1.8.1        markdown_1.1       
# [56] reprex_2.0.1        gifski_1.6.6-1      renv_0.15.5         modelr_0.1.8        vctrs_0.5.2        
# [61] tzdb_0.3.0          selectr_0.4-2       cellranger_1.1.0    gtable_0.3.1        assertthat_0.2.1   
# [66] broom_1.0.0         rsvg_2.3.1          janeaustenr_1.0.0   ragg_1.2.2          googledrive_2.0.0  
# [71] gargle_1.2.0        ellipsis_0.3.2      

# RStudio 2022.12.0+353 "Elsbeth Geranium" Release (7d165dcfc1b6d300eb247738db2c7076234f6ef0, 2022-12-03) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2022.12.0+353 Chrome/102.0.5005.167 Electron/19.1.3 Safari/537.36

