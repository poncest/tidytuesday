 
## Challenge: #TidyTuesday 2023 week 04
## Data:      Alone (tv show) data
## Author:    Steven Ponce
## Date:      2023-01-23
 

## 1. LOAD PACKAGES & SETUP ----  
library(pacman)   
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, skimr, glue, camcorder, scales)
p_load(tidytext, textdata, ggwordcloud)


#  NRC lexicon (If needed)
# library(remotes)
# install_github("EmilHvitfeldt/textdata")
# install_github("juliasilge/tidytext")


# figure size
gg_record(
    dir    = here("temp_plots"),
    device = "png",
    width  = 8,
    height = 6,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 04) 

episodes     <- tt$episodes %>% clean_names()

# survivalists <- tt$survivalists %>% clean_names()
# loadouts     <- tt$loadouts %>% clean_names()
# seasons      <- tt$seasons %>% clean_names()

readme(tt) 
rm(tt)   


## 3. EXAMINING THE DATA ----
glimpse(episodes) 


## 4. TIDYDATA ---- 

# tidy text
tidy_quotes <- episodes %>% 
    select(season, episode, title, quote) %>% 
    group_by(season) %>% 
    mutate(line_number = row_number()) %>% 
    ungroup() %>% 
    unnest_tokens(word, quote) %>% 
    anti_join(stop_words) %>% 
    count(word, sort = TRUE) %>% 
    mutate(proportion = n / sum(n))

# sentiments
quotes_sentiments <- tidy_quotes %>% 
    inner_join(get_sentiments("nrc")) %>% 
    count(word, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
    mutate(sentiment = positive - negative)

# tidy episodes
episodes_tidy <- episodes %>% 
    select(season, episode, title, quote) %>% 
    group_by(season) %>% 
    mutate(line_number = row_number()) %>% 
    ungroup() %>% 
    unnest_tokens(word, quote) %>% 
    anti_join(stop_words)

# worldcloud
wordcloud_df <- episodes_tidy %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) 


# 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 

bkg_col      <- "#D44D5C"
title_col    <-  "white"              
subtitle_col <-  "white" 
caption_col  <-  "white" 
col_palette  <- c("negative" = "#0B0014", "positive" = "#F5E9E2")


# |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 04 &bull; Source: alone data package<br>")
tw <- str_glue("<span style='font-family:fa-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")

title_text <- str_glue("Alone")

subtitle_text <- str_glue("Sentiment Analysis episode quotes for seasons 1 - 9") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 | {gh} poncest | Tools: #rstats #ggplot")


# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Rammetto One", family = "title")                          
font_add_google("Sniglet", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)      
                

# |-  main plot ----
wordcloud_df %>% 
    ggplot() + 
    
    # geom
    geom_text_wordcloud_area(aes(label = word, size = n, color = sentiment)) +
    
    # scales
    scale_size_area(max_size = 20) +                    
    scale_color_manual(values = col_palette) +
    
    # labs
    labs(x = "", y = "",
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text
    ) +
    
    # theme
    theme_minimal(
        base_size   = 16,
        base_family = 'text'
    ) +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',

        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        axis.text          = element_text(colour = "white"),

        axis.title.x       = element_text(colour = "white"),
        axis.title.y       = element_text(colour = "white", angle = 0, vjust = 0.85, hjust = 0.5),
        
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_blank(),

        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 50,  
            margin         = margin(t = 5, b = 5)),
        
        plot.subtitle      = element_text(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.8, 
            face           = "bold",
            size           = 22,
            margin         = margin(b = 10)),
        
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
# [1] ggwordcloud_0.5.0  textdata_0.4.4     tidytext_0.4.1     scales_1.2.1       camcorder_0.1.0    glue_1.6.2        
# [7] skimr_2.1.5        here_1.0.1         janitor_2.1.0      showtext_0.9-5     showtextdb_3.0     sysfonts_0.8.8    
# [13] ggtext_0.1.2       tidytuesdayR_1.0.2 forcats_0.5.1      stringr_1.4.0      dplyr_1.0.9        purrr_0.3.4       
# [19] readr_2.1.2        tidyr_1.2.0        tibble_3.1.7       ggplot2_3.3.6      tidyverse_1.3.2    pacman_0.5.1       

# loaded via a namespace (and not attached):
# [1] fs_1.6.0            usethis_2.1.6       bit64_4.0.5         lubridate_1.8.0     httr_1.4.3          rprojroot_2.0.3    
# [7] SnowballC_0.7.0     repr_1.1.4          tools_4.2.2         backports_1.4.1     utf8_1.2.2          R6_2.5.1           
# [13] DBI_1.1.3           colorspace_2.0-3    withr_2.5.0         tidyselect_1.2.0    bit_4.0.5           curl_4.3.2         
# [19] compiler_4.2.2      textshaping_0.3.6   cli_3.6.0           rvest_1.0.2         xml2_1.3.3          labeling_0.4.2     
# [25] rappdirs_0.3.3      systemfonts_1.0.4   digest_0.6.29       svglite_2.1.1       base64enc_0.1-3     pkgconfig_2.0.3    
# [31] htmltools_0.5.2     dbplyr_2.2.1        fastmap_1.1.0       rlang_1.0.6         readxl_1.4.0        rstudioapi_0.13    
# [37] farver_2.1.1        generics_0.1.3      jsonlite_1.8.0      vroom_1.6.1         tokenizers_0.3.0    googlesheets4_1.0.0
# [43] magrittr_2.0.3      Matrix_1.4-1        Rcpp_1.0.10         munsell_0.5.0       fansi_1.0.4         lifecycle_1.0.3    
# [49] stringi_1.7.12      snakecase_0.11.0    grid_4.2.2          parallel_4.2.2      crayon_1.5.2        lattice_0.20-45    
# [55] haven_2.5.0         gridtext_0.1.4      hms_1.1.2           magick_2.7.3        knitr_1.39          pillar_1.8.1       
# [61] markdown_1.1        reprex_2.0.1        gifski_1.6.6-1      renv_0.15.5         modelr_0.1.8        vctrs_0.5.2        
# [67] png_0.1-7           tzdb_0.3.0          selectr_0.4-2       cellranger_1.1.0    gtable_0.3.0        assertthat_0.2.1   
# [73] xfun_0.31           broom_1.0.0         rsvg_2.3.1          janeaustenr_1.0.0   ragg_1.2.2          googledrive_2.0.0  
# [79] gargle_1.2.0        ellipsis_0.3.2      

# RStudio 2022.12.0+353 "Elsbeth Geranium" Release (7d165dcfc1b6d300eb247738db2c7076234f6ef0, 2022-12-03) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2022.12.0+353 Chrome/102.0.5005.167 Electron/19.1.3 Safari/537.36

