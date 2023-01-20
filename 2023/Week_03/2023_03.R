 
## Challenge: #TidyTuesday 2023 week 03
## Data:      Art history data
## Author:    Steven Ponce
## Date:      2023-01-16
 

## 1. LOAD PACKAGES & SETUP ----  
library(pacman)   
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, skimr, glue, camcorder, scales)
p_load(MetBrewer, ggbeeswarm)

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
tt <- tidytuesdayR::tt_load(2023, week = 03) 
artists <- tt$artists %>% clean_names()

readme(tt) 
rm(tt)  


#|- offline ----   
# write_csv(artists, '2023/Week_03/artists.csv')
# artists <- read_csv(file = '2023/Week_03/artists.csv') %>% clean_names()


## 3. EXAMINING THE DATA ----
glimpse(artists) 
colnames(artists)

artists$artist_ethnicity %>% unique()
artists$artist_nationality %>% unique()


## 4. TIDYDATA ---- 
artist_tbl <- artists %>%
    rename_at(vars(matches("artist_")), ~ str_remove(., "artist_")) %>% 
    filter(gender != "N/A") %>% 
    
    group_by(name, nationality, book, gender) %>% 
    summarise(
        count = n(), 
        space_ratio_per_page = sum(space_ratio_per_page_total),
        avg_space_ratio_per_page = sum(space_ratio_per_page_total) / count,
    ) %>% 
    ungroup() %>% 
    arrange(desc(count)) 


# 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 

bkg_col      <- "#1e193c"
title_col    <-  "white"              
subtitle_col <-  "white" 
caption_col  <-  "white" 
col_palette  <- c("Female" = "#FFA630", "Male" = "#A4B494")


# |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 03 &bull; Source: arthistory data package<br>")
tw <- str_glue("<span style='font-family:fa-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")

title_text <- str_glue("Art History")

subtitle_text <- str_glue("There is about 9X more <span style='font-size:22pt; color:#A4B494'>**Male**</span> artist than <span style='font-size:22pt; color:#FFA630'>**Female**</span> artist<br> in Gardner or Janson textbook") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 | {gh} poncest | Tools: #rstats #ggplot")


# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Gravitas One", family = "title")                          
font_add_google("Roboto", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  
 

# |-  main plot ----
p1 <- artist_tbl %>% 
    ggplot(aes(book, avg_space_ratio_per_page, fill = gender)) +
    
    # geometries
    geom_point(aes(size = count), shape = 21, alpha = 0.5, na.rm = TRUE,
                position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1, seed = 123)) +
    
    # scales
    scale_x_discrete()+
    scale_y_continuous(breaks = seq(0, 4, by = 1),
                       limits = c(0, 4)) +
    scale_fill_manual(values = col_palette) +
    coord_flip(clip = 'off') + 
    
    # labs
    labs(x = "", y = "Average space per page (cm2)",
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
        # legend.title          = element_text(color = "white", size = 14, ),
        # legend.text           = element_text(color = "white", size = 14),
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.text        = element_text(colour = "white"),
        # axis.text        = element_blank(),
        
        axis.title.x       = element_text(colour = "white"),
        axis.title.y       = element_text(colour = "white", angle = 0, vjust = 0.85, hjust = 0.5),
        
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_blank(),
        
        plot.margin        = margin(t = 10, r = 10, b = 10, l = 10),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 45,  
            margin         = margin(t = 5, b = 5)),
        
        plot.subtitle      = element_markdown(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.8, 
            face           = "bold",
            size           = 18,
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

# |- Annotated Plot ----
p1 + 
    # annotations + arrows
    # Janson / Picasso
    annotate("curve", x = 1.8, y = 3.1, xend = 2.1, yend = 3.28, 
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"), 
             curvature = 0.3, color = "gray72", size = 0.5) +
    
    annotate("richtext", x = 1.7, y = 2.6, lineheight = 0.9, fill = NA, 
             label.colour = NA, size = 4.1, color = "gray72", family = 'text',
             label = str_glue("Artist: Pablo Picasso<br>",
                              "Appearances: 9<br>",
                              "Avg Area: 3.27 cm2")) +
    
    # Gardner / Dorothea Rockburne
    annotate("curve", x = 0.6, y = 1.1, xend = 0.82, yend = 0.79,
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
             curvature = -0.3, color = "gray72", size = 0.5) +

    annotate("richtext", x = 0.6, y = 1.7, lineheight = 0.9, fill = NA,
             label.colour = NA, size = 4.1, color = "gray72", family = 'text',
             label = str_glue("Artist: Dorothea Rockburne<br>",
                              "Appearances: 1<br>",
                              "Avg Area: 0.74 cm2"))



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
# [1] ggbeeswarm_0.6.0     MetBrewer_0.2.0      scales_1.2.1         camcorder_0.0.2.9000 glue_1.6.2          
# [6] skimr_2.1.4          here_1.0.1           janitor_2.1.0        showtext_0.9-5       showtextdb_3.0      
# [11] sysfonts_0.8.8       ggtext_0.1.1         tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0       
# [16] dplyr_1.0.9          purrr_0.3.4          readr_2.1.2          tidyr_1.2.0          tibble_3.1.7        
# # [21] ggplot2_3.3.6        tidyverse_1.3.1      pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] fs_1.5.2          usethis_2.1.6     lubridate_1.8.0   bit64_4.0.5       httr_1.4.3        rprojroot_2.0.3  
# [7] repr_1.1.4        tools_4.2.2       backports_1.4.1   utf8_1.2.2        R6_2.5.1          vipor_0.4.5      
# [13] DBI_1.1.3         colorspace_2.0-3  withr_2.5.0       tidyselect_1.1.2  bit_4.0.4         curl_4.3.2       
# [19] compiler_4.2.2    textshaping_0.3.6 cli_3.3.0         rvest_1.0.2       xml2_1.3.3        labeling_0.4.2   
# [25] askpass_1.1       systemfonts_1.0.4 digest_0.6.29     base64enc_0.1-3   pkgconfig_2.0.3   htmltools_0.5.2  
# [31] dbplyr_2.2.1      fastmap_1.1.0     rlang_1.0.4       readxl_1.4.0      rstudioapi_0.13   farver_2.1.1     
# [37] generics_0.1.3    jsonlite_1.8.0    vroom_1.5.7       magrittr_2.0.3    Rcpp_1.0.9        munsell_0.5.0    
# [43] fansi_1.0.3       lifecycle_1.0.1   stringi_1.7.8     snakecase_0.11.0  grid_4.2.2        parallel_4.2.2   
# [49] crayon_1.5.1      haven_2.5.0       gridtext_0.1.4    hms_1.1.1         magick_2.7.3      knitr_1.39       
# [55] pillar_1.8.1      markdown_1.1      reprex_2.0.1      pdftools_3.3.0    qpdf_1.2.0        gifski_1.6.6-1   
# [61] renv_0.15.5       modelr_0.1.8      vctrs_0.4.1       tzdb_0.3.0        selectr_0.4-2     cellranger_1.1.0 
# [67] gtable_0.3.0      assertthat_0.2.1  xfun_0.31         broom_1.0.0       rsvg_2.3.1        ragg_1.2.2       
# [73] beeswarm_0.4.0    ellipsis_0.3.2   

# RStudio 2022.12.0+353 "Elsbeth Geranium" Release (7d165dcfc1b6d300eb247738db2c7076234f6ef0, 2022-12-03) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2022.12.0+353 Chrome/102.0.5005.167 Electron/19.1.3 Safari/537.36

