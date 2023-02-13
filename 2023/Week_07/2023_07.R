 
## Challenge: #TidyTuesday 2023 week 07
## Data:      Hollywood Age Gaps
## Author:    Steven Ponce
## Date:      2023-02-13
 

## 1. LOAD PACKAGES & SETUP ----  
library(pacman)   
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, glue, camcorder, scales)

 
# figure size
gg_record(
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 8,
    height = 12,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 07) 

age_gaps <- tt$age_gaps %>% clean_names()

readme(tt) 
rm(tt)   


## 3. EXAMINING THE DATA ----
glimpse(age_gaps) 


## 4. TIDYDATA ----  

# James Bond movies
# reference - https://en.wikipedia.org/wiki/List_of_James_Bond_films
jb_movies_list <- c("Dr. No", "From Russia with Love", "Goldfinger", "Thunderball", "You Only Live Twice", "On Her Majesty's Secret Service", "Diamonds Are Forever", "Live and Let Die", "The Man with the Golden Gun", "The Spy Who Loved Me", "Moonraker", "For Your Eyes Only", "Octopussy", "A View to a Kill", "The Living Daylights", "Licence to Kill", "GoldenEye", "Tomorrow Never Dies", "The World Is Not Enough", "Die Another Day", "Casino Royale", "Quantum of Solace", "Skyfall", "Spectre", "No Time to Die")


jb_movies_tbl <- age_gaps %>% 
    # select James Bond movies
    filter(movie_name %in% jb_movies_list) %>% 
    # sort by year
    arrange(release_year) %>% 
    # select specific columns
    select(-c(actor_1_birthdate, actor_2_birthdate, character_1_gender, character_2_gender)) %>% 
    # add decade column
    mutate(decade = paste0((release_year - 1) %/% 10 * 10, "'s")) %>% 
    # factor reorder by release_year
    mutate(movie_name = fct_reorder(movie_name, release_year, .desc = TRUE)) %>% 
    # label (year + movie title)
    mutate(year_movie_title = str_glue("{movie_name}, {release_year}")) %>% 
    # factor reorder by release_year
    mutate(year_movie_title = fct_reorder(year_movie_title, release_year, .desc = TRUE)) 


# mean age diff by decade   
jb_movies_mean_age_diff_tbl <- jb_movies_tbl %>% 
    group_by(decade) %>% 
    summarise_at(vars(age_difference), list(mean_age_diff = mean)) %>% 
    ungroup()
    
# data for plot
data_plot <- jb_movies_tbl %>% 
    left_join(y = jb_movies_mean_age_diff_tbl, by = "decade") %>% 
    select(movie_name:age_difference, mean_age_diff, decade, everything()) %>% 
    # highlight color
    mutate(highlight = ifelse(age_difference > mean_age_diff, "Above Average", "Below Avarage"))


# JB movies count  
jb_movies_count <- jb_movies_tbl %>% 
    group_by(movie_name, release_year) %>% 
    count(movie_name) %>% distinct(movie_name) %>% 
    ungroup() %>% 
    arrange(release_year)


# 5. VISUALIZATION ---- 
### |- plot aesthetics ---- 
bkg_col      <- "#FFF4ED"
title_col    <- "gray10"              
subtitle_col <- "gray10" 
caption_col  <- "gray10" 
palette_col  <- c("Below Avarage" = "#A89580", "Above Average" = "#534A3F" )    


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 07 &bull; Source: Hollywood Age Gap<br>")
tw <- str_glue("<span style='font-family:fa6-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text <- str_glue("Age Gap in James Bond Movies, 1962-2015")      

subtitle_text <- str_glue("Each dot represents the age difference for a pair of actors.") 

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social) &bull; Code: {gh} poncest &bull; Tools: #rstats #ggplot")


### |-  fonts ----
font_add('fa6-brands', 'fonts/fa6-brands-regular-400.otf')
font_add_google("Alata", family = "title") 
font_add_google("Alata", family = "subtitle") 
font_add_google("Roboto Condensed ", family = "text")              
font_add_google("PT Sans Narrow", family = "caption")  
showtext_auto(enable = TRUE)     
                

### |-  main plot ----
p1 <- 
    data_plot %>% 
    ggplot(aes(movie_name, age_difference, color = highlight)) + 
    
    # geoms
    geom_point(size = 3.5)+
    geom_hline(aes(yintercept = `mean_age_diff`), color = 'gray', size = 0.6)+ 
    
    # scale
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(0, 30, by = 5),
                       limits = c(0, 30)) +
    
    scale_color_manual(values = palette_col, name = "") +
    coord_flip(clip = 'off')+
    
    # labs
    labs(x = "", y = "Age Difference",
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text
         ) +
    
    # facet
    facet_grid(rows = vars(decade), scales = 'free_y', space = 'free_y') + 
    
    # theme  
    theme_minimal(12, base_family = 'text')+ 
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",

        legend.position       = 'top', 
        legend.justification  = c(-2.7,-2.7),  
        legend.direction      = 'horizontal', 
        
        axis.title            = element_text(size = 16, face = 'bold', margin = margin(t = 10)), 
        axis.text             = element_text(size = 12),
        
        panel.grid            = element_blank(),
        panel.spacing         = unit(2, 'lines'),
        
        axis.line.x           = element_line(color = "black"),
        panel.grid.major.x    = element_line(linetype = "dotted", size = 0.4, color = 'gray'),
        
        strip.text.y          = element_text(size = 14, face = "bold", angle = 0),
        
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin           = margin(t = 10, r = 25, b = 10, l = 25),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 26,  
            margin         = margin(t = 5, b = 5)),
        
        plot.subtitle      = element_text(
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

### |-  Annotated Plot -----
# DF for annotations
arrowdf <- tibble(decade = "1970's", label_text = "Mean age difference for each decade")

p1 +
    # arrows 
    geom_segment(data = arrowdf, 
                 aes(x = 5.5, xend = 4.5, y = 20.090909+5, yend = 20.090909), 
                 color = "gray50", size = 0.8, alpha = 0.9, 
                 arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
                 ) +
    
    # decade mean age difference text
    geom_text(data = arrowdf, 
             aes(x = 6.4, y = 29, 
             label = str_wrap(label_text, width = 20)),
             color = "gray50") 
    

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
# [1] scales_1.2.1       camcorder_0.1.0    glue_1.6.2         here_1.0.1         janitor_2.1.0      showtext_0.9-5    
# [7] showtextdb_3.0     sysfonts_0.8.8     ggtext_0.1.2       tidytuesdayR_1.0.2 forcats_0.5.1      stringr_1.4.0     
# [13] dplyr_1.0.9        purrr_0.3.4        readr_2.1.2        tidyr_1.2.0        tibble_3.1.7       ggplot2_3.3.6     
# [19] tidyverse_1.3.2    pacman_0.5.1      

# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.10         svglite_2.1.1       lubridate_1.8.0     rsvg_2.3.1          assertthat_0.2.1    rprojroot_2.0.3    
# [7] utf8_1.2.2          R6_2.5.1            cellranger_1.1.0    backports_1.4.1     reprex_2.0.1        httr_1.4.3         
# [13] pillar_1.8.1        rlang_1.0.6         googlesheets4_1.0.0 curl_4.3.2          readxl_1.4.0        gifski_1.6.6-1     
# [19] rstudioapi_0.13     magick_2.7.3        googledrive_2.0.0   munsell_0.5.0       gridtext_0.1.4      broom_1.0.0        
# [25] compiler_4.2.2      modelr_0.1.8        systemfonts_1.0.4   pkgconfig_2.0.3     tidyselect_1.2.0    fansi_1.0.4        
# [31] crayon_1.5.2        tzdb_0.3.0          dbplyr_2.2.1        withr_2.5.0         grid_4.2.2          jsonlite_1.8.0     
# [37] gtable_0.3.0        lifecycle_1.0.3     DBI_1.1.3           magrittr_2.0.3      cli_3.6.0           stringi_1.7.12     
# [43] renv_0.15.5         fs_1.6.0            snakecase_0.11.0    xml2_1.3.3          ellipsis_0.3.2      generics_0.1.3     
# [49] vctrs_0.5.2         tools_4.2.2         hms_1.1.2           colorspace_2.0-3    gargle_1.2.0        rvest_1.0.2        
# [55] haven_2.5.0         usethis_2.1.6      

# RStudio 2022.12.0+353 "Elsbeth Geranium" Release (7d165dcfc1b6d300eb247738db2c7076234f6ef0, 2022-12-03) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2022.12.0+353 Chrome/102.0.5005.167 Electron/19.1.3 Safari/537.36

