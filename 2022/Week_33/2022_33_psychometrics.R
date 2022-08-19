
## Challenge: #TidyTuesday 2022 week 33
## Data:      Open-Source Psychometrics Project
## Author:    Steven Ponce
## Date:      2022-08-16 


## 1. LOAD PACKAGES & SETUP ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, glue, camcorder, ggimage, cowplot)

gg_record(
    dir    = here("temp_plots"),
    device = "png",
    width  = 20,
    height = 11,
    units  = "in",
    dpi    = 600)

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 33) 

# Focus on Ted Lasso tv series
characters   <- tt$characters   %>% filter(uni_name == 'Ted Lasso') %>% clean_names()
myers_briggs <- tt$myers_briggs %>% filter(uni_name == 'Ted Lasso') %>% clean_names()
psych_stats  <- tt$psych_stats  %>% filter(uni_name == 'Ted Lasso') %>% clean_names() 
rm(tt) 


#|- offline ----   
# write_csv(characters, '2022/Week_33/characters.csv')
# write_csv(myers_briggs, '2022/Week_33/myers_briggs.csv')
# write_csv(psych_stats, '2022/Week_33/psych_stats.csv')
# characters <- read_csv(file = '2022/Week_33/characters.csv') %>% clean_names()


## 3. EXAMINING THE DATA ----
unique(characters$uni_name) %>% sort()
unique(psych_stats$personality) %>% sort()


## 4. TIDYDATA ----

# |- character_mbti function ----

#' Function to extract the Myers Briggs Type Indicator of Ted Lasso characters
#' 
#' @param data           - data tibble e.g myers_briggs
#' @param universe_name  - universe ID, e.g. Ted Lasso
#' @param character_name - character name e.g Coach Beard'
#' 
#' @return tibble with character MBTI personality type
#' 
#' @example 
#' 
#' character_mbti(myers_briggs, 'Ted Lasso', 'Sam Obisanya')
#' 
#' char_id char_name    uni_id uni_name  myers_briggs avg_match_perc number_users
#' <chr>   <chr>        <chr>  <chr>     <chr>                 <dbl>        <dbl>
#'  TL10    Sam Obisanya TL     Ted Lasso ENFJ                   70.6         3842
#'  

character_mbti <- function(data, universe_name, character_name ) {
    
    mbti <- data %>% 
        filter(
        uni_name  == universe_name,
        char_name == character_name
        ) %>% 
        arrange(desc(avg_match_perc)) %>% 
        slice(1)
    
    return(mbti)
}
    

# |- MBTI of Ted Lasso characters ----
Sam     <- character_mbti(myers_briggs, 'Ted Lasso', 'Sam Obisanya')
Rebecca <- character_mbti(myers_briggs, 'Ted Lasso', 'Rebecca Welton')
Roy     <- character_mbti(myers_briggs, 'Ted Lasso', 'Roy Kent')
Ted     <- character_mbti(myers_briggs, 'Ted Lasso', 'Ted Lasso')
Keeley  <- character_mbti(myers_briggs, 'Ted Lasso', 'Keeley Jones')
Jamie   <- character_mbti(myers_briggs, 'Ted Lasso', 'Jamie Tartt')
Beard   <- character_mbti(myers_briggs, 'Ted Lasso', 'Coach Beard')
Sharon  <- character_mbti(myers_briggs, 'Ted Lasso', 'Dr. Sharon Fieldstone')
Leslie  <- character_mbti(myers_briggs, 'Ted Lasso', 'Leslie Higgins')
Nate    <- character_mbti(myers_briggs, 'Ted Lasso', 'Nathan Shelley')
 

# Combining all mbti in one tibble
mbti <- bind_rows(Sam, Rebecca, Roy, Ted, Keeley, Jamie, Beard, Sharon, Leslie, Nate) %>% 
    select(char_name, myers_briggs, avg_match_perc)


# |- Images of Ted Lasso characters ----
images <- characters %>% 
    select(name, image_link)


# |- Combining mbti and images tibbles ----
mbti_images <- bind_cols(mbti, images) 


# |- Personality traits of Ted Lasso characters ----
questions <- c('active/slothful', 'adventurous/stick-in-the-mud', 'brave/careful', 
               'decisive/hesitant', 'desperate/high standards', 'persistent/quitter')

personality <- psych_stats  %>% 
    filter(question %in% questions) %>% 
    select(char_name, personality, avg_rating)


# |- Data for plot ---

# select specific characters
names <- c('Sam Obisanya', 'Rebecca Welton', 'Ted Lasso', 
               'Jamie Tartt', 'Coach Beard')

# traits
traits <- c('active', 'adventurous', 'brave', 'decisive', 'high standards', 'persistent')


data_plot <- inner_join(x = mbti_images, y = personality)

data_plot <- data_plot %>%  
    filter(personality %in% traits) %>% 
    filter(name %in% names) %>% 
    mutate(facet_title = str_c(name, ' - ', myers_briggs)) 


# remove variables, save memory
rm(Sam, Rebecca, Roy, Ted, Keeley, Jamie, Beard, Sharon, Leslie, Nate,
   mbti, mbti_images, images, personality, characters, myers_briggs, psych_stats)

gc() 
 

# 5. VISUALIZATION ---- 

# |- aesthetics ---- 
bkg_col          <- 'light gray' 
title_col        <- 'black'
subtitle_col     <- "black"
caption_col      <- "black"

palette_col    <- met.brewer("Pillement", n = 6, type = "discrete")
# monochromeR::generate_palette("#A3B9C9", modification = "go_lighter", n_colours = 3, view_palette = T)


# |-  titles and caption ---- 
title_text    <- 'Ted Lasso'

subtitle_text <- str_glue("Myers-Briggs Type Indicator® (MBTI®) - Selected Characters.<br>",
                          "KEY: [E] Extrovert • [I] Introvert • [S] Sensor • [N] Intuitives<br>",
                          "[T] Thinkers • [F] Feelers • [J] Judgers • [P] Perceiver"
                          )

caption_text   <- str_glue("
                         #TidyTuesday: 2022 Week 33 &bull; Source: Open-Source Psychometrics Project<br>",
                           "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                           " <span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                           "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                           "<span style='font-family:fa-brands'>&#x23;</span> ggplot")


# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Cinzel Decorative", family = "title")
font_add_google("Share", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |-  main plot ----
main <- data_plot %>% 
    ggplot(aes(personality, avg_rating, fill = personality)) + 
    
    # geoms
    geom_col() + 
    
    geom_text(aes(personality, avg_rating + 10, label = glue("{round(avg_rating, 0)}%")) ,
              vjust = "outward",
              hjust = "outward",
              size  = 6,
              ) +
    
    geom_image(aes(x = 0.1, y = 0, image = image_link), size = 0.15 ) +

    ylim(-10, 130) + 

    # facet
    facet_wrap( .~ factor(facet_title, levels = c('Ted Lasso - ENFJ', 'Rebecca Welton - ESTJ', 
                                                  'Sam Obisanya - ENFJ', 'Coach Beard - INFJ', 
                                                  'Jamie Tartt - ESTP'))) +  
    
    # scales
    scale_fill_manual(values = palette_col) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    coord_polar() +

    # labs
    labs(
        x        = '', 
        y        = '',
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text) +
    
    # theme
    theme_minimal(
        base_size   = 14,
        base_family = 'text') +
    
    theme(
        plot.title.position   = "panel",
        plot.caption.position = "panel",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        panel.grid         = element_blank(), 

        plot.margin        = margin(t = 5, r = 20, b = 5, l = 5),
        
        strip.text         = element_text(face = 'bold', size = 20, 
                                          hjust = 0.5, margin = margin(b =10)),
        
        axis.title         = element_blank(),
        axis.text.y        = element_blank(),
        
        axis.text.x        = element_blank(),

        panel.spacing.x    = unit(6, 'lines'),
        panel.spacing.y    = unit(3, 'lines'),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 65,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_markdown(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 1.2, 
            face           = "plain",
            size           = 20,
            margin         = margin(t = 20, b = 30)),
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 12,
            hjust          = 0.5,
            margin         = margin(t = 10, b = 10)),
    )
      

# |-  legend plot ----
legend <- data_plot %>% 
    filter(name == 'Ted Lasso') %>% 
    ggplot(aes(personality, avg_rating)) +
    
    # geoms
    geom_col(color = 'black', fill = NA) + 
    
    # annotations for legend
    annotate("text", x = 0, y = 100,
             hjust    = 2, 
             vjust    = 0,
             color    = 'black',
             family   = 'text',
             fontface = "bold",
             size     = 4,                
             label    = 'Persistent') +
    
    annotate("text", x = 0, y = 70,
             hjust    = -2.5, 
             vjust    = 0,
             color    = 'black',
             family   = 'text',
             fontface = "bold",
             size     = 4,
             label    = 'Active') +
    
    annotate("text", x = 0, y = 0,
             hjust    = -1.5, 
             vjust    = 4.8,
             color    = 'black',
             family   = 'text',
             fontface = "bold",
             size     = 4,
             label    = 'Adventurous') +
    
    annotate("text", x = 0, y = 0,
             hjust    = 0, 
             vjust    = 10,
             color    = 'black',
             family   = 'text',
             fontface = "bold",
             size     = 4,
             label    = 'Brave') +
    
    annotate("text", x = 0, y = 0,
             hjust    = 2.5, 
             vjust    = 9,
             color    = 'black',
             family   = 'text',
             fontface = "bold",
             size     = 4,
             label    = 'Decisive') +
    
    annotate("text", x = 0, y = 0,
             hjust    = 2.0, 
             vjust    = 1.8,
             color    = 'black',
             family   = 'text',
             fontface = "bold",
             size     = 4,
             label    = "High Standards") +
    
    ylim(0, 130) + 
    
    # scales
    scale_fill_manual(values = palette_col) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    coord_polar() +
    
    # labs
    labs(
        x        = '', 
        y        = '',
        title    = '',
        subtitle = '',
        caption  = '') +
    
    # theme
    theme_minimal(
        base_size   = 14,
        base_family = 'text') +
    
    theme(

        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        panel.grid         = element_blank(), 
        
        plot.margin        = margin(t = 0, r = 0, b = 0, l = 0),

        axis.title         = element_blank(),
        axis.text          = element_blank(),

    )

# |-  final plot ----

final <- ggdraw(plot = main) +
    
    draw_plot(legend, 
              scale  = 0.5,   
              x      = 0.25, 
              y      = -0.25,
              width  = .9, 
              height = .9 )

final

## 6. SESSION INFO ---- 

# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)

# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     

# other attached packages:
# [1] patchwork_1.1.1      cowplot_1.1.1        ggimage_0.3.1        camcorder_0.0.2.9000 glue_1.6.2           skimr_2.1.4         
# [7] here_1.0.1           janitor_2.1.0        MetBrewer_0.2.0      showtext_0.9-5       showtextdb_3.0       sysfonts_0.8.8      
# [13] ggtext_0.1.1         tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0        dplyr_1.0.9          purrr_0.3.4         
# [19] readr_2.1.2          tidyr_1.2.0          tibble_3.1.7         ggplot2_3.3.6        tidyverse_1.3.1      pacman_0.5.1        
# 
# loaded via a namespace (and not attached):
# [1] fs_1.5.2           usethis_2.1.6      lubridate_1.8.0    bit64_4.0.5        httr_1.4.3         rprojroot_2.0.3   
# [7] repr_1.1.4         tools_4.2.1        backports_1.4.1    utf8_1.2.2         R6_2.5.1           DBI_1.1.3         
# [13] colorspace_2.0-3   withr_2.5.0        tidyselect_1.1.2   bit_4.0.4          curl_4.3.2         compiler_4.2.1    
# [19] textshaping_0.3.6  cli_3.3.0          rvest_1.0.2        xml2_1.3.3         labeling_0.4.2     scales_1.2.0      
# [25] askpass_1.1        systemfonts_1.0.4  digest_0.6.29      yulab.utils_0.0.5  base64enc_0.1-3    pkgconfig_2.0.3   
# [31] htmltools_0.5.2    dbplyr_2.2.1       fastmap_1.1.0      rlang_1.0.4        readxl_1.4.0       rstudioapi_0.13   
# [37] farver_2.1.1       gridGraphics_0.5-1 generics_0.1.3     jsonlite_1.8.0     vroom_1.5.7        magrittr_2.0.3    
# [43] ggplotify_0.1.0    Rcpp_1.0.9         munsell_0.5.0      fansi_1.0.3        lifecycle_1.0.1    stringi_1.7.8     
# [49] snakecase_0.11.0   grid_4.2.1         parallel_4.2.1     crayon_1.5.1       haven_2.5.0        gridtext_0.1.4    
# [55] hms_1.1.1          magick_2.7.3       knitr_1.39         pillar_1.8.0       markdown_1.1       reprex_2.0.1      
# [61] pdftools_3.3.0     ggfun_0.0.6        qpdf_1.2.0         gifski_1.6.6-1     renv_0.15.5        modelr_0.1.8      
# [67] vctrs_0.4.1        tzdb_0.3.0         selectr_0.4-2      cellranger_1.1.0   gtable_0.3.0       assertthat_0.2.1  
# [73] xfun_0.31          broom_1.0.0        rsvg_2.3.1         ragg_1.2.2         ellipsis_0.3.2


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36


