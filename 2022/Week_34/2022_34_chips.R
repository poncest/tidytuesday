
## Challenge: #TidyTuesday 2022 week 34
## Data:      CHIP dataset
## Author:    Steven Ponce
## Date:      2022-08-23  
 

## 1. LOAD PACKAGES & SETUP ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, glue, camcorder)

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
tt <- tidytuesdayR::tt_load(2022, week = 34) 

# Focus on Ted Lasso tv series
chips <- tt$chips %>% clean_names()
rm(tt) 


#|- offline ----   
# write_csv(chips, '2022/Week_34/chips.csv')
# chips <- read_csv(file = '2022/Week_34/chips.csv') %>% clean_names()


## 3. EXAMINING THE DATA ----
glimpse(chips)
skim(chips)


## 4. TIDYDATA ----

#|- main df ----
df <- chips %>% 
    drop_na(year) %>% 
    select(year, process_size_nm, transistors_million) %>% 
    pivot_longer(!year, names_to = 'name', values_to = 'value')

#|- bounds 1 (fill between the two lines) ----
bounds  <- df %>% 
    drop_na(year) %>% 
    pivot_wider(names_from = name, values_from = value) %>%
    mutate(
        ymax = pmax(process_size_nm, transistors_million),
        ymin = pmin(process_size_nm, transistors_million),
        fill = process_size_nm >= transistors_million
    )

#|- bounds 2 (fill the gab between 2004 - 2005) ----
bounds2 <- bind_rows(
    bounds,
    tibble(year = 2004, 
           process_size_nm = 115.41353, 
           transistors_million = 115.41353, 
           ymax = 115.41353,
           ymin = 115.41353, 
           fill = FALSE)
    ) %>%
    arrange(year)


# 5. VISUALIZATION ---- 

# |- aesthetics ---- 
bkg_col      <- '#D0CFD1'    
title_col    <- 'black'
subtitle_col <- "black"
caption_col  <- "black"

#palette_col  <- met.brewer("Pillement", n = 6, type = "discrete")
# monochromeR::generate_palette("#C5C3C6", modification = "go_lighter", n_colours = 5, view_palette = T)


# |-  titles and caption ---- 
title_text    <- "Moore's Law in the semiconductor industry "

subtitle_text <- str_glue("The effect that Moore’s law has had on the electronics industry cannot be overstated. The idea that the number of transistors in a dense integrated circuit<br>doubles approximately every two years was advanced in a paper written by Gordon E. Moore in 1965 and was validated over the course of the next half a century.<br>While Moore’s Law held true for more than 50 years, eventually it became more difficult to exploit its advantages.<br><br>",
                          
                          "<span style='font-size:20pt; color:#46494C'>**&#8212; Transistor Count (Millions)**</span>  <span style='font-size:20pt; color:#D0CFD1'>**&#8212; __**</span>  <span style='font-size:20pt; color:#1985A1'>**&#8212; Process Size (nm)**</span>")

caption_text  <- str_glue("#TidyTuesday: 2022 Week 34 &bull; Source: CHIP dataset<br>",
                           "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                           " <span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                           "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                           "<span style='font-family:fa-brands'>&#x23;</span> ggplot")



# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Bungee Inline", family = "title")
font_add_google("Coda", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |-  main plot ----
ggplot(df) +
    # geoms
    geom_ribbon(data = bounds2, 
                aes(year, ymin = ymin, ymax = ymax, fill = fill), 
                alpha = 0.65) +
    
     geom_line(aes(year, value, color = name), size = 2) +       
   
    
    # scales
    scale_x_continuous(breaks = seq(2000, 2022, by = 2),
                       limits = c(2000, 2021)) +
    
    scale_y_continuous(trans = 'log10',
                       breaks = c(10, 100, 1000, 10000, 13000),
                       labels = c(10, 100, 1000, 10000, 'Logarithmic Scale\n'))+
    
    scale_fill_manual(values = c("#1985A1", "#46494C"), name = "") + 
    scale_color_manual(values = c("#1985A1", "#46494C"), name = "") +

    coord_cartesian(clip = "off") +
    guides(linetype = "none", fill = "none") +
    
    # labs
    labs(
        x = '',  y = '',
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text) +
    
    # theme
    theme_minimal(
        base_size   = 16,
        base_family = 'text') +
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title.y     = element_text(size = 16,
                                        face = 'bold',
                                        margin = margin (r = 10)),
        
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black"),
        
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.4, color = 'gray'),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 50,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_markdown(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.6, 
            face           = "plain",
            size           = 20,
            margin         = margin(t = 10, b = 30)),
        
        plot.caption       = element_markdown( 
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 12,
            hjust          = 1.0,
            margin         = margin(t = 10, b = 10)),
    )




## 6. SESSION INFO ---- 

# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)

# Matrix products: default
 
# locale: 
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

# other attached packages:
# [1] camcorder_0.0.2.9000 glue_1.6.2           skimr_2.1.4          here_1.0.1           janitor_2.1.0       
# [6] MetBrewer_0.2.0      showtext_0.9-5       showtextdb_3.0       sysfonts_0.8.8       ggtext_0.1.1        
# [11] tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0        dplyr_1.0.9          purrr_0.3.4         
# [16] readr_2.1.2          tidyr_1.2.0          tibble_3.1.7         ggplot2_3.3.6        tidyverse_1.3.1     
# [21] pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] fs_1.5.2          usethis_2.1.6     lubridate_1.8.0   bit64_4.0.5       httr_1.4.3        rprojroot_2.0.3  
# [7] repr_1.1.4        tools_4.2.1       backports_1.4.1   utf8_1.2.2        R6_2.5.1          DBI_1.1.3        
# [13] colorspace_2.0-3  withr_2.5.0       tidyselect_1.1.2  bit_4.0.4         curl_4.3.2        compiler_4.2.1   
# [19] textshaping_0.3.6 cli_3.3.0         rvest_1.0.2       xml2_1.3.3        scales_1.2.1      askpass_1.1      
# [25] systemfonts_1.0.4 digest_0.6.29     base64enc_0.1-3   pkgconfig_2.0.3   htmltools_0.5.2   dbplyr_2.2.1     
# [31] fastmap_1.1.0     rlang_1.0.4       readxl_1.4.0      rstudioapi_0.13   farver_2.1.1      generics_0.1.3   
# [37] jsonlite_1.8.0    vroom_1.5.7       magrittr_2.0.3    Rcpp_1.0.9        munsell_0.5.0     fansi_1.0.3      
# [43] lifecycle_1.0.1   stringi_1.7.8     snakecase_0.11.0  grid_4.2.1        parallel_4.2.1    crayon_1.5.1     
# [49] haven_2.5.0       gridtext_0.1.4    hms_1.1.1         magick_2.7.3      knitr_1.39        pillar_1.8.1     
# [55] markdown_1.1      reprex_2.0.1      pdftools_3.3.0    qpdf_1.2.0        gifski_1.6.6-1    renv_0.15.5      
# [61] modelr_0.1.8      vctrs_0.4.1       tzdb_0.3.0        selectr_0.4-2     cellranger_1.1.0  gtable_0.3.0     
# [67] assertthat_0.2.1  xfun_0.31         broom_1.0.0       rsvg_2.3.1        ragg_1.2.2        ellipsis_0.3.2


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36


