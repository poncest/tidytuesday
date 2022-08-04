
## Challenge: #TidyTuesday 2022 week 31
## Data:      Oregon Spotted Frog
## Author:    Steven Ponce
## Date:      2022-08-02  


## 1. LOAD PACKAGES & SETUP ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, camcorder, ggwaffle, waffle, patchwork)

gg_record(
    dir    = here("temp_plots"), 
    device = "png", 
    width  = 12, 
    height = 16, 
    units  = "in", 
    dpi    = 600)

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 31) 
frogs <- tt$frogs %>% clean_names()
rm(tt)


#|- offline ----
# write_csv(frogs, '2022/Week_31/frogs.csv')
# frogs <- read_csv(file = '2022/Week_31/frogs.csv') %>% clean_names()


### 3. EXAMINING THE DATA ----
glimpse(frogs)
skim(frogs)
colnames(frogs)

range(frogs$survey_date)
unique(frogs$site) 
unique(frogs$subsite) 
unique(frogs$detection) 


## 4. TIDYDATA ----
frogs_tbl <- frogs %>% 
    select(-ordinal, -frequency, -utme_83, -utmn_83, -interval) %>% 
    mutate(female = case_when(
        female == 0 ~ 'No',
        TRUE        ~ 'Yes'
    )) %>% 
    group_by(hab_type, detection) %>% 
    summarise(count = n()) %>% 
    mutate(pct = round(count / sum(count) * 100, digits = 0)) %>% 
    ungroup()


# 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 
bkg_col        <- 'white'    #'#F6F6F6'  
title_col      <- 'black'
subtitle_col   <- "black"
caption_col    <- "black"
palette_col    <- met.brewer("Hokusai2", n = 6, type = "discrete")[c(6,3,1)]
# monochromeR::generate_palette("brown", modification = "go_lighter", n_colours = 10, view_palette = T)


# |-  titles and caption ----
title_text    <- 'Where are the Frogs?'

subtitle_text <- str_wrap("Oregon spotted frog (_Rana pretiosa_),<br> USGS Survey: Sep 28 - Oct 01, 2018", width = 80)

caption_text   <- str_glue("
                         #TidyTuesday: 2022 Week 31 &bull; Source: USGS spotted frog data<br>",
                         "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                         " <span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                         "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                         "<span style='font-family:fa-brands'>&#x23;</span> ggplot")

legend_text <- str_glue("
                        <span style='font-size:35pt; color:#abc9c8'>**NO VISUAL**</span><br>",
                        "<span style='font-size:35pt; color:#4692b0'>**VISUAL**</span><br>",
                        "<span style='font-size:35pt; color:#0a3351'>**CAPTURE**</span><br>",
                    )


# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Roboto Condensed", family = "title")
font_add_google("Roboto Condensed", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |- data for the plot ----
pond      <- c('Capture'= 16, 'Visual' = 9, 'No Visual'= 75) 
reservoir <- c('Capture'= 10, 'Visual' = 5, 'No Visual'= 85) 
river     <- c('Capture'= 12, 'Visual' = 72, 'No Visual'= 16) 


# |- pond plot ----
p1 <- waffle(
    pond,
    rows = 10 ,
    size = 0.9,
    colors = palette_col,
    flip = TRUE,
) +
    ggtitle("Pond") +
    theme(
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.title      = element_text(hjust = 0.5, size = 27, face = "bold"),
        legend.text     = element_text(size = 15),
        legend.position = "plot"
    )

# |- reservoir plot ----
p2 <- waffle(
    reservoir,
    rows = 10 ,
    size = 0.9,
    colors = palette_col,
    #xlab = "1 square equals to 1%",
    flip = TRUE,
) +
    ggtitle("Reservoir") +
    theme(
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.title      = element_text(hjust = 0.5, size = 27, face = "bold"),
        legend.text     = element_text(size = 15),
        legend.position = "plot"
    )

# |- river plot ----
p3 <- waffle(
    river,
    rows = 10 ,
    size = 0.9,
    colors = palette_col,
    xlab = "1 square equals to 1%",
    flip = TRUE,
) +
    ggtitle("River") +
    theme(
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.title      = element_text(hjust = 0.5, size = 27, face = "bold"),
        legend.text     = element_text(size = 15),
        legend.position = "plot"
    )


# |- text plot ----
text <-ggplot() +
    theme_void() +
    
    # title
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'title',
                 label       = title_text,
                 color       = title_col,
                 size        = 22, 
                 fill        = NA,     
                 box.color   = NA,
                 width       = unit(6.5, "in"),
                 vjust       = -3.2,
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 fontface    ="bold", 
                 lineheight  = 1) +
    
    # subtitle
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'title',
                 label       = subtitle_text,
                 color       = title_col,
                 size        = 10, 
                 fill        = NA,     
                 box.color   = NA,
                 width       = unit(6.5, "in"),
                 vjust       = -1, 
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 fontface    ="plain", 
                 lineheight  = 1) +
    
    # legend
    geom_textbox(aes(x = 0, y = 0),
                 family      = 'title',
                 label       = legend_text,
                 color       = title_col,
                 size        = 10,
                 fill        = NA,
                 box.color   = NA,
                 width       = unit(6.5, "in"),
                 vjust       = 2.0,
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
                 size        = 6, 
                 fill        = NA, 
                 box.color   = NA,
                 width       = unit(6.5, "in"),
                 vjust       = 11.4, 
                 valign      = 0.5,
                 hjust       = 0.4, 
                 halign      = 0,
                 fontface    = 'plain',
    ) +
    
    coord_cartesian(clip = "off") +
    scale_y_continuous(limits = c(0, 0), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 0), expand = c(0, 0)) +
    
    # theme
    theme(
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin = margin(t = 0, r = 20, b = 30, l = 20),
        
        plot.title     = element_markdown(
            family     = 'title',
            color      = title_col,
            face       = "bold",
            size       = 5,
            lineheight = 0.5,
            margin     = margin(t = 5))
    )


# |- putting all together ----
p4 <- (p1 / p2 / p3)

# final plot
text + p4


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
# [1] patchwork_1.1.1      waffle_0.7.0         ggwaffle_0.2.2       camcorder_0.0.2.9000 skimr_2.1.4          here_1.0.1        
# [7] janitor_2.1.0        MetBrewer_0.2.0      showtext_0.9-5       showtextdb_3.0       sysfonts_0.8.8       ggtext_0.1.1      
# [13] tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0        dplyr_1.0.9          purrr_0.3.4          readr_2.1.2      
# [19] tidyr_1.2.0          tibble_3.1.7         ggplot2_3.3.6        tidyverse_1.3.1      pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] fs_1.5.2           usethis_2.1.6      bit64_4.0.5        lubridate_1.8.0    RColorBrewer_1.1-3 httr_1.4.3        
# [7] rprojroot_2.0.3    repr_1.1.4         tools_4.2.1        backports_1.4.1    utf8_1.2.2         R6_2.5.1          
# [13] DBI_1.1.3          colorspace_2.0-3   withr_2.5.0        tidyselect_1.1.2   gridExtra_2.3      bit_4.0.4         
# [19] curl_4.3.2         compiler_4.2.1     extrafontdb_1.0    textshaping_0.3.6  cli_3.3.0          rvest_1.0.2       
# [25] xml2_1.3.3         labeling_0.4.2     scales_1.2.0       askpass_1.1        systemfonts_1.0.4  digest_0.6.29     
# [31] base64enc_0.1-3    pkgconfig_2.0.3    htmltools_0.5.2    extrafont_0.18     dbplyr_2.2.1       fastmap_1.1.0     
# [37] rlang_1.0.4        readxl_1.4.0       rstudioapi_0.13    farver_2.1.1       generics_0.1.3     jsonlite_1.8.0    
# [43] vroom_1.5.7        magrittr_2.0.3     Rcpp_1.0.9         munsell_0.5.0      fansi_1.0.3        lifecycle_1.0.1   
# [49] stringi_1.7.8      snakecase_0.11.0   grid_4.2.1         parallel_4.2.1     crayon_1.5.1       haven_2.5.0       
# [55] gridtext_0.1.4     hms_1.1.1          magick_2.7.3       knitr_1.39         pillar_1.8.0       markdown_1.1      
# [61] reprex_2.0.1       glue_1.6.2         pdftools_3.3.0     qpdf_1.2.0         gifski_1.6.6-1     renv_0.15.5       
# [67] modelr_0.1.8       vctrs_0.4.1        tzdb_0.3.0         selectr_0.4-2      Rttf2pt1_1.3.10    cellranger_1.1.0  
# [73] gtable_0.3.0       assertthat_0.2.1   xfun_0.31          broom_1.0.0        rsvg_2.3.1         ragg_1.2.2        
# [79] ellipsis_0.3.2 


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36


