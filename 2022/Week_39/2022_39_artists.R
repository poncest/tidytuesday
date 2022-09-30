
## Challenge: #TidyTuesday 2022 week 39
## Data:      Artists in the USA
## Author:    Steven Ponce
## Date:      2022-09-27   
 

## 1. LOAD PACKAGES & SETUP ----  
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext)
p_load(janitor, here, skimr, glue, camcorder, scales, patchwork)

gg_record(
    dir    = here("temp_plots"),
    device = "png",
    width  = 16,
    height = 10,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 39) 
artists <- tt$artists %>% clean_names()
readme(tt) 
rm(tt)  


#|- offline ----   
# write_csv(artists, '2022/Week_39/artists.csv')
# artists <- read_csv(file = '2022/Week_39/artists.csv') %>% clean_names()


## 3. EXAMINING THE DATA ----
glimpse(artists) 
skim(artists)

unique(artists$type) %>% sort()
unique(artists$race) %>% sort()



## 4. TIDYDATA ---- 

# |-  PR map ----
map <- map_data("world") %>% 
    filter(region == "Puerto Rico") %>% 
    mutate(label_text = "Total Artists:<br>13,370 ")


# |- Artists in PR ----
pr_artists <- artists %>% 
    filter(state == "Puerto Rico") %>% 
    select(-location_quotient) %>%
    drop_na() %>%
    filter(artists_n != 0) %>% 
    arrange(desc(artists_share)) 

# |- Top-3 artist type in PR ----
top3_pr_artists <- pr_artists %>%
    slice(1:3) %>% 
    
    mutate(
        artists_share_rounded = round(artists_share, 4),
        artists_pct           = scales::percent(artists_share_rounded) 
        ) %>% 
    
    mutate(
        type = case_when(
            type == "Fine Artists, Art Directors, And Animators" ~ "Fine Artists, Art Directors,<br> and Animators",
            TRUE ~  as.character(type))) %>%
    
    mutate(
        type = as_factor(type),
        type = fct_relevel(type, "Designers", "Architects", "Fine Artists, Art Directors,<br> and Animators")
        
    ) %>% 
    
    mutate(
        label_text = str_glue("{type}<br>",
                              "Count: {artists_n}<br>",
                              "Percentage: {artists_pct}<br>")
        )

 
# 5. VISUALIZATION ---- 
# |- plot aesthetics ----  
map_col      <- "#1C6E8C"             
bkg_col      <- "#F1F2EE"            
title_col    <- "black"
subtitle_col <- "black"
caption_col  <- "black"
col_palette  <- list("Designers" = "#498BA3",    
                     "Architects"  = "#76A8BA", 
                     "Fine Artists, Art Directors,<br> and Animators" = "#A4C5D1") 

# |-  titles and caption ----
title_text    <- "National Endowment for the Arts"
                         
subtitle_text <- str_glue("Top-3 Artists Type in Puerto Rico." )

caption_text  <- str_glue("#TidyTuesday: 2022 Week 39 &bull; Source: arts.gov<br>",
                           "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                           "<span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                           "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                           "<span style='font-family:fa-brands'>&#x23;</span> ggplot")

# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Roboto Condensed", family = "title")                          
font_add_google("Roboto Condensed", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |- map ----  
pr_map <- ggplot(data = map) +
    
    geom_polygon(aes(x = long, y = lat, group = group), 
                 color = map_col, fill = map_col) +
    
    geom_textbox(aes(label = label_text,
                     x = -66.5, 
                     y = 18.2,
                     halign = 0.5,
                     vjust  = 0.5), 
                 family     = "text", 
                 color      = "white", 
                 fontface   = "bold",
                 size       = 20, 
                 width      = unit(50, "line"),
                 box.color  = NA,
                 alpha      = 0.9,
                 fill       = NA,
                 ) +
    
    # scales
    scale_color_manual(values = col_palette) +
    coord_fixed(1.3) +
    
    # labs
    labs(title    = title_text,
         subtitle = subtitle_text) +
    
    # theme
    theme_void() +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 60,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_text(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.8, 
            face           = "bold",
            size           = 30,
            margin         = margin(t = 10, b = 30)),
    )

pr_map


# |- KPI  ----   
kpi <- ggplot(data = top3_pr_artists) + 

    # geoms
    geom_rect(aes(fill = type), 
              xmin  = -1, 
              ymin  = -1, 
              xmax  = 1, 
              ymax  = 1, 
              color = "black") +
    
    geom_textbox(aes(label = label_text,
                     x = 0, 
                     y = 0,
                     halign = 0.5,
                     vjust  = 0.5), 
                 family     = "text", 
                 color      = "white", 
                 fontface   = "bold",
                 size       = 6, 
                 width      = unit(50, "line"),
                 box.color  = NA,
                 alpha      = 0.9,
                 fill       = NA,
    ) +
    
    # facet
    facet_grid(~ type) + 
    
    # scales
    scale_fill_manual(values = col_palette) +
    coord_fixed(xlim = c(-1,1), ylim = c(-1,1)) +
    
    # labs
    labs(caption  = caption_text) +
    
    # theme
    theme_void() +
    theme(
        
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        panel.spacing.x    = unit(-5,"points"),
        strip.text         = element_blank(),
    
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.caption       = element_markdown(
            family         = "caption",
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 12,
            hjust          = 0.5,
            margin         = margin(t = 10, b = 10)),
    )

kpi


# |- Final plot ----     

final <- pr_map / kpi +  
    plot_layout(nrow = 2, heights = c(1.25, 1)) +
    plot_annotation(
        theme = theme(
            plot.margin = margin(5,5,5,5),
            plot.background = element_rect(fill = bkg_col, color = bkg_col),
            )
    )
  
final


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
# [1] patchwork_1.1.1      scales_1.2.1         camcorder_0.0.2.9000 glue_1.6.2           skimr_2.1.4          here_1.0.1          
# [7] janitor_2.1.0        showtext_0.9-5       showtextdb_3.0       sysfonts_0.8.8       ggtext_0.1.1         tidytuesdayR_1.0.2  
# [13] forcats_0.5.1        stringr_1.4.0        dplyr_1.0.9          purrr_0.3.4          readr_2.1.2          tidyr_1.2.0         
# [19] tibble_3.1.7         ggplot2_3.3.6        tidyverse_1.3.1      pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] fs_1.5.2          usethis_2.1.6     lubridate_1.8.0   bit64_4.0.5       httr_1.4.3        rprojroot_2.0.3   repr_1.1.4       
# [8] tools_4.2.1       backports_1.4.1   utf8_1.2.2        R6_2.5.1          DBI_1.1.3         colorspace_2.0-3  withr_2.5.0      
# [15] tidyselect_1.1.2  bit_4.0.4         curl_4.3.2        compiler_4.2.1    textshaping_0.3.6 cli_3.3.0         rvest_1.0.2      
# [22] xml2_1.3.3        labeling_0.4.2    askpass_1.1       systemfonts_1.0.4 digest_0.6.29     base64enc_0.1-3   pkgconfig_2.0.3  
# [29] htmltools_0.5.2   dbplyr_2.2.1      fastmap_1.1.0     maps_3.4.0        rlang_1.0.4       readxl_1.4.0      rstudioapi_0.13  
# [36] farver_2.1.1      generics_0.1.3    jsonlite_1.8.0    vroom_1.5.7       magrittr_2.0.3    Rcpp_1.0.9        munsell_0.5.0    
# [43] fansi_1.0.3       lifecycle_1.0.1   stringi_1.7.8     snakecase_0.11.0  grid_4.2.1        parallel_4.2.1    crayon_1.5.1     
# [50] haven_2.5.0       gridtext_0.1.4    hms_1.1.1         magick_2.7.3      knitr_1.39        pillar_1.8.1      markdown_1.1     
# [57] reprex_2.0.1      pdftools_3.3.0    qpdf_1.2.0        gifski_1.6.6-1    renv_0.15.5       modelr_0.1.8      vctrs_0.4.1      
# [64] tzdb_0.3.0        selectr_0.4-2     cellranger_1.1.0  gtable_0.3.0      assertthat_0.2.1  xfun_0.31         broom_1.0.0      
# [71] rsvg_2.3.1        ragg_1.2.2        ellipsis_0.3.2   


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36 


