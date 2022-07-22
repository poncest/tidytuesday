
## Challenge: #TidyTuesday 2022 week 29
## Data:      Technology Adoption 
## Author:    Steven Ponce
## Date:      2022-07-19


## 1. LOAD PACKAGES ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, patchwork)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 29) 
technology <- tt$technology %>% clean_names()
rm(tt)


#|- offline ----
# write_csv(rent, '2022/Week_29/technology.csv')
# technology <- read_csv(file = '2022/Week_29/technology.csv')


## 3. EXAMINING THE DATA ----
dim(technology) 
glimpse(technology)
colnames(technology)
unique(technology$group) %>% sort()
unique(technology$category) %>% sort()


## 4. TIDYDATA ----
energy_tbl <-  technology %>% 
    
    # Filter for USA country, Energy sector, and energy types 
    filter(
        iso3c == 'USA',
        category == 'Energy',
        variable != 'elecprod' & variable != 'electric_gen_capacity' & variable != 'elec_cons'
        ) %>% 
    
    # rename variables
    mutate(
        type = case_when(
            variable == 'elec_coal'        ~ 'Coal',
            variable == 'elec_gas'         ~ 'Gas',
            variable == 'elec_hydro'       ~ 'Hydroelectric',
            variable == 'elec_nuc'         ~ 'Nuclear',
            variable == 'elec_oil'         ~ 'Oil',
            variable == 'elec_renew_other' ~ 'Renewable - Others',
            variable == 'elec_solar'       ~ 'Solar',
            TRUE                           ~ 'Wind'
        )
    ) %>% 
    
    # manual reorder
    mutate(
            type =  fct_relevel(type, c('Coal', 'Hydroelectric', 'Gas', 'Renewable - Others',
                                        'Oil', 'Solar', 'Nuclear', 'Wind'))
        )


## 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 
bkg_col        <- '#F4FFF4'   
title_col      <- 'black'
subtitle_col   <- "black"
caption_col    <- "black"
facet_col      <- 'transparent'
palette_col    <- met.brewer("Ingres", n = 8, type = "discrete")
# life-saver
# monochromeR::generate_palette("green", modification = "go_lighter", n_colours = 10, view_palette = T)

 
# |-  titles and caption ----
title_text    <- 'Renewable Energy Alternatives Steadily Increasing in the USA'

subtitle_text <- "Historical Adpotion of Technology in the U.S., 1985-2020"

caption_text  <- paste0("**#TidyTuesday:** 2022 Week 29 • **Source:** data.nber.org<br>",
                        "**Visualization:** Steven Ponce (@sponce1) • **Tools:** #rstats, #ggplot")

body_text_1 <- "Traditional energy sources like **coal** and **oil** seem to be decreasing while there is more demand for **gas** and **nuclear**."

body_text_2 <- "Renewable energy sources like **hydro** and **others** seem to normalize or plateau.  However, there has been a rapid growth for **solar** and **wind** in the last couple of years."


# |-  fonts ----
font_add_google("Gugi", family = "title")
font_add_google("Jura", family = "subtitle") 
font_add_google("Jura", family = "text")
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE) 


# |- main plot ----
p1 <- energy_tbl %>% 
    ggplot(aes(x = year, y = value, fill = label)) +
    
    # geoms
    geom_area()+
    
    # scales
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) + 
    
    # color
    scale_fill_manual(values = palette_col) +
    coord_cartesian(clip = "off") +
    
    # labs
    labs(
        x = '', 
        y = '',
        title = 'Energy Output in Terawatt Hour (TWh)',
        caption  = caption_text
    ) +
    
    # facets
    facet_wrap(~ type, scales = 'free_y', ncol = 2) +
    
    # theme
    theme_minimal(base_size = 16)  +                                   
    theme(

        plot.title       = element_text(hjust=0.5),
        
        legend.position  = 'plot',
        
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title.y     = element_markdown(size = 14,
                                            face = 'bold',
                                            margin = margin (r = 10), 
                                            hjust = 0.5,
                                            vjust = 0.5,
                                            angle = 0),
        
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black"),
        
        strip.text       = element_textbox(size      = 12, 
                                           face     = 'bold', 
                                           hjust    = 0.5,
                                           halign   = 0.5,
                                           linetype = 1, 
                                           r        = unit(5, "pt"), 
                                           width    = unit(1, "npc"),
                                           padding  = margin(2, 0, 1, 0), 
                                           margin   = margin(3, 3, 3, 3),
                                           fill     = facet_col 
        ),
        
        panel.grid.minor   = element_blank(),
        panel.spacing      = unit(2, 'lines'),
        panel.grid.major.x = element_blank(),
        
        plot.margin        = margin(t = 25, r = 25, b = 25, l = 25),
        
        plot.caption       = element_markdown(
            family         = 'caption',
            color          = caption_col, 
            size           = 13,
            hjust          = 1.0,
            margin         = margin(t = 10, b = 10)),
    )


# |- text plot ----
p2 <- ggplot() +
    theme_void() +
    
    # title
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'title',
                 label       = title_text,
                 color       = title_col,
                 size        = 17, 
                 fill        = NA,     
                 box.color   = NA,
                 width       = unit(9, "in"),
                 vjust       = -1.2,
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 fontface    ="bold", 
                 lineheight  = 1) +
    
    # subtitle
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'subtitle',
                 label       = subtitle_text,
                 color       = title_col,
                 size        = 14, 
                 fill        = NA,     
                 box.color   = NA,
                 width       = unit(7, "in"),
                 vjust       = 0, 
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 #fontface    ="bold", 
                 lineheight  = 1) +
    
    # text 1
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'text',
                 label       = body_text_1,
                 color       = title_col,
                 size        = 8, 
                 fill        = NA, 
                 box.color   = NA,
                 width       = unit(8, "in"),
                 vjust       = 1.5, 
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0,   
                 fontface    = 'plain',
    ) +
     
    # text 2
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'text',
                 label       = body_text_2,
                 color       = title_col,
                 size        = 8, 
                 fill        = NA, 
                 box.color   = NA,
                 width       = unit(8, "in"),
                 vjust       = 2.4, 
                 valign      = 0.5,
                 hjust       = 0.5, 
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
        
        plot.title    = element_markdown(                 
            family    = 'title',
            color     = title_col,
            face      = "bold",
            size      = 5,
            lineheight= 0.5,
            margin    = margin(t = 5))
    )


# putting all plots together
p2 + p1 +
    plot_layout(widths = c(1.5, 2)) &
    theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 10))

 
# |- resolution ----
showtext_opts(dpi = 400)


## 6. SAVE FINAL FIGURE ----  
ggsave('2022/Week_29/2022_29_technology.png',
       width = 20, height = 11, units = 'in', dpi = 400)

showtext_auto(FALSE)


## 7. SESSION INFO ----
# sessionInfo()

# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)

# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     
# 
# other attached packages:
# [1] gghighlight_0.3.3  skimr_2.1.4        here_1.0.1         janitor_2.1.0      MetBrewer_0.2.0
# [6] showtext_0.9-5     showtextdb_3.0     sysfonts_0.8.8     ggtext_0.1.1       tidytuesdayR_1.0.2 
# [11] forcats_0.5.1      stringr_1.4.0      dplyr_1.0.9        purrr_0.3.4        readr_2.1.2 
# [16] tidyr_1.2.0        tibble_3.1.7       ggplot2_3.3.6      tidyverse_1.3.1    pacman_0.5.1      
# 
# loaded via a namespace (and not attached):
# [1] httr_1.4.3       bit64_4.0.5      vroom_1.5.7      jsonlite_1.8.0   modelr_0.1.8    
# [6] assertthat_0.2.1 selectr_0.4-2    renv_0.15.5      cellranger_1.1.0 pillar_1.7.0    
# [11] backports_1.4.1  glue_1.6.2       digest_0.6.29    gridtext_0.1.4   rvest_1.0.2     
# [16] snakecase_0.11.0 colorspace_2.0-3 htmltools_0.5.2  pkgconfig_2.0.3  broom_1.0.0     
# [21] haven_2.5.0      scales_1.2.0     tzdb_0.3.0       generics_0.1.3   usethis_2.1.6   
# [26] ellipsis_0.3.2   withr_2.5.0      repr_1.1.4       cli_3.3.0        magrittr_2.0.3  
# [31] crayon_1.5.1     readxl_1.4.0     fs_1.5.2         fansi_1.0.3      xml2_1.3.3      
# [36] tools_4.2.1      hms_1.1.1        lifecycle_1.0.1  munsell_0.5.0    reprex_2.0.1    
# [41] compiler_4.2.1   rlang_1.0.3      grid_4.2.1       rstudioapi_0.13  base64enc_0.1-3 
# [46] gtable_0.3.0     DBI_1.1.3        curl_4.3.2       R6_2.5.1         lubridate_1.8.0 
# [51] knitr_1.39       fastmap_1.1.0    bit_4.0.4        utf8_1.2.2       rprojroot_2.0.3 
# [56] stringi_1.7.8    parallel_4.2.1   Rcpp_1.0.9       vctrs_0.4.1      dbplyr_2.2.1    
# [61] tidyselect_1.1.2 xfun_0.31  

# RStudio 2021.09.0+351 "Ghost Orchid" Release (077589bcad3467ae79f318afe8641a1899a51606, 2021-09-20) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36


