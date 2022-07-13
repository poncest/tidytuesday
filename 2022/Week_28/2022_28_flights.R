
## Challenge: #TidyTuesday 2022 week 28 
## Data:      European Flights
## Author:    Steven Ponce
## Date:      2022-07-12


## 1. Load packages ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, gghighlight)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 28) 
flights <- tt$flights %>% clean_names()
rm(tt)

#|- offline ----
# write_csv(rent, '2022/Week_28/flights.csv')
# flights <- read_csv(file = '2022/Week_28/flights.csv')


## 3. Examine the data ----
dim(flights) 
glimpse(flights)
skim(flights)
colnames(flights)
range(flights$year)
unique(flights$state_name)


## 4. Tidydata ----
UK_flights <- flights %>% 
    # focus on UK
    filter(state_name == 'United Kingdom') %>% 
    # group by and summarize
    group_by(year, apt_name) %>% 
    summarise(
        departures = sum(flt_dep_1),
        arrivals   = sum(flt_arr_1),
        total      = sum(departures, arrivals)
    ) %>% 
    ungroup() %>% 
    arrange(year) %>% 
    mutate(airport = as_factor(apt_name)) %>% 
    select(year, airport, everything(), -apt_name) 


# |- data for plot----
UK_flights_plot <- UK_flights %>% 
    # the new castle airport appears after 2017
    # filter out 2022 data because it only contains the month of Jan.
    filter(year > 2017 & year < 2022) %>% 
    
    # trying to calculate year-over-year change (%)
    mutate(total_lag = lag(total, n = 19)) %>% 
    
    mutate(
        total_lag = case_when(
            is.na(total_lag) ~ total,          # if NA, return total value
            TRUE             ~ total_lag)      # otherwise, return total lag value
    )  %>% 
    
    # year-over-year
    mutate(yoy_pct = ((total - total_lag) / total_lag) * 100)   

    
## 5. Visualization ---- 
# |- Plot aesthetics ---- 
bkg_col        <- '#F9F9F9'
title_col      <- 'black'
subtitle_col   <- "black"
caption_col    <- "black"
palette_col    <- met.brewer("Wissing", n = 19, type = "continuous")
# life-saver
# monochromeR::generate_palette("brown", modification = "go_lighter", n_colours = 10, view_palette = T)


# |-  titles and caption ----
title_text    <- 'Negative Effect of COVID-19 in the U.K. Flight Industry'

subtitle_text <- "Year-Over-Year Percent Change in Total Flights, 2018-2021"

caption_text <- paste0("**#TidyTuesday:** 2022 Week 28 • **Source:** Eurocontrol<br>",
                       "**Visualization:** Steven Ponce (@sponce1) • **Tools:** #rstats, #ggplot")


# |-  fonts ----
font_add_google("Kalam", family = "title")
font_add_google("Kalam", family = "subtitle") 
font_add_google("Jura", family = "text")
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE) 


# annotation to individual facet (e.g. Aberdeen)
f_label <- data.frame(airport = c('Aberdeen'), 
                      label   = str_glue("March 11, 2020:\n",
                                         "The World Health Organization\n",
                                         "declares COVID-19 a pandemic"))


# |- main plot ----
UK_flights_plot %>% 
    ggplot(aes(x = year, y = yoy_pct, color = airport)) +
    
    # geoms
    geom_point(size = 3) + 
    geom_line(size  = 1) +
    
    geom_hline(yintercept = 0, color = 'gray40', linetype = 1, size = 1.0) +
    geom_vline(xintercept = 2020, color = 'gray40', linetype = 2, size = 0.7) + 
    
    # annotations
    geom_text(x = 2018.3, y = -70, 
              aes(label  = label),
              data       = f_label,
              color      = 'gray30',                    
              size       = 3,
              lineheight = 0.9,
              hjust      = 0) +
    
    # highlight
    gghighlight(use_direct_label = FALSE,
                unhighlighted_params = list(size = 0.1, color = 'gray60')) +
    
    # scales
    scale_x_continuous(breaks = seq(2018, 2022, by = 1),
                       expand = c(0.03, 0.03)) +
    
    scale_y_continuous(breaks = seq(-100, 50, by = 50),
                       limits = c(-100, 50),
                       expand = c(0.03, 0.03),
                       ) +

    # color
    scale_color_manual(values = palette_col) +
    
    coord_cartesian(clip = "off") +
    
    # labs
    labs(
        x = '', y = "% Change Total Number<br>of Flights",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text
    ) +

    # facet 
    facet_wrap(~ airport) +
    
    # theme
    theme_minimal()  +                                   
    
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        
        plot.background  = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title.y     = element_markdown(size = 10,
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
                                            fill     = '#E5C1C1' 
                                            ),

        panel.grid.minor   = element_blank(),
        panel.spacing      = unit(2, 'lines'),
        panel.grid.major.x = element_blank(),
        
        plot.margin   = margin(t = 25, r = 25, b = 25, l = 25),
        
        plot.title    = element_text(
            family    = 'title',
            color     = title_col,
            face      = "bold",
            size      = 40,  
            margin    = margin(t = 5)),
        
        plot.subtitle = element_text(
            family    = 'subtitle',
            color     = title_col,
            lineheight= 0.6, 
            size      = 26,
            margin    = margin(t = 5, b = 25)),
        
        plot.caption  = element_markdown(
            family    = 'caption',
            color     = caption_col, 
            size      = 13,
            hjust     = 1.0,
            margin    = margin(t = 10, b = 10)),
    )
 
 
# |-  resolution ----
showtext_opts(dpi = 400)
 
 
# ## 6. Save final figure ----
ggsave('2022/Week_28/2022_28_flights.png',
       width = 20, height = 11, units = 'in', dpi = 400)

showtext_auto(FALSE) 

# |- sessionInfo()

# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)

# Matrix products: default

# locale: 
#     [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    

# attached base packages:
#     [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
#     [1] lubridate_1.8.0    skimr_2.1.4        here_1.0.1         janitor_2.1.0      MetBrewer_0.2.0   
# [6] showtext_0.9-5     showtextdb_3.0     sysfonts_0.8.8     ggtext_0.1.1       tidytuesdayR_1.0.2
# [11] forcats_0.5.1      stringr_1.4.0      dplyr_1.0.9        purrr_0.3.4        readr_2.1.2       
# [16] tidyr_1.2.0        tibble_3.1.7       ggplot2_3.3.6      tidyverse_1.3.1    pacman_0.5.1      
 
# loaded via a namespace (and not attached):
#     [1] Rcpp_1.0.9       digest_0.6.29    assertthat_0.2.1 rprojroot_2.0.3  utf8_1.2.2       repr_1.1.4      
# [7] R6_2.5.1         cellranger_1.1.0 backports_1.4.1  reprex_2.0.1     httr_1.4.3       pillar_1.7.0    
# [13] rlang_1.0.3      curl_4.3.2       readxl_1.4.0     rstudioapi_0.13  selectr_0.4-2    bit_4.0.4       
# [19] munsell_0.5.0    gridtext_0.1.4   broom_1.0.0      xfun_0.31        compiler_4.2.1   modelr_0.1.8    
# [25] base64enc_0.1-3  pkgconfig_2.0.3  htmltools_0.5.2  tidyselect_1.1.2 fansi_1.0.3      crayon_1.5.1    
# [31] tzdb_0.3.0       dbplyr_2.2.1     withr_2.5.0      grid_4.2.1       jsonlite_1.8.0   gtable_0.3.0    
# [37] lifecycle_1.0.1  DBI_1.1.3        magrittr_2.0.3   scales_1.2.0     vroom_1.5.7      cli_3.3.0       
# [43] stringi_1.7.6    fs_1.5.2         snakecase_0.11.0 xml2_1.3.3       ellipsis_0.3.2   generics_0.1.3  
# [49] vctrs_0.4.1      tools_4.2.1      bit64_4.0.5      glue_1.6.2       hms_1.1.1        parallel_4.2.1  
# [55] fastmap_1.1.0    colorspace_2.0-3 rvest_1.0.2      knitr_1.39       haven_2.5.0      usethis_2.1.6   

# RStudio 2021.09.0+351 "Ghost Orchid" Release (077589bcad3467ae79f318afe8641a1899a51606, 2021-09-20) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36