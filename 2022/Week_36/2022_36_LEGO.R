
## Challenge: #TidyTuesday 2022 week 36
## Data:      Pell Grants
## Author:    LEGO database
## Date:      2022-09-06   
 

## 1. LOAD PACKAGES & SETUP ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, glue, camcorder, scales, ggrepel)

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
tt <- tidytuesdayR::tt_load(2022, week = 36) 

# Focus on Ted Lasso tv series
sets <- tt$sets %>% clean_names()
rm(tt) 


#|- offline ----   
#sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
# write_csv(sets, '2022/Week_36/sets.csv')
sets <- read_csv(file = "2022/Week_36/data/sets.csv") %>% clean_names()


## 3. EXAMINING THE DATA ----
glimpse(sets)
skim(sets)
range(sets$year)



## 4. TIDYDATA ---- 

# |- focus on Harry Potter sets ----
set_df <- sets %>% 
    filter(num_parts >= 3) %>% 
    
    # filter for Harry Potter related
    filter(grepl("Harry Potter|Hogwarts|Grimmauld|Dumbledore|Quidditch|Shrieking Shack|Sirius|Hagrid|Slytherin|Dobby|Snape|Knockturn|Aragog|Chamber of Secrets|Knight Bus|Merpeople|Graveyard Duel|Diagon|Weasley|Voldemort|Expecto Patronum|Forbidden Forest|Monster Book|Hermione|Gryffindor", name)) %>% 
    
    arrange(year) %>% 
    select(-set_num, -theme_id) %>% 
    
    # add decade column
    mutate(decade = paste0((year - 1) %/% 10 * 10, " 's"))

# |- Lego part per year ----
num_parts_year_tbl <- set_df %>% 
    group_by(year) %>% 
    summarize(
        count   = n(),
        median  = median(num_parts),
        average = mean(num_parts),
        total   = sum(num_parts)
    ) %>%
    ungroup() %>%
    mutate(decade = paste0((year - 1) %/% 10 * 10, " 's"))


# |- Lego part per decade ----
num_parts_decade_tbl <- set_df %>% 
    group_by(decade) %>% 
    summarize(
        count = n(),
        total = sum(num_parts)
    ) %>%
    ungroup()

    
# |- over 1k Lego pieces ----
over_1000_pieces <- set_df %>% 
    filter(num_parts > 1000) %>% 
    arrange(desc(num_parts)) %>% 
    mutate(rank = row_number()) %>% 
    distinct(name, .keep_all = TRUE) %>% 
    mutate(name = str_replace(name, pattern = "’", replacement = " ")) %>% 
    mutate(name = as_factor(name) %>% fct_reorder(num_parts)) %>%
    mutate(
        label_text = str_glue(
            "Set: {name}\nCount: {scales::comma(num_parts)}, Year: {year}"
    ))
    
    
# 5. VISUALIZATION ---- 
# |- plot aesthetics ---- 
col_1        <- "black"
bkg_col      <- "#AA82D0"   #"#93F1F7"
title_col    <- 'black'
subtitle_col <- "black"
caption_col  <- "black"

# |-  titles and caption ----
title_text    <- "Harry Potter LEGO Sets"

subtitle_text <- str_wrap("Top-10 sets with over 1,000 pieces.", width = 80)

caption_text   <- str_glue("
                         #TidyTuesday: 2022 Week 36 &bull; Source: frebrickable<br>",
                           "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                           " <span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                           "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                           "<span style='font-family:fa-brands'>&#x23;</span> ggplot")

# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Rampart One", family = "title")
font_add_google("Share", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |-  main plot ----
over_1000_pieces %>% 
    ggplot(aes(x =  num_parts,  y = name)) +
    
    # geometries
    geom_segment(aes(xend = 0, yend = name), size = 2, color = col_1) +
    
    geom_point(size = 6, color = col_1) +
    
    geom_vline(xintercept = 0) +

    geom_label_repel(aes(label= label_text),
                     fontface      = "bold", 
                     box.padding   = 0.8,
                     label.padding = 0.8,
                     fill          = bkg_col, 
                     color         = col_1,
                     direction     = "y",
                     hjust         = 0.5,
                     vjust         = 0.5,
                     nudge_x       = 1,
                     size          = 4
                     ) +
    
    # scales
    scale_x_continuous(breaks = seq(0, 6000, by = 1000),
                       limits = c(0, 7500)) +
    scale_y_discrete() + 
    coord_flip() +
    
    # labs
    labs(
        x        = "", 
        y        = "",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text) +
    
    # theme
    theme_void (
        base_size   = 16,
        base_family = 'text') +

    # theme
    theme_minimal(
        base_size   = 16,
        base_family = 'text') +
     
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title         = element_blank(),
        axis.text          = element_blank(),
        
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_blank(),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.title         = element_markdown(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 65,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_markdown(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.6, 
            face           = "bold",
            size           = 35,
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
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    

# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     

# other attached packages:
# [1] ggrepel_0.9.1        scales_1.2.1         camcorder_0.0.2.9000 glue_1.6.2           skimr_2.1.4         
# [6] here_1.0.1           janitor_2.1.0        MetBrewer_0.2.0      showtext_0.9-5       showtextdb_3.0      
# [11] sysfonts_0.8.8       ggtext_0.1.1         tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0       
# [16] dplyr_1.0.9          purrr_0.3.4          readr_2.1.2          tidyr_1.2.0          tibble_3.1.7        
# [21] ggplot2_3.3.6        tidyverse_1.3.1      pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] httr_1.4.3       bit64_4.0.5      vroom_1.5.7      jsonlite_1.8.0   modelr_0.1.8     assertthat_0.2.1
# [7] pdftools_3.3.0   askpass_1.1      renv_0.15.5      cellranger_1.1.0 qpdf_1.2.0       pillar_1.8.1    
# [13] backports_1.4.1  digest_0.6.29    gridtext_0.1.4   rvest_1.0.2      snakecase_0.11.0 colorspace_2.0-3
# [19] htmltools_0.5.2  pkgconfig_2.0.3  broom_1.0.0      gifski_1.6.6-1   haven_2.5.0      magick_2.7.3    
# [25] tzdb_0.3.0       generics_0.1.3   usethis_2.1.6    ellipsis_0.3.2   withr_2.5.0      repr_1.1.4      
# [31] cli_3.3.0        magrittr_2.0.3   crayon_1.5.1     readxl_1.4.0     fs_1.5.2         fansi_1.0.3     
# [37] xml2_1.3.3       tools_4.2.1      hms_1.1.1        lifecycle_1.0.1  munsell_0.5.0    reprex_2.0.1    
# [43] compiler_4.2.1   rlang_1.0.4      grid_4.2.1       rstudioapi_0.13  rsvg_2.3.1       base64enc_0.1-3 
# [49] gtable_0.3.0     DBI_1.1.3        curl_4.3.2       R6_2.5.1         lubridate_1.8.0  knitr_1.39      
# [55] bit_4.0.4        fastmap_1.1.0    utf8_1.2.2       rprojroot_2.0.3  stringi_1.7.8    parallel_4.2.1  
# [61] Rcpp_1.0.9       vctrs_0.4.1      dbplyr_2.2.1     tidyselect_1.1.2 xfun_0.31 


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36


