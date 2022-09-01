
## Challenge: #TidyTuesday 2022 week 35
## Data:      Pell Grants
## Author:    Steven Ponce
## Date:      2022-08-30   
 

## 1. LOAD PACKAGES & SETUP ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr, glue, camcorder, scales, patchwork)

gg_record(
    dir    = here("temp_plots"),
    device = "png",
    width  = 10,
    height = 15,
    units  = "in",
    dpi    = 600)

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 35) 

# Focus on Ted Lasso tv series
pell <- tt$pell %>% clean_names()
rm(tt) 


#|- offline ----   
# write_csv(pell, '2022/Week_35/pell.csv')
# pell <- read_csv(file = '2022/Week_35/pell.csv') %>% clean_names()


## 3. EXAMINING THE DATA ----
glimpse(pell)
skim(pell)
unique(pell$state)
range(pell$year)


## 4. TIDYDATA ---- 

# |- focus on Puerto Rico (PR) ----
PR_pell <- pell |> 
    filter(state == "PR") 


# |- award per recipient ----
PR_award_per_recipient_school_tbl <- PR_pell |> 
    group_by(year, name) |> 
    mutate(award_per_recipient = award / recipient)  


# |- median award per recipient (p1) ----
PR_median_award_per_recipient_year_tbl <- PR_pell |> 
    group_by(year) |> 
    summarize(
        median_award     = median(award),
        median_recipient = median(recipient)
        ) |> 
    mutate(median_award_per_recipient = median_award / median_recipient) |> 
    arrange(year)


# |- start/end line points ----
years <- c("1999", "2017")

start_end_points_tbl  <- PR_median_award_per_recipient_year_tbl |>     
    filter(year %in% years) 

# |- label text for p1 ----
label_text <- tibble(x = c(1999, 2017),
                     y = c(start_end_points_tbl$median_award_per_recipient[1] ,
                           start_end_points_tbl$median_award_per_recipient[2]),
                     hjust = c(0.5, 0.5),                                            
                      
                     year_text   = c(start_end_points_tbl$year[1], start_end_points_tbl$year[2]),
                      
                     amount_text = c(start_end_points_tbl$median_award_per_recipient[1],
                                      start_end_points_tbl$median_award_per_recipient[2])
                    )
      

# |- KPI (p2) ----
label_text <- label_text |> 
    mutate(KPI_text = scales::percent(amount_text[1] / amount_text[2])) |> 
    add_column(KPI_subtext = "(Not adjusted by cost of living)")


# |- median award per school (p3) ----
PR_schools_year_tbl <- PR_pell |> 
    group_by(year, name) |> 
    summarize(
        median_award     = median(award),
        median_recipient = median(recipient)
    ) |> 
    mutate(median_award_per_recipient = median_award / median_recipient) |> 
    arrange(year) |> 

    group_by(year) |> 
    mutate(
        total_median_recipient = sum(median_recipient),
        total_median_award     = sum(median_award)
    ) |> 
    ungroup() |> 
    
    mutate(
        recipient_pct = median_recipient / total_median_recipient,
        award_pct     = median_award / total_median_award
    )


#|- labels for p3 ----
# tibble for text
annotations_p3 <- tibble(x       = c(10664, 25343, 20453),
                         y       = c(199e6, 168e6, 145e6),
                         hjust   = c(0.6, 1, 0.4),
                         hjust_2 = c(0.6, 1.5, 0.4),           
                         vjust   = c(1, 1, 1),
                         vjust_2 = c(1, -0.3, 1),             
                         
                         title = c("University of Puerto Rico - Central Administration - Rio Piedras", "Inter American University of Puerto Rico", "University of Puerto Rico - Central Administration"),
                         
                         full_text = c("2009, $179 M", "2009, $157 M", "2008, $141 M"))


# tibble for vertical segments
label_segment_vertical_p3 <- tibble(x    = c(19664, 26664, 32964),
                                    xend = c(19664, 26664, 32964),
                                    y    = c(200e6, 170e6, 146e6),
                                    yend = c(179e6, 152e6, 125e6))


# tibble for horizontal segments
label_segment_horizontal_p3 <- tibble(x    = c(33664, 34664, 34964),
                                      xend = c(20664, 27664, 33664),
                                      y    = c(190e6, 160e6, 136e6),
                                      yend = c(190e6, 160e6, 136e6))

# tibble for arrows
label_arrows_p3 <- tibble(x    = c(33664, 34664, 34964),
                          xend = c(39664, 37364, 36664),
                          y    = c(190e6, 160e6, 136e6),
                          yend = c(180e6, 157e6, 139e6))



# 5. VISUALIZATION ----  

# |- aesthetics ---- 
col_1        <- 'black'    
col_2        <- 'black'     
bkg_col      <- "#F7E371"    
title_col    <- 'black'
subtitle_col <- "black"
caption_col  <- "black"

#palette_col  <- met.brewer("Pillement", n = 6, type = "discrete")
# monochromeR::generate_palette("#C5C3C6", modification = "go_lighter", n_colours = 5, view_palette = T)


# |-  titles and caption ---- 
title_text    <- str_glue("Pell Grants")

subtitle_text <- str_glue("Puerto Rico, 1999 - 2017")

caption_text  <- str_glue("#TidyTuesday: 2022 Week 35 &bull; Source: US Dept of Education<br>",
                           "Visualization: <span style='font-family:fa-brands'>&#xf099;</span> @sponce1 &bull; ",
                           " <span style='font-family:fa-brands'>&#xf09b;</span> poncest &bull; ",
                           "Tools: <span style='font-family:fa-brands'>&#x23;</span> rstats",
                           "<span style='font-family:fa-brands'>&#x23;</span> ggplot")


# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Bungee", family = "title")
font_add_google("Coda", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |- Line chart for Median Pell grant per recipient in PR, 1999 - 2017 -----
line_plot <- PR_median_award_per_recipient_year_tbl |> 
    ggplot(aes(x = year, y = median_award_per_recipient)) + 
    
    # geoms
    geom_line(size = 2, color = col_2) + 
    
    geom_point(data = start_end_points_tbl, aes(x = year, y = median_award_per_recipient), size = 7, color = col_1) + 
    
    geom_text(data = label_text, aes(x = x, y = y + 800, label = scales::comma(round(amount_text), prefix = "$"), 
                                     hjust = hjust), vjust = 1, size = 10, fontface = "bold", color = col_1) + 
    
    geom_text(data = label_text, aes(x = x, y = y + 500, label = year_text, hjust = hjust), 
              vjust = 1, size = 10, fontface = "bold", color = col_1) +
    
    # scales
    scale_x_continuous() + 
    scale_y_continuous() + 
    coord_cartesian(clip = "off") +
    
    # labs
    labs(
        title    = title_text,
        subtitle = subtitle_text
        ) +
    
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
            size           = 50,  
            margin         = margin(t = 10)),
        
        plot.subtitle      = element_markdown(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.6, 
            face           = "bold",
            size           = 30,
            margin         = margin(t = 10, b = 30)),
    )

line_plot


# |- KPI plot ----
KPI_plot <- label_text |> 
    ggplot() + 
    
    # geoms
    # triangle
    geom_point(aes(x = 3.1, y = 2.0), size = 10, shape = 17, color = col_2) +
    
    # big number
    geom_text(aes(x = 3.9, y = 5, label = KPI_text[1], hjust = hjust), 
              vjust = 1, size = 24, fontface = "bold", color = col_2) +
    
    # disclaimer 
    geom_text(aes(x = 3.65, y = 0.8, label = KPI_subtext[1], hjust = hjust), 
              vjust = 1, size = 4, fontface = "bold", color = col_2) +
    
    # scales
    scale_x_continuous(limits = c(0, 5))+
    scale_y_continuous(limits = c(0, 5))+
    
    # theme
    theme_void() 

KPI_plot


# |- Scatter plot median award vs. median recipient  -----
scatter_plot <- PR_schools_year_tbl |> 
    ggplot(aes(x = median_recipient, y = median_award)) + 
    
    # geoms
    geom_point(size = 5, na.rm = TRUE, shape = 21, color = 'black', fill = 'gray20', alpha = 0.45) + 
    
    geom_smooth(method = 'lm', se = FALSE, size = 0.6, linetype = 2, color = 'black' ) +
    
    # university label
    geom_text(data = annotations_p3, aes(x = x, y = y + 250, label = str_wrap(title, 40), hjust = hjust, vjust = vjust),
              size = 5, fontface = "bold", color = col_1) +
    
    # year and grant amount
    geom_text(data = annotations_p3, aes(x = x, y = y - 15e6, label = full_text, hjust = hjust_2, vjust = vjust_2),
              size = 8, fontface = "bold", color = col_1) +  
    
    # label segments
    geom_segment(data = label_segment_vertical_p3, aes(x=x, xend = xend, y = y,  yend = yend), color = col_1) +
    geom_segment(data = label_segment_horizontal_p3, aes(x=x, xend = xend, y = y,  yend = yend), color = col_1) +
    
    # label arrows
    geom_segment(data = label_arrows_p3, aes(x=x, xend = xend, y = y,  yend = yend),
                 arrow = arrow(length = unit(2, "mm")), color = col_1) +
    
    # scales
    scale_x_continuous(labels = scales::comma,
                       breaks = seq(0, 45000, by = 10000),
                       limits = c(-5, 45000)) +    
    
    scale_y_continuous(labels = dollar_format(suffix = " M", scale = 1e-6),
                       breaks = seq(0, 200e6, by = 50e6),
                       limits = c(0, 200e6)) +            
                
    # labs
    labs(
        x = "Median Recipients", y = "Median Award",
        caption  = caption_text) +

    # theme
    theme_minimal(base_size   = 16, base_family = 'text') +
    theme(
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        axis.title.x       = element_text(size = 16, face = 'bold', margin = margin (t = 10)),
        axis.title.y       = element_text(size = 16, face = 'bold', margin = margin (r = 10)),
        
        axis.line.x        = element_line(color = "black"),
        axis.ticks.x       = element_line(color = "black"),
        
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.4, color = 'gray'),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.caption       = element_markdown( 
            family         = 'caption',
            color          = caption_col, 
            lineheight     = 0.6, 
            size           = 12,
            hjust          = 0.5,
            margin         = margin(t = 20, b = 10)),
    )


# |- Putting all plots together ---
top <- line_plot +  
    inset_element(KPI_plot, left = 0, bottom = 0, right = 1.02, top = 0.2)


final <- top / scatter_plot +
    plot_layout(heights = c(2, 3))



## 6. SESSION INFO ---- 

# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)

# Matrix products: default
 
# locale: 
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

# other attached packages:
# [1] patchwork_1.1.1      scales_1.2.1         camcorder_0.0.2.9000 glue_1.6.2          
# [5] skimr_2.1.4          here_1.0.1           janitor_2.1.0        MetBrewer_0.2.0     
# [9] showtext_0.9-5       showtextdb_3.0       sysfonts_0.8.8       ggtext_0.1.1        
# [13] tidytuesdayR_1.0.2   forcats_0.5.1        stringr_1.4.0        dplyr_1.0.9         
# [17] purrr_0.3.4          readr_2.1.2          tidyr_1.2.0          tibble_3.1.7        
# [21] ggplot2_3.3.6        tidyverse_1.3.1      pacman_0.5.1        

# loaded via a namespace (and not attached):
# [1] nlme_3.1-158      fs_1.5.2          usethis_2.1.6     lubridate_1.8.0   bit64_4.0.5      
# [6] httr_1.4.3        rprojroot_2.0.3   repr_1.1.4        tools_4.2.1       backports_1.4.1  
# [11] utf8_1.2.2        R6_2.5.1          mgcv_1.8-40       DBI_1.1.3         colorspace_2.0-3 
# [16] withr_2.5.0       tidyselect_1.1.2  bit_4.0.4         curl_4.3.2        compiler_4.2.1   
# [21] textshaping_0.3.6 cli_3.3.0         rvest_1.0.2       xml2_1.3.3        labeling_0.4.2   
# [26] askpass_1.1       systemfonts_1.0.4 digest_0.6.29     base64enc_0.1-3   pkgconfig_2.0.3  
# [31] htmltools_0.5.2   dbplyr_2.2.1      fastmap_1.1.0     rlang_1.0.4       readxl_1.4.0     
# [36] rstudioapi_0.13   farver_2.1.1      generics_0.1.3    jsonlite_1.8.0    vroom_1.5.7      
# [41] magrittr_2.0.3    Matrix_1.4-1      Rcpp_1.0.9        munsell_0.5.0     fansi_1.0.3      
# [46] lifecycle_1.0.1   stringi_1.7.8     snakecase_0.11.0  grid_4.2.1        parallel_4.2.1   
# [51] crayon_1.5.1      lattice_0.20-45   splines_4.2.1     haven_2.5.0       gridtext_0.1.4   
# [56] hms_1.1.1         magick_2.7.3      knitr_1.39        pillar_1.8.1      markdown_1.1     
# [61] reprex_2.0.1      pdftools_3.3.0    qpdf_1.2.0        gifski_1.6.6-1    renv_0.15.5      
# [66] modelr_0.1.8      vctrs_0.4.1       tzdb_0.3.0        selectr_0.4-2     cellranger_1.1.0 
# [71] gtable_0.3.0      assertthat_0.2.1  xfun_0.31         broom_1.0.0       rsvg_2.3.1       
# [76] ragg_1.2.2        ellipsis_0.3.2   


# RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36


