 
## Challenge: #TidyTuesday 2023 week 06
## Data:      Pet Cats UK
## Author:    Steven Ponce
## Date:      2023-02-06
 

## 1. LOAD PACKAGES & SETUP ----  
library(pacman)   
p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, here, glue, camcorder, scales)
p_load(lubridate, tidyquant, broom, umap, MetBrewer)   


# figure size
gg_record(
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 6,
    height = 4,
    units  = "in",
    dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 06) 

# Manual
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')

big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')


readme(tt) 
rm(tt)   



## 3. EXAMINING THE DATA ----
glimpse(big_tech_stock_prices) 

big_tech_stock_prices$stock_symbol %>% unique() %>% sort()
range(big_tech_stock_prices$date)

# left join
big_tech_stock_tbl <-big_tech_stock_prices %>% 
    left_join(y = big_tech_companies, by = "stock_symbol") 

# save memory
rm(big_tech_stock_prices, big_tech_companies)



## 4. TIDYDATA ----  

### a) Convert stock prices to a standardized format (daily returns) ----

big_tech_daily_returns_tbl <- big_tech_stock_tbl %>% 
    # select specific columns
    select(stock_symbol, date, adj_close) %>% 
    
    # Filter to dates beginning in the year 2018 and beyond.
    filter(date >= ymd('2011-01-01')) %>% 
    
    # Compute a Lag of 1 day on the adjusted stock price. 
    group_by(stock_symbol) %>% 
    mutate(lag_1day = lag(x = adj_close, n = 1)) %>% 
    ungroup() %>%
    
    # Remove an `NA` values from the lagging operation
    drop_na(lag_1day) %>% 
    
    # Compute the difference between adjusted and the lag
    # Compute the percentage difference by dividing the difference by that lag. 
    mutate(
        diff       = adj_close - lag_1day,
        pct_return = diff / lag_1day) %>% 
    
    # Return only specific columns
    select(stock_symbol, date, pct_return)
 


### b) Convert to User-Item Format ----

stock_date_matrix_tbl <- big_tech_daily_returns_tbl %>% 
    # Spread the `date` column to get the values as percentage returns. 
    # Fill an `NA` values with zeros. 
    pivot_wider(names_from = date, values_from = pct_return, values_fill = 0)



### c) K-Means Clustering ----

k_means_obj <- stock_date_matrix_tbl %>% 
    # Drop the non-numeric column, `symbol`
    select(-stock_symbol) %>% 
    
    # Perform `kmeans()` with `centers = 4` and `nstart = 20`
    kmeans(centers = 4, nstart = 20)

# Use `glance()` to get the `tot.withinss`
glance(k_means_obj) %>% 
    pull(tot.withinss)



### d) Find the optimal value of K ----

kmeans_mapper <- function(center = 3) {
    stock_date_matrix_tbl %>%
        select(-stock_symbol) %>%
        kmeans(centers = center, nstart = 20)
}


# Use purrr to map
k_means_mapped_tbl <- 
    
    # Create a tibble containing column called `centers` that go from 1 to 13
    tibble(centers = 1:13) %>%
    
    # Add a column named `k_means` with the `kmeans_mapper()` output. 
    mutate(k_means = centers %>% map(kmeans_mapper)) %>%
    
    # Use `mutate()` to add the column and `map()` to map centers to the `kmeans_mapper()` function.
    mutate(glance  = k_means %>% map(glance))  


# Visualize Scree Plot 
k_means_mapped_tbl %>%
    
    # Unnest the `glance` column
    unnest(glance) %>% 
    
    # Plot the `centers` column (x-axis) versus the `tot.withinss` column (y-axis) 
    ggplot(aes(x = centers, y = tot.withinss)) + 
    
    # using `geom_point()` and `geom_line()`
    geom_line(size = 0.8) +
    geom_point(size = 2.5) +
    geom_vline(xintercept = 5, linetype = 'dashed', size = 0.8, color = 'gray70') +
    labs(
        title = 'Scree Plot',
        x = "Centers", 
        y = "Total within-cluster\n sum of squares")+
    scale_x_continuous(breaks = seq(0, 10, by = 1))+
    theme_classic()



### e) Dimensionality Reduction ----

# Apply UMAP
umap_results <- stock_date_matrix_tbl %>% 
    
    # De-select the `symbol` column
    select(-stock_symbol) %>% 
    
    # umap()
     
    umap(n_neighbors = 8, n_components = 2, metric = "euclidean")


# Convert umap results to tibble with symbols
umap_results_tbl <- umap_results$layout %>% 
    as_tibble() %>% 
    bind_cols(stock_date_matrix_tbl %>% select(stock_symbol))

umap_results_tbl


# Visualize UMAP results
umap_results_tbl %>% 
    ggplot(aes(x = V1, y = V2)) + 
    geom_point(size = 2, alpha = 0.5, color = "#7E6B8F") +
    labs(title = 'UMAP Projection')+
    ggrepel::geom_label_repel(aes(label = stock_symbol), size = 2)+
    theme_classic()



### f) Combine K-Mean and UMAP ----

# Get the k_means_obj from the 10th center
k_means_obj <- k_means_mapped_tbl %>% 
    
    # Filter to `centers == 5`
    filter(centers == 5) %>% 
    
    # Pull the `k_means` column
    pull(k_means) %>% 
    
    # Pluck the first element
    pluck(1)


# Combine the k_means_obj with the umap_results_tbl
umap_kmeans_results_tbl <- k_means_obj %>%  
    
    # Augment the `k_means_obj` with the `stock_date_matrix_tbl` 
    # to get the clusters added to the end of the tibble
    augment(data = stock_date_matrix_tbl) %>% 
    
    # Select just the `symbol` and `.cluster` columns
    select(stock_symbol, .cluster) %>% 
    
    # Left join the result with the `umap_results_tbl` by the `symbol` column
    left_join(y = umap_results_tbl, by = 'stock_symbol') %>% 
    
    # Left join  
    left_join(y = big_tech_stock_tbl %>% select(stock_symbol, company), by = 'stock_symbol')



data_plot <- umap_kmeans_results_tbl %>% 
    # add label text
    mutate(label_text = str_glue("Stock: {stock_symbol}
                                 Company: {company}")) %>% 
    distinct()
    

# 5. VISUALIZATION ---- 
### |- plot aesthetics ---- 

bkg_col      <- "#F9F9F9"
title_col    <-  "gray10"              
subtitle_col <-  "gray10" 
caption_col  <-  "gray10" 
palette_col  <- met.brewer("Lakota", n = 5, type = "discrete")[c(1,3,4,5,6)]


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: 2023 Week 06 &bull; Source: Big Tech Stock Prices on Kaggle<br>")
tw <- str_glue("<span style='font-family:fa-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")
#mn <- str_glue("<span style='font-family:fa-brands'>&#xf4f6;</span>")

title_text <- str_glue("Big Tech Stocks, 2018-2023")     

subtitle_text <- str_glue("UMAP 2D Projection with K-Means Cluster Assignment") 

# caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 | {mn} @sponce1@graphic.social | {gh} poncest | Tools: #rstats #ggplot")

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 | {gh} poncest | Tools: #rstats #ggplot")

 
### |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Days One", family = "title") 
font_add_google("Rajdhani", family = "subtitle") 
font_add_google("Roboto Condensed", family = "text")                       
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)      
                

### |-  main plot ----

# Visualize the combined K-Means and UMAP results

data_plot %>%                                
    
    ggplot(aes(x = V1, y = V2, color = .cluster)) + 
    
    # geoms
    geom_point(size = 2, alpha = 0.95) +
    ggrepel::geom_label_repel(aes(label = label_text),
                              size = 2,
                              box.padding = 0.3,
                              fill = bkg_col,
                              seed = 46)+
    
    # labs
    labs(
        x = "UMAP X", y = "UMAP Y",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text 
        ) +
    
    # scales
    scale_color_manual(values = palette_col, name = "") +
    scale_x_continuous()+
    scale_y_continuous()+
    
    # theme 
    theme_tq()+
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'plot',
        
        plot.background    = element_rect(fill = bkg_col, color = bkg_col),
        panel.background   = element_rect(fill = bkg_col, color = bkg_col),
        
        plot.margin        = margin(t = 10, r = 20, b = 10, l = 20), 
        
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_blank(),  
        
        plot.title         = element_text(
            family         = 'title',
            color          = title_col,
            face           = "bold",
            size           = 18,  
            margin         = margin(t = 5, b = 5)),   
        
        plot.subtitle      = element_text(
            family         = 'subtitle',
            color          = title_col,
            lineheight     = 0.8, 
            face           = "bold",
            size           = 14,
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
# [1] MetBrewer_0.2.0            umap_0.2.9.0               broom_1.0.0               
# [4] tidyquant_1.0.6            quantmod_0.4.20            TTR_0.24.3                
# [7] PerformanceAnalytics_2.0.4 xts_0.12.2                 zoo_1.8-11                
# [10] lubridate_1.8.0            scales_1.2.1               camcorder_0.1.0           
# [13] glue_1.6.2                 here_1.0.1                 janitor_2.1.0             
# [16] showtext_0.9-5             showtextdb_3.0             sysfonts_0.8.8            
# [19] ggtext_0.1.2               tidytuesdayR_1.0.2         forcats_0.5.1             
# [22] stringr_1.4.0              dplyr_1.0.9                purrr_0.3.4               
# [25] readr_2.1.2                tidyr_1.2.0                tibble_3.1.7              
# [28] ggplot2_3.3.6              tidyverse_1.3.2            pacman_0.5.1              

# loaded via a namespace (and not attached):
# [1] fs_1.6.0            usethis_2.1.6       bit64_4.0.5         httr_1.4.3         
# [5] rprojroot_2.0.3     tools_4.2.2         backports_1.4.1     utf8_1.2.2         
# [9] R6_2.5.1            DBI_1.1.3           colorspace_2.0-3    withr_2.5.0        
# [13] tidyselect_1.2.0    bit_4.0.5           curl_4.3.2          compiler_4.2.2     
# [17] textshaping_0.3.6   cli_3.6.0           rvest_1.0.2         xml2_1.3.3         
# [21] labeling_0.4.2      quadprog_1.5-8      askpass_1.1         digest_0.6.29      
# [25] systemfonts_1.0.4   svglite_2.1.1       pkgconfig_2.0.3     dbplyr_2.2.1       
# [29] rlang_1.0.6         readxl_1.4.0        rstudioapi_0.13     farver_2.1.1       
# [33] generics_0.1.3      jsonlite_1.8.0      vroom_1.6.1         googlesheets4_1.0.0
# [37] magrittr_2.0.3      Matrix_1.4-1        Rcpp_1.0.10         Quandl_2.11.0      
# [41] munsell_0.5.0       fansi_1.0.4         reticulate_1.28     lifecycle_1.0.3    
# [45] stringi_1.7.12      snakecase_0.11.0    grid_4.2.2          parallel_4.2.2     
# [49] ggrepel_0.9.1       crayon_1.5.2        lattice_0.20-45     haven_2.5.0        
# [53] gridtext_0.1.4      hms_1.1.2           magick_2.7.3        pillar_1.8.1       
# [57] markdown_1.1        reprex_2.0.1        gifski_1.6.6-1      renv_0.15.5        
# [61] modelr_0.1.8        vctrs_0.5.2         png_0.1-7           tzdb_0.3.0         
# [65] cellranger_1.1.0    gtable_0.3.0        openssl_2.0.2       assertthat_0.2.1   
# [69] rsvg_2.3.1          RSpectra_0.16-1     ragg_1.2.2          googledrive_2.0.0  
# [73] gargle_1.2.0        ellipsis_0.3.2      

# RStudio 2022.12.0+353 "Elsbeth Geranium" Release (7d165dcfc1b6d300eb247738db2c7076234f6ef0, 2022-12-03) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2022.12.0+353 Chrome/102.0.5005.167 Electron/19.1.3 Safari/537.36

