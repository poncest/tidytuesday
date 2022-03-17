## Challenge: #TidyTuesday 2022 week 11
## Author: Steven Ponce
## Date: 2022-03-15


## 1. Load packages -----------------------------------------------
library(pacman)
p_load(tidyverse, tidytuesdayR, showtext, ggtext, skimr, lubridate, ggrepel)


## 2. Read in the data --------------------------------------------
tt <- tidytuesdayR::tt_load('2022-03-15')
cran <- tt$cran 

# rm(tt)

# OFFLINE - In case of VPN issues
# cran <- write_csv(cran, '2022/Week_11/cran.csv')
# cran <- read_csv('2022/Week_11/cran.csv')

## 3. Examine the data --------------------------------------------

glimpse(cran)
skim(cran)

unique(cran$package)

## 4. Tidydata --------------------------------------------

# focus on cran dataset and core tidymodels packages
core_tidymodels <- c('tidymodels', 'rsample', 'parsnip', 'recipes', 'workflows', 'tune', 'yardstick', 'broom', 'dials')


# prep the data
data <- cran %>%
    # select core tidymodels
    filter(package %in% core_tidymodels) %>% 
    
    select(package, rmd) %>% 
    
    group_by(package) %>% 
    
    summarise(rmd_count = sum(rmd)) %>% 
    
    ungroup() %>% 
    
    # order by rmd count from High to Low
    mutate(package = package %>% fct_reorder(rmd_count)) %>% 
    
    # rank
    arrange(desc(rmd_count)) %>% 
    mutate(rank = row_number()) %>% 
    
    # label text
    mutate(label_text = str_glue(
        'Rank: {rank}\nPkg: {package}\nRmd Count: {rmd_count}'
        ))


## 5. Visualization --------------------------------------------

## fonts
font_add_google(family='Goldman', 'Goldman')                              # title
font_add_google(family='Meera Inimai', 'Meera Inimai')                    # text
font_add_google(family='Saira Semi Condensed', 'Saira Semi Condensed')    # caption
showtext_auto(enable = TRUE) 

data %>% 
    
    ggplot(aes(x =  rmd_count,  y = package)) +
    
    # geometries
    geom_segment(aes(xend = 0, yend = package),  
                 size = 2,
                 color = '#fea789') + 
    
    geom_point(size = 6,
               color = '#f17c83') + 
    
    geom_label_repel(aes(label = label_text),
               fontface = 'bold.italic', 
               box.padding = 0.25,
               label.padding = 0.2,
               fill="#ffdcd0", 
               color ="#727272",
               direction = 'both',
               hjust = 0.95,
               size = 4,
               ) +
    
    # scales
    scale_x_continuous(limits = c(0, 150)) + 
    scale_y_discrete() +

    # labs
    labs(
        title = 'Ranking of CRAN RMarkdown Vignettes ',
        subtitle = "for Core Tidymodels Packages from 2014 to 2021",
        caption = paste0(
            "#TidyTuesday: Week 11 • Data: Robert Flight GitHub • Visualization: Steven Ponce (@sponce1)"),
        x = '',
        y = '' ) +
    
    # theme
    theme_minimal(base_family = "Meera Inimai") +
     
    theme(
        plot.title.position = "plot",
        axis.text   = element_blank(),
        panel.grid  = element_blank(),
        plot.background = element_rect(fill = "#ffdcd0", color = "#ffdcd0"),
        panel.background = element_rect(fill = "#ffdcd0", color = "#ffdcd0"),
    
        plot.margin = margin(t = 15, r = 0, b = 10, l = 10),
        
        plot.title = element_text(
            family = 'Goldman',
            color = "#48494a",
            face = "bold",
            size = 50,  
            margin = margin(t = 10)),
        
        plot.subtitle = element_text(
            family = 'Goldman',
            color = "#48494a",
            size = 32,  
            margin = margin(t = 5, b = 10)),
        
        plot.caption = element_text(
            color = "#8a8b91",
            family = 'Saira Semi Condensed',
            size = 12,
            hjust = .5,
            margin = margin(t = 10, b = 10)))

# resolution
showtext_opts(dpi = 300)
          
     

## 6. Save final figure --------------------------------------------
ggsave("2022/Week_11/2022_11_cran_bioc_vignattes.png", plot = last_plot(),
       width = 20, height = 11, units = 'in',  dpi = 300)


showtext_auto(FALSE)
