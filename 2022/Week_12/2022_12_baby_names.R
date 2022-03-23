## Challenge: #TidyTuesday 2022 week 12
## Author: Steven Ponce
## Date: 2022-03-22


## 1. Load packages ----
library(pacman)
p_load(tidyverse, tidytuesdayR, showtext, ggtext, skimr)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load('2022-03-22')
data <- tt$babynames 
# rm(tt)

# OFFLINE - In case of VPN issues
# babynames <- write_csv(babynames, '2022/Week_12/baynames.csv')
# data <- read.csv('2022/Week_12/babynames.csv')

## 3. Examine the data ----

glimpse(data)
skim(data)

unique(data$name)
range(data$year)

## 4. Tidydata ----

df1 <- data %>% 
    group_by(year)%>%
    summarise(
        N = sum(n[name == 'Brenda']),
        total = sum(n)) %>% 
    mutate(name = 'Brenda') %>% 
    ungroup() 


df2 <- data %>% 
    group_by(year)%>%
    summarise(
        N = sum(n[name == 'Andrea']),
        total = sum(n)) %>% 
    mutate(name = 'Andrea') %>% 
    ungroup() 


df3 <- data %>% 
    group_by(year)%>%
    summarise(
        N = sum(n[name == 'Gabriela']),
        total = sum(n)) %>% 
    mutate(name = 'Gabriela') %>% 
    ungroup() 

#rm(data)

## 5. Visualization ----

# colors
col_1      <- '#B68A81'
col_2      <- '#C6A67A'
col_3      <- '#90A295'
bkg_col    <- '#f7f5f4'
title_col  <- '#48494A'
 
# fonts
font_add_google(family='Barrio', 'Barrio')                             # title
font_add_google(family='Slackey', 'Slackey')                           # text
font_add_google(family='Saira Semi Condensed', 'Saira Semi Condensed') # caption
showtext_auto(enable = TRUE) 

# base plot  
g <- df1 %>% 
    
    ggplot(aes(x = year, y = N)) + 
    
    # geometries
    geom_area(fill = col_1, size = 1, alpha = .85) +

    geom_area(data = df2,
              fill = col_2, size = 1, alpha = .85) +
    
    geom_area(data = df3,
              fill = col_3, size = 1, alpha = .55) +
    
    # annotations
    annotate("text", x = 1955, y = 24876,
             hjust    = 0, 
             vjust    = 0,
             color    = col_1,
             family   = 'Slackey',
             fontface = "bold",
             size     = 6,
             label    = str_glue("Brenda\n",
                                 "(1957: 24,376)")) +
    
    annotate("text", x = 1981, y = 12000,
             hjust    = 0.5, 
             vjust    = 0,
             color    = col_2,
             family   = 'Slackey',
             fontface = "bold",
             size     = 6,
             label    = str_glue("Andrea\n",
                                 "(1981: 11,836)")) +
    
    annotate("text", x = 2003, y = 4268,
             hjust    = 0.5, 
             vjust    = 0.5,
             color    = col_3,
             family   = 'Slackey',
             fontface = "bold",
             size     = 6,
             label    = str_glue("Gabriela\n",
                                 "(2003: 3,368)"))
    
g
    

# final plot ----
g +

# scales
scale_y_continuous(limits = c(0, 25000))+
scale_x_continuous(limits = c(1900, 2017))+
coord_cartesian(clip = 'off')+

# labs 
labs(
   title = "The popularity of my girl's name across the years",
   subtitle = "1880 - 2017",
   caption = paste0(
       "#TidyTuesday: Week 12 • Data: US babynames & nzbabynames • Visualization: Steven Ponce (@sponce1)"),
   x = '',
   y = '' ) +

# theme
theme_minimal(base_family = 'Slackey') +  

theme(
   plot.title.position = "plot",
   axis.text.x      = element_text(size = 14), 
   axis.text.y      = element_blank(),  
   panel.grid       = element_blank(),
   plot.background  = element_rect(fill = bkg_col, color = bkg_col),
   panel.background = element_rect(fill = bkg_col, color = bkg_col),
   
   plot.margin = margin(t = 15, r = 0, b = 15, l = 10),

   plot.title = element_text(
       family = 'Barrio',
       color  = title_col,
       face   = "bold",
       size   = 50,  
       margin = margin(t = 10)),
   
   plot.subtitle = element_text(
       family    = 'Barrio',
       color     = title_col,
       size      = 32,  
       margin    = margin(t = 15, b = 10)),
   
   plot.caption = element_text(
       family   = 'Saira Semi Condensed',
       color    = "#8a8b91",
       size     = 14,
       hjust    = 0.5,
       margin   = margin(t = 10, b = 10)))
   
 
# resolution
showtext_opts(dpi = 300)
     

## 6. Save final figure --------------------------------------------
ggsave("2022/Week_12/2022_12_baby_names.png", plot = last_plot(),
       width = 20, height = 11, units = 'in',  dpi = 300)


showtext_auto(FALSE)

