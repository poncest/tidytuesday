
## Challenge: #TidyTuesday 2022 week 08
## Author: Steven Ponce
## Date: 2022-02-22


## 1. Load packages and data ----
library(pacman)
p_load(tidyverse, tidytuesdayR, skimr, here, showtext, ggtext, janitor, ggbump)

tt <- tidytuesdayR::tt_load('2022-02-22')
freedom <- tt$freedom %>% 
    clean_names()


## 2. Examine the data ----
skim(freedom)
glimpse(freedom)


## data for bump chart
americas <- freedom %>% 
    # filter  region 
    filter(region_name == 'Americas' ) %>%      
    
    # reorganize, remove unnecessary columns
    select(-region_code, -is_ldc)  %>% 
    select(region_name, country, year, cl, pr, status) %>% 
    
    # add score column
    rowwise() %>%
    mutate(score = mean(c_across(cl:pr))) %>% 
    
    # group every 5 years 
    mutate(
        years5_group = ifelse(year %in% 1995, '1995',
                             ifelse(year %in% 1996:2000, '2000',
                                    ifelse(year %in% 2001:2005, '2005',
                                           ifelse(year %in% 2006:2010, '2010',
                                                  ifelse(year %in% 2011:2015, '2015',
                                                         ifelse(year %in% 2016:2020, '2020', ""
                                                         ))))))
    ) %>% 
    
    group_by(region_name, country, years5_group) %>%
    
    # calculate median score
    summarise(score_median = median(score)) %>%  
    
     # add combined status column
    mutate(
        combined_status  = case_when(
            score_median <= 2.5 ~ 'Free',
            score_median > 2.5 & score_median < 5.1  ~ 'Partially Free',
            TRUE ~ 'Not Free'
        )) %>% 

    ungroup()



## 3. Bump chart -----

## add google fonts
font_add_google(family='PT Serif', 'PT Serif')
showtext_auto()   

# south american countries
countries <-  c("Brazil", "Colombia","El Salvador", "Guyana", "Suriname","Uruguay","Argentina","Belize", 
                "Paraguay",  "Venezuela", "Bolivia" , "Chile", "Peru")

g <- americas %>%
    
    filter(country %in% countries) %>% 
    
    ggplot(aes(x = as.numeric(years5_group),
               y = score_median,
               color = country)) + 
    
    geom_bump(size = .4) +
    geom_point(size = 2) +
    
    # avoid clipping of points close to the border
    coord_cartesian(clip = "off") +
    
    ## change colors - only highlight Venezuela
    scale_color_manual(values = c('grey', 'grey', 'grey', 'grey', 'grey', 'grey', 'grey',
                                  'grey', 'grey', 'grey', 'grey', 'grey', '#ee3e23')) +
    
    scale_x_continuous(breaks = seq(1995, 2020, by = 5.0)) + 
    
    scale_y_continuous(breaks = seq(1, 7, by = 2.0),
                       limits = c(1, 7)) +
    
    # labels for status
    annotate(geom = "text", x = 1991, y = 2, hjust = 0, size = 6, color = "black", label = "Free", family = "PT Serif") + 
    annotate(geom = "text", x = 1991, y = 4, hjust = 0, size = 6, color = "black", label = "Partially Free", family = "PT Serif") + 
    annotate(geom = "text", x = 1991, y = 6, hjust = 0, size = 6, color = "black", label = "Not Free", family = "PT Serif") + 
    
    ## denote status range
    annotate(geom = "segment", x = 1994, xend = 1994, y = 1, yend = 2.5, size = .3, color = "#010101", alpha=.4) +
    annotate(geom = "segment", x = 1994, xend = 1994, y = 3, yend = 5, size = .3, color = "#010101", alpha=.4) +
    annotate(geom = "segment", x = 1994, xend = 1994, y = 5.5, yend = 7, size = .3, color = '#010101', alpha=.4) +
    
    # labels
    labs(
        x = "",
        y = '',
        title = 'Civil Liberties & Political Rights in South America',
        subtitle = "Dramatic Shift in <span style='font-size:35pt; color:#ee3e23'>**Venezuela**</span> since 1995",
        caption = paste0("Data: UN and Freedom House • Visualization: Steven Ponce • #TidyTuesday • 2022 • Week 08")
    ) +

    # theme
    theme_minimal(base_family = "PT Serif") +
    theme(legend.position = "none") +
    
    theme(
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#f7f1f0", color = "#f7f1f0"),
        plot.background = element_rect(fill = "#f7f1f0", color = "#f7f1f0"),
       
         plot.margin = margin(t = 10, r = 15, b = 10, l = 5),
        
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "#010101",
                                   size = 16,
                                   margin = margin(t = 10)),
        
        plot.title = element_text(
            color = "#010101",
            face = "bold",
            size = 40,
            margin = margin(t = 10)),
        
        plot.subtitle = element_markdown(
            color = "#010101",
            size = 30,
            margin = margin(t = 10, b = 10)),
        
        plot.caption = element_text(
            color = "grey50",
            size = 14,
            hjust = .5,
            margin = margin(t = 15, b = 5)) 
)


## 4. Save final figure ----
ggsave("2022/Week_08/2022_08_world_freedom_index.png", g, width=1366, height=836,units="px")

