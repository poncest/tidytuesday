## Challenge: #TidyTuesday 2022 week 14
## Author: Steven Ponce
## Date: 2022-04-05


## 1. Load packages ----
library(tidyverse, quietly = TRUE)
library(tidytuesdayR, quietly = TRUE)
library(showtext)
library(ggtext)
library(skimr)
library(highcharter)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 14)
data <- tt$news_orgs
# rm(tt)

# OFFLINE - In case of VPN issues
# data <- write_csv(data, '2022/Week_14/news_orgs.csv')
# data <- read.csv('2022/Week_14/news_orgs.csv')

## 3. Examine the data ----

glimpse(data)
skim(data)

range(data$year_founded)
unique(data$owner) 


## 4. Tidydata ----

pr_news <- data %>% 
    # focus on news_orgs in PR
    filter(state == 'PR') %>%   
    
    # select specific columns
    #select(state, city, year_founded, publication_name) %>% 
    select(year_founded, city, publication_name) %>% 
    
    # replace `NA` with not specified
    mutate_at('city', ~replace(., is.na(.), 'Not Specified')) %>% 
    
    # trim white spaces
    mutate(publication_name = str_trim(publication_name))



## 5. Sankey diagram ----

# base plot
p <- hchart(data_to_sankey(pr_news), "sankey", name = "News Orgs in PR")
p


# customization
p %>%
    # theme
    hc_add_theme(hc_theme_ft()) %>% 
    
    # margins
    hc_chart(
        marginTop     = 120,
        marginBottom  = 50,
        marginLeft    = 20,
        marginRight   = 10,
        spacingBottom = 10
        
    ) %>% 
    
    # labels
    hc_title(
        text = "Reported News Organization in Puerto Rico (2007 - 2017)",
        align = "left",
        style = list(fontSize   = "60px", 
                     color      = "#262626", 
                     fontFamily = "Abril Fatface",
                     fontWeight = "bold")
    ) %>%
    
    hc_subtitle(
        text = "Major news outlets on the island are missing from this dataset",
        align = "left",
        style = list(fontSize  = "40px", 
                     color     = "#333333", 
                     fontStyle = "italic")
    ) %>% 
    
    hc_caption(
        text = "#TidyTuesday: Week 14 • Source: Project Oasis • Visualization: Steven Ponce (@sponce1)",
        align = "center",
        style = list(fontSize   = "16px", 
                     color      = "#999999", 
                     fontFamily = "Barlow Condensed",
                     fontWeight = "bold")
    ) %>% 
    
    # data labels
    hc_plotOptions(
        series = list(dataLabels = list(style = list(fontSize      = "30px", 
                                                     color         = "#003d59", 
                                                     fontFamily    = "Barlow Condensed",
                                                     align         = "right",
                                                     verticalAlign = "top",
                                                     fontWeight    = "bold"))))
                                                     #textOutline  = 'none'))))

 

# resolution
showtext_opts(dpi = 300) 
     

## 6. Save final figure ----
ggsave('2022/Week_14/2022_14_news_orgs.png', 
       width = 20, height = 11, units = 'in',  dpi = 300)


showtext_auto(FALSE)

