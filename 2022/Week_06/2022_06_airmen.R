
## Challenge: #TidyTuesday 2022 week 06
## Author: Steven Ponce
## Date: 2022-02-09

## 1. Load packages and data ----
library(pacman)
p_load(tidyverse, tidytuesdayR, skimr, highcharter, lubridate)

tt <- tidytuesdayR::tt_load('2022-02-08')
airmen <- tt$airmen

## Examine the data
skim(airmen)
glimpse(airmen)


## 2. Data frame for Sankey plot ----
data_plot <- airmen %>% 
   
  # selected columns
  select(graduated_from, rank_at_graduation, pilot_type) %>%  

  # string replace
  mutate_at('rank_at_graduation', str_replace, 'Captain', 'Capt') %>% 
  mutate_at('pilot_type', str_replace, 'Liason pilot', 'Liaison pilot') %>% 
  mutate_at('graduated_from', str_replace, 'TAAF', 'Tuskegee Army Air Field') %>% 
  
  # remove NA and Unk
  filter(rank_at_graduation != 'N/A' & rank_at_graduation != 'Unk') %>% 
  drop_na()
  

## 3. Sankey diagram ----
p <- hchart(data_to_sankey(data_plot), "sankey", name = "Tuskegee Airmen")

p %>%
  hc_title(text= "Sankey Diagram for Tuskegee Airmen") %>%
  hc_subtitle(text= "The majority of the Tuskegee Airmen graduated as 2nd LT from the Tuskegee Army Air Field<br> and flew a single-engine plane")  %>%
  hc_credits(enabled = TRUE, text = "Data Source: Commemorative Air Force (CAF)")%>%
  hc_caption(text = "Visualization: Steven Ponce | #TidyTuesday | 2022 Week 06.")%>%
  hc_plotOptions(series = list(dataLabels = list(style = list(fontSize = "12px")))) %>%
  hc_add_theme(hc_theme_elementary())


## 4. save final figure ----
ggsave("./2022/Week_06/2022_06_airmen.png", width = 12, height = 10, units = "in", dpi = 320)

