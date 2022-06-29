
## Challenge: #TidyTuesday 2022 week 26
## Data:      UK gender pay gap
## Author:    Steven Ponce
## Date:      2022-06-28


## 1. Load packages ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, here, skimr)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 26) 
paygap <- tt$paygap
rm(tt)


## 3. Examine the data ----
dim(paygap)
glimpse(paygap)
skim(paygap)
colnames(paygap)
unique(paygap$'employer_name') %>% sort()


## 4. Tidydata ----
# paygap in pharma companies 
pharma_tbl <- paygap %>% 
    # lower case 
    mutate(employer_name = str_to_lower(employer_name)) %>%
    
    # pharma companies
    mutate(name = case_when(
        employer_name = str_starts(employer_name, 'pfizer ')        ~ 'Pfizer',
        employer_name = str_starts(employer_name, 'bristol-myers')  ~ 'Bristol Myers Squibb',
        employer_name = str_starts(employer_name, 'astrazeneca')    ~ 'AstraZeneca',
        employer_name = str_starts(employer_name, 'merck ')         ~ 'Merck',
        employer_name = str_starts(employer_name, 'glaxo')          ~ 'GSK',
        employer_name = str_starts(employer_name, 'johnson & ')     ~ 'J&J',
        
        employer_name = str_starts(employer_name, 'roche ')         ~ 'Roche',
        employer_name = str_starts(employer_name, 'novartis')       ~ 'Novartis',
        employer_name = str_starts(employer_name, 'abbvie')         ~ 'Abbvie',
        employer_name = str_starts(employer_name, 'takeda ')        ~ 'Takeda',
        employer_name = str_starts(employer_name, 'eli lilly')      ~ 'Eli Lilly',
        employer_name = str_starts(employer_name, 'boehringer ')    ~ 'Boehringer',
        
        employer_name = str_starts(employer_name, 'gilead ')        ~ 'Gilead',
        employer_name = str_starts(employer_name, 'teva')           ~ 'Teva',
        employer_name = str_starts(employer_name, 'biogen')         ~ 'Biogen',
        employer_name = str_starts(employer_name, 'bayer ')         ~ 'Bayer',
        employer_name = str_starts(employer_name, 'astellas')       ~ 'Astellas',
        employer_name = str_starts(employer_name, 'amgen ')         ~ 'Amgen',
        
        employer_name = str_starts(employer_name, 'novo nordisk ')  ~ 'Novo Nordisk',
        employer_name = str_starts(employer_name, 'vertex')         ~ 'Vertex',
        employer_name = str_starts(employer_name, 'catalent')       ~ 'Catalent',
        employer_name = str_starts(employer_name, 'abbott ')        ~ 'Abbott',
        employer_name = str_starts(employer_name, 'actavist')       ~ 'Actavist')
    ) %>% 
        
        select(employer_name, name, male_top_quartile , female_top_quartile)  %>% 
        drop_na(name)  %>% 
    
    # group and summarize
    group_by(name) %>% 
    summarise(
        median_top_male_quartile   = median(male_top_quartile),
        median_top_female_quartile = median(female_top_quartile)
        ) %>% 
    ungroup() %>% 
    
    # pivot longer 
    pivot_longer(cols      = c(median_top_male_quartile, median_top_female_quartile),
                 names_to  = 'gender',
                 values_to = 'median_top_quartile_pct') %>% 
    
    # didn't quite works as intended
    # mutate(name = fct_reorder(name, median_top_quartile_pct, .fun = min))  
    
    # manual reorder 
    mutate(name = fct_relevel(name, c('Roche', 'Amgen', 'Novo Nordisk', 'Biogen', 'Abbvie',
                                      'Bristol Myers Squibb', 'Catalent', 'Novartis', 'J&J',
                                      'Takeda', 'Bayer', 'Gilead', 'Boehringer', 'GSK', 'Merck',
                                      'Eli Lilly', 'Pfizer', 'AstraZeneca', 'Vertex', 
                                      'Teva', 'Astellas', 'Abbott')))


## 5. Visualization ----
# Plot aesthetics 
col_1          <- '#8700f9' # female    
col_2          <- '#00c4aa' # male
bkg_col        <- 'white'    
title_col      <- 'black' 
subtitle_col   <- "black" 
caption_col    <- "black"

# titles and caption
title_text    <- 'UK gender pay gap in the pharmaceutical sector?'

subtitle_text <- "The top quartile earners are predominatly <span style='font-size:40pt; color:#00c4aa'>**males**</span> - with notable exceptions from<br>Roche, Amgen, Novo Nordisk, Biogen, Abbvie, and Bristol Myers Squibb"

caption_text <- paste0("**#TidyTuesday:** 2022 Week 26<br>",
                       "**Source:** gender-pay-gap.service.gov.uk<br>",
                       "**Visualization:** Steven Ponce (@sponce1) • **Tools:** #rstats, #ggplot")

# fonts
font_add_google("Anton", family = "title")
font_add_google("Oswald", family = "subtitle")
font_add_google("Oswald", family = "text")
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE) 

# plot
pharma_tbl %>% 
    ggplot(aes(x = median_top_quartile_pct, 
               y = name, 
               fill = gender)) + 
    
    # geoms
    geom_col(width = 1.0) + 
    geom_vline(xintercept = 50, 
               linetype   = 5,
               size       = 0.6,
               color      = 'black',
               alpha      = 0.6 ) +
    
    # scales
    scale_fill_manual(values = c(col_1, col_2)) +
    
    scale_x_continuous(position = 'top',
                       expand   = c(0, 0),
                       labels   = c('0', '25',  '50%', '75', '100'),
                       limits   = c(-5,100)) + 
    
    coord_cartesian(clip = 'off', expand = FALSE) +
    
    # labs
    labs(
        x = '', y = '',
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text
    ) +
    
    # theme
    theme_minimal(18)  +
        theme(
            plot.title.position   = "plot",
            plot.caption.position = "plot",
            legend.position       = 'plot',
            
            panel.grid            = element_blank(),
            plot.background       = element_rect(fill = bkg_col, color = bkg_col),
            panel.background      = element_rect(fill = bkg_col, color = bkg_col),
            axis.text.y           = element_text(margin = margin(r = -210), hjust = 0),
            plot.margin           = margin(t = 30, r = 80, b = 30, l = 80),
           
             plot.title   = element_text(
                family    = 'title',
                color     = title_col,
                face      = "bold",
                size      = 60,  
                margin    = margin(t = 10)),
            
            plot.subtitle = element_markdown(
                family    = 'subtitle',
                color     = title_col,
                lineheight= 0.6, 
                size      = 40,
                margin    = margin(t = 10, b = 10)),
            
            plot.caption  = element_markdown(
                family    = 'caption',
                color     = caption_col, 
                size      = 14,
                hjust     = 0.98,
                margin    = margin(t = 10, b = 10)),
            )
      
 
# resolution
showtext_opts(dpi = 300)


## 6. Save final figure ----
ggsave('2022/Week_26/2022_26_paygap.png',
       width = 20, height = 11, units = 'in', dpi = 300)

showtext_auto(FALSE)

