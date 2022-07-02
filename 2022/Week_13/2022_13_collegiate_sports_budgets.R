## Challenge: #TidyTuesday 2022 week 13
## Author: Steven Ponce
## Date: 2022-03-29


## 1. Load packages ----
library(tidyverse, quietly = TRUE)
library(tidytuesdayR, quietly = TRUE)
library(showtext)
library(ggtext)
library(skimr)
library(gghighlight)

## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 13)
data <- tt$sports
# rm(tt)

# OFFLINE - In case of VPN issues
# data <- write_csv(data, '2022/Week_13/sports.csv')
# data <- read.csv('2022/Week_13/sports.csv')

## 3. Examine the data ----

glimpse(data)
skim(data)

range(data$year)
unique(data$sports) 


## 4. Tidydata ----

rodeo <- data %>% 
    # focus on rodeo and specific states
    filter(sports == 'Rodeo', state_cd %in% c('TX')) %>% 
    
    # select specific cols
    select(year, institution_name:state_cd, classification_name, 
           ef_male_count, ef_female_count, sum_partic_men:sports) %>% 
    
    # normalize the data
    mutate(
        prop_partic_men    =  sum_partic_men / sum(ef_male_count),
        prop_partic_female =  sum_partic_women / sum(ef_female_count),
        
        perc_partic_men    = scales::percent((prop_partic_men)),
        perc_partic_female = scales::percent((prop_partic_female)),
        
        # ratio = 0.75, means that $0.75 are spent for every dollar earned in revenue.
        # we want to minimize this ratio 
        exp_rev_ratio_men    = exp_men / rev_men,
        exp_rev_ratio_female = exp_women / rev_women
    ) %>% 
    
    # select specific cols
    select(year:classification_name, sports:exp_rev_ratio_female) %>% 
    
    # filter NA 
    filter(!is.na(exp_rev_ratio_men)) %>% 
    filter(!is.na(exp_rev_ratio_female))


# rm(data)

## 5. Visualization ----

# fonts
font_add_google(family='Smokum', 'Smokum')                             # title
font_add_google(family='Roboto', 'Roboto')                           # text
font_add_google(family='Saira Semi Condensed', 'Saira Semi Condensed') # caption
showtext_auto(enable = TRUE) 


# base plot - mens data only
g <- rodeo %>% 
    
    ggplot(aes(x = year, y = exp_rev_ratio_men, color = institution_name)) +
    
    # geometries
    geom_line(size = 1) + 
    geom_point(size = 3) +
    # refence line
    geom_hline(yintercept = 1, color = "grey20", size = 0.6, linetype = "dashed") +
    
    # scales
    scale_y_continuous(
        limits = c(0.5, 1.5),
        breaks = c(0.5, 1.0, 1.5)) +
    
    scale_x_continuous(
        expand = c(0.01, 0.01),
        limits = c(2015, 2019),
        breaks = c(2015, 2017, 2019)) +
    
    coord_cartesian(clip = "off") 
    
g  

# customization 
g +
    # facet
    facet_wrap(~ institution_name) +
    
    # highlight break-even or profit (ratio < 1.0)
    gghighlight(exp_rev_ratio_men < 1, 
                label_key = institution_name,
                use_direct_label = FALSE,
                unhighlighted_params = list(color = "grey80", size = .6)
                ) +
    
    # colors
    scale_color_viridis_d(option = "C", end = 0.8, begin = 0.1, guide = "none") +
    
    # labs
    labs(
        x = "", y = 'Revenue / Expense Ratio',
        title = 'Schools in the state of Texas where Rodeo break-even or is profitable',
        subtitle = "Panola College and Western Texas Collage are spending less than a dollar for every dollar earned in revenue for mens participation.",
        caption = "#TidyTuesday: Week 13 • Data: Collegiate Sports Budgets • Visualization: Steven Ponce (@sponce1)"
    ) +
    
    # theme
    theme_minimal(base_family = 'Roboto') +
    
    theme( 
        
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#f7f1f0", color = "#f7f1f0"),
        plot.background = element_rect(fill = "#f7f1f0", color = "#f7f1f0"),

        panel.grid.minor = element_blank(),
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black"),
        axis.text        = element_text(size = 14),
        axis.title.y     =  element_text(size = 16,face="bold"),
        
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = 'none',

        strip.text    = element_text(size = 16, face = "bold"),
        panel.spacing = unit(2, "lines"),
        
        plot.margin   = margin(t = 15, r = 10, b = 15, l = 10),
        
        plot.title  = element_text(
            family  = 'Smokum',
            #color  = title_col,
            face    = "bold",
            size    = 55,  
            margin  = margin(t = 10)),
        
        plot.subtitle = element_text(
            family    = 'Roboto',
            #color    = title_col,
            size      = 20,  
            margin    = margin(t = 10, b = 10)),
        
        plot.caption = element_text(
            family   = 'Saira Semi Condensed',
            #color   = "#8a8b91",
            size     = 14,
            hjust    = 0.99,
            margin   = margin(t = 10, b = 10)))
) 
    

# resolution
showtext_opts(dpi = 300) 
     

## 6. Save final figure ----
ggsave("2022/Week_13/2022_13_collegiate_sports_budgets.png", plot = last_plot(),
       width = 20, height = 11, units = 'in',  dpi = 300)

showtext_auto(FALSE)

