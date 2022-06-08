
## Challenge: #TidyTuesday 2022 week 23
## Author:    Steven Ponce
## Date:      2022-06-07  
 

## 1. Load packages ----
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext)
p_load(janitor, gt, gtExtras)
 
 
## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 23) 

static_list         <- tt$static_list %>% clean_names()
pride_aggregates    <- tt$pride_aggregates %>% clean_names()


## 3. Examine the data ----
glimpse(static_list)
glimpse(pride_aggregates)


## 4. Tidydata ----

# static_list 
static_tbl <- static_list %>% 
    select(company:hrc_business_pledge)

# pride aggregate 
pride_tbl <- pride_aggregates %>% 
    select(company:total_contributed) %>% 
    filter(company != 'Grand Total')

# combining the two previous tibbles together
company_sponsor_pledge_contribution_tbl <- pride_tbl %>% 
    left_join(y = static_tbl, by = 'company') %>% 
    select(company, pride, hrc_business_pledge, total_contributed) 
   

remove(tt, static_list, pride_aggregates)
gc()


## 5. Visualization ----
# text
title_text <- "Public Perception vs. Quiet Action"

subtitle_text <- "Companies identified as Pride friendly but have donated towards anti-LBGTQ campaigns"

footnote_text <- str_glue("#TidyTuesday: 2022 Week 23 • ",
                          "Source: Data For Progress • ",
                          "Visualization: Steven Ponce (@sponce1) • Tools: #rstats, #ggplot")

# table / viz:
tab_1 <- company_sponsor_pledge_contribution_tbl %>% 
    
    mutate(`anti-LBGTQ donations` = scales::dollar(total_contributed)) %>% 
    
    mutate(ratio = total_contributed / sum(total_contributed)) %>% 
    
    # add check mark and x
    mutate(`pride sponsor` = case_when(
        pride == 'TRUE' ~ "✓", 
        TRUE ~ 'x'
    )) %>% 
    
    mutate(`pride pledge` = case_when(
        hrc_business_pledge == 'TRUE' ~ "✓",
        TRUE ~ 'x'
    )) %>% 
    
    select(company, `pride sponsor`, `pride pledge`, `anti-LBGTQ donations`, ratio) %>%
    
    gt() %>% 
    
    # bar plor
    gt_plt_bar(column = ratio, scaled = TRUE) %>% 
    
    # highlight sponsor & pledge
    gt_highlight_rows(
        rows = c(2, 4, 7, 24, 25, 26, 29), 
        fill = "lightgrey",
        bold_target_only = TRUE,
        target_col = company
    ) %>% 
    
    # theme
    gt_theme_538() %>% 
    
    # title, subtitle, and caption
    tab_header(title = html(title_text), 
               subtitle =  html(subtitle_text)) %>% 
    
    tab_footnote(footnote = html(footnote_text), 
                 placement = 'auto') 

# format 
tab_1 <- tab_1 %>% 
    tab_options(
        heading.title.font.size      = px(50),
        heading.subtitle.font.size   = px(20),
        heading.padding              = px(10),
        footnotes.font.size          = px(15),
        footnotes.padding.horizontal = px(80),
    )
 
tab_1


## 6. Save final figure ----
tab_1 %>% gtsave(path ='2022/Week_23/', filename = "2022_23_donations.png")

sessionInfo()
