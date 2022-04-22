
# REFERENCE: https://mjskay.github.io/ggdist/articles/dotsinterval.html

library(pacman)
p_load(lubridate, tidyverse, ggtext, showtext, ggridges, ggdist, janitor, skimr)



## Big Dave’s Crossword Blog (The Daily Telegraph, The Sunday Telegraph)
data <- read.csv('2022/Week_16/big_dave.csv')

## 4. Tidydata ----
test_data <- data %>% 
    drop_na() %>% 
    
    # select specific columns
    select(clue:puzzle_name) %>%
    select(-clue_number) %>% 
    
    # date 
    mutate(
        puzzle_date = ymd(puzzle_date),
        year        = year(puzzle_date)
    ) %>% 
    
    # lower case 
    mutate(
        clue       = str_to_lower(clue),
        answer     = str_to_lower(answer),
        definition = str_to_lower(definition)
    ) %>%            
    
    # remove digits ans punctuation in puzzle_name & clue
    mutate(
        puzzle_name = gsub("\\ [0−9]+$","", puzzle_name),
        puzzle_name = gsub("Daily Telegraph[0−9]+$","Daily Telegraph", puzzle_name),
        puzzle_name = gsub("Sunday Telegraph[0−9]+$","Sunday Telegraph", puzzle_name),
        puzzle_name = gsub("Daily Telegraph-[0−9]+$","Daily Telegraph", puzzle_name),
        puzzle_name = gsub("MPP-[0−9]+$","MPP", puzzle_name),
        puzzle_name = gsub("â€“","", puzzle_name),
        clue        = gsub("\\ ([0−9])+$","", clue),
        
        # trim 
        puzzle_name = str_trim(puzzle_name %>% fct_relevel('MPP', after = 4)) ,   
        clue        = str_trim(clue),
        definition  = str_trim(definition)
    ) %>% 
    
    # distinct answer
    distinct(answer, .keep_all = TRUE) %>%              
    
    # number of distinct answer  & clue characters
    mutate(
        answer_chr      = str_length(answer),
        clue_chr        = str_length(clue),
        definition_chr  = str_length(definition)
        )
 

# Top_5 `puzzle_name` with count >= 1,000

top_5 <- test_data %>%
    select(answer, puzzle_name, answer_chr) %>% 
    group_by(puzzle_name) %>% 
    summarise(N = n()) %>% 
    arrange(desc(N)) %>% 
    ungroup() %>% 
    filter(N > 800) 

top_5_names <- top_5$puzzle_name


# median labels
labels <- test_data %>% 
    group_by(puzzle_name) %>% 
    filter(puzzle_name %in% top_5_names) %>% 
    summarise(med_answer = median(answer_chr)) %>%
    arrange(desc(med_answer)) %>% 
    ungroup()


## I"M HERE   ----
test_data %>% 
    # top 5 puzzle with total count > 800
    filter(puzzle_name %in% top_5_names) %>% 
    
    # plot canvas
    ggplot(aes(x = answer_chr, y = puzzle_name, fill = puzzle_name)) +
    
    # geometries
    geom_density_ridges(rel_min_height  = 0.0005, 
                                  scale = 0.8,
                                  stat  = "binline", 
                                  bins  = 20,
                                  alpha = 0.6) +
    
    stat_pointinterval(
        aes(fill = puzzle_name),
        .width = c(0.025, 0.975),
        color = "black",
        shape = 21,
        stroke = 0.5
    ) +
    
    # data labels
    geom_text(
        data = labels,
        mapping = aes(x = med_answer,
                      y = puzzle_name,
                      label = paste0('median: ', med_answer)),
        color   = title_col,
        size    = 5,
        hjust   = 0.5,
        vjust   = 0.001,
        nudge_y = 0.15
    ) +
    
    # scales
    scale_fill_brewer(palette = 3, direction = 1) +        
    
    scale_y_discrete(expand = c(0.01, 0)) + 
    
    scale_x_continuous(breaks = seq(0, 35, by = 5),
                       limits = c(0, 35),
                       expand = c(0.01, 0)) +
    
    labs(
        title = 'Top 5 Crossword Puzzles - Big Dave’s Crossword Blog',
        subtitle = "show a similar distribution for the number of character in answer ",
        caption = "#TidyTuesday: Week 16 • Data: cryptics.georgeho.org • Visualization: Steven Ponce (@sponce1)",
        x = "Number of characters in answer", 
        y = '',
    )  +
    
    # theme
   theme_minimal(base_family = 'Cutive Mono') + 
   theme(
       plot.title.position   = 'plot',
       legend.position       = 'none',
       
       panel.grid            = element_blank(),
       plot.background       = element_rect(fill = bkg_col, color = bkg_col),
       panel.background      = element_rect(fill = bkg_col, color = bkg_col),
       
       panel.grid.minor      = element_blank(),
       axis.line.x           = element_line(color = "black"),
       axis.ticks.x          = element_line(color = "black"),
       axis.text             = element_text(size = 18),
       axis.title.x          = element_text(margin = margin(t = 20),
                                            hjust = 0.5,
                                            size = 22, 
                                            face="bold"),
       
       plot.margin      = margin(t = 30, r = 40, b = 30, l = 30),
       
       plot.title    = element_markdown(
           family    = 'Bebas Neue',
           color     = title_col,
           face      = "bold",
           size      = 65,  
           margin    = margin(t = 10)),
        
       plot.subtitle = element_markdown(
           family    = 'Bebas Neue',
           color     = title_col,  
           #face      = "bold",
           size      = 45,  
           margin    = margin(t = 5, b = 40)),
       
       plot.caption  = element_markdown(
           family    = 'Saira Semi Condensed',
           color     = '#808080', 
           size      = 14,
           hjust     = 0.98,
           margin    = margin(t = 20, b = 10)),
    ) 


#6843BD
col_1      <- '#E5F5F9'
col_2      <- '#99D8C9'
bkg_col    <- '#CDC1E9'    #'#A690D8' 
title_col  <- '#48494A'

# google fonts
font_add_google(family="Bebas Neue", "Bebas Neue")                  # title
font_add_google(family="Cutive Mono", "Cutive Mono")                  # text
font_add_google(family='Saira Semi Condensed', 'Saira Semi Condensed')  # caption
showtext_auto(enable = TRUE) 

 

              