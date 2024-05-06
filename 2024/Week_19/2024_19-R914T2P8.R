
## Challenge: #TidyTuesday 2024 week 19
## Data:      Rolling Stone Album Rankings
## Author:    Steven Ponce
## Date:      2024-05-06


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(
tidyverse,   # Easily Install and Load the 'Tidyverse'
ggtext,      # Improved Text Rendering Support for 'ggplot2'
showtext,    # Using Fonts More Easily in R Graphs
janitor,     # Simple Tools for Examining and Cleaning Dirty Data
skimr,       # Compact and Flexible Summaries of Data
scales,      # Scale Functions for Visualization
lubridate,   # Make Dealing with Dates a Little Easier
MetBrewer,   # Color Palettes Inspired by Works at the Metropolitan Museum of Art
MoMAColors,  # Color Palettes Inspired by Artwork at the Museum of Modern Art in New York City
glue         # Interpreted String Literals
)

### |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 5.5,
    height = 7,
    units  = "in",
    dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(x = base::as.double("2024"),
                           week = base::as.double("19"))

rolling_stone <- tt$rolling_stone |> clean_names() |> glimpse()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(rolling_stone)
skim(rolling_stone)
colnames(rolling_stone)

rolling_stone |> count(clean_name, sort = T)
rolling_stone |> count(genre, sort = T)
rolling_stone |> count(type, sort = T)

wwbi_data$year |> range()


## 4. TIDYDATA ----

### |- tidy data ---
temp_df <- left_join(
    x = wwbi_data, 
    y = wwbi_country, 
    by = "country_code"
    )

tidy_df <- temp_df |> 
    left_join(
        y = wwbi_series, 
        by = "indicator_code"
        )

# housekeeping
rm(tt, wwbi_country, wwbi_data, wwbi_series, temp_df)

# tidy

tidy_df |> count(indicator_name, count = T)


plot_data <- tidy_df |> 
    # summarise(
    #     mean_value = mean(value),
    #     .by = c(short_name, indicator_code, year, region, income_group)
    # ) |> 
    filter(
        region == "Latin America & Caribbean",
        short_name == "Puerto Rico",
        indicator_name != "Sample size",
        value > 0
        ) |> 
    mutate(indicator_name = str_wrap(indicator_name, width = 35)) 

plot_data |> 
    ggplot(aes(x = indicator_name, y = value)) +
   # geom_boxplot() +
    geom_point() +
    coord_flip() +
    facet_wrap(vars(short_name), scales = "free_y")

# Vix
#' dumbell plot ? or slope chart?
#' direction? meaning good or bad?
#' review code names
#' perhaps focus on bi.pop.totl.cv for the Caribbean 

### |- plot data ---

# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('brown', 0.95) 
title_col    <- "gray10"             
subtitle_col <- "gray10"     
caption_col  <- "gray20"   
text_col     <- "gray20"     
col_palette  <- MoMAColors::moma.colors("Abbott", n = 7, type = "discrete") 

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 18 } &bull; Source: XXXXXX<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("U.S. Energy Consumption by Type from 1949 to 2023") 

subtitle_text <- str_glue("Comparing various sources of energy consumption over time")

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Oswald", regular.wt = 400, family = "title")                 
font_add_google("Quattrocento Sans", regular.wt = 400, family = "subtitle")  
font_add_google("Quattrocento Sans", regular.wt = 400, family = "text")        
font_add_google("Noto Sans", regular.wt = 400,family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
    axis.text             = element_text(size = rel(.8), color = text_col, family = "text"),
    axis.line.x           = element_line(color = "gray90", linewidth = .2),
    panel.grid.minor.y    = element_blank(),
    panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.05, color = 'gray'),
    panel.grid.minor.x    = element_blank(),
    panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray'),
)


### |-  final plot ----  
plot_data |> 
    ggplot(aes(x = year, y = consumption_trillion_btu, 
               group = consumption_type, color = consumption_type)) + 
    
    # Geoms
    geom_smooth(linewidth = 0.5) +
    geom_point(size = 0.3) +
    
    geom_text(
        data = label_data,
        aes(x = year, y = consumption_trillion_btu, label = consumption_type),
        nudge_x  = label_data$nudge_x,
        nudge_y  = label_data$nudge_y,
        hjust    = -0.05,
        vjust    = 0.5,
        fontface = 'bold',
        family   = 'text',
        size     = 2.5
    ) +
    
    # Scales
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    scale_y_continuous(
        breaks = seq(0, 3000, by = 1000),
        limits = c(-200, 3000),
        labels = comma_format()
    ) +
    coord_cartesian(clip = 'off') + 
    scale_color_manual(values = col_palette) +
    guides(colour = guide_legend(title = "Energy Type")) +
    
    # Labs
    labs(x = "Year", 
         y = "Consumption (Trillion BTU)",
         title    = title_text,
         subtitle = subtitle_text,
         caption  = caption_text
    ) +
    
    # Theme
    theme(
        plot.title      = element_text(
            size          = rel(1.4),
            family        = "title",
            face          = "bold",
            color         = title_col,
            lineheight    = 1.1,
            margin        = margin(t = 5, b = 5)
        ),
        plot.subtitle   = element_markdown(
            size          = rel(1), 
            family        = 'subtitle',
            color         = subtitle_col,
            lineheight    = 1.1, 
            margin        = margin(t = 0, b = 5)
        ),
        plot.caption    = element_markdown(
            size          = rel(.5),
            family        = "caption",
            color         = caption_col,
            lineheight    = 1.1,
            hjust         = 0.5,
            halign        = 0.5,
            margin        = margin(t = 5, b = 5)
        )
    )



# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

