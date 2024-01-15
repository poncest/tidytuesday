
## Challenge: #TidyTuesday 2024 week xx 
## Data:      xxx xxx xxx
## Author:    Steven Ponce
## Date:      2024-xx-xx


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, skimr, scales, lubridate)
pacman::p_load()


### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 5,
  units  = "in",
  dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(x = base::as.double(yr_chr), 
                            week = base::as.double(week_chr)) 

var <- tt$var |> clean_names() |> glimpse()

tidytuesdayR::readme(tt) 
rm(tt)  


## 3. EXAMINING THE DATA ----
skim(var)
glimpse(var)
colnames(var) |> sort()


## 4. TIDYDATA ----

### |- Tidy ----



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- ""
title_col    <- ""          
subtitle_col <- ""  
caption_col  <- ""  
text_col     <- ""   

col_palette  <- c("" = "", "" = "")


### |-  titles and caption ----
tt <- str_glue("#TidyTuesday: { yr_chr } Week { week_chr } &bull; Source: insert source here <br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text    <- str_glue("insert title here") 

subtitle_text <- str_glue("insert subtitle.<br>",
                          "insert subtitle.<br>")
                          

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Crimson Pro", family = "title")                            
font_add_google("Crimson Pro", family = "subtitle")   
font_add_google("Crimson Pro", family = "text")  
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = 'plot',
  
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(.8), color = text_col, family   = 'text', face = 'bold'),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(.8), color = text_col, family   = 'text', face = 'bold'),
  axis.text             = element_text(size = rel(.65), color = text_col, family   = 'text'),
  
  # panel.spacing         = unit(.9, "lines"),
  # strip.text            = element_text(size = rel(1)),
  
  plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),

  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  
)

### |-  final plot ----  

data |> 
  
  ggplot(aes(x = , y = )) +
  
  # Geoms
  
  # Scales
  
  # Labels
  
  # Facets (if applicable)
  
  # Theme
  theme( 
    panel.grid.minor      = element_blank(),
    panel.grid.major      = element_blank(),
    
    # strip.text            = element_textbox(size     = 11,
    #                                       color    = text_col,
    #                                       family   = 'text',
    #                                       r        = unit(5, "pt"),
    #                                       width    = unit(5.5, "npc"),
    #                                       padding  = margin(0, 0, 0, 0),
    #                                       margin   = margin(0, 20, 0, -1),
    #                                       fill     = "transparent"),
    
    # panel.spacing      = unit(1.5 , 'lines'),
    
    plot.title           = element_text(
      size               = rel(1), 
      family             = 'title',
      color              = title_col,
      margin             = margin(t = 5, b = 10)),
    
    plot.subtitle        = element_text(
      size               = rel(.8), 
      family             = 'subtitle',
      color              = title_col,
      lineheight         = 0.8, 
      margin             = margin(t = 5, b = 20)),
    
    plot.caption         = element_markdown(
      size               = rel(.5), 
      family             = 'caption',
      color              = caption_col,
      lineheight         = 0.6,
      hjust              = 0.5,
      halign             = 0.5,
      margin             = margin(t = 10, b = 5)),
  )

# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

