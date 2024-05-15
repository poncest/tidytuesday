
## Challenge: #TidyTuesday 2024 week 20
## Data:      The Great American Coffee Taste Test
## Author:    Steven Ponce
## Date:      2024-05-13


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
    glue,        # Interpreted String Literals
    ggalluvial,  # Alluvial Plots in 'ggplot2' # Alluvial Plots in 'ggplot2' # Alluvial Plots in 'ggplot2'
    ggrepel      # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
)

### |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  = 6,         # 5.5
    height = 8,           # 
    units  = "in",
    dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
coffee_survey <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv') |> 
    clean_names() |> glimpse()


## 3. EXAMINING THE DATA ----
glimpse(coffee_survey)
skim(coffee_survey)
colnames(coffee_survey)

coffee_survey |> count(age, sort = T)
coffee_survey |> count(where_drink, sort = T)
coffee_survey |> count(favorite, sort = T)
coffee_survey |> count(purchase_other, sort = T)
coffee_survey |> count(additions, sort = T)
coffee_survey |> count(wfh, sort = T)
coffee_survey |> count(gender, sort = T)


## 4. TIDYDATA ----
### |- tidy data ---
alluvial_data <- coffee_survey |>
    filter(!is.na(favorite) & !is.na(additions)) |>
    count(favorite, additions) |>
    arrange(desc(n)) |>
    slice_head(n = 20)                                                          # Top 20 combinations


### |- plot data ---
plot_data <- alluvial_data |>
    mutate(
        favorite = gsub("Milk, dairy alternative, or coffee creamer", "Milk/Creamer", favorite),
        additions = gsub("Sugar or sweetener", "Sugar/Sweet", additions),
        additions = gsub("Milk, dairy alternative, or coffee creamer", "Milk/Creamer", additions),
        additions = gsub("No - just black", "Black", additions),
        additions = gsub("Flavor syrup", "Flavor", additions)
    )   
    

# 5. VISUALIZATION ---- 
### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten("gray", .8) 
title_col    <- "gray10"             
subtitle_col <- "gray10"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette <- c(
    "#1E1311",  # Dark Roast
    "#3C1A10",  # Espresso
    "#4B2E2F",  # Mocha
    "#62231B",  # Coffee Bean
    "#73331E",  # Cappuccino
    "#8B4513",  # Brown Sugar
    "#A0522D",  # Cocoa
    "#B5651D",  # Caramel
    "#CD853F",  # Medium Roast
    "#D2691E",  # Light Roast
    "#DEB887",  # Biscuit
    "#F1B18B",  # Latte
    "#FFE4C4",  # Cream
    "#FFDAB9",  # Peach Puff
    "#FFF5EE"   # Seashell (very light cream)
)

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 20 } &bull; Source: James Hoffmann and Cometeer Great American Coffee Taste Test Survey 500<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Coffee Preferences and Additions") 

subtitle_text <- str_glue("Top 20 combinations based on number of responses")

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
    axis.title            = element_blank(),
    axis.text             = element_text(size = rel(1), color = text_col, family = "text"),
    axis.text.x           = element_text(size = rel(1), color = text_col, family = "text", face = "bold"),
    axis.line.x           = element_line(color = "gray90", linewidth = .2),
    panel.grid.minor.y    = element_blank(),
    panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.05, color = 'gray'),
    panel.grid.minor.x    = element_blank(),
    panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray'),
)

# Reference: https://corybrunson.github.io/ggalluvial/articles/labels.html

### |-  main plot ----  

# Plot the Alluvial diagram
plot_data |> 
    ggplot(aes(axis1 = favorite, axis2 = additions, y = n)) +
    
    # Geoms
    geom_alluvium(aes(fill = favorite), width = 1/12) +
    geom_stratum(aes(fill = favorite), width = 1/8, alpha = .4) +
    
    ggrepel::geom_text_repel(
        aes(label = after_stat(stratum)),
        stat = "stratum", size = 3.5, direction = "y", nudge_x = -0.2, nudge_y = 1,
        color = "black", segment.color = "grey50"
    ) +
    
    # Scales
    scale_x_discrete(limits = c("Favorite Coffee", "Additions"), expand = c(.2, .1)) +
    scale_fill_manual(values = col_palette) +    
    
    # Labs
    labs(
        x        = NULL,
        y        = NULL,
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text
    ) +
   
    # Theme
    theme(
        plot.title      = element_markdown(
            size          = rel(1.8),            
            family        = "title",
            face          = "bold",
            color         = title_col,
            lineheight    = 1.1,
            margin        = margin(t = 5, b = 5)
        ),
        plot.subtitle     = element_markdown(
            size          = rel(1.1), 
            family        = 'subtitle',
            color         = subtitle_col,
            lineheight    = 1.1, 
            margin        = margin(t = 5, b = 5)
        ),
        plot.caption      = element_markdown(
            size          = rel(.5),
            family        = "caption",
            color         = caption_col,
            lineheight    = 1.1,
            hjust         = 0,
            halign        = 0,
            margin        = margin(t = 5, b = 5)
        ),
    )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ──────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.0 (2024-04-24 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-05-15
# rstudio  2024.04.0+735 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────
# ! package     * version    date (UTC) lib source
# base        * 4.4.0      2024-04-24 [2] local
# P base64enc     0.1-3      2015-07-28 [?] CRAN (R 4.4.0)
# P bit           4.0.5      2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5      2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder     0.1.0      2022-10-03 [?] CRAN (R 4.4.0)
# P cli           3.6.2      2023-12-11 [?] CRAN (R 4.4.0)
# colorspace    2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1      2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0      2024-04-24 [?] local
# cowplot       1.1.3      2024-01-22 [1] CRAN (R 4.4.0)
# P crayon        1.5.2      2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1      2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0      2024-04-24 [?] local
# P digest        0.6.35     2024-03-11 [?] CRAN (R 4.4.0)
# dplyr       * 1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
# P fansi         1.0.6      2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.1      2022-07-06 [1] CRAN (R 4.4.0)
# P fastmap       1.1.1      2023-02-24 [?] CRAN (R 4.4.0)
# forcats     * 1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
# P ggalluvial  * 0.12.5     2023-02-22 [?] CRAN (R 4.4.0)
# ggplot2     * 3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
# P ggrepel     * 0.9.5      2024-01-10 [?] CRAN (R 4.4.0)
# ggstream      0.1.0      2021-05-06 [1] CRAN (R 4.4.0)
# P ggtext      * 0.1.2      2022-09-16 [?] CRAN (R 4.4.0)
# P gifski        1.12.0-2   2023-08-12 [?] CRAN (R 4.4.0)
# P glue        * 1.7.0      2024-01-09 [?] CRAN (R 4.4.0)
# P graphics    * 4.4.0      2024-04-24 [?] local
# P grDevices   * 4.4.0      2024-04-24 [?] local
# P grid          4.4.0      2024-04-24 [?] local
# P gridtext      0.1.5      2022-09-16 [?] CRAN (R 4.4.0)
# gtable        0.3.5      2024-04-22 [1] CRAN (R 4.4.0)
# P here          1.0.1      2020-12-13 [?] CRAN (R 4.4.0)
# P hms           1.1.3      2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools     0.5.8.1    2024-04-04 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0      2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.8      2023-12-04 [?] CRAN (R 4.4.0)
# P knitr         1.46       2024-04-06 [?] CRAN (R 4.4.0)
# labeling      0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle     1.0.4      2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3      2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3      2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3      2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.12       2023-12-06 [?] CRAN (R 4.4.0)
# P MetBrewer   * 0.2.0      2022-03-21 [?] CRAN (R 4.4.0)
# P methods     * 4.4.0      2024-04-24 [?] local
# MoMAColors  * 0.0.0.9000 2024-05-02 [1] Github (BlakeRMills/MoMAColors@6f5d75d)
# munsell       0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
# P pacman        0.5.1      2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0      2024-04-24 [?] local
# P pillar        1.9.0      2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3      2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1      2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0      2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12     2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5      2024-01-10 [?] CRAN (R 4.4.0)
# P renv          1.0.7      2024-04-11 [?] CRAN (R 4.4.0)
# P repr          1.1.7      2024-03-22 [?] CRAN (R 4.4.0)
# P rlang         1.1.3      2024-01-10 [?] CRAN (R 4.4.0)
# P rprojroot     2.0.4      2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0     2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0      2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2      2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7      2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0        2020-06-04 [?] CRAN (R 4.4.0)
# P skimr       * 2.1.5      2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1     2023-08-27 [?] CRAN (R 4.4.0)
# P stats       * 4.4.0      2024-04-24 [?] local
# P stringi       1.8.3      2023-12-11 [?] CRAN (R 4.4.0)
# P stringr     * 1.5.1      2023-11-14 [?] CRAN (R 4.4.0)
# P svglite       2.1.3      2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts    * 0.8.9      2024-03-02 [?] CRAN (R 4.4.0)
# P systemfonts   1.0.6      2024-03-07 [?] CRAN (R 4.4.0)
# P textshaping   0.3.7      2023-10-09 [?] CRAN (R 4.4.0)
# P tibble      * 3.2.1      2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
# P tidyverse   * 2.0.0      2023-02-22 [?] CRAN (R 4.4.0)
# P timechange    0.3.0      2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0      2024-04-24 [?] local
# P tzdb          0.4.0      2023-05-12 [?] CRAN (R 4.4.0)
# P utf8          1.2.4      2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0      2024-04-24 [?] local
# P vctrs         0.6.5      2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5      2023-12-05 [?] CRAN (R 4.4.0)
# P withr         3.0.0      2024-01-16 [?] CRAN (R 4.4.0)
# P xfun          0.43       2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6      2023-12-04 [?] CRAN (R 4.4.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/windows/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/ebed5364
# 
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────────────
# > 

