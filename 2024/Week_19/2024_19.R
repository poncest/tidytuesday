
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
glue,        # Interpreted String Literals
beeswarm    # The Bee Swarm Plot, an Alternative to Stripchart
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


## 4. TIDYDATA ----

### |- tidy data ---
rolling_stone_tidy <- rolling_stone |> 
    filter(!is.na(genre)) |>
    mutate(
        genre = case_when(
            genre == "Soul/Gospel/R&B"                    ~ "Soul, Gospel,\nR&B",
            genre == "Hip-Hop/Rap"                        ~ "Hip-Hop,\nRap",
            genre == "Blues/Blues Rock"                   ~ "Blues,\nBlues Rock",
            genre == "Indie/Alternative Rock"             ~ "Indie,\nAlternative Rock",
            genre == "Punk/Post-Punk/New Wave/Power Pop"  ~ "Punk, Post-Punk,\nNew Wave, Power Pop",
            TRUE ~ factor(genre)
        )
    ) 

### |- plot data ---
plot_data <- rolling_stone_tidy |>
    mutate(
        genre = fct_lump(genre, n = 5),
        genre = fct_reorder(genre, weeks_on_billboard, .na_rm = TRUE),
        highlight = ifelse(weeks_on_billboard > 400, "yes", "no")
      ) |> 
    filter(genre != "Other") 


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
colors  <- MoMAColors::moma.colors("Flash", n = 8, type = "discrete")[c(1,4, 7)]
bkg_col      <- colorspace::lighten(colors[1], .8) 
title_col    <- "gray10"             
subtitle_col <- "gray10"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <- c(no = colorspace::lighten(colors[2], .1), 
                  yes = colorspace::lighten(colors[3], .2)) 

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 19 } &bull; Source: Rolling Stone 500<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Are you tired of the constant rap beef between Kendrick and Drake?<br> 
                          Well, move away from that drama because there\\'s a queen in town -<br>
                          and her name is Adele!") 

subtitle_text <- str_glue("A comparative look at the top 5 genres from Rolling Stone\\'s 500 albums<br>
                          based on their Billboard longevity<br><br>
                           <span style='font-size:8pt; color:gray30'>__Note__: The data points that did not have a specified genre were excluded
                          from the visualization</span>")

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


### |-  main plot ----  
p <- plot_data  |> 
    ggplot(aes(x = weeks_on_billboard, y = genre, color = highlight)) +
    
    # Geoms
    ggbeeswarm::geom_beeswarm(cex = 1, size = 1.2, alpha = 0.75, shape = 21, na.rm = TRUE) +
    
    ggbeeswarm::geom_beeswarm(data = plot_data |>  filter(weeks_on_billboard > 400),
                              cex = 1, size = 2, alpha = 0.75, shape = 19, na.rm = TRUE) +

    # Scales
    scale_x_continuous() +
    scale_y_discrete() +
    scale_color_manual(values = col_palette) +
    coord_cartesian(clip = "off") +
    
    # Labs
    labs(
        x        = "Weeks on Billboard",
        y        = "Genre",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text
    ) +
    
    # Theme
    theme(
        plot.title      = element_markdown(
            size          = rel(1.05),
            family        = "title",
            face          = "bold",
            color         = title_col,
            lineheight    = 1.1,
            margin        = margin(t = 5, b = 5)
        ),
        plot.subtitle     = element_markdown(
            size          = rel(0.9), 
            family        = 'subtitle',
            color         = subtitle_col,
            lineheight    = 1.1, 
            margin        = margin(t = 10, b = 5)
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
        axis.title.y = element_text(angle = 0, vjust = 0.9)
    )
    
### |-  Annotated plot ----  
p +

  # Adele
  annotate(
    "text",
    x = 350, y = "Soul, Gospel,\nR&B", label = "Adele\nAlbum: 21, Wks: 588",
    color = colors[3], size = 3, vjust = -1.3, hjust = 0.5,
    fontface = "bold", family = "text"
  ) +

  annotate(
    "curve",
    x = 480,
    y = as.numeric(factor("Indie,\nAlternative Rock", levels = levels(plot_data$genre))) - 1.6,  # Converting factor level to numeric
    xend = 588,
    yend = as.numeric(factor("Soul, Gospel,\nR&B", levels = levels(plot_data$genre))) - 0.95,    # Converting factor level to numeric
    color = "gray60",
    size = .2,
    linetype = "solid",
    curvature = -0.3,
    arrow = arrow(
      length = unit(0.2, "cm"),
      type = "closed",
      ends = "last"
    )
  ) +
    
    # Kendrick
    annotate(
        "text",
        x = 280, y = "Hip-Hop,\nRap", label = "Kendrick Lamar\nAlbum: good kid, m.A.A.d city, Wks: 575",
        color = colors[3], size = 3, vjust = -1.75, hjust = 0.5,
        fontface = "bold", family = "text"
    ) +
    
    annotate(
        "curve",
        x = 510,
        y = as.numeric(factor("Soul, Gospel,\nR&B", levels = levels(plot_data$genre))) - 1.5, 
        xend = 575,
        yend = as.numeric(factor("Hip-Hop,\nRap", levels = levels(plot_data$genre))) - 0.95,   
        color = "gray60",
        size = .2,
        linetype = "solid",
        curvature = -0.3,
        arrow = arrow(
            length = unit(0.2, "cm"),
            type = "closed",
            ends = "last"
        )
    ) +
    
    # Drake
    annotate(
        "text",
        x = 340, y = "Blues,\nBlues Rock", label = "Drake\nAlbum: Take Care, Wks: 557",
        color = colors[3], size = 3, vjust = -1.55, hjust = 0.5,
        fontface = "bold", family = "text"
    ) +
    
    annotate(
        "curve",
        x = 500,
        y = as.numeric(factor("Blues,\nBlues Rock", levels = levels(plot_data$genre))) + .48, 
        xend = 557,
        yend = as.numeric(factor("Hip-Hop,\nRap", levels = levels(plot_data$genre))) - 1.05,   
        color = "gray60",
        size = .2,
        linetype = "solid",
        curvature = 0.3,
        arrow = arrow(
            length = unit(0.2, "cm"),
            type = "closed",
            ends = "last"
        )
    )
    

# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ─────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.0 (2024-04-24 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-05-06
# rstudio  2024.04.0+735 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────
# ! package      * version    date (UTC) lib source
# base         * 4.4.0      2024-04-24 [2] local
# P base64enc      0.1-3      2015-07-28 [?] CRAN (R 4.4.0)
# P beeswarm     * 0.4.0      2021-06-01 [?] CRAN (R 4.4.0)
# P bit            4.0.5      2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5      2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0      2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger     1.1.0      2016-07-27 [?] CRAN (R 4.4.0)
# P cli            3.6.2      2023-12-11 [?] CRAN (R 4.4.0)
# colorspace     2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1      2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0      2024-04-24 [?] local
# cowplot        1.1.3      2024-01-22 [1] CRAN (R 4.4.0)
# P crayon         1.5.2      2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1      2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0      2024-04-24 [?] local
# P digest         0.6.35     2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
# P fansi          1.0.6      2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.1      2022-07-06 [1] CRAN (R 4.4.0)
# P fastmap        1.1.1      2023-02-24 [?] CRAN (R 4.4.0)
# forcats      * 1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
# P fs             1.6.4      2024-04-25 [?] CRAN (R 4.4.0)
# generics       0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
# P ggbeeswarm     0.7.2      2023-04-29 [?] CRAN (R 4.4.0)
# ggplot2      * 3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
# ggstream       0.1.0      2021-05-06 [1] CRAN (R 4.4.0)
# P ggtext       * 0.1.2      2022-09-16 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2   2023-08-12 [?] CRAN (R 4.4.0)
# P glue         * 1.7.0      2024-01-09 [?] CRAN (R 4.4.0)
# P graphics     * 4.4.0      2024-04-24 [?] local
# P grDevices    * 4.4.0      2024-04-24 [?] local
# P grid           4.4.0      2024-04-24 [?] local
# P gridtext       0.1.5      2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5      2024-04-22 [1] CRAN (R 4.4.0)
# P here           1.0.1      2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3      2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools      0.5.8.1    2024-04-04 [?] CRAN (R 4.4.0)
# P httr           1.4.7      2023-08-15 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0      2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8      2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.46       2024-04-06 [?] CRAN (R 4.4.0)
# labeling       0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle      1.0.4      2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3      2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3      2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3      2022-03-30 [?] CRAN (R 4.4.0)
# P markdown       1.12       2023-12-06 [?] CRAN (R 4.4.0)
# P MetBrewer    * 0.2.0      2022-03-21 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0      2024-04-24 [?] local
# MoMAColors   * 0.0.0.9000 2024-05-02 [1] Github (BlakeRMills/MoMAColors@6f5d75d)
# munsell        0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
# P pacman         0.5.1      2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0      2024-04-24 [?] local
# P patchwork    * 1.2.0      2024-01-08 [?] CRAN (R 4.4.0)
# P pillar         1.9.0      2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3      2019-09-22 [?] CRAN (R 4.4.0)
# P pkgload        1.3.4      2024-01-16 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache        0.16.0     2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3    1.8.2      2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo           1.26.0     2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils        2.12.3     2023-11-18 [?] CRAN (R 4.4.0)
# P R6             2.5.1      2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.0      2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp           1.0.12     2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5      2024-01-10 [?] CRAN (R 4.4.0)
# P readxl         1.4.3      2023-07-06 [?] CRAN (R 4.4.0)
# P renv           1.0.7      2024-04-11 [?] CRAN (R 4.4.0)
# P repr           1.1.7      2024-03-22 [?] CRAN (R 4.4.0)
# P rlang          1.1.3      2024-01-10 [?] CRAN (R 4.4.0)
# P rprojroot      2.0.4      2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0     2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0      2023-10-08 [?] CRAN (R 4.4.0)
# P rvest          1.0.4      2024-02-12 [?] CRAN (R 4.4.0)
# scales       * 1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
# P selectr        0.4-2      2019-11-20 [?] CRAN (R 4.4.0)
# P sessioninfo    1.2.2      2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7      2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0        2020-06-04 [?] CRAN (R 4.4.0)
# P skimr        * 2.1.5      2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1     2023-08-27 [?] CRAN (R 4.4.0)
# P stats        * 4.4.0      2024-04-24 [?] local
# P stringi        1.8.3      2023-12-11 [?] CRAN (R 4.4.0)
# P stringr      * 1.5.1      2023-11-14 [?] CRAN (R 4.4.0)
# P styler         1.10.3     2024-04-07 [?] CRAN (R 4.4.0)
# P svglite        2.1.3      2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts     * 0.8.9      2024-03-02 [?] CRAN (R 4.4.0)
# P systemfonts    1.0.6      2024-03-07 [?] CRAN (R 4.4.0)
# P textshaping    0.3.7      2023-10-09 [?] CRAN (R 4.4.0)
# P tibble       * 3.2.1      2023-03-20 [?] CRAN (R 4.4.0)
# tidyr        * 1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
# P tidytuesdayR   1.0.3      2023-12-13 [?] CRAN (R 4.4.0)
# P tidyverse    * 2.0.0      2023-02-22 [?] CRAN (R 4.4.0)
# P timechange     0.3.0      2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0      2024-04-24 [?] local
# P tzdb           0.4.0      2023-05-12 [?] CRAN (R 4.4.0)
# P usethis        2.2.3      2024-02-19 [?] CRAN (R 4.4.0)
# P utf8           1.2.4      2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0      2024-04-24 [?] local
# P vctrs          0.6.5      2023-12-01 [?] CRAN (R 4.4.0)
# P vipor          0.4.7      2023-12-18 [?] CRAN (R 4.4.0)
# P vroom          1.6.5      2023-12-05 [?] CRAN (R 4.4.0)
# P withr          3.0.0      2024-01-16 [?] CRAN (R 4.4.0)
# P xfun           0.43       2024-03-25 [?] CRAN (R 4.4.0)
# P xml2           1.3.6      2023-12-04 [?] CRAN (R 4.4.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/windows/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/ebed5364
# 
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────
# > 

