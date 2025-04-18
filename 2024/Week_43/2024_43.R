## Challenge: #TidyTuesday 2024 week 43
## Data:      The CIA World Factbook
## Author:    Steven Ponce
## Date:      2024-10-21

## Reference: 
# https://pmassicotte.github.io/paletteer_gallery/


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,         # Easily Install and Load the 'Tidyverse'
    ggtext,            # Improved Text Rendering Support for 'ggplot2'
    showtext,          # Using Fonts More Easily in R Graphs
    janitor,           # Simple Tools for Examining and Cleaning Dirty Data
    skimr,             # Compact and Flexible Summaries of Data
    scales,            # Scale Functions for Visualization
    lubridate,         # Make Dealing with Dates a Little Easier
    glue,              # Interpreted String Literals
    patchwork,         # The Composer of Plots
    here,              # A Simpler Way to Find Your Files
    ggcorrplot,        # Correlogram
    camcorder          # Record plots
)  

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  10,
    height =  10,
    units  = "in",
    dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <-tidytuesdayR::tt_load(2024, week = 43) 

cia_factbook <- tt$cia_factbook |> clean_names() |> glimpse()

tidytuesdayR::readme(tt)
rm(tt)



## 3. EXAMINING THE DATA ----
glimpse(cia_factbook)
skim(cia_factbook)



## 4. TIDYDATA ----

### |- tidy data ----
# Select numeric variables and compute correlation matrix
cor_matrix <- cia_factbook |>
    select(where(is.numeric)) |>
    cor(use = "complete.obs")

# Clean column names for better readability in plots
colnames(cor_matrix) <- gsub("_", " ", colnames(cor_matrix)) |> str_to_title()
rownames(cor_matrix) <- gsub("_", " ", rownames(cor_matrix)) |> str_to_title()

# Convert correlation matrix to a long format for plotting labels
cor_df <- as.data.frame(as.table(cor_matrix)) |>
    rename(Var1 = Var1, Var2 = Var2, Correlation = Freq) |>
    filter(!is.na(Correlation)) 



# 5. VISUALIZATION ----

### |- plot aesthetics ----
bkg_col      <- colorspace::lighten('#f7f5e9', 0.05)    
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <- paletteer::paletteer_d("NineteenEightyR::miami1")[c(1,3,5)] 
# show_col(col_palette)

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 43 } &bull; Source: usdatasets R package<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Exploring Relationships Among CIA Factbook Variables")
subtitle_text <- str_glue("This figure shows correlations between various CIA Factbook variables.\nLarge circles indicate strong correlations, with positive values in teal and negative values in orange.")
caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Merriweather Sans", regular.wt = 400, family = "subtitle")
font_add_google("Merriweather Sans", regular.wt = 400, family = "text")
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
font_add_google("Shadows Into Light", regular.wt = 400, family = "anotation")
showtext_auto(enable = TRUE)

### |-  plot theme ----
theme_set(theme_minimal(base_size = 14, base_family = "text"))                

cor_theme <- function() {
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        plot.background       = element_rect(fill = bkg_col, color = bkg_col),
        panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        plot.margin           = margin(t = 20, r = 20, b = 20, l = 20),
        panel.grid.minor      = element_blank(),
        panel.grid.major      = element_line(color = "gray80", size = 0.4, linetype = 'dotted'),
        axis.title            = element_blank(),
        axis.text.y           = element_text(color = text_col, family = "text", size = rel(1)),
        axis.text.x           = element_text(color = text_col, family = "text", size = rel(1), angle = 30, hjust = 1),
        axis.ticks.x          = element_line(color = text_col),
        axis.line.x           = element_line(color = "#252525", linewidth = .2)
    )
}

### |- initial plot ----

# Create correlation heatmap with annotations
p <- cor_matrix |>
    
    # Geoms
    ggcorrplot(
        method = "circle", type = "lower",
        hc.order = TRUE, lab = FALSE, show.legend = TRUE,
        colors = col_palette, outline.color = "black",
        ggtheme = cor_theme()
    ) +
    
    # Scales
    scale_x_discrete() +
    scale_y_discrete() +
    scale_fill_stepsn(
        colors = col_palette,
        transform = "identity",
        guide = guide_colorsteps(
            barheight = unit(16, "lines"),
            barwidth = unit(.5, "lines")
        )
    ) +
    
    # Labs
    labs(
        x = NULL,
        y = NULL,
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text
    ) +
    
    # Theme
    theme(
        plot.title = element_text(
            size = rel(2),
            family = "title",
            face = "bold",
            color = title_col,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.subtitle = element_text(
            size = rel(1),
            family = "subtitle",
            color = subtitle_col,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.caption = element_markdown(
            size = rel(0.75),
            family = "caption",
            color = caption_col,
            lineheight = 1.1,
            hjust = 0.5,
            halign = 1,
            margin = margin(t = 15, b = 5)
        )
    )

### |- Annotated plot ----

# Add annotation text
annotation_text <- str_glue(
    "Variables like birth rate and infant mortality rate
    show a strong positive correlation, whereas others, 
    such as life expectancy at birth and infant mortality rate, 
    exhibit a strong negative correlation.
    
    These relationships suggest that countries 
    with higher birth rates also tend to have 
    higher infant mortality, indicating possible 
    healthcare challenges."
)

p +
    # text
    annotate(
        "text",
        x = 1, y = 7.3, label = annotation_text,
        family = "anotation",
        colour = text_col,
        size = 5,
        lineheight = 1.2,
        hjust = 0
    ) 


# 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-10-20
# rstudio  2024.09.0+375 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder    * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger     1.1.0    2016-07-27 [?] CRAN (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# P digest         0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# P fs             1.6.4    2024-04-25 [?] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# P ggcorrplot   * 0.1.4.1  2023-09-05 [?] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue         * 1.7.0    2024-01-09 [?] CRAN (R 4.4.0)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here         * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P httr           1.4.7    2023-08-15 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.46     2024-04-06 [?] CRAN (R 4.4.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown       1.12     2023-12-06 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P paletteer      1.6.0    2024-01-21 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P patchwork    * 1.3.0    2024-09-16 [?] CRAN (R 4.4.1)
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P plyr           1.8.9    2023-10-02 [?] CRAN (R 4.4.0)
# P prismatic      1.1.2    2024-04-10 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P readxl         1.4.3    2023-07-06 [?] CRAN (R 4.4.0)
# P rematch2       2.1.2    2020-05-01 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# P reshape2       1.4.4    2020-04-09 [?] CRAN (R 4.4.0)
# rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# P rvest          1.0.4    2024-02-12 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P selectr        0.4-2    2019-11-20 [?] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats        * 4.4.0    2024-04-24 [?] local
# stringi        1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite        2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts    1.1.0    2024-05-15 [1] CRAN (R 4.4.0)
# textshaping    0.4.0    2024-05-24 [1] CRAN (R 4.4.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidytuesdayR   1.0.3    2023-12-13 [?] CRAN (R 4.4.0)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P usethis        2.2.3    2024-02-19 [?] CRAN (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# withr          3.0.1    2024-07-31 [1] CRAN (R 4.4.1)
# P xfun           0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────
# > 