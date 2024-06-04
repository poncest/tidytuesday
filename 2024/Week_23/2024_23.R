
## Challenge: #TidyTuesday 2024 week 23
## Data:      Cheese
## Author:    Steven Ponce
## Date:      2024-06-04


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
    patchwork,   # The Composer of Plots
    ggchicklet   # Create 'Chicklet' (Rounded Segmented Column) Charts 
)

### |- figure size ---- 
camcorder::gg_record( 
    dir    = here::here("temp_plots"), 
    device = "png",
    width  =  11.25,
    height =  5.9,
    units  = "in",
    dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(x = base::as.double("2024"),
                            week = base::as.double("23"))

cheeses <- tt$cheeses |> clean_names() |> glimpse()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(cheeses)
skim(cheeses)
colnames(cheeses)


## 4. TIDYDATA ----

### |- plot data ---  

# Count of cheeses per country ----
country_counts <- cheeses  |>
  # Count the occurrence of each country
  count(country) |>
  arrange(desc(n)) |>
  slice_head(n = 5)

# Aggregate cheeses by region and country ---
cheese_by_region <- cheeses  |>
    group_by(country, region) |>
    summarise(cheese_count = n(), .groups = 'drop') |>
    arrange(desc(cheese_count)) |> 
    na.omit()

# Top regions with the most cheese varieties
top_regions <- cheese_by_region |>
    top_n(10, cheese_count)

# Aggregate data by cheese family ----
cheese_family_distribution <- cheeses  |>
  group_by(family) |>
  summarise(count = n(), .groups = "drop") |>
  na.omit() |>
  arrange(desc(count))

# Aggregate data by texture ----
texture_analysis <- cheeses  |>
    group_by(texture) |>
    summarise(count = n(), .groups = 'drop') |>
    na.omit() |>
    arrange(desc(count)) |> 
    top_n(10, count)


# 5. VISUALIZATION ---- 
### |- plot aesthetics ---- 
bkg_col      <- "#323238" 
title_col    <- "#d7d7d8"             
subtitle_col <- "#d7d7d8"   
caption_col  <- "#d7d7d8"   
text_col     <- "#d7d7d8"  


### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 23 } &bull; Source: cheese.com<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Say Cheeeeese!") 

subtitle_text <- str_glue("A detailed look at global cheese production")

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
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
    axis.title.y          = element_blank(),
    axis.text.y           = element_text(size = rel(1), color = text_col, family = 'text'),
    axis.text.x           = element_blank(),
    panel.grid.minor.y    = element_blank(),
    panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray40'),
    panel.grid.minor.x    = element_blank(),
    panel.grid.major.x    = element_blank(),
)

### |-  plot function -----
plot_bar <- function(data, x, y, title, subtitle, fill_color = NULL, reorder_var = NULL) {
    
    # Convert character inputs to symbols directly inside the function
    x_sym <- rlang::sym(x)
    y_sym <- rlang::sym(y)
    reorder_var_sym <- if (!is.null(reorder_var)) rlang::sym(reorder_var) else NULL
    
    # ggplot object
    p <- data |> 
        ggplot(aes(x = !!x_sym, y = !!y_sym)) +
        # Check if reordering is needed
        {
            if (!is.null(reorder_var)) {
                aes(x = reorder(!!x_sym, !!reorder_var_sym))
            }
        } +
        # Geoms
        geom_chicklet(
            width = 0.75,
            fill = fill_color,
            radius = grid::unit(8, "pt"),
            colour = NA
        ) +
        geom_text(
            aes(label = !!y_sym),
            family = "text",
            size = 4,
            color = text_col,
            vjust = 0.5, 
            hjust = -0.35
        ) +
        # Scales
        scale_x_discrete() +
        scale_y_continuous() +
        coord_flip(clip = "off") +
        # Labs
        labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
        # Theme
        theme(
            plot.title = element_text(
                size = rel(1),
                family = "title",
                color = title_col,
                face = "bold",
                lineheight = 0.85,
                margin = margin(t = 5, b = 5)
            ),
            plot.subtitle = element_markdown(
                size = rel(0.85),
                family = "subtitle",
                color = title_col,
                lineheight = 1,
                margin = margin(t = 5, b = 10)
            )
        )
    return(p)
}


### |-  Plots  ---- 

# Plot - top 10 cheese-producing countries ----
p1 <- plot_bar(
  data = country_counts,
  x = "country",
  y = "n",
  title = "Top 5 Cheese-Producing Countries",
  subtitle = "Number of Cheeses by Country",
  fill_color = "#8DD3C7",
  reorder_var = "n"
)

# Plot - Top regions with the most cheese varieties ----
p2 <- plot_bar(
    data = top_regions,
    x = "region",
    y = "cheese_count",
    title = "Top Regions by Cheese Variety",
    subtitle = "Number of Cheeses by Region",
    fill_color = "#FFFFB3",
    reorder_var = "cheese_count"  
)

# Plot - Aggregate data by cheese family ----
p3 <- plot_bar(
    data = cheese_family_distribution |> filter(count >= 10),
    x = "family",
    y = "count",
    title = "Distribution of Cheese Families",
    subtitle = "Number of Cheeses by Cheese Family",
    fill_color = "#BEBADA",
    reorder_var = "count"  
)

# Plot - Aggregate data by texture ----
p4 <- plot_bar(
    data = texture_analysis,
    x = "texture",
    y = "count",
    title = "Distribution of Cheese Textures",
    subtitle = "Number of Cheeses by Texture",
    fill_color = "#FB8072",
    reorder_var = "count"  
)


### |-  main plot ----  
main_plot <- p1 + p2 + p3 + p4 +
    patchwork::plot_layout(
        guides = "collect",
        widths = c(1, 1, 1, 1)
    ) +
    theme(
        axis.text = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(0.9))
    )

### |-  title plot ----  
title_plot <- ggplot() +
  theme_void() +

  # labs
  labs(
    title    = title_text,
    subtitle = subtitle_text
  ) +

  # Theme
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = "plot",
    
    plot.title      = element_text(
      size          = rel(2.2),
      family        = "title",
      face          = "bold",
      color         = title_col,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size          = rel(1.4),
      family        = "subtitle",
      face          = "bold",
      color         = subtitle_col,
      lineheight    = 1.1,
      margin        = margin(t = 0, b = 2)
    )
  )

### |- caption plot ---- 
caption_plot <- ggplot() +
  theme_void() +

  # labs
  labs(caption = caption_text) +

  # Theme
  theme(
    plot.caption = element_markdown(
      size       = rel(.65),
      family     = "caption",
      color      = caption_col,
      lineheight = 1.1,
      hjust      = 0.5,
      halign     = 0.5,
      margin     = margin(t = 5, b = 5)
    )
  )

### |-  final plot ----  
final_plot <- (title_plot /
  (main_plot) /
  caption_plot) +
  patchwork::plot_layout(
    guides = "collect",
    heights = c(0.01, 1, 0.05)
  )

final_plot


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.0 (2024-04-24 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-06-03
# rstudio  2024.04.1+748 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version    date (UTC) lib source
# base         * 4.4.0      2024-04-24 [2] local
# P base64enc      0.1-3      2015-07-28 [?] CRAN (R 4.4.0)
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
# ggchicklet   * 0.6.0      2024-06-03 [1] Github (hrbrmstr/ggchicklet@64c468d)
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
# P purrr        * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache        0.16.0     2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3    1.8.2      2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo           1.26.0     2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils        2.12.3     2023-11-18 [?] CRAN (R 4.4.0)
# P R6             2.5.1      2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.0      2024-03-13 [?] CRAN (R 4.4.0)
# RColorBrewer * 1.1-3      2022-04-03 [1] CRAN (R 4.4.0)
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
# systemfonts    1.1.0      2024-05-15 [1] CRAN (R 4.4.0)
# textshaping    0.4.0      2024-05-24 [1] CRAN (R 4.4.0)
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
# ───────────────────────────────────────────────────────────────────────────────────────────────────
# > 

