
## Challenge: #TidyTuesday 2024 week 46
## Data:      ISO Country Codes
## Author:    Steven Ponce
## Date:      2024-11-10


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,         # Easily Install and Load the 'Tidyverse'
    ggtext,            # Improved Text Rendering Support for 'ggplot2'
    showtext,          # Using Fonts More Easily in R Graphs
    janitor,           # Simple Tools for Examining and Cleaning Dirty Data
    scales,            # Scale Functions for Visualization
    glue,              # Interpreted String Literals
    here,              # A Simpler Way to Find Your Files
    ggridges,          # Ridgeline Plots in 'ggplot2'
    waffle,            # Create Waffle Chart Visualizations
    patchwork          # The Composer of Plots
)    

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  10,
    height =  12,
    units  = "in",
    dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <-tidytuesdayR::tt_load(2024, week = 46) 

countries            <- tt$countries |> clean_names() |> glimpse()
country_subdivisions <- tt$country_subdivisions |> clean_names() |> glimpse()
former_countries     <- tt$former_countries |> clean_names() |> glimpse()

tidytuesdayR::readme(tt)
rm(tt)



## 3. EXAMINING THE DATA ----
glimpse(countries)
glimpse(country_subdivisions)
glimpse(former_countries)



## 4. TIDYDATA ----

### |- ridge data ----
ridge_data <- countries |>
    mutate(
        first_letter = substr(alpha_2, 1, 1),
        first_letter = factor(first_letter, levels = rev(sort(unique(first_letter))))
    )

### |- waffle data ----
waffle_data <- countries |>
    summarise(
        `Complete System` = sum(!is.na(alpha_2) & !is.na(alpha_3) & !is.na(numeric)),
        `Partial System` = n() - sum(!is.na(alpha_2) & !is.na(alpha_3) & !is.na(numeric))
    ) |>
    pivot_longer(
        cols = everything(),
        names_to = "key",
        values_to = "value"
    ) |>
    mutate(
        value = value / 10,
        key = factor(key, levels = c("Complete System", "Partial System"))
    )



# 5. VISUALIZATION ----
### |- plot aesthetics ----
bkg_col      <- "#f5f5f2"  
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray30"    
col_palette  <- viridis::rocket(5)
# show_col(col_palette)

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 46 } &bull; Source: ISOcodes R Package<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

# text
caption_text  <- str_glue("{tt} {li} stevenponce &bull; {bs} sponce1 &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add("fa6-brands", "fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Source Sans Pro", family = "text")  
font_add_google("Roboto Mono", family = "numbers")   
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
showtext_auto(enable = TRUE)

### |-  plot theme ----
theme_set(theme_minimal(base_size = 14, base_family = "text"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = "plot",
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    plot.margin           = margin(t = 5, r = 15, b = 5, l = 15),
    panel.spacing         = unit(1, "lines"),
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), 
                                         size = rel(1),
                                         color = text_col,
                                         family = "text",
                                         face = "bold"),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0),
                                         size = rel(1),
                                         color = text_col,
                                         family = "text",
                                         face = "bold"),
    axis.text             = element_text(size = rel(0.8),
                                         color = text_col,
                                         family = "text"),
    panel.grid.minor      = element_blank(),
    panel.grid.major      = element_line(color = "gray95", linewidth = 0.2),
    panel.grid.major.y    = element_blank()
)

### |- ridge plot ----
ridge_plot <- ridge_data |>
    ggplot(aes(x = numeric, y = first_letter, fill = after_stat(x))) +
    
    # Geoms
    # add reference lines
    geom_vline(
        xintercept = seq(0, 1000, 200),
        color = "gray90",
        linetype = "dashed"
    ) +
    # ridges
    geom_density_ridges_gradient(
        alpha = 0.8,
        scale = 0.95,
        rel_min_height = 0.005,
        bandwidth = 25,
        color = text_col,
        show.legend = TRUE
    ) +
    # add country positions
    geom_point(
        data = ridge_data,
        aes(x = numeric, y = first_letter),
        size = 0.8,
        alpha = 0.3,
        color = text_col
    ) +
    # Annotations
    annotate(
        "text",
        x = 10,       
        y = "E",       
        label = "Early alphabet countries\noften have lower codes",
        size = 3.0,
        family = "text",
        color = "gray25",
        lineheight = 0.9,
        hjust = 0.4
    ) +
    annotate(
        "text",
        x = 850,
        y = "W",
        label = "Higher codes cluster in\nlater alphabet regions",
        size = 3.0,
        family = "text",
        color = "gray25",
        lineheight = 0.9,
        vjust = 0.5
    ) +
    
    # Scales
    scale_x_continuous(
        breaks = seq(0, 1000, 200),
        expand = c(0.02, 0)
    ) +
    scale_y_discrete() +
    scale_fill_gradientn(
        colors = col_palette,
        name = "Numeric Code Range",
        guide = guide_colorbar(
            title.position = "top",
            barwidth = unit(15, "lines"),
            barheight = unit(0.5, "lines")
        )
    ) +
    coord_cartesian(clip = "off") +
    
    # Labs
    labs(
        x = "Numeric Country Code (0-999)",
        y = "First Letter of Alpha-2 Code"
    ) +
    
    # Theme
    theme(
        legend.position = "top",
        legend.justification = "right",
        legend.direction = "horizontal",
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.7)),
        legend.key.width = unit(2.2, "cm"),                 
        legend.key.height = unit(0.3, "cm"),
        legend.spacing.x = unit(0.2, "cm"),                 
        plot.margin = margin(-5, 30, 5, 30),
        panel.spacing = unit(2, "lines")
    )

### |- waffle plot ----
waffle_plot <- waffle_data |>
    ggplot(aes(fill = key, values = value)) +
    
    # Geom
    geom_waffle(
        n_rows = 5,
        size = 0.5,
        colour = "white",
        flip = TRUE,
        radius = unit(2, "pt")
    ) +
    # Scale
    scale_fill_manual(values = col_palette[c(1, 5)]) +
    coord_equal(ratio = 1) +
    
    # Theme
    theme_void() +
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        plot.margin = margin(10, 5, 0, 5),
        plot.background = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
    )

### |- title plot ----

# Add total count annotation
n_countries <- nrow(countries)

title_plot <- ggplot() +
    annotate(
        "text",
        x = 0, y = 0.85,
        label = "The Architecture of\nGlobal Country Codes",
        hjust = 0,
        size = 13,
        lineheight = 1,
        fontface = "bold",
        family = "title",
        color = title_col
    ) +
    annotate(
        "text",
        x = 0, y = 0.5,
        label = str_wrap(
            "Analysis of ISO country codes reveals geographic and alphabetic patterns
            in international standardization. Most nations maintain three distinct identifiers.",
            width = 65
        ),
        hjust = 0,
        size = 4,
        color = title_col,
        family = "text"
    ) +
    annotate(
        "text",
        x = 0, y = 0.3,
        label = glue("Analysis of {n_countries} country codes"),
        hjust = 0,
        size = 3.5,
        family = "text",
        color = title_col
    ) +
    annotate("text",
             x = 0, y = 0.1,
             label = "Example:\nUS (Alpha-2)\nUSA (Alpha-3)\n840 (Numeric)",
             hjust = 0,
             size = 2.8,
             color = title_col,
             family = "text",
             lineheight = 1.2
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void() +
    theme(
        plot.background = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        plot.margin = margin(5, 10, 0, 10)
    )

### |- explanatory text plot for the waffle chart ----
waffle_explanation <- ggplot() +
    
    # Geoms
    annotate("text",
             x = 0, y = 0.85,
             label = "Country Code Systems\n",
             hjust = 0, size = 3.5, fontface = "bold", family = "text"
    ) +
    annotate("text",
             x = 0, y = 0.55,
             label = str_wrap("Each square represents 10 countries. A complete system means a country has
                     all three standardized codes shown in the ridge plot below: Alpha-2 (e.g., US),
                     Alpha-3 (USA), and Numeric (840).", 55),
             hjust = 0, size = 2.8, color = title_col, family = "text"
    ) +
    
    # Scale
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    
    # Theme
    theme_void() +
    theme(
        plot.background = element_rect(fill = bkg_col, color = bkg_col),
        panel.background = element_rect(fill = bkg_col, color = bkg_col),
        plot.margin = margin(5, 10, 0, 10)
    )

### |- combined plot ----

# define layout design
design <- c(
    area(1, 1, 2, 4), # title_plot
    area(1, 5, 1, 6), # waffle_plot
    area(2, 5, 2, 6), # waffle_explanation
    area(3, 1, 5, 6)  # ridge_plot
)

combined_plot <- title_plot + waffle_plot + waffle_explanation + ridge_plot +
    plot_layout(
        design = design,
        heights = c(1.2, 1, 2, 1, 1),     
        widths = c(1, 1, -0.4, 1, 0.9, 0.9)  
    ) +
    plot_annotation(
        caption = caption_text,
        theme = theme(
            plot.background = element_rect(fill = bkg_col, color = bkg_col),
            panel.background = element_rect(fill = bkg_col, color = bkg_col),
            plot.margin = margin(10, 10, 10, 10),
            plot.caption = element_markdown(
                size = rel(0.60),
                family = "caption",
                color = alpha(caption_col, 0.9),
                lineheight = 0.65,
                hjust = 0.5,
                margin = margin(t = 10, b = 5)
            )
        )
    )

# Print final plot
print(combined_plot)



# 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-11-08
# rstudio  2024.09.0+375 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
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
# P DT             0.33     2024-04-04 [?] CRAN (R 4.4.0)
# P extrafont      0.19     2023-01-18 [?] CRAN (R 4.4.0)
# P extrafontdb    1.0      2012-06-11 [?] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# P fs             1.6.4    2024-04-25 [?] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggridges     * 0.5.6    2024-01-23 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue         * 1.7.0    2024-01-09 [?] CRAN (R 4.4.0)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridExtra      2.3      2017-09-09 [?] CRAN (R 4.4.0)
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here         * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P htmlwidgets    1.6.4    2023-12-06 [?] CRAN (R 4.4.0)
# P httr           1.4.7    2023-08-15 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown       1.12     2023-12-06 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P patchwork    * 1.3.0    2024-09-16 [?] CRAN (R 4.4.1)
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P plyr           1.8.9    2023-10-02 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P readxl         1.4.3    2023-07-06 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# P Rttf2pt1       1.3.12   2023-01-22 [?] CRAN (R 4.4.0)
# P rvest          1.0.4    2024-02-12 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P selectr        0.4-2    2019-11-20 [?] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
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
# P waffle       * 1.0.2    2023-09-30 [?] CRAN (R 4.4.0)
# withr          3.0.1    2024-07-31 [1] CRAN (R 4.4.1)
# P xfun           0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────────────────────────
# > 
