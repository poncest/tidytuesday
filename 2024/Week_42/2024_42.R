
## Challenge: #TidyTuesday 2024 week 42
## Data:      Southern Resident Killer Whale Encounters
## Author:    Steven Ponce
## Date:      2024-10-14

## Reference: 
# https://pmassicotte.github.io/paletteer_gallery/
#
# This chart was inspired by Yan Holtz "Combine charts in ggiraph" post at
# https://r-graph-gallery.com/414-map-multiple-charts-in-ggiraph.html
 

## 1. LOAD PACKAGES & SETUP ----
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
    sf,                # Simple Features for R
    ggiraph,           # Make 'ggplot2' Graphics Interactive
    htmltools,         # Tools for HTML
    rnaturalearth,     # World Map Data from Natural Earth 
    rnaturalearthhires # High Resolution World Vector Map Data from Natural Earth used inrnaturalearth
)  

# Note: disabled { camcorder }. Issues with plot rendering (ggiraph)

# ### |- figure size ---- 
# camcorder::gg_record(
#     dir    = here::here("temp_plots"),
#     device = "png",
#     width  =  10,
#     height =  10,
#     units  = "in",
#     dpi    = 320
# )

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <-tidytuesdayR::tt_load(2024, week = 42) 

orcas_data <- tt$orcas |> clean_names() |> glimpse()

tidytuesdayR::readme(tt)
rm(tt)



## 3. EXAMINING THE DATA ----
glimpse(orcas_data)
skim(orcas_data)



## 4. TIDYDATA ----

### |- tidy data ----
orcas_data_clean <- orcas_data |>
    filter(!is.na(vessel)) |>
    # Standardize 'ids_encountered' to be comma-separated
    mutate(
        ids_encountered = str_replace_all(ids_encountered, "and", ","),
        ids_encountered = str_replace_all(ids_encountered, ",[[:space:]]*", ", "),
        ids_encountered = str_trim(ids_encountered)
    ) |>  
    # Extract numeric duration from the 'duration' column and convert to minutes
    mutate(
        duration_minutes = str_extract(duration, "[[:digit:]]+") |> as.numeric() / 60,
        duration_minutes = ifelse(duration_minutes < 0, NA, duration_minutes)
    ) |>
    # Convert 'date' to proper Date class and create month column
    mutate(
        date = as.Date(date),
        month = month(date, label = TRUE, abbr = FALSE)
    ) |>
    # Handle missing values
    filter(!is.na(encounter_number), !is.na(begin_latitude), !is.na(begin_longitude))


# ### |- data for bar plot ----

# Number of Encounters per Vessel
bar_plot_data <- orcas_data_clean |>
    filter(!is.na(vessel)) |>
    count(vessel, sort = TRUE) |>
    filter(n > 1) |>
    mutate(
        vessel = str_wrap(vessel, width = 20),
        vessel = fct_reorder(vessel, -n)
    )



# 5. VISUALIZATION ----

### |- plot aesthetics ----
bkg_col      <- colorspace::lighten('#f7f5e9', 0.05)    
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <- paletteer::paletteer_d("lisa::OdilonRedon")[c(1,2)] 
show_col(col_palette)

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 42 } &bull; Source: Center for Whale Research<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Orca Encounter Observations: Leading Vessels")
subtitle_text <- str_glue("Highlighting vessels with more than one recorded orca encounter in the Salish Sea region.")
caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Merriweather Sans", regular.wt = 400, family = "subtitle")
font_add_google("Merriweather Sans", regular.wt = 400, family = "text")
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
showtext_auto(enable = TRUE)

### |-  plot theme ----
theme_set(theme_minimal(base_size = 14, base_family = "text"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    plot.margin           = margin(t = 10, r = 10, b = 5, l = 10),
    
    panel.grid.minor      = element_blank(),
    panel.grid.major.y    = element_blank(),
    panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.2, color = 'gray'),
    
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(0.85), 
                                         color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(0.85), 
                                         color = text_col, family = "text", face = "bold", hjust = 0.5),
    
    axis.text.y           = element_text(color = text_col, family = "text", size = rel(0.65)),
    axis.text.x           = element_text(color = text_col, family = "text", size = rel(0.65)),
    
    axis.ticks.x          = element_line(color = text_col),  
    axis.line.x           = element_line(color = "#252525", linewidth = .2)
)

### |- world map for background ----
world <- st_as_sf(rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf'))

### |- Base map ----

# Focus on the Salish Sea region
map <- ggplot() +
    
    # Geoms
    geom_sf(data = world, fill = "lightgrey", color = "white") +
    geom_sf_interactive(
        data = st_as_sf(orcas_data_clean, coords = c("begin_longitude", "begin_latitude"), crs = 4326),
        aes(tooltip = paste("Date:", date, "<br>Location:", location), data_id = vessel), 
        color = col_palette[1],
        alpha = 0.5,
        shape = 1,
        size = 1
    ) +
    
    # Scales
    coord_sf(xlim = c(-127, -121), ylim = c(47, 51), expand = FALSE) +            # Focus on the Salish Sea region
    
    # Labs 
    labs(
        title = "",
        x = "Longitude",
        y = "Latitude"
    ) +
    
    # Theme
    ggthemes::theme_map() 


### |- Bar plot plot ----

# Number of Encounters per Vessel
bar <- ggplot(bar_plot_data, aes(x = vessel, -n, y = n)) +
    
    # Geom
    geom_bar_interactive(
        aes(
            tooltip = paste("Vessel:", vessel, "<br>Encounters:", n), 
            data_id = vessel), 
        stat = "identity", 
        fill = col_palette[1]
    ) +
    
    # Scale
    scale_x_discrete() +
    scale_y_continuous() +    
    coord_flip() +
    
    # Labs
    labs(
        x = "Vessel",
        y = "Number of Encounters",
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text
    )  +
    
    # Theme
    theme(
        plot.title = element_text(
            size = rel(1.3),
            family = "title",
            face = "bold",
            color = title_col,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.subtitle = element_text(
            size = rel(0.65),
            family = "subtitle",
            color = subtitle_col,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.caption = element_markdown(    
            size = rel(0.40),
            family = "caption", 
            color = caption_col,
            lineheight = 1.1,
            hjust = 0.5,
            halign = 1,
            margin = margin(t = 5, b = 5)
        )
    )

### |- inset element ----

# Insert the map into the upper right corner of the bar plot 
combined_plot <- bar + 
    inset_element(map, left = 0, bottom = 0.2, right = 1.3, top = 1.05)

### |-  interactive plots ----

# Create the interactive plot with ggiraph
interactive_plot <- girafe(ggobj = combined_plot)

interactive_plot <- girafe_options(
    interactive_plot,
    opts_tooltip(
        opacity = 0.8, use_fill = TRUE,
        use_stroke = FALSE,
        css = "padding:5pt;font-family: 'Open Sans', sans-serif;font-size:1.2rem;color:white"
    ),
    opts_hover_inv(css = "opacity:0.4"),
    opts_hover(
        css = girafe_css(
            css = glue("fill:{col_palette[2]};"),
            text = "stroke:none;fill:white;fill-opacity:1;"
        )
    ),
    opts_selection(type = "single", css = "fill:yellow;stroke:black;stroke-width:2px;"),
    opts_sizing(rescale = TRUE), # Adjust width and height, make it responsive
    opts_toolbar(saveaspng = TRUE) # Add an option to save as PNG
)

interactive_plot 
 
# Save as an html widget
htmltools::save_html(interactive_plot, "2024/Week_42/2024_42.html")

# Save as PNG
ragg::agg_png("2024/Week_42/2024_42.png", width = 8, height = 6, units = "in", res = 320)
print(combined_plot)
dev.off()



# 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-10-14
# rstudio  2024.09.0+375 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────
# ! package            * version    date (UTC) lib source
# V base               * 4.4.1      2024-04-24 [2] local (on disk 4.4.0)
# P base64enc            0.1-3      2015-07-28 [?] CRAN (R 4.4.0)
# P bit                  4.0.5      2022-11-15 [?] CRAN (R 4.4.0)
# P bit64                4.0.5      2020-08-30 [?] CRAN (R 4.4.0)
# P cellranger           1.1.0      2016-07-27 [?] CRAN (R 4.4.0)
# P class                7.3-22     2023-05-03 [?] CRAN (R 4.4.0)
# P classInt             0.4-10     2023-09-05 [?] CRAN (R 4.4.0)
# cli                  3.6.3      2024-06-21 [1] CRAN (R 4.4.1)
# P codetools            0.2-20     2024-03-31 [?] CRAN (R 4.4.0)
# colorspace           2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark           1.9.1      2024-01-30 [?] CRAN (R 4.4.0)
# P compiler             4.4.0      2024-04-24 [?] local
# P crayon               1.5.2      2022-09-29 [?] CRAN (R 4.4.0)
# P curl                 5.2.1      2024-03-01 [?] CRAN (R 4.4.0)
# P datasets           * 4.4.0      2024-04-24 [?] local
# P DBI                  1.2.2      2024-02-16 [?] CRAN (R 4.4.0)
# P digest               0.6.35     2024-03-11 [?] CRAN (R 4.4.0)
# dplyr              * 1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
# P e1071                1.7-14     2023-12-06 [?] CRAN (R 4.4.0)
# P fansi                1.0.6      2023-12-08 [?] CRAN (R 4.4.0)
# farver               2.1.2      2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap              1.1.1      2023-02-24 [?] CRAN (R 4.4.0)
# forcats            * 1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
# P fs                   1.6.4      2024-04-25 [?] CRAN (R 4.4.0)
# generics             0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
# P ggiraph            * 0.8.10     2024-05-17 [?] CRAN (R 4.4.0)
# ggplot2            * 3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext             * 0.1.2      2022-09-16 [?] CRAN (R 4.4.0)
# P ggthemes             5.1.0      2024-02-10 [?] CRAN (R 4.4.0)
# P glue               * 1.7.0      2024-01-09 [?] CRAN (R 4.4.0)
# P graphics           * 4.4.0      2024-04-24 [?] local
# P grDevices          * 4.4.0      2024-04-24 [?] local
# P grid                 4.4.0      2024-04-24 [?] local
# P gridtext             0.1.5      2022-09-16 [?] CRAN (R 4.4.0)
# gtable               0.3.5      2024-04-22 [1] CRAN (R 4.4.0)
# P here               * 1.0.1      2020-12-13 [?] CRAN (R 4.4.0)
# P hms                  1.1.3      2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools          * 0.5.8.1    2024-04-04 [?] CRAN (R 4.4.0)
# P htmlwidgets          1.6.4      2023-12-06 [?] CRAN (R 4.4.0)
# P httr                 1.4.7      2023-08-15 [?] CRAN (R 4.4.0)
# P janitor            * 2.2.0      2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite             1.8.8      2023-12-04 [?] CRAN (R 4.4.0)
# P KernSmooth           2.23-22    2023-07-10 [?] CRAN (R 4.4.0)
# P knitr                1.46       2024-04-06 [?] CRAN (R 4.4.0)
# labeling             0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle            1.0.4      2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate          * 1.9.3      2023-09-27 [?] CRAN (R 4.4.0)
# P magrittr             2.0.3      2022-03-30 [?] CRAN (R 4.4.0)
# P markdown             1.12       2023-12-06 [?] CRAN (R 4.4.0)
# P methods            * 4.4.0      2024-04-24 [?] local
# munsell              0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
# P pacman               0.5.1      2019-03-11 [?] CRAN (R 4.4.0)
# P paletteer            1.6.0      2024-01-21 [?] CRAN (R 4.4.0)
# P parallel             4.4.0      2024-04-24 [?] local
# P patchwork          * 1.3.0      2024-09-16 [?] CRAN (R 4.4.1)
# P pillar               1.9.0      2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig            2.0.3      2019-09-22 [?] CRAN (R 4.4.0)
# P prismatic            1.1.2      2024-04-10 [?] CRAN (R 4.4.0)
# P proxy                0.4-27     2022-06-09 [?] CRAN (R 4.4.0)
# P purrr              * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
# P R6                   2.5.1      2021-08-19 [?] CRAN (R 4.4.0)
# P ragg                 1.3.0      2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp                 1.0.12     2024-01-09 [?] CRAN (R 4.4.0)
# P readr              * 2.1.5      2024-01-10 [?] CRAN (R 4.4.0)
# P readxl               1.4.3      2023-07-06 [?] CRAN (R 4.4.0)
# P rematch2             2.1.2      2020-05-01 [?] CRAN (R 4.4.0)
# P renv                 1.0.7      2024-04-11 [?] CRAN (R 4.4.0)
# P repr                 1.1.7      2024-03-22 [?] CRAN (R 4.4.0)
# rlang                1.1.4      2024-06-04 [1] CRAN (R 4.4.1)
# P rnaturalearth      * 1.0.1      2023-12-15 [?] CRAN (R 4.4.1)
# rnaturalearthhires * 1.0.0.9000 2024-10-14 [1] Github (ropensci/rnaturalearthhires@e02c28d)
# P rprojroot            2.0.4      2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi           0.16.0     2024-03-24 [?] CRAN (R 4.4.0)
# P rvest                1.0.4      2024-02-12 [?] CRAN (R 4.4.0)
# scales             * 1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
# P selectr              0.4-2      2019-11-20 [?] CRAN (R 4.4.0)
# P sessioninfo          1.2.2      2021-12-06 [?] CRAN (R 4.4.0)
# P sf                 * 1.0-16     2024-03-24 [?] CRAN (R 4.4.0)
# P showtext           * 0.9-7      2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb         * 3.0        2020-06-04 [?] CRAN (R 4.4.0)
# P skimr              * 2.1.5      2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase            0.11.1     2023-08-27 [?] CRAN (R 4.4.0)
# P stats              * 4.4.0      2024-04-24 [?] local
# stringi              1.8.4      2024-05-06 [1] CRAN (R 4.4.0)
# P stringr            * 1.5.1      2023-11-14 [?] CRAN (R 4.4.0)
# P sysfonts           * 0.8.9      2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts          1.1.0      2024-05-15 [1] CRAN (R 4.4.0)
# terra                1.7-78     2024-05-22 [1] CRAN (R 4.4.1)
# textshaping          0.4.0      2024-05-24 [1] CRAN (R 4.4.0)
# P tibble             * 3.2.1      2023-03-20 [?] CRAN (R 4.4.0)
# tidyr              * 1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect           1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
# P tidytuesdayR         1.0.3      2023-12-13 [?] CRAN (R 4.4.0)
# P tidyverse          * 2.0.0      2023-02-22 [?] CRAN (R 4.4.0)
# P timechange           0.3.0      2024-01-18 [?] CRAN (R 4.4.0)
# P tools                4.4.0      2024-04-24 [?] local
# P tzdb                 0.4.0      2023-05-12 [?] CRAN (R 4.4.0)
# P units                0.8-5      2023-11-28 [?] CRAN (R 4.4.0)
# P usethis              2.2.3      2024-02-19 [?] CRAN (R 4.4.0)
# P utf8                 1.2.4      2023-10-22 [?] CRAN (R 4.4.0)
# P utils              * 4.4.0      2024-04-24 [?] local
# P uuid                 1.2-0      2024-01-14 [?] CRAN (R 4.4.0)
# P vctrs                0.6.5      2023-12-01 [?] CRAN (R 4.4.0)
# P vroom                1.6.5      2023-12-05 [?] CRAN (R 4.4.0)
# withr                3.0.1      2024-07-31 [1] CRAN (R 4.4.1)
# P xfun                 0.43       2024-03-25 [?] CRAN (R 4.4.0)
# P xml2                 1.3.6      2023-12-04 [?] CRAN (R 4.4.0)
# P yaml                 2.3.8      2023-12-11 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────────────────────────
# > 