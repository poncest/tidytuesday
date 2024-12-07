
## Challenge: #TidyTuesday 2024 week 50
## Data:      The Scent of Data - Exploring the Parfumo Fragrance Dataset
## Author:    Steven Ponce
## Date:      2024-12-07


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,      # Easily Install and Load the 'Tidyverse'
    ggtext,         # Improved Text Rendering Support for 'ggplot2'
    showtext,       # Using Fonts More Easily in R Graphs
    janitor,        # Simple Tools for Examining and Cleaning Dirty Data
    skimr,          # Compact and Flexible Summaries of Data
    scales,         # Scale Functions for Visualization
    glue,           # Interpreted String Literals
    here            # A Simpler Way to Find Your Files
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  8,
    height =  16,
    units  = "in",
    dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)



## 2. READ IN THE DATA ----
# tt <- tidytuesdayR::tt_load(2024, week = 50)
#
# parfumo_data_raw  <- tt$parfumo_data |> clean_names()
#
# tidytuesdayR::readme(tt)
# rm(tt)

# Option 2: Read directly from GitHub
parfumo_data_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv")
parfumo_data_raw <- parfumo_data_raw |> clean_names()


## 3. EXAMINING THE DATA ----
glimpse(parfumo_data_raw)
skim(parfumo_data_raw)



## 4. TIDYDATA ----
parfumo_brand_stats <- parfumo_data_raw |>
    group_by(brand) |>
    summarise(
        n_fragrances = n(),
        avg_rating = mean(rating_value, na.rm = TRUE),
        .groups = "drop"
    ) |>
    filter(n_fragrances >= 50) |> # Filter for brands with at least 50 fragrances
    arrange(desc(avg_rating)) |>
    head(20) |> # Select the top 20 brands based on average rating
    # Create y-coordinates for rectangles
    mutate(
        ymax = cumsum(n_fragrances),
        ymin = lag(ymax, default = 0),
        font_size = ifelse(n_fragrances > 200, 7.5, 4.5), # Larger bars get a bigger font size
        brand = case_when(
            brand == "Abdul Samad Al Qurashi / عبدالصمد القرشي" ~ "Abdul Samad Al Qurashi", # Arabic characters were not rendering properly
            brand == "Teone Reinthal Natural Perfume" ~ "Teone Reinthal\nNatural Perfume",
            TRUE ~ as.character(brand)
        )
    )



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
bkg_col      <- "#f5f5f2"
title_col    <- "gray20"
subtitle_col <- "gray20"
caption_col  <- "gray30"
text_col     <- "gray30"
note_col     <- "gray40" 
bar_col      <- "#1B2B48"

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 50 } &bull; Source: Parfumo Fragrance Dataset<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

# text
title_text    <- str_glue("Top 20 Perfume Brands Ranked by Average Rating<br>
                          and Portfolio Size")
subtitle_text <- str_glue("This chart displays the leading perfume brands based on their average<br>
                          customer ratings. The length of each bar represents the brand\\'s average<br>
                          rating, while the height indicates the number of fragrances in their portfolio.")
caption_text <- str_glue("{tt} {li} stevenponce &bull; {bs} sponce1 &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add("fa6-brands", here::here("fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf"))
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
    legend.position       = "plot",
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.1),
                                         color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.title.y.right    = element_text(margin = margin(0, 20, 0, 10), size = rel(1.1),
                                         color = text_col, family = "text", face = "bold",
                                         angle = 0, vjust = 1.0, hjust = 1),
    axis.line.x           = element_line(color = "#252525", linewidth = .2),
    axis.title            = element_text(size = rel(0.93), face = "bold", color = text_col),
    axis.text             = element_text(size = rel(0.79), color = text_col),
    legend.title          = element_blank(),
    legend.text           = element_text(size = rel(0.71), color = text_col),
    panel.grid.major.x    = element_blank(),
    panel.grid.major.y    = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor      = element_blank()
)

### |-  initial plot ----

p <- ggplot(parfumo_brand_stats) +
    # Horizontal bar chart with variable width
    geom_rect(
        aes(
            xmin = 0,
            xmax = avg_rating,
            ymin = ymin,
            ymax = ymax
        ),
        color = "#e0e0e0",
        fill = bar_col,
        linewidth = ifelse(parfumo_brand_stats$n_fragrances > 200, 1, 0.3)
    ) +
    # Add brand names to bars
    geom_text(
        aes(
            x = 0.2,
            y = ymin + ((ymax - ymin) / 2),
            label = ifelse(n_fragrances >= 80, brand, ""),
        ),
        color = ifelse(parfumo_brand_stats$n_fragrances < 200, "#e0e0e0", "white"),
        hjust = 0,
        size = parfumo_brand_stats$font_size
    ) +
    # Add ratings to bars
    geom_text(
        aes(
            x = avg_rating - 0.3,
            y = ymin + ((ymax - ymin) / 2),
            label = ifelse(n_fragrances >= 80,
                           paste(sprintf("(%.1f)", avg_rating)),
                           ""
            ),
        ),
        color = ifelse(parfumo_brand_stats$n_fragrances < 200, "#e0e0e0", "white"),
        hjust = 1,
        size = 3.2
    ) +
    # Scales
    scale_x_continuous(
        limits = c(0, 11),
        breaks = seq(0, 11, 2),
        expand = c(0, 0, 0.05, 0)
    ) +
    scale_y_continuous(
        expand = c(0, 0, 0.01, 0),
        position = "right",
        labels = scales::comma,
        breaks = seq(0, max(parfumo_brand_stats$ymax), 1000)
    ) +
    # Labs
    labs(
        x = "Average Rating",
        y = "Number of\nFragrances",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text
    ) +
    # Theme
    theme(
        plot.title      = element_markdown(
            size        = rel(1.8),
            family      = "title",
            face        = "bold",
            color       = title_col,
            lineheight  = 1.1,
            margin      = margin(t = 5, b = 5)
        ),
        plot.subtitle   = element_markdown(
            size        = rel(1.06),
            family      = "subtitle",
            color       = text_col,
            lineheight  = 1.1,
            margin      = margin(t = 5, b = 20)
        ),
        plot.caption    = element_markdown(
            size        = rel(.65),
            family      = "caption",
            color       = caption_col,
            lineheight  = 0.65,
            hjust       = 0.5,
            halign      = 0.5,
            margin      = margin(t = 10, b = 5)
        ),
    )

### |-  annotated plot ----

annotation_data <- tibble(
    x = c(9, 9, 9),
    y = c(parfumo_brand_stats$ymin[3] + ((parfumo_brand_stats$ymax[3] - parfumo_brand_stats$ymin[3]) / 2),
          parfumo_brand_stats$ymin[18] + ((parfumo_brand_stats$ymax[18] - parfumo_brand_stats$ymin[18]) / 2),
          parfumo_brand_stats$ymin[8] + ((parfumo_brand_stats$ymax[8] - parfumo_brand_stats$ymin[8]) / 2)),
    xend = c(parfumo_brand_stats$avg_rating[3], parfumo_brand_stats$avg_rating[18], parfumo_brand_stats$avg_rating[8]),
    yend = c((parfumo_brand_stats$ymin[3] + parfumo_brand_stats$ymax[3]) / 2,
             (parfumo_brand_stats$ymin[18] + parfumo_brand_stats$ymax[18]) / 2,
             (parfumo_brand_stats$ymin[8] + parfumo_brand_stats$ymax[8]) / 2),
    label = c("Ensar Oud / Oriscent has\n534 fragrances in its portfolio",
              "Guerlain has 586 fragrances\nin its portfolio",
              "Sunnamusk has 57 fragrances\nin its portfolio")
)

p +
    # Add annotation outside the grid
    coord_cartesian(
        expand = FALSE,
        clip = "off",
        xlim = c(0, 11)
    ) +
    
    # Add labels and curves programmatically
    geom_label(
        data = annotation_data,
        aes(
            x = x + 0.2,
            y = y,
            label = label
        ),
        size = 4,
        color = note_col,
        hjust = 0,
        lineheight = 1,
        label.size = NA,
        label.padding = unit(0, "lines"),
        fill = "transparent"
    ) +
    geom_curve(
        data = annotation_data,
        aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend
        ),
        curvature = 0,
        color = note_col,
        arrow = arrow(type = "closed", length = unit(0.1, "inches"))
    )



# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-12-03
# rstudio  2024.09.1+394 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# P digest        0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi         1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap       1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext      * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski        1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue        * 1.7.0    2024-01-09 [?] CRAN (R 4.4.0)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# P gridtext      0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable        0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here        * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools     0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr         1.46     2024-04-06 [?] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.12     2023-12-06 [?] CRAN (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P renv          1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# P repr          1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang         1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr       * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr     * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite       2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts   1.1.0    2024-05-15 [1] CRAN (R 4.4.0)
# textshaping   0.4.0    2024-05-24 [1] CRAN (R 4.4.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidyverse   * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange    0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# P tzdb          0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# withr         3.0.1    2024-07-31 [1] CRAN (R 4.4.1)
# P xfun          0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# > 
