
## Challenge: #TidyTuesday 2024 week 32
## Data:      Olympics athletes and medals
## Author:    Steven Ponce
## Date:      2024-08-07


## 1. LOAD PACKAGES & SETUP ----
pacman::p_load(
  tidyverse,     # Easily Install and Load the 'Tidyverse'
  ggtext,        # Improved Text Rendering Support for 'ggplot2'
  showtext,      # Using Fonts More Easily in R Graphs
  janitor,       # Simple Tools for Examining and Cleaning Dirty Data
  skimr,         # Compact and Flexible Summaries of Data
  scales,        # Scale Functions for Visualization
  lubridate,     # Make Dealing with Dates a Little Easier
  MetBrewer      # Color Palettes Inspired by Works at the Metropolitan Museum of Art
 )  


### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  8,
  height =  10,
  units  = "in",
  dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(
  x = base::as.double("2024"),
  week = base::as.double("32")
)

olympics <- tt$olympics |> clean_names() |> glimpse()

tidytuesdayR::readme(tt)
rm(tt)



## 3. EXAMINING THE DATA ----
glimpse(olympics)
skim(olympics)



## 4. TIDYDATA ----

# Judo df
judo <- olympics |> 
    filter(sport == "Judo") |> 
    mutate(
        gender = case_when(
            str_detect(event, "Men's")   ~ "Men",
            str_detect(event, "Women's") ~ "Women",
            TRUE ~ "Unspecified"  
        ),
        weight_class = str_replace_all(event, c("Men's " = "", "Women's " = "")),
        weight_class = str_replace_all(weight_class, c("Judo " = "")),
        weight_class = factor(weight_class, levels = c("Extra-Lightweight", "Half-Lightweight", "Lightweight", 
                                                       "Half-Middleweight", "Middleweight" , "Half-Heavyweight", 
                                                       "Heavyweight", "Open Class"))
    )  

# Judo medals
judo_medals <- judo |>
    filter(medal %in% c("Gold", "Silver", "Bronze")) |>
    count(country = team, year, gender, medal)

# Count medals for top countries
top_countries_medals <- judo_medals |>
    group_by(country) |>
    summarize(total_medals = sum(n)) |>
    slice_max(total_medals, n = 10) |>
    pull(country)

# Filter for the top 10 countries and calculate medals count
filtered_judo_medals <- judo |>
    filter(team %in% top_countries_medals) |>
    group_by(country = team, year, gender) |>
    summarise(
        medal_count = sum(medal %in% c("Gold", "Silver", "Bronze")), 
        .groups = "drop"
        ) |>
    arrange(country, year, gender)



# 5. VISUALIZATION ----

### |- plot aesthetics ----
bkg_col      <- colorspace::lighten('#f7f5e9', 0.05)    
title_col    <- "#3d3d3d"           
subtitle_col <- "#3d3d3d"     
caption_col  <- "gray30"   
text_col     <- colorspace::darken("#8e8a7b" , 0.2)   

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 32 } &bull; Source: Kaggle Olypmic history data <br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Judo’s Elite: Medal Achievements of the Top 10 Nations at the<br>
                          Summer Olympics Games, 1964 - 2016")

subtitle_text <- str_glue("Overview of Olympic medal counts by gender across decades,<br>
                          highlighting judo's leading nations.")

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

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'top',
    legend.justification  = "right",
    legend.title.position = "top",
    legend.title.align    = 1,  
    legend.box.just       = "right",  
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    plot.margin           = margin(t = 20, r = 25, b = 20, l = 25),
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.2), color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1.2), color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.text             = element_text(size = rel(0.8), color = text_col, family = "text"),
    axis.line.x           = element_line(color = "gray40", linewidth = .15),
    panel.grid.minor.x    = element_blank(),
    panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray'),
    panel.grid.minor.y    = element_blank(),
    panel.grid.major.y    = element_blank(),
    
    strip.text            = element_textbox(size     = rel(1.1),
                                            face     = 'bold',
                                            color    = text_col,
                                            hjust    = 0.5,
                                            halign   = 0.5,
                                            r        = unit(5, "pt"),
                                            width    = unit(5.5, "npc"),
                                            padding  = margin(3, 0, 3, 0),
                                            margin   = margin(3, 3, 3, 3),
                                            fill     = "transparent"),
    
    panel.spacing       = unit(3, 'lines'),
)  


### |-  initial plot ----  
p <- filtered_judo_medals |>
    ggplot(aes(x = year, y = fct_reorder(country, medal_count, .fun = sum), fill = medal_count)) +
    
  # Geoms
  geom_tile(color = "white", linewidth = 0.15) +
  geom_text(data = filtered_judo_medals |> filter(gender == "Men"),
            aes(label = "Country", x = 1962, y = "Japan"),
            size = 6.5, color = text_col, family = "text", fontface = "bold", 
            hjust = 1.4, vjust = -1.5) +

  # Scales
  scale_x_continuous(breaks = seq(
      min(filtered_judo_medals$year), 
      max(filtered_judo_medals$year), 
      by = 4), 
      labels = function(x) paste0("'", substr(x, 3, 4))) +
  scale_y_discrete() +
  coord_cartesian(clip = "off") +
  scale_fill_stepsn(
    colors = c('#DFEDEB', "#A1FCDF", "#7FD8BE", "#FCD29F", "#FCAB64"),
    limits = c(0, 7), 
    breaks = 0:7,
    labels = as.character(0:7),
    guide = guide_colorsteps(
        direction = "horizontal",
        barwidth = unit(10, "cm"),
        barheight = unit(0.5, "cm"),
        frame.colour = NA,
        title.position = "top",
        title.hjust = 1
        )
    ) +

  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x = "Year",
    y = "",
    fill = "Medal Count"
    ) +

  # Facet
  facet_wrap(vars(gender), nrow = 2, axes = "all") +

  # Theme
  theme(
      plot.title      = element_markdown(
          size        = rel(1.5),
          family      = "title",
          color       = title_col,
          face        = "bold",
          lineheight  = 0.85,
          margin      = margin(t = 5, b = 10)
      ),
      plot.subtitle   = element_markdown(
          size        = rel(1.1),
          family      = "subtitle",
          color       = title_col,
          lineheight  = 1,
          margin      = margin(t = 5, b = 10)
      ),
      plot.caption    = element_markdown(
          size        = rel(.6),
          family      = "caption",
          color       = caption_col,
          lineheight  = 0.6,
          hjust       = 0,
          halign      = 0,
          margin      = margin(t = 10, b = 5)
      ),
  ) 
    
### |-  annotated plot ----  

men_text <- str_glue("Judo made its first Olympic appearance in 1964,\nbut was not included on the program of\nthe 1968 Olympic Games.")

women_text <- str_glue("Women's judo made its first appearance at the\n1988 Olympic Games, as a demonstration sport.\nWomen's Judo became an official part of the\nOlympic games from the 1992 Barcelona games")

### |-  final plot ----  
p +
    
    # Men's Judo History Note
    geom_text(data = filtered_judo_medals |> filter(gender == "Men"),
              aes(x = 1964, y = Inf, label = men_text),
              hjust = .6, vjust = -1.2, 
              size = 3.5, color = text_col,  family = "anotation", 
              lineheight = 0.9) +
    
    # Curved Arrow for Men's Note
    geom_curve(data = filtered_judo_medals |> filter(gender == "Men"),
               aes(x = 1968, y = "Japan", xend = 1968, yend = "South Korea"), 
               curvature = 0, arrow = arrow(type = "closed", length = unit(0.08, "inches")),
               linewidth = .5,  
               color = text_col, size = 0.7) +
    
    # Women's Judo History Note
    geom_text(data = filtered_judo_medals %>% filter(gender == "Women"),
              aes(label = women_text, x = 1962, y = "Japan"),
              hjust = 0, vjust = 1, size = 3.5, color = text_col, family = "anotation", 
              lineheight = 0.9) 



# 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────
# setting  value
# version  R version 4.4.0 (2024-04-24 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-08-06
# rstudio  2024.04.2+764 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.4.0    2024-04-24 [2] local
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger     1.1.0    2016-07-27 [?] CRAN (R 4.4.0)
# P cli            3.6.2    2023-12-11 [?] CRAN (R 4.4.0)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# P digest         0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# P fs             1.6.4    2024-04-25 [?] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue           1.7.0    2024-01-09 [?] CRAN (R 4.4.0)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here           1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
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
# P MetBrewer    * 0.2.0    2022-03-21 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman         0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P readxl         1.4.3    2023-07-06 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# P rlang          1.1.3    2024-01-10 [?] CRAN (R 4.4.0)
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
# P stringi        1.8.3    2023-12-11 [?] CRAN (R 4.4.0)
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
# P withr          3.0.0    2024-01-16 [?] CRAN (R 4.4.0)
# P xfun           0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/windows/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/ebed5364
# 
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────
# > 