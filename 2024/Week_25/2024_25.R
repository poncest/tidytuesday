## Challenge: #TidyTuesday 2024 week 25
## Data:      US Federal Holidays
## Author:    Steven Ponce
## Date:      2024-06-17


## 1. LOAD PACKAGES & SETUP ----
pacman::p_load(
  tidyverse,     # Easily Install and Load the 'Tidyverse'
  ggtext,        # Improved Text Rendering Support for 'ggplot2'
  showtext,      # Using Fonts More Easily in R Graphs
  janitor,       # Simple Tools for Examining and Cleaning Dirty Data
  skimr,         # Compact and Flexible Summaries of Data
  scales,        # Scale Functions for Visualization
  lubridate,     # Make Dealing with Dates a Little Easier
  marquee,       # Markdown Parser and Renderer for R Graphics
  tidytext       # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools
)

### |- tidy data ---

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  7.77,
  height =  8,
  units  = "in",
  dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(
  x = base::as.double("2024"),
  week = base::as.double("25")
)

federal_holidays <- tt$federal_holidays |>
  clean_names() |>
  glimpse()
# proposed_federal_holidays <- tt$pproposed_federal_holidays |> clean_names() |> glimpse()

proposed_federal_holidays <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-18/proposed_federal_holidays.csv") |>
  clean_names() |>
  glimpse()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(federal_holidays)
glimpse(proposed_federal_holidays)


## 4. TIDYDATA ----

### |- tidy data ---
tf_idf_data <- proposed_federal_holidays |>
  unnest_tokens(word, details) |>
  anti_join(stop_words, by = "word") |> # Removing stop words
  count(official_name, date, word) |>
  bind_tf_idf(word, official_name, n) |>
  mutate(
    word = reorder(word, tf_idf),
    strip_title = str_glue("{official_name}<br>{date}"),
    strip_title = fct_reorder(strip_title, tf_idf, .desc = TRUE, .na_rm = TRUE)
  ) |> 
  filter(tf_idf > 0.08)


# 5. VISUALIZATION ----

### |- plot aesthetics ----
bkg_col      <- "#3D3D3D"
title_col    <- "#E1E6E1"
subtitle_col <- "#E1E6E1"
caption_col  <- "#E1E6E1"
text_col     <- "#E1E6E1"
col_palette <- c("#4E79A7", "#F28E2C", "#E15759", "#76B7B2", "#59A14F", "#EDC949", "#B07AA2", "#FF9DA7", "#9C755F", "#BAB0AB")

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 25 } &bull; Source: Wikipedia Federal holidays in the United States<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text <- str_glue("Unpacking the Language of Change: Proposed US Federal Holidays")

subtitle_text <- str_glue("Insight into the distinctive terms shaping new holiday proposals and their cultural implications")

caption_text <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Quattrocento Sans", regular.wt = 400, family = "subtitle")
font_add_google("Quattrocento Sans", regular.wt = 400, family = "text")
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
showtext_auto(enable = TRUE)

### |-  plot theme ----
theme_set(theme_minimal(base_size = 10, base_family = "text"))

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.text             = element_text(size = rel(0.8), color = text_col, family = "text"),
    panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray'),
    panel.grid.minor.x    = element_blank(),
    panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray'),
    panel.grid.minor.y    = element_blank(),
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
    panel.spacing       = unit(1, 'lines')
)  

### |-  final plot ----
tf_idf_data |>
  ggplot(aes(x = word, y = tf_idf, fill = official_name)) +
  
  # Geoms
  geom_col() +

  # Scales
  scale_fill_manual(values = col_palette) +
  coord_flip(clip = "off") +

  # Scales
  scale_x_discrete() +
  scale_y_continuous(breaks = pretty_breaks(n = 3)) +
  scale_color_manual(values = col_palette) +

  # Labs
  labs(
    x = NULL,
    y = "Term Frequency-Inverse Document Frequency (TF-IDF)",
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +

  # Facets
  facet_wrap(vars(strip_title), scales = "free") +

  # Theme
  theme(
    plot.title = element_text(
      size        = rel(1.7),
      family      = "title",
      color       = title_col,
      face        = "bold",
      lineheight  = 0.85,
      margin      = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_marquee(
      size        = rel(1.2),
      family      = "subtitle",
      color       = title_col,
      lineheight  = 1,
      margin      = margin(t = 5, b = 5)
    ),
    plot.caption = element_markdown(
      size        = rel(.6),
      family      = "caption",
      color       = caption_col,
      lineheight  = 0.6,
      hjust       = 0,
      halign      = 0,
      margin      = margin(t = 10, b = 5)
    ),
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.0 (2024-04-24 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-06-16
# rstudio  2024.04.2+764 Chocolate Cosmos (desktop)
# pandoc   NA
#
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.4.0    2024-04-24 [2] local
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P cachem         1.0.8    2023-05-01 [?] CRAN (R 4.4.0)
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
# P ggforce        0.4.2    2024-02-19 [?] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggraph       * 2.2.1    2024-03-07 [?] CRAN (R 4.4.0)
# P ggrepel        0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue           1.7.0    2024-01-09 [?] CRAN (R 4.4.0)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P graphlayouts   1.1.1    2024-03-09 [?] CRAN (R 4.4.0)
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridExtra      2.3      2017-09-09 [?] CRAN (R 4.4.0)
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here           1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P httr           1.4.7    2023-08-15 [?] CRAN (R 4.4.0)
# P igraph       * 2.0.3    2024-03-13 [?] CRAN (R 4.4.0)
# P janeaustenr    1.0.0    2022-08-26 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr          1.46     2024-04-06 [?] CRAN (R 4.4.0)
# P lattice        0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown       1.12     2023-12-06 [?] CRAN (R 4.4.0)
# P marquee      * 0.1.0    2024-05-28 [?] CRAN (R 4.4.0)
# P MASS           7.3-60.2 2024-04-24 [?] local
# P Matrix         1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P memoise        2.0.1    2021-11-26 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman         0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P polyclip       1.10-6   2023-09-27 [?] CRAN (R 4.4.0)
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
# P SnowballC      0.7.1    2023-04-25 [?] CRAN (R 4.4.0)
# P stats        * 4.4.0    2024-04-24 [?] local
# P stringi        1.8.3    2023-12-11 [?] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite        2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts    1.1.0    2024-05-15 [1] CRAN (R 4.4.0)
# textshaping    0.4.0    2024-05-24 [1] CRAN (R 4.4.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# P tidygraph      1.3.1    2024-01-30 [?] CRAN (R 4.4.0)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidytext     * 0.4.2    2024-04-10 [?] CRAN (R 4.4.0)
# P tidytuesdayR   1.0.3    2023-12-13 [?] CRAN (R 4.4.0)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tokenizers     0.3.0    2022-12-22 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tweenr         2.0.3    2024-02-26 [?] CRAN (R 4.4.0)
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P usethis        2.2.3    2024-02-19 [?] CRAN (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P viridis        0.6.5    2024-01-29 [?] CRAN (R 4.4.0)
# viridisLite    0.4.2    2023-05-02 [1] CRAN (R 4.4.0)
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
# ──────────────────────────────────────────────────────────────────────────────────────────────────
# >
