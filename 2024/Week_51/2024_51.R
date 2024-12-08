
## Challenge: #TidyTuesday 2024 week 51
## Data:      Dungeons and Dragons Spells (2024)
## Author:    Steven Ponce
## Date:      2024-12-08


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
    here,           # A Simpler Way to Find Your Files
    camcorder,      # Record Your Plot History 
    patchwork,      # The Composer of Plots
    marquee,        # Markdown Parser and Renderer for R Graphics 
    gghighlight     # Highlight Lines and Points in 'ggplot2'
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  12,
    height =  8,
    units  = "in",
    dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)



## 2. READ IN THE DATA ----
# tt <- tidytuesdayR::tt_load(2024, week = 51)
#
# spells_raw  <- tt$spells |> clean_names()
#
# tidytuesdayR::readme(tt)
# rm(tt)

# Option 2: Read directly from GitHub
spells_raw <- spells <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-17/spells.csv')

spells_raw <- spells_raw |> clean_names()


## 3. EXAMINING THE DATA ----
glimpse(spells_raw)
skim(spells_raw)



## 4. TIDYDATA ----

## Plot 1: Exclusive Spells Data ----
exclusive_df <- spells_raw |>
    # Calculate class availability
    mutate(
        available_to = rowSums(select(spells_raw, bard:wizard))
    ) |>
    # Get exclusive spells
    filter(available_to == 1) |>  
    select(bard:wizard) |>
    # Calculate totals
    summarise(across(everything(), sum)) |>
    # Reshape to long format
    pivot_longer(
        everything(),
        names_to = "class",
        values_to = "exclusive_spells"
    ) |>
    # Format and calculate percentages
    mutate(
        class = str_to_title(class),
        total_spells = sum(exclusive_spells),
        pct = exclusive_spells / total_spells,
        label = scales::percent(pct, accuracy = 0.1),
        class = fct_reorder(class, exclusive_spells, .desc = TRUE)
    )

## Plot 2: Progression Data ----
progression_df <- spells_raw |>
    # Initial selection and reshape
    select(level, bard:wizard) |>
    pivot_longer(
        -level,
        names_to = "class",
        values_to = "has_spell"
    ) |>
    # Process available spells
    filter(has_spell) |>
    # Count spells per class and level
    group_by(class, level) |>
    summarise(
        count = n(),
        .groups = "drop"
    ) |>
    # Calculate totals and format
    group_by(class) |>
    mutate(
        total_spells = sum(count)
    ) |>
    ungroup() |>
    # Format and order class factor
    mutate(
        class = str_to_title(class),
        # Order by total spells descending
        class = fct_reorder(class, total_spells, .desc = TRUE)
    )



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
bkg_col      <- "#f5f5f2"
title_col    <- "gray20"
subtitle_col <- "gray30"
text_col     <- "gray30"
caption_col  <- "gray40"
col_palette  <- "#AB4459"        

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 51 } &bull; Source: D&D Free Rules (2024), Spell Descriptions<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

# text
title_text    <- str_glue("The Arcane Hierarchy: D&D Spellcasting Classes Compared")
subtitle_text <- "While **_Wizards_** master the most spells overall, **_Clerics_** maintain the largest collection of unique divine magic, highlighting distinct magical specializations across classes."
caption_text  <- str_glue("{tt} {li} stevenponce &bull; {bs} sponce1 &bull; {gh} poncest &bull; #rstats #ggplot2")

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
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.05),
                                         color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1.05),
                                         color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.line.x           = element_line(color = "#252525", linewidth = .2),
    axis.title            = element_text(size = rel(0.93), face = "bold", color = text_col),
    axis.text             = element_text(size = rel(0.79), color = text_col),
    legend.title          = element_blank(),
    legend.text           = element_text(size = rel(0.71), color = text_col),
    panel.grid.major.x    = element_blank(),
    panel.grid.major.y    = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor      = element_blank(),
    strip.text            = element_textbox(size     = rel(0.9),
                                            face     = 'bold',
                                            color    = text_col,
                                            hjust    = 0.5,
                                            halign   = 0.5,
                                            r        = unit(3, "pt"),
                                            width    = unit(6, "npc"),
                                            padding  = margin(2, 0, 2, 0),
                                            margin   = margin(3, 3, 3, 3),
                                            fill     = "transparent"),
    panel.spacing         = unit(1.5, 'lines')
)

### |-  Plot 1 ----
exclusive_plot <- ggplot(exclusive_df,
                         aes(y = fct_reorder(class, exclusive_spells), x = exclusive_spells)) +
    # Geoms
    geom_bar(stat = "identity", 
             fill = col_palette[1],
             alpha = 0.8,
             width = 0.75
    ) +
    geom_text(
        aes(label = sprintf("%d spells", exclusive_spells),),
        # hjust = -0.2,
        size = 3.5,
        color = if_else(exclusive_df$exclusive_spells < 15, text_col, "#fafafa"),
        hjust = if_else(exclusive_df$exclusive_spells < 15, -0.2, 1.2),
    ) +
    
    # Scales
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.05)),
        breaks = seq(0, 25, by = 5)
    ) +
    scale_y_discrete() +
    coord_cartesian(clip = 'off') +
    
    # Labs
    labs(
        title = "Class-Exclusive Spells in D&D",
        subtitle = "Distribution of spells unique to each character class",
        x = "Number of Exclusive Spells",
        y = NULL
    ) +
    
    # Theme
    theme(
        plot.title = element_text(
            family = "title", 
            size   = rel(1.4), 
            face   = "bold",
            color  = title_col,
            margin = margin(b = 10)
        ),
        plot.subtitle = element_text(
            family = "text",
            size   = rel(0.9),
            color  = subtitle_col,
            margin = margin(b = 5)
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
    ) 
  
### |-  Plot 2 ----
progression_plot <- ggplot(progression_df,
                           aes(x = level, y = count, group = class)) +
    # Geoms
    geom_line(size = 0.3, alpha = 0.2) +
    geom_point(size = 1, alpha = 0.2) +
    gghighlight(
        use_direct_label = FALSE,
        unhighlighted_params = list(
            size = 0.3,
            alpha = 0.2,
            color = 'gray20'
        )
    ) +
    geom_line(color = col_palette[1], size = 1.2) +
    geom_point(color = col_palette[1], size = 2.5) +
    
    # Scales
    scale_x_continuous(breaks = seq(0, 9, by = 3)) +
    scale_y_continuous(breaks = seq(0, 35, by = 10)) +
    coord_cartesian(clip = 'off') +
    
    # Labs
    labs(
        title = "Spell Progression Patterns by Character Class",
        subtitle = "Each class shows distinct patterns in spell availability across levels",
        x = "Spell Level",
        y = "Number of Available Spells"
    ) +
    
    # Facet
    facet_wrap(~class, ncol = 4) +
    
    # Theme
    theme(
        plot.title = element_text(
            family = "title", 
            size   = rel(1.4), 
            face   = "bold",
            color  = title_col,
            margin = margin(b = 10)
        ),
        plot.subtitle = element_text(
            family = "text",
            size   = rel(0.9),
            color  = subtitle_col,
            margin = margin(b = 15)
        )
    ) 

### |-  combined plots ----
combined_plot <- (
    exclusive_plot + plot_spacer() + progression_plot + 
        plot_layout(widths = c(0.6, 0.02, 1.2))  
)

combined_plot <- combined_plot +
    plot_annotation(
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text,
        theme = theme(
            plot.title = element_text(
                family = "title", 
                size   = rel(2.3), 
                face   = "bold",
                color  = title_col,
                margin = margin(b = 10)
            ),
            plot.subtitle = element_marquee(
                family = "text",
                lineheight = 1.1,
                width  = 1,
                size   = rel(1.1),
                color  = subtitle_col,
                margin = margin(b = 5)
            ),
            plot.caption = element_markdown(
                family = "caption",
                size   = rel(0.65),
                color  = caption_col,
                hjust  = 0.5,
                margin = margin(t = 5)
            ),
            plot.margin = margin(10, 10, 10, 10),
            plot.background = element_rect(fill = bkg_col, color = bkg_col),
            panel.background = element_rect(fill = bkg_col, color = bkg_col)
        )
    ) 

combined_plot 



# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-12-06
# rstudio  2024.09.1+394 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# P annotater      0.2.3    2024-01-26 [?] CRAN (R 4.4.0)
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
# P gghighlight  * 0.4.1    2023-12-16 [?] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue         * 1.7.0    2024-01-09 [?] CRAN (R 4.4.0)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridExtra    * 2.3      2017-09-09 [?] CRAN (R 4.4.0)
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
# P marquee      * 0.1.0    2024-05-28 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P patchwork    * 1.3.0    2024-09-16 [?] CRAN (R 4.4.1)
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache        0.16.0   2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3    1.8.2    2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo           1.26.0   2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils        2.12.3   2023-11-18 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.3    2024-09-11 [?] CRAN (R 4.4.2)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P readxl         1.4.3    2023-07-06 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# P rvest          1.0.4    2024-02-12 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats        * 4.4.0    2024-04-24 [?] local
# stringi        1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P styler         1.10.3   2024-04-07 [?] CRAN (R 4.4.0)
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
# ───────────────────────────────────────────────────────────────────────────────
# > 
