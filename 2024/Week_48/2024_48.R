
## Challenge: #TidyTuesday 2024 week 48
## Data:      U.S. Customs and Border Protection (CBP) Encounter Data
## Author:    Steven Ponce
## Date:      2024-11-24


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,         # Easily Install and Load the 'Tidyverse'
    ggtext,            # Improved Text Rendering Support for 'ggplot2'
    showtext,          # Using Fonts More Easily in R Graphs
    janitor,           # Simple Tools for Examining and Cleaning Dirty Data
    skimr,             # Compact and Flexible Summaries of Data
    scales,            # Scale Functions for Visualization
    glue,              # Interpreted String Literals
    here,              # A Simpler Way to Find Your Files
    patchwork,         # The Composer of Plots
    usmap              # US Maps Including Alaska and Hawaii
)   

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  16,
    height =  10,
    units  = "in",
    dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)



## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2024, week = 48) 

cbp_resp  <- tt$cbp_resp |> clean_names()
cbp_state <- tt$cbp_state |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)



## 3. EXAMINING THE DATA ----
glimpse(cbp_resp)
glimpse(cbp_state)



## 4. TIDYDATA ----

# 1. Demographic Evolution Data
demographic_trends_df <- cbp_resp |>
    group_by(fiscal_year, demographic) |>
    summarise(total_encounters = sum(encounter_count), .groups = 'drop') |>
    arrange(fiscal_year, demographic)

# Get end points for direct labeling
label_data <- demographic_trends_df |>
    filter(fiscal_year == max(fiscal_year))


# 2. State Map Data
state_map_df <- cbp_state |>
    group_by(state) |>
    summarise(encounter_count = sum(encounter_count), .groups = 'drop')

# 3. Demographic Authority Data
demographic_authority_df <- cbp_resp |>
    group_by(demographic, title_of_authority) |>
    summarise(total_encounters = sum(encounter_count), .groups = 'drop') |>
    pivot_wider(
        names_from = title_of_authority,
        values_from = total_encounters
    ) |>
    mutate(
        demographic = fct_reorder(demographic, `Title 8`),  
        `Title 8` = `Title 8` / 1e6,
        `Title 42` = `Title 42` / 1e6
    )

# 4. Seasonal Citizenship Data
# Get correct month order (Fiscal Year)
month_order <- c("OCT", "NOV", "DEC", "JAN", "FEB", "MAR", 
                 "APR", "MAY", "JUN", "JUL", "AUG", "SEP")

top_10_citizenship <- cbp_resp |>
    group_by(citizenship) |>
    summarise(total_encounters = sum(encounter_count), .groups = 'drop') |>
    arrange(desc(total_encounters)) |>
    slice_head(n = 10) |>
    pull(citizenship)

# Get top 5 countries instead of 10 to reduce visual complexity
top_5_citizenship <- head(top_10_citizenship, 5)

seasonal_citizenship_df <- cbp_resp |>
    filter(citizenship %in% top_10_citizenship) |>
    group_by(month_abbv, citizenship) |>
    summarise(avg_encounters = mean(encounter_count), .groups = 'drop') |>
    mutate(
        month_abbv = factor(month_abbv, levels = month_order),
        citizenship = factor(citizenship, levels = top_10_citizenship)
    )



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
bkg_col      <- "#f5f5f2"  
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray30"  

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 48 } &bull; Source: U.S. Customs and Border Protection (CBP)<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

# text
title_text    <- str_glue("U.S. Border Encounters Analysis (FY2020-2024)")

subtitle_text <- "Evolution, Geographic Distribution, Processing Types, and Seasonal Patterns"

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
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    legend.position       = "bottom",
    plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.1),
                                         color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1.1),
                                         color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.line.x           = element_line(color = "#252525", linewidth = .2),
    plot.title            = element_text(size = rel(1.14), face = "bold", hjust = 0.5, color = title_col),
    plot.subtitle         = element_text(size = rel(0.86), hjust = 0.5, color = subtitle_col),
    axis.title            = element_text(size = rel(0.93), face = "bold", color = text_col),
    axis.text             = element_text(size = rel(0.79), color = text_col),
    
    legend.title          = element_blank(),
    legend.text           = element_text(size = rel(0.71), color = text_col),

    panel.grid.major.x    = element_blank(),
    panel.grid.major.y    = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor      = element_blank()
)


### |-  plot 1 ----
demographic_evolution <- demographic_trends_df |>
  ggplot(aes(x = fiscal_year, y = total_encounters, color = demographic)) +

  # Geoms
  geom_line(size = 1.2) +
  geom_point(size = 3) +

  # Scales
  scale_color_manual(
    values = c(
      "#E64B35", "#4DBBD5", "#91D1C2", "#8491B4", "#F39B7F"
    )
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 2000000, by = 500000),
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  scale_x_continuous(
    breaks = 2020:2024,
    limits = c(2020, 2024)
  ) +

  # Labs
  labs(
    title = "Evolution of Border Encounters by Demographic Group",
    subtitle = "Trends in total encounters from FY2020 to FY2024",
    x = NULL,
    y = NULL,
    color = NULL
  ) +

  # Theme
  theme(
    legend.position = "top",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


### |-  plot 2 ----
state_encounters_map <- plot_usmap(
  data = state_map_df,
  values = "encounter_count",
  color = "#d9d9d9",
  size = 0.25
) +

  # Scales
  coord_sf(clip = "off") +
  scale_fill_gradientn(
    colors = c(
      "#f7f7f7",
      "#67a9cf",
      "#2166ac",
      "#feb24c"
    ),
    breaks = c(500000, 1000000, 2000000),
    labels = c("500K", "1M", "2M"),
    guide = guide_colorbar(
      title = "Encounter Count",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 12,
      barheight = 1,
      direction = "horizontal",
      ticks = FALSE,
      margin = margin(t = 10, b = 10)
    )
  ) +

  # Labs
  labs(
    title = "Geographic Distribution of Border Encounters",
    subtitle = "Total encounters by state, FY2020-2024",
    x = NULL,
    y = NULL
  ) +

  # Theme
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(fill = bkg_col, color = bkg_col),
    panel.background = element_rect(fill = bkg_col, color = bkg_col),
    legend.background = element_rect(fill = bkg_col, color = NA),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = title_col),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = subtitle_col)
  )


### |-  plot 3 ----

title_42_color <- "#E64B35"  
title_8_color <- "#4DBBD5"  

demographic_authority_plot <- ggplot(demographic_authority_df) +
 
  # Geoms
  geom_vline(
    xintercept = seq(0, 6, by = 2),
    color = "gray90",
    linewidth = 0.2
  ) +
  geom_segment(
    aes(
      y = demographic,
      x = `Title 42`,
      xend = `Title 8`,
      yend = demographic
    ),
    color = "gray80",
    linewidth = 0.5
  ) +
  geom_point(
    aes(x = `Title 42`, y = demographic),
    color = title_42_color,
    size = 3
  ) +
  geom_point(
    aes(x = `Title 8`, y = demographic),
    color = title_8_color,
    size = 3
  ) +
  geom_text(
    aes(
      x = `Title 42`, y = demographic,
      label = sprintf("%.2fM", `Title 42`)
    ),
    color = title_42_color,
    vjust = 2,
    hjust = 1.2,
    size = 4,
    family = "mono",
    fontface = "bold"
  ) +
  geom_text(
    aes(
      x = `Title 8`, y = demographic,
      label = sprintf("%.2fM", `Title 8`)
    ),
    color = title_8_color,
    vjust = 2,
    hjust = -0.2,
    size = 4,
    family = "mono",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 3,
    y = max(as.numeric(demographic_authority_df$demographic)) + 0.2,
    label = "Title 42",
    color = title_42_color,
    hjust = 1,
    vjust = 0,
    size = 5,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 4.8,
    y = max(as.numeric(demographic_authority_df$demographic)) + 0.2,
    label = "Title 8",
    color = title_8_color,
    hjust = 0,
    vjust = 0,
    size = 5,
    fontface = "bold"
  ) +

  # Scales
  scale_x_continuous(
    breaks = seq(0, 6, by = 2),
    labels = function(x) paste0(x, "M"),
    limits = c(0, 6),
    expand = c(0.15, 0.1)
  ) +
  scale_y_discrete() +

  # Labs
  labs(
    title = "Border Encounters by Demographic and Authority Type",
    subtitle = "Comparison of Title 8 vs Title 42 processing (in millions)",
    x = NULL,
    y = NULL
  ) +

  # Theme
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(t = 30, r = 20, b = 20, l = 20),
    axis.text.y = element_text(size = 11)
  )


### |-  plot 4 ----
seasonal_citizenship_plot <- seasonal_citizenship_df |>
  filter(citizenship %in% top_5_citizenship) |>
  mutate(
    month_abbv = factor(month_abbv, levels = month_order),
    citizenship = fct_reorder(citizenship, -avg_encounters, sum)
  ) |>
  ggplot(aes(x = month_abbv, y = avg_encounters, color = citizenship, group = citizenship)) +

  # Geoms
  geom_line(alpha = 0.15, linewidth = 0.7) +
  geom_point(size = 3.5) +

  # Scales
  scale_color_manual(
    values = c(
      "#E64B35", "#4DBBD5", "#91D1C2", "#8491B4", "#F39B7F"
    )
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1, suffix = "K"),
    breaks = seq(0, 600, by = 200),
    expand = expansion(mult = c(0.02, 0.1))
  ) +

  # Labs
  labs(
    title = "Seasonal Patterns of Border Encounters by Citizenship",
    subtitle = "Monthly average encounters for top 5 countries (Fiscal Year: October to September)",
    x = NULL,
    y = NULL
  ) +
  theme(
    # Legend refinements
    legend.position = "top",
    legend.direction = "horizontal",
    legend.spacing.x = unit(0.5, "cm"),
    legend.margin = margin(t = 5, b = 15),
    legend.title = element_blank(),
    legend.text = element_text(size = 9.5),
    panel.grid.major.x = element_blank()
  )


### |-  combined plots ----
combined_plot <- (
  demographic_evolution + state_encounters_map +
    plot_layout(widths = c(1, 1))
  ) / (
  demographic_authority_plot + seasonal_citizenship_plot
  )

combined_plot <- combined_plot +
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        theme = theme(
            plot.title = element_text(
                family = "title", 
                size = rel(2.5), 
                face = "bold",
                hjust = 0.5,
                color = title_col,
                margin = margin(b = 10)
            ),
            plot.subtitle = element_text(
                family = "text",
                size = rel(1.3),
                hjust = 0.5,
                color = subtitle_col,
                margin = margin(b = 20)
            ),
            plot.caption = element_markdown(
                family = "caption",
                size = rel(0.75),
                color = caption_col,
                hjust = 0.5,
                margin = margin(t = 20)
            ),
            plot.margin = margin(10, 10, 10, 10)
        )
    )


combined_plot


# 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-11-24
# rstudio  2024.09.1+394 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger     1.1.0    2016-07-27 [?] CRAN (R 4.4.0)
# P class          7.3-22   2023-05-03 [?] CRAN (R 4.4.0)
# P classInt       0.4-10   2023-09-05 [?] CRAN (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# P DBI            1.2.2    2024-02-16 [?] CRAN (R 4.4.0)
# P digest         0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P e1071          1.7-14   2023-12-06 [?] CRAN (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# P fs             1.6.4    2024-04-25 [?] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
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
# P KernSmooth     2.23-22  2023-07-10 [?] CRAN (R 4.4.0)
# P knitr          1.46     2024-04-06 [?] CRAN (R 4.4.0)
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
# P proxy          0.4-27   2022-06-09 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
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
# P selectr        0.4-2    2019-11-20 [?] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P sf             1.0-16   2024-03-24 [?] CRAN (R 4.4.0)
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
# P units          0.8-5    2023-11-28 [?] CRAN (R 4.4.0)
# P usethis        2.2.3    2024-02-19 [?] CRAN (R 4.4.0)
# usmap        * 0.7.1    2024-03-21 [1] CRAN (R 4.4.2)
# usmapdata      0.3.0    2024-05-17 [1] CRAN (R 4.4.2)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# withr          3.0.1    2024-07-31 [1] CRAN (R 4.4.1)
# P xfun           0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────
# > 
