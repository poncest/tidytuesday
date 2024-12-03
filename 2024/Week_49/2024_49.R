## Challenge: #TidyTuesday 2024 week 49
## Data:      National Highways Traffic Flow
## Author:    Steven Ponce
## Date:      2024-11-30


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
    paletteer,         # Comprehensive Collection of Color Palettes
    gghighlight,       # Highlight Lines and Points in 'ggplot2'
    lubridate          # Working with Dates and Times
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
# tt <- tidytuesdayR::tt_load(2024, week = 49) 

# A64_traffic  <- tt$A64_traffic |> clean_names()
 
# tidytuesdayR::readme(tt)
# rm(tt)

# Option 2: Read directly from GitHub
A64_traffic <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-03/A64_traffic.csv')
A64_traffic <-  A64_traffic |> clean_names() 


## 3. EXAMINING THE DATA ----
glimpse(A64_traffic)
skim(A64_traffic)



## 4. TIDYDATA ----

# Hourly Volume Patterns (plot 1) ----
hourly_volume_clean <- A64_traffic |>
    group_by(report_date, hour = lubridate::hour(time_period_ending)) |>
    summarise(
        hourly_volume = sum(total_volume, na.rm = TRUE),
        .groups = 'drop'
    ) |>
    mutate(
        month_day = format(report_date, "%b %d"),
        hour_label = sprintf("%02d:00", hour),
        hour_f = factor(hour, levels = c(7, 8, 9, 16, 17, 18), ordered = TRUE),
        is_peak = hour %in% c(7:9, 16:18)
    )

# Daily Volume Trends (plot 2) ----
daily_volume_clean <- A64_traffic |>
    group_by(date = as.Date(report_date)) |>
    summarise(
        daily_volume = sum(total_volume, na.rm = TRUE),
        .groups = 'drop'
    ) |>
    mutate(
        week_num = week(date),
        month_day = format(date, "%b %d")
    )

# Weekend Patterns (plot 3) ----
weekend_patterns_clean <- A64_traffic |>
    mutate(
        hour = lubridate::hour(time_period_ending),
        is_weekend = ifelse(lubridate::wday(report_date) %in% c(1, 7), "Weekend", "Weekday"),
        hour_label = sprintf("%02d:00", hour)
    ) |>
    group_by(hour, hour_label, is_weekend) |>
    summarise(
        avg_volume = mean(total_volume, na.rm = TRUE),
        avg_speed = mean(avg_mph, na.rm = TRUE),
        .groups = 'drop'
    ) |>
    pivot_longer(
        cols = c(avg_speed, avg_volume),
        names_to = "metric",
        values_to = "value"
    ) |>
    mutate(
        metric = factor(metric,
                        levels = c("avg_speed", "avg_volume"),
                        labels = c("Average Speed (mph)", "Average Volume (count)"))
    ) |>
    group_by(hour, metric) |>
    summarise(
        weekday = value[is_weekend == "Weekday"],
        weekend = value[is_weekend == "Weekend"],
        .groups = 'drop'
    )

# Sensor Speed Patterns (plot 4) ----
sensor_speed_clean <- A64_traffic |>
    group_by(
        date = as.Date(report_date), 
        site_id,
        site_name
    ) |>
    summarise(
        avg_speed = mean(avg_mph, na.rm = TRUE),
        .groups = 'drop'
    ) |>
    mutate(
        month_day = format(date, "%b %d"),
        site_id = factor(site_id) 
    )



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
bkg_col      <- "#f5f5f2"  
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray30"  

viz_colors <- list(
    morning = "#83c5be",    
    evening = "#7B7FD4",     
    orange  = "#EE6100FF",   
    gray_light = "gray90",   
    gray_dark  = "gray30"    
)

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 49 } &bull; Source: WebTRIS Traffic Flow API<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

# text
title_text    <- str_glue("Traffic Flow Analysis: A64 Road, May 2021")
subtitle_text <-  str_glue("Analysis of traffic patterns across multiple dimensions")
caption_text  <- str_glue("{tt} {li} stevenponce &bull; {bs} sponce1 &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add("fa6-brands", "fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Source Sans Pro", family = "text")  
font_add_google("Roboto Mono", family = "numbers")   
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
showtext_auto(enable = TRUE)

### |-  plot theme ----
theme_traffic <- function() {
    theme_minimal(base_size = 14, base_family = "text") +
        theme(
            # Background
            plot.background  = element_rect(fill = bkg_col, color = bkg_col),
            panel.background = element_rect(fill = bkg_col, color = bkg_col),
            plot.margin      = margin(t = 10, r = 20, b = 10, l = 20),
            
            # Title, subtitle, caption
            plot.title = element_markdown(
                family = "title",
                size = rel(1.4),
                face = "bold",
                color = title_col,
                hjust = 0.5,
                margin = margin(t = 10, b = 5)
            ),
            plot.subtitle = element_markdown(
                family = "text",
                size = rel(1),
                color = subtitle_col,
                hjust = 0.5,
                margin = margin(b = 10)
            ),
            plot.caption = element_markdown(
                family = "caption",
                size = rel(0.7),
                color = caption_col
            ),
            
            # Axis formatting
            axis.title = element_text(
                family = "text",
                size = rel(0.93),
                face = "bold",
                color = text_col
            ),
            axis.text = element_text(
                family = "text",
                size = rel(0.79),
                color = text_col
            ),
            axis.line.x = element_line(
                color = "#252525",
                linewidth = 0.2
            ),
            
            # Grid lines
            panel.grid.major.y = element_line(
                color = "gray90",
                linewidth = 0.2
            ),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            
            # Legend
            legend.position = "top",
            legend.title = element_text(
                size = rel(0.8),
                face = "bold"
            ),
            legend.text = element_text(
                size = rel(0.71)
            ),
            legend.key.width = unit(1.5, "cm"),
            legend.spacing.x = unit(0.2, 'cm')
        )
}


### |-  plot 1 ----
p1 <- hourly_volume_clean |>
    ggplot(aes(x = report_date, 
               y = hourly_volume, 
               group = hour)) +
    # Add non-peak hours with gray color
    geom_line(
        data = hourly_volume_clean |> filter(!hour %in% c(7:9, 16:18)),
        color = viz_colors$gray_dark,
        alpha = 0.2,
        linewidth = 0.5
    ) +
    # Add peak hours with colors
    geom_line(
        data = hourly_volume_clean |> filter(hour %in% c(7:9, 16:18)),
        aes(color = hour_f),
        alpha = 0.9,
        linewidth = 0.7
    ) +
    scale_color_manual(
        values = c(
            "7" = viz_colors$orange,
            "8" = viz_colors$orange,
            "9" = viz_colors$orange,
            "16" = viz_colors$evening,
            "17" = viz_colors$evening,
            "18" = viz_colors$evening
        ),
        name = "Hour of Day"
    ) +
    scale_y_continuous(
        labels = scales::comma_format(),
        breaks = scales::breaks_pretty(n = 6),
        expand = c(0.02, 0.02)
    ) +
    scale_x_datetime(
        date_breaks = "1 week",
        date_labels = "%b %d",
        expand = c(0.02, 0.02)
    ) +
    labs(
        title = "Hourly Traffic Volume Throughout May",
        subtitle = "Highlighting rush hour periods (7-9 AM, 4-6 PM)",
        x = "Date",
        y = "Hourly Volume"
    ) +
    theme_traffic() +
    theme(
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.title = element_text(
            size = rel(0.8),
            face = "bold",
            margin = margin(b = 5)
        ),
        legend.box.spacing = unit(0.5, "cm")
    ) +
    guides(
        color = guide_legend(
            nrow = 2,
            byrow = TRUE
        )
    )

### |-  plot 2 ----
p2 <- daily_volume_clean |>
    ggplot(aes(x = date, y = daily_volume)) +
    # Add confidence band and line
    geom_smooth(
        method = "loess",
        span = 0.4,
        color = viz_colors$evening,        
        fill = alpha(viz_colors$evening, 0.25),
        linewidth = 1.0,
        se = TRUE
    ) +
    # Add values as points
    geom_point(
        color = viz_colors$orange,
        alpha = 0.6,
        size = 1.8
    ) +
    # Scales
    scale_y_continuous(
        labels = scales::comma_format(),
        breaks = seq(0, 50000, by = 10000),
        limits = c(0, 50000),
        expand = c(0, 0)  
    ) +
    scale_x_date(
        date_breaks = "1 week",
        date_labels = "%b %d",
        expand = c(0.02, 0.02)
    ) +
    # Labs
    labs(
        title = "Daily Traffic Volume Patterns Throughout May",
        subtitle = "Showing daily total volume with 95% confidence interval",
        x = "Date",
        y = "Daily Volume"
    ) +
    # Theme 
    theme_traffic() +
    theme(
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )

### |-  plot 3 ----
p3 <- ggplot(weekend_patterns_clean, aes(x = hour)) +
    # Add ribbons (weekday vs weekend)
    geom_ribbon(
        aes(
            ymin = pmin(weekday, weekend),
            ymax = weekend,
            fill = "Weekend"
        ),
        alpha = 0.15
    ) +
    geom_ribbon(
        aes(
            ymin = pmin(weekday, weekend),
            ymax = weekday,
            fill = "Weekday"
        ),
        alpha = 0.15
    ) +
    # Add lines 
    geom_line(
        aes(y = weekday, color = "Weekday"), 
        linewidth = 1.0
    ) +
    geom_line(
        aes(y = weekend, color = "Weekend"), 
        linewidth = 1.0
    ) +
    # Scales
    scale_x_continuous(
        breaks = seq(0, 23, by = 4),
        labels = function(x) sprintf("%02d:00", x),
        expand = c(0.02, 0.02)
    ) +
    # Add legend 
    scale_color_manual(
        name = "Day Type",
        values = c(
            "Weekday" = viz_colors$evening,
            "Weekend" = viz_colors$orange
        )
    ) +
    scale_fill_manual(
        name = "Day Type",
        values = c(
            "Weekday" = viz_colors$evening,
            "Weekend" = viz_colors$orange
        )
    ) +
    # Labs
    labs(
        title = "Traffic Patterns: Weekday vs Weekend",
        subtitle = "Comparing average speed and volume throughout the day",
        x = "Hour of Day",
        y = NULL
    ) +
    # Facet 
    facet_wrap(~metric, scales = "free_y", nrow = 1) +
    # Theme
    theme_traffic() +
    theme(
        strip.text = element_text(size = rel(1), face = "bold"),
        panel.grid.major.y = element_line(
            color = viz_colors$gray_light, 
            linewidth = 0.3
        ),
        legend.key.width = unit(2, "cm")
    ) 
    

### |-  plot 4 ----
p4 <- sensor_speed_clean |>
    ggplot(aes(x = date, y = avg_speed, color = site_id, group = site_id)) +
    # Add lines
    geom_line(linewidth = 1.0, alpha = 0.9) +

    # Scales 
    scale_y_continuous(
        breaks = seq(30, 50, by = 5),
        limits = c(25, 55),
        expand = c(0, 0)
    ) +
    scale_x_date(
        date_breaks = "1 week",
        date_labels = "%b %d",
        expand = c(0.02, 0)
    ) +
    scale_color_manual(
        values = c(
            "6867" = viz_colors$evening,    
            "7035" = viz_colors$orange,    
            "7042" = viz_colors$evening,   
            "7058" = viz_colors$orange  
        ),
        name = "Sensor ID"
    ) +
    # Customize legend
    guides(
        color = guide_legend(
            nrow = 1,
            byrow = TRUE
        )
    ) +
    # Labs
    labs(
        title = "Average Speed Trends by Sensor Location",
        subtitle = "Comparing speed patterns across different monitoring points",
        x = "Date",
        y = "Average Speed (mph)"
    ) +
    # Theme
    theme_traffic() +
    theme(
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        legend.key.width = unit(2, "cm"),
        legend.text = element_text(size = rel(0.9))
    ) 
   
### |-  combined plots ----
combined_plot <- (
    p1 + p2 + 
        plot_layout(widths = c(1, 1))
) / (
    p3 + p4
)

combined_plot <- combined_plot +
    plot_annotation(
        title = title_text,
        subtitle = subtitle_text  ,
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
            plot.margin = margin(10, 10, 10, 10),
            plot.background = element_rect(fill = bkg_col, color = bkg_col),
            panel.background = element_rect(fill = bkg_col, color = bkg_col)
        )
    ) &
    theme(
        legend.position = "top",
        legend.box = "vertical",
        legend.margin = margin(t = 10),
        legend.spacing = unit(1, "cm"),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.3, "cm")
    )

combined_plot 



# 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-11-29
# rstudio  2024.09.1+394 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────────
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
# P gghighlight * 0.4.1    2023-12-16 [?] CRAN (R 4.4.0)
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
# labeling      0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lattice       0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.12     2023-12-06 [?] CRAN (R 4.4.0)
# P Matrix        1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# P mgcv          1.9-1    2023-12-21 [?] CRAN (R 4.4.0)
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P nlme          3.1-164  2023-11-27 [?] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P paletteer   * 1.6.0    2024-01-21 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P patchwork   * 1.3.0    2024-09-16 [?] CRAN (R 4.4.1)
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P prismatic     1.1.2    2024-04-10 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P rematch2      2.1.2    2020-05-01 [?] CRAN (R 4.4.0)
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
# P splines       4.4.0    2024-04-24 [?] local
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
# viridisLite   0.4.2    2023-05-02 [1] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# withr         3.0.1    2024-07-31 [1] CRAN (R 4.4.1)
# P xfun          0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/windows/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/d6ee0ff8
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# > 
