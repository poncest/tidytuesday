
## Challenge: #TidyTuesday 2024 week 52
## Data:      Global Holidays and Travel
## Author:    Steven Ponce
## Date:      2024-12-09


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
    camcorder       # Record Your Plot History 
)

### |- figure size ----
gg_record(
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
# tt <- tidytuesdayR::tt_load(2024, week = 52)
#
# spells_raw  <- tt$spells |> clean_names()
#
# tidytuesdayR::readme(tt)
# rm(tt)

# Option 2: Read directly from GitHub
global_holidays_raw <- readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/global_holidays.csv') |> 
    clean_names()

monthly_passengers_raw <- readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/monthly_passengers.csv') |> 
    clean_names()



## 3. EXAMINING THE DATA ----
glimpse(global_holidays_raw)
skim(global_holidays_raw)

glimpse(monthly_passengers_raw)
skim(monthly_passengers_raw)



## 4. TIDYDATA ----

# Clean and join the datasets
monthly_passengers_clean <- monthly_passengers_raw |>
    mutate(
        date = ymd(paste(year, month, "01", sep = "-")),
        total_passengers = coalesce(total, total_os)     # Use total_os when total is NA
    )

monthly_holidays_clean <- global_holidays_raw |>
    mutate(
        year = year(date),
        month = month(date)
    ) |>
    group_by(iso3, year, month) |>
    summarise(
        holiday_count = n(),
        public_holidays = sum(type == "Public holiday"),
        .groups = "drop"
    )

combined_data <- monthly_passengers_clean |>
    left_join(monthly_holidays_clean, by = c("iso3", "year", "month"))

# Housekeeping
rm(global_holidays_raw, monthly_passengers_raw, monthly_holidays_clean, monthly_passengers_clean)


# data plot ---
volatility_df <- combined_data |>
    # Calculate summary statistics by country
    group_by(iso3) |>
    summarise(
        mean_traffic = mean(total_passengers, na.rm = TRUE),
        sd_traffic = sd(total_passengers, na.rm = TRUE),
        cv = sd_traffic / mean_traffic,
        avg_holidays = mean(holiday_count, na.rm = TRUE),
        total_observations = n(),
        traffic_size = sum(total_passengers, na.rm = TRUE),
        .groups = "drop"
    ) |>
    # Remove NA, infinite, or outlier values
    filter(
        complete.cases(cv, avg_holidays),
        total_observations >= 12,              # At least one year of data
        cv >= 0,                               # Ensure no negative coefficients of variation
        cv <= quantile(cv, 0.95, na.rm = TRUE) # Remove extreme outliers
    ) |>
    # Add size categories for visualization
    mutate(
        size_category = cut(
            traffic_size,
            breaks = quantile(traffic_size, probs = seq(0, 1, 0.25), na.rm = TRUE),
            labels = c("Small", "Medium", "Large", "Very Large"),
            include.lowest = TRUE
        )
    )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
bkg_col      <- "#f5f5f2"
title_col    <- "gray20"
subtitle_col <- "gray30"
text_col     <- "gray30"
caption_col  <- "gray40"
col_palette  <- c("#4B79B7", "#F8F9FA", "#2C3E50", "#34495E", "#7F8C8D")      

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 52 } &bull; Source: WorldPop Hub<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

# text
title_text    <- str_glue("More Holidays Associated with Lower Air Traffic Volatility\nin Larger Markets")
subtitle_text <- str_glue("Higher holiday frequency correlates with reduced traffic volatility, especially in larger markets<br>
                          Lower CV values indicate more stable traffic patterns<br><br>
                          **Coefficient of Variation in Traffic**")
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
    panel.grid.major.x    = element_blank(),
    panel.grid.major.y    = element_line(color = alpha(col_palette[5], 0.2), linewidth = 0.2),
    panel.grid.minor      = element_blank(),
    strip.text            = element_textbox(
        size              = rel(0.9),
        face              = 'bold',
        color             = col_palette[3],
        fill              = alpha(col_palette[1], 0.1),
        box.color         = alpha(col_palette[1], 0.5),
        halign            = 0.5,
        linetype          = 1,
        r                 = unit(3, "pt"),
        width             = unit(1, "npc"),
        padding           = margin(5, 10, 5, 10),
        margin            = margin(b = 10)
    ),
    panel.spacing.x       = unit(2, 'lines'),
    panel.spacing.y       = unit(1, 'lines'),
    legend.margin         = margin(-25, 5, 0, 0), # align the legend with the y-axis label
    legend.justification.top = "right",
    legend.position       = "top",
    legend.title          = element_text(size = rel(0.7)),
    legend.text           = element_text(size = rel(0.6)),
    
)

### |-  Plot  ----
ggplot(volatility_df, aes(x = avg_holidays, y = cv)) +
    # Add reference line for overall median
    geom_hline(
        yintercept = median(volatility_df$cv),
        linetype = "dashed",
        color = "gray50",
        alpha = 0.3
    ) +
    # Add points
    geom_point(
        aes(
            size = traffic_size,
            alpha = cv  # Vary transparency by CV
        ),
        color = col_palette[1]
    ) +
    # Add trend line
    geom_smooth(
        color = col_palette[3],
        method = "loess",
        linewidth = 1,
        se = TRUE
    ) +
    # Add labels for extreme points (cv)
    ggrepel::geom_text_repel(
        data = volatility_df |> 
            group_by(size_category) |>  
            filter(cv == max(cv) | cv == min(cv)),
        aes(label = iso3),
        size = 3,
        color = col_palette[4],
        max.overlaps = 2,
        box.padding = 0.5,
        segment.color = col_palette[5],
        segment.alpha = 0.5
    ) +
    # Add single annotation for the median line
    geom_text(
        data = volatility_df |> filter(size_category == "Small"),
        x = 9,  
        y = median(volatility_df$cv) + 0.02, 
        label = "Industry median volatility",
        size = 3,
        color = "gray50",
        hjust = 1,
        vjust = -0.5
    ) +
    # Add correlation annotation in each facet
    geom_text(
        data = volatility_df |>  
            group_by(size_category) |>
            summarise(
                cor = cor(avg_holidays, cv),
                .groups = "drop"
            ),
        aes(x = 8, y = 0.65, 
            label = sprintf("r = %.2f", cor)),
        size = 3,
        hjust = 1
    ) +
    
    # Scales
    scale_y_continuous(
        breaks = seq(0, 1, by = .25),
        limits = c(-.25, .75),
        labels = percent_format()
    ) +
    scale_x_continuous(
        breaks = seq(2, 8, by = 2),
        limits = c(1, 9),
        expand = expansion(mult = c(0.02, 0.08))          
    ) +
    scale_size_continuous(
        range = c(2, 8),
        labels = scales::label_number(scale = 1e-6, suffix = "M")
    ) +
    scale_alpha_continuous(
        range = c(0.4, 0.8),
        guide = "none"
    ) +
    # Labs
    labs(
        x = "Average Number of Holidays per Month",
        y = NULL,
        size  = "Annual Passenger Traffic (M)",
        color = "Market Size",
        title    = title_text,
        subtitle = subtitle_text,
        caption  = caption_text
    ) +
    # Facets
    facet_wrap(
        ~size_category,
        labeller = as_labeller(function(x) paste(x, "Market")),
        scales = "fixed"
    ) +
    # Theme
    theme(
        plot.title = element_text(
            size   = rel(2),
            family = "title",
            face   = "bold",
            color  = title_col,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.subtitle = element_markdown(
            size   = rel(1),
            family = "subtitle",
            color  = subtitle_col,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
        ),
        plot.caption = element_markdown(
            family = "caption",
            size   = rel(0.65),
            color  = caption_col,
            hjust  = 0.5,
            margin = margin(t = 10)
        )
    )  

