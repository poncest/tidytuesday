## TidyTuesday Contribution Heatmap 
## Consolidated Test Script

# 1. LOAD PACKAGES ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, ggtext, showtext, glue, here)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  = 10,
    height = 4,
    units  = "in",
    dpi    = 320
)

# 2. DATA EXTRACTION ----
get_contributions <- function(base_path, use_mock = FALSE) {
    if (use_mock) {
        # Generates random data for testing purposes   
        return(tibble(
            year = rep(2022:2025, each = 40),
            week = sample(1:52, 160, replace = TRUE)
        ) |> distinct())
    }
    
    year_folders <- list.dirs(base_path, recursive = FALSE, full.names = FALSE)
    years <- year_folders[str_detect(year_folders, "^\\d{4}$")] |> as.integer()
    
    map_dfr(years, \(yr) {
        week_folders <- list.dirs(file.path(base_path, yr), recursive = FALSE, full.names = FALSE)
        week_folders <- week_folders[str_detect(week_folders, "^Week_\\d+$")]
        if (length(week_folders) == 0) return(NULL)
        tibble(year = yr, week = as.integer(str_extract(week_folders, "\\d+")))
    }) 
}

# 3. BUILD CALENDAR GRID ----
build_calendar_grid <- function(contributions) {
    current_date <- Sys.Date()
    
    # Create a full sequence of weeks based on actual dates
    full_grid <- tibble(
        date = seq(as.Date("2022-01-01"), as.Date("2025-12-31"), by = "week")
    ) |>
        mutate(
            year = year(date),
            week = isoweek(date)
        ) |>
        filter(week <= 52) |>
        left_join(
            contributions |> mutate(contributed = TRUE),
            by = c("year", "week")
        ) |>
        mutate(
            contributed = replace_na(contributed, FALSE),
            # Grey out future weeks
            contributed = if_else(date > current_date, NA, contributed)
        )
    
    return(full_grid)
}

# 4. VISUALIZATION ----
create_heatmap <- function(calendar_data) {
    
    # Fonts
    font_add_google("Oswald", "title")
    font_add_google("Inter", "text")
    showtext_auto()
    
    # Colors
    col_contributed <- "#7209b7" 
    col_empty       <- "#1b2129" 
    col_future      <- "#0d1117" 
    col_text        <- "#7d8590"
    col_title       <- "#e6edf3"
    bkg_col         <- "#0d1117"
    
    stats <- calendar_data |> 
        summarise(
            total = sum(contributed, na.rm = TRUE),
            years = n_distinct(year)
        )
    
    year_totals <- calendar_data |>
        group_by(year) |>
        summarize(n = sum(contributed, na.rm = TRUE))
    
    p <- ggplot(calendar_data, aes(x = week, y = factor(year, levels = rev(sort(unique(year)))))) +
        # Use geom_tile with fixed borders
        geom_tile(aes(fill = contributed), color = bkg_col, linewidth = 0.8) +
        # Year labels on the right
        geom_text(data = year_totals, aes(x = 54, label = n), 
                  color = col_text, family = "text", size = 6, hjust = 0) +
        # Formatting
        scale_fill_manual(
            values = c("TRUE" = col_contributed, "FALSE" = col_empty),
            na.value = col_future, guide = "none"
        ) +
        scale_x_continuous(
            breaks = c(1, 13, 26, 39, 52),
            labels = c("Jan", "Apr", "Jul", "Oct", "Dec"),
            expand = expansion(mult = c(0, 0.1))
        ) +
        coord_fixed(ratio = 1) + 
        labs(
            title = "#TidyTuesday Contributions",
            subtitle = glue("{stats$total} visualizations across {stats$years} years"),
            x = NULL, y = NULL
        ) +
        theme_minimal() +
        theme(
            plot.background = element_rect(fill = bkg_col, color = NA),
            panel.grid = element_blank(),
            axis.text.y = element_text(color = col_text, face = "bold", size = 20),
            axis.text.x = element_text(color = col_text, size = 20),
            plot.title = element_text(family = "title", color = col_title, size = 50),
            plot.subtitle = element_text(family = "text", color = col_text, size = 25),
            plot.margin = margin(5, 5, 5, 5)
        )
    
    return(p)
}

# 5. EXECUTION ----

# Set use_mock = FALSE when running in your actual repo
df_contributions <- get_contributions(here(), use_mock = FALSE) 
df_grid          <- build_calendar_grid(df_contributions)
viz              <- create_heatmap(df_grid)

# Show plot
viz

# Save
ggsave(
    filename = here::here("summary/tidytuesday_heatmap.png"),
    plot = viz,
    width = 8,
    height = 2,
    dpi = 320,
    bg = "#0d1117"
)
