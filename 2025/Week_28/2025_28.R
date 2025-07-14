## Challenge: #TidyTuesday 2025 week 28
## Data:      British Library Funding
## Author:    Steven Ponce
## Date:      2025-07-14


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,  # Easily Install and Load the 'Tidyverse'
    ggtext,     # Improved Text Rendering Support for 'ggplot2'
    showtext,   # Using Fonts More Easily in R Graphs
    janitor,    # Simple Tools for Examining and Cleaning Dirty Data
    scales,     # Scale Functions for Visualization
    glue,       # Interpreted String Literals
    patchwork   # The Composer of Plots
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 12,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 28)

bl_funding <- tt$bl_funding |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(bl_funding)


## 4. TIDYDATA ----

# Plot 1 Data ----
calculate_variability <- function(data, columns, type_name) {
  data |>
    select(year, all_of(columns)) |>
    pivot_longer(cols = -year, names_to = "source", values_to = "amount") |>
    group_by(source) |>
    summarise(
      mean_val = mean(amount, na.rm = TRUE),
      sd_val = sd(amount, na.rm = TRUE),
      cv = sd_val / mean_val,
      .groups = "drop"
    ) |>
    mutate(type = type_name) |>
    arrange(desc(cv))
}

# Calculate variability for both nominal and inflation-adjusted
nominal_cols <- c(
  "gia_gbp_millions", "voluntary_gbp_millions", "investment_gbp_millions",
  "services_gbp_millions", "other_gbp_millions"
)

adjusted_cols <- c(
  "gia_y2000_gbp_millions", "voluntary_y2000_gbp_millions",
  "investment_y2000_gbp_millions", "services_y2000_gbp_millions",
  "other_y2000_gbp_millions"
)

nominal_var <- calculate_variability(bl_funding, nominal_cols, "Nominal")
adjusted_var <- calculate_variability(bl_funding, adjusted_cols, "Inflation-Adjusted")

# Plot data
variability_comparison <- bind_rows(nominal_var, adjusted_var) |>
  mutate(
    source_clean = str_remove(source, "_gbp_millions|_y2000_gbp_millions") |>
      str_replace("gia", "grant-in-aid") |>
      str_replace_all("_", " ") |>
      str_to_sentence()
  ) |>
  group_by(source_clean) |>
  mutate(avg_cv = mean(cv)) |>
  ungroup() |>
  mutate(source_clean = fct_reorder(source_clean, avg_cv))

# Plot 2 Data ----
# Define key events
events <- tibble(
  year = c(2001, 2007, 2008, 2010, 2013, 2016, 2020),
  event = c(
    "9/11 Economic\nUncertainty", "Pre-Crisis\nPeak",
    "Financial\nCrisis", "Austerity\nBegins", "Digital Legal\nDeposit",
    "Brexit\nVote", "COVID-19\nPandemic"
  ),
  type = c("negative", "positive", "negative", "negative", "neutral", "negative", "negative"),
  label_offset = c(1.08, 1.02, 1.12, 0.98, 1.06, 1.01, 1.09)
)

max_funding_value <- bl_funding |>
  filter(!is.na(total_y2000_gbp_millions)) |>
  pull(total_y2000_gbp_millions) |>
  max()

events <- events |>
  mutate(y_pos = max_funding_value * label_offset)

# Plot 3 Data ----
gap_analysis <- bl_funding |>
  mutate(
    peak_total = max(total_y2000_gbp_millions, na.rm = TRUE),
    peak_gia = max(gia_y2000_gbp_millions, na.rm = TRUE),
    funding_gap = peak_total - total_y2000_gbp_millions,
    gia_gap = peak_gia - gia_y2000_gbp_millions,
    gap_percentage = funding_gap / peak_total * 100
  )

# Plot data
gap_data <- gap_analysis |>
  select(year, funding_gap, gia_gap) |>
  pivot_longer(-year, names_to = "gap_type", values_to = "gap_amount") |>
  mutate(gap_type = factor(case_when(
    gap_type == "funding_gap" ~ "Total Funding Gap",
    gap_type == "gia_gap" ~ "Grant-in-Aid Gap"
  ), levels = c("Grant-in-Aid Gap", "Total Funding Gap")))

# Key milestone annotations
key_years_data <- gap_analysis |>
  filter(year %in% c(2010, 2015, 2020, 2023)) |>
  select(year, funding_gap, gap_percentage)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    primary = 'gray20',
    secondary = "#8B0000",
    accent = "#DAA520",
    neutral = "#666666",
    background = '#FDFDFD',
    success = "#2d5a27",
    warning = "#d73027"
  )
)

### |- titles and caption ----
title_text <- str_glue("The British Library Funding Crisis: A Three-Part Analysis")

subtitle_text <- str_glue("From variable income streams through external shocks to cumulative impact")

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 28,
  source_text =  "BL Funding Over Time"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

# Start with base theme
base_theme <- create_base_theme(colors)

# Add weekly-specific theme elements
weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Axis elements
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    axis.title.x = element_text(color = colors$text, face = "bold", size = rel(0.8), margin = margin(t = 15)),
    axis.title.y = element_text(color = colors$text, face = "bold", size = rel(0.8), margin = margin(r = 10)),

    # Grid elements
    panel.grid.major.y = element_line(color = "gray50", linewidth = 0.05),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    # Legend elements
    legend.position = "plot",
    legend.title = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.8), face = "bold"),
    legend.text = element_text(family = fonts$tsubtitle, color = colors$text, size = rel(0.7)),

    # Plot margin
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
  )
)

# Set theme
theme_set(weekly_theme)

# Plot 1: Variability Analysis ----
p1_subtitle <- str_glue(
    "Coefficient of variation: <span style='color:{colors$palette[\"accent\"]}'>**Nominal values**</span> vs ",
    "<span style='color:{colors$palette[\"primary\"]}'>**inflation<br>adjusted values**</span><br>",
    "Higher values = less predictable funding"
)

p1 <- variability_comparison |>
  ggplot(aes(x = source_clean, y = cv, fill = type)) +
  # Geoms
  geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", cv)),
    position = position_dodge(width = 0.7),
    vjust = -0.3, hjust = -0.25, size = 3.2,
    color = colors$palette["neutral"], fontface = "bold"
  ) +
  # Scales
  scale_fill_manual(
    values = setNames(
      c(colors$palette["accent"], colors$palette["primary"]),
      unique(variability_comparison$type)
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  # Labs
  labs(
    title = "Funding Source Reliability",
    subtitle = p1_subtitle,
    x = NULL,
    y = "Coefficient of Variation"
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = 16, family = fonts$title,
      face = "bold", color = colors$title, lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = 11, family = fonts$subtitle,
      color = colors$palette["neutral"], lineheight = 1.1,
      margin = margin(t = 5, b = 10)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.25)
  )

# Plot 2: Event Timeline ----
p2 <- bl_funding |>
  ggplot(aes(x = year, y = total_y2000_gbp_millions)) +
  # Geoms
  geom_line(color = colors$palette["primary"], linewidth = 1.5, alpha = 0.9) +
  geom_point(color = colors$palette["primary"], size = 2.8, alpha = 0.8) +
  geom_vline(
    data = events,
    aes(xintercept = year, color = type),
    linetype = "dashed", alpha = 0.7, linewidth = 0.6
  ) +
  geom_text(
    data = events,
    aes(x = year, y = y_pos, label = event, color = type),
    angle = 90, hjust = 0, vjust = -0.2, size = 2.9, fontface = "bold",
    lineheight = 0.9
  ) +
  # Scales
    scale_color_manual(
        values = setNames(
            c(colors$palette["success"], colors$palette["warning"], colors$palette["neutral"]),
            c("positive", "negative", "neutral")
        )
    ) +
  scale_y_continuous(
    labels = label_dollar(prefix = "£", suffix = "M"),
    expand = expansion(mult = c(0.02, 0.28))
  ) +
  scale_x_continuous(
    breaks = seq(1998, 2023, 5),
    limits = c(1997, 2024)
  ) +
  # Labs
  labs(
    title = "External Shocks Shape Funding Trajectory",
    subtitle = "Major economic and political events coincide with funding changes",
    x = "Year",
    y = "Total Funding (2000 GBP)"
  ) +
  # Theme
  theme(
      plot.title = element_text(
          size = 16, family = fonts$title,
          face = "bold", color = colors$title, lineheight = 1.1,
          margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
          size = 11, family = fonts$subtitle,
          color = colors$palette["neutral"], lineheight = 1.1,
          margin = margin(t = 5, b = 10)
      ),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.25),
    panel.grid.major.x = element_blank()
  )

# Plot 3: Funding Gap ----
p3_subtitle <- str_glue(
  "<span style='color:{colors$palette[\"warning\"]}'>**Total funding gap**</span> and ",
  "<span style='color:{colors$palette[\"secondary\"]}'>**Grant-in-Aid gap**</span> from historical peak<br>",
  "Dotted line shows trend • Percentages show scale of loss"
)

p3 <- gap_data |>
  ggplot(aes(x = year, y = gap_amount, fill = gap_type)) +
  # Geoms
  geom_area(alpha = 0.75, position = "identity") +
  geom_smooth(
    data = gap_analysis, aes(x = year, y = funding_gap),
    inherit.aes = FALSE, method = "loess", se = FALSE,
    color = "darkred", linetype = "dotted", linewidth = 1.2, alpha = 0.8
  ) +
  geom_text(
    data = key_years_data,
    aes(x = year, y = funding_gap, label = paste0(round(gap_percentage, 0), "%")),
    inherit.aes = FALSE, vjust = -0.5, size = 3.3,
    color = colors$palette["secondary"], fontface = "bold"
  ) +
  # Scales
  scale_fill_manual(
        values = setNames(
            c(colors$palette["secondary"], colors$palette["warning"]),
            unique(gap_data$gap_type)
        )
    ) +
  scale_y_continuous(
    labels = label_dollar(prefix = "£", suffix = "M"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_x_continuous(
    breaks = seq(1998, 2023, 5),
    limits = c(1997, 2024)
  ) +
  # Labs
  labs(
    title = "The Cumulative Cost of Underfunding",
    subtitle = p3_subtitle,
    x = "Year",
    y = "Funding Gap from Peak (2000 GBP)"
  ) +
  # Theme
  theme(plot.title = element_text(
      size = 16, family = fonts$title,
      face = "bold", color = colors$title, lineheight = 1.1,
      margin = margin(t = 5, b = 5)
  ),
  plot.subtitle = element_markdown(
      size = 11, family = fonts$subtitle,
      color = colors$palette["neutral"], lineheight = 1.1,
      margin = margin(t = 5, b = 10)
  ),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.25),
    panel.grid.major.x = element_blank()
  )

# Plot 3: Funding Gap ----
combined_plot <- p2 / (p1 + p3) +
  plot_layout(heights = c(1.1, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = 24, face = "bold", color = colors$palette["primary"],
        hjust = 0, margin = margin(b = 8), family = fonts$title,
      ),
      plot.subtitle = element_text(
        size = 14, color = colors$palette["neutral"],
        hjust = 0, margin = margin(b = 10), family = fonts$subtitle
      ),
      plot.caption = element_markdown(
        size = 9, color = colors$palette["neutral"],
        hjust = 0.5, margin = margin(t = 15), family = fonts$caption,
      ),
      plot.background = element_rect(fill = colors$palette["background"], color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
  )

combined_plot


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-07-14
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
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
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gh             1.4.1    2024-03-28 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P gitcreds       0.1.2    2022-09-08 [?] CRAN (R 4.4.0)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.4.2)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here         * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P httr2          1.0.1    2024-04-01 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lattice        0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P Matrix         1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# P mgcv           1.9-1    2023-12-21 [?] CRAN (R 4.4.0)
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P nlme           3.1-164  2023-11-27 [?] CRAN (R 4.4.0)
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
# P rappdirs       0.3.3    2021-01-31 [?] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P renv           1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# rlang          1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P splines        4.4.0    2024-04-24 [?] local
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
# P tidytuesdayR   1.1.2    2024-09-09 [?] CRAN (R 4.4.2)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.4.3)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr          3.0.2    2024-10-28 [?] CRAN (R 4.4.2)
# P xfun           0.52     2025-04-02 [?] CRAN (R 4.4.3)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────
# > 