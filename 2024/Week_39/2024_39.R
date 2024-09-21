
## Challenge: #TidyTuesday 2024 week 39
## Data:      International Mathematical Olympiad (IMO) Data
## Author:    Steven Ponce
## Date:      2024-09-21


## 1. LOAD PACKAGES & SETUP ----
pacman::p_load(
  tidyverse,         # Easily Install and Load the 'Tidyverse'
  ggtext,            # Improved Text Rendering Support for 'ggplot2'
  showtext,          # Using Fonts More Easily in R Graphs
  janitor,           # Simple Tools for Examining and Cleaning Dirty Data
  skimr,             # Compact and Flexible Summaries of Data
  scales,            # Scale Functions for Visualization
  lubridate,         # Make Dealing with Dates a Little Easier
  MetBrewer,         # Color Palettes Inspired by Works at the Metropolitan Museum of Art
  MoMAColors,        # Color Palettes Inspired by Artwork at the Museum of Modern Art in New York City
  glue,              # Interpreted String Literals
  patchwork,         # The Composer of Plots
  geomtextpath       # Curved Text in 'ggplot2' 
 )  

### |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  11,
  height =  10,
  units  = "in",
  dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <-tidytuesdayR::tt_load(2024, week = 39) 

country_results <- tt$country_results_df |> clean_names() |> glimpse()
individual_results <- tt$individual_results_df |> clean_names() |> glimpse()
timeline <- tt$timeline_df |> clean_names() |> glimpse()

tidytuesdayR::readme(tt)
rm(tt)



## 3. EXAMINING THE DATA ----
glimpse(country_results)
glimpse(individual_results)
glimpse(timeline)


 
## 4. TIDYDATA ----

# first plot data (pyramid style chart) ----

# Calculate the total number of male and female contestants per country
gender_by_country_summary <- timeline |>
  group_by(country) |>
  summarize(
    male = sum(male_contestant, na.rm = TRUE),
    female = sum(female_contestant, na.rm = TRUE),
    total_contestants = male + female
  ) |>
  ungroup() |>
  arrange(desc(total_contestants))

# Prepare the data for a pyramid chart
gender_by_country <- gender_by_country_summary |>
  pivot_longer(
    cols = c(male, female),
    names_to = "gender",
    values_to = "count"
  ) |>
  mutate(count = ifelse(gender == "female", -count, count)) # Negative for female

# Modify the country labels to shorten or reformat names
gender_by_country <- gender_by_country |>
  mutate(
    country = case_when(
      country == "United States of America" ~ "USA",
      country == "United Kingdom" ~ "UK",
      country == "People's Republic of China" ~ "China",
      country == "Union of Soviet Socialist Republics" ~ "USSR",
      country == "Republic of Korea" ~ "South Korea",
      country == "Russian Federation" ~ "Russia",
      country == "German Democratic Republic" ~ "East Germany",
      TRUE ~ country # Keep all other country names as they are
    )
  )


# second plot (line chart) ----

# Data prep: Normalize by total contestants and calculate the gap
gender_representation_normalized <- timeline |>
  filter(!is.na(female_contestant) & !is.na(male_contestant)) |>
  mutate(
    total_contestants = female_contestant + male_contestant,
    female_percentage = (female_contestant / total_contestants) * 100,
    male_percentage = (male_contestant / total_contestants) * 100
  ) |>
  select(year, female_percentage, male_percentage)

# Pivot longer
gender_representation_normalized_long <- gender_representation_normalized |>
  pivot_longer(
    cols = c(female_percentage, male_percentage),
    names_to = "gender", values_to = "percentage"
  ) |>
  mutate(gender = ifelse(gender == "female_percentage", "Female", "Male"))

# Split the data into two separate datasets for ribbon use
male_data <- gender_representation_normalized_long |> filter(gender == "Male")
female_data <- gender_representation_normalized_long |> filter(gender == "Female")



# 5. VISUALIZATION ----

### |- plot aesthetics ----
bkg_col      <- colorspace::lighten('#f7f5e9', 0.05)    
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <- MoMAColors::moma.colors(palette_name = 'Klein', type = "discrete")[c(1,2)]

### |-  titles and caption ----
# icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 39 } &bull; Source: IMO Team and Individual Results<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
male   <- str_glue("<span style='color:{ col_palette[2] }'>**male**</span>")
female <- str_glue("<span style='color:{ col_palette[1] }'>**female**</span>")

title_text    <- str_glue("Gender Representation in the International Mathematical Olympiad")
subtitle_text <- str_glue("__Left:__ Total number of  { male } and { female } contestants by country (_raw counts_).<br>
                          __Right:__ Proportion of total contestants who were { male } and { female }  each year (_% of total contestants per year_)")
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
    plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.1), 
                                         color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1.1), 
                                         color = text_col, family = "text", face = "bold", hjust = 0.5),
    axis.text             = element_text(size = rel(0.8), color = text_col, family = "text"),
)  


### |-  first plot ----  

# Pyramid style chart
p1 <- ggplot(gender_by_country, aes(x = reorder(country, total_contestants), y = count, fill = gender)) +
  geom_bar(stat = "identity", width = 0.75, alpha = 0.85) +

  # Geoms
  # Adding labels outside the bars
  geom_text(aes(label = comma(abs(count))),
    position = position_nudge(y = ifelse(gender_by_country$gender == "female", -50, 50)),
    size = 3.6, hjust = ifelse(gender_by_country$gender == "female", 1, 0), color = text_col
  ) +

  # Adding a single country label next to the bars
  geom_text(aes(y = -900, label = country), # Position countries next to the bars
    size = 3.6, hjust = 0.5, vjust = 0, color = text_col
  ) +

  # Scales
  scale_y_continuous(
    breaks = seq(-1000, 1000, by = 500),
    labels = scales::comma_format(),
    limits = c(-1200, 1600)
  ) +
  scale_fill_manual(values = col_palette) +
  coord_flip(clip = "off") +

  # labs
  labs(
    x = NULL,
    y = "Number of Contestants",
    fill = "Gender"
  ) +

  # Theme
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
  )


### |-  second plot ----  

# Create the plot, including the ribbon and the textlines
p2 <- ggplot() +

  # Geoms
  # Add ribbon to fill the area between male and female percentages
  geom_ribbon(aes(x = male_data$year, ymin = female_data$percentage, ymax = male_data$percentage),
    fill = "lightblue", alpha = 0.5
  ) +

  # Add the geom_textline for male and female percentages
  geom_textline(aes(x = year, y = percentage, color = gender, label = gender),
    data = gender_representation_normalized_long,
    linewidth = 1,
    family = "text",
    size = 5,
    fontface = "bold",
    hjust = 0.5, # move labels to the right
    offset = unit(0.3, "cm"), # move labels up
    text_smoothing = 30 # smooth text (more legible)
  ) +

  # Adding geom_point and geom_text for the start and end percentages for male and female
  geom_point(
    data = filter(gender_representation_normalized_long, year == min(year) | year == max(year)),
    aes(x = year, y = percentage, color = gender), size = 4
  ) +

  # Female
  geom_text(
    data = filter(
      gender_representation_normalized_long,
      (year == min(year) | year == max(year)) & gender == "Female"
    ),
    aes(
      x = year, y = percentage, label = scales::percent(percentage / 100, accuracy = 1),
      color = gender
    ), size = 5, nudge_x = -0.005, vjust = -1.3, fontface = "bold", family = "text"
  ) +

  # Male
  geom_text(
    data = filter(
      gender_representation_normalized_long,
      (year == min(year) | year == max(year)) & gender == "Male"
    ),
    aes(
      x = year, y = percentage, label = scales::percent(percentage / 100, accuracy = 1),
      color = gender
    ), size = 5, nudge_x = 0.005, vjust = 1.9, fontface = "bold", family = "text"
  ) +

  # Labs
  labs(
    x = "Year",
    y = "Percentage of Contestants",
    color = "Gender"
  ) +

  # Scales
  scale_x_continuous() +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_color_manual(values = col_palette) +
  coord_cartesian(clip = "off")


#### |-  combined plot ----
# Annotation and aspect ratio of p2
p2 <- p2 +
  annotate(
    "text",
    x = 1962,
    y = 50,
    label = "Even though the overall contestant count has increased,\n the gender gap has remained the same.",
    size = 4,
    fontface = "italic",
    family = "text",
    color = "gray40",
    hjust = 0
  ) +
  theme(aspect.ratio = 0.85)

# Combine plots
combined_plot <- (p1 | p2) +
  patchwork::plot_layout(
    ncol = 2,
    widths = c(1, 1.25), # Adjusting relative widths
    guides = "collect" # Collect legends
  ) +

  # Labs
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) &

  # Theme
  theme(
    plot.margin = margin(10, 20, 10, 20),
    plot.title = element_markdown(
      size = rel(1.7),
      family = "title",
      face = "bold",
      color = title_col,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.88),
      family = "subtitle",
      color = subtitle_col,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = "caption",
      color = caption_col,
      lineheight = 1.1,
      hjust = 0.5,
      halign = 1,
      margin = margin(t = 5, b = 5)
    )
  )

# Show the combined plot
combined_plot



# 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-09-20
# rstudio  2024.04.2+764 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────
# ! package          * version    date (UTC) lib source
# V base             * 4.4.1      2024-04-24 [2] local (on disk 4.4.0)
# P base64enc          0.1-3      2015-07-28 [?] CRAN (R 4.4.0)
# P bit                4.0.5      2022-11-15 [?] CRAN (R 4.4.0)
# P bit64              4.0.5      2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder          0.1.0      2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger         1.1.0      2016-07-27 [?] CRAN (R 4.4.0)
# cli                3.6.3      2024-06-21 [1] CRAN (R 4.4.1)
# colorspace         2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark         1.9.1      2024-01-30 [?] CRAN (R 4.4.0)
# P compiler           4.4.0      2024-04-24 [?] local
# cowplot            1.1.3      2024-01-22 [1] CRAN (R 4.4.0)
# P crayon             1.5.2      2022-09-29 [?] CRAN (R 4.4.0)
# P curl               5.2.1      2024-03-01 [?] CRAN (R 4.4.0)
# P datasets         * 4.4.0      2024-04-24 [?] local
# P digest             0.6.35     2024-03-11 [?] CRAN (R 4.4.0)
# dplyr            * 1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
# P fansi              1.0.6      2023-12-08 [?] CRAN (R 4.4.0)
# farver             2.1.2      2024-05-13 [1] CRAN (R 4.4.1)
# P fastmap            1.1.1      2023-02-24 [?] CRAN (R 4.4.0)
# forcats          * 1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
# P fs                 1.6.4      2024-04-25 [?] CRAN (R 4.4.0)
# generics           0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
# P geomtextpath     * 0.1.4      2024-06-13 [?] CRAN (R 4.4.1)
# ggplot2          * 3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
# ggstream           0.1.0      2021-05-06 [1] CRAN (R 4.4.0)
# P ggtext           * 0.1.2      2022-09-16 [?] CRAN (R 4.4.0)
# P gifski             1.12.0-2   2023-08-12 [?] CRAN (R 4.4.0)
# P glue             * 1.7.0      2024-01-09 [?] CRAN (R 4.4.0)
# P graphics         * 4.4.0      2024-04-24 [?] local
# P grDevices        * 4.4.0      2024-04-24 [?] local
# P grid               4.4.0      2024-04-24 [?] local
# P gridtext           0.1.5      2022-09-16 [?] CRAN (R 4.4.0)
# gtable             0.3.5      2024-04-22 [1] CRAN (R 4.4.0)
# P here               1.0.1      2020-12-13 [?] CRAN (R 4.4.0)
# P hms                1.1.3      2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools          0.5.8.1    2024-04-04 [?] CRAN (R 4.4.0)
# P httr               1.4.7      2023-08-15 [?] CRAN (R 4.4.0)
# P janitor          * 2.2.0      2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite           1.8.8      2023-12-04 [?] CRAN (R 4.4.0)
# P knitr              1.46       2024-04-06 [?] CRAN (R 4.4.0)
# labeling           0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle          1.0.4      2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate        * 1.9.3      2023-09-27 [?] CRAN (R 4.4.0)
# magick             2.8.3      2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr           2.0.3      2022-03-30 [?] CRAN (R 4.4.0)
# P markdown           1.12       2023-12-06 [?] CRAN (R 4.4.0)
# P MetBrewer        * 0.2.0      2022-03-21 [?] CRAN (R 4.4.0)
# P methods          * 4.4.0      2024-04-24 [?] local
# MoMAColors       * 0.0.0.9000 2024-05-02 [1] Github (BlakeRMills/MoMAColors@6f5d75d)
# munsell            0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
# P NatParksPalettes * 0.2.0      2022-10-09 [?] CRAN (R 4.4.1)
# P pacman             0.5.1      2019-03-11 [?] CRAN (R 4.4.0)
# P parallel           4.4.0      2024-04-24 [?] local
# P patchwork        * 1.2.0      2024-01-08 [?] CRAN (R 4.4.0)
# P pillar             1.9.0      2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig          2.0.3      2019-09-22 [?] CRAN (R 4.4.0)
# P purrr            * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
# P R6                 2.5.1      2021-08-19 [?] CRAN (R 4.4.0)
# P ragg               1.3.0      2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp               1.0.12     2024-01-09 [?] CRAN (R 4.4.0)
# P readr            * 2.1.5      2024-01-10 [?] CRAN (R 4.4.0)
# P readxl             1.4.3      2023-07-06 [?] CRAN (R 4.4.0)
# P renv               1.0.7      2024-04-11 [?] CRAN (R 4.4.0)
# P repr               1.1.7      2024-03-22 [?] CRAN (R 4.4.0)
# rlang              1.1.4      2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot          2.0.4      2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi         0.16.0     2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg               2.6.0      2023-10-08 [?] CRAN (R 4.4.0)
# P rvest              1.0.4      2024-02-12 [?] CRAN (R 4.4.0)
# scales           * 1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
# P selectr            0.4-2      2019-11-20 [?] CRAN (R 4.4.0)
# P sessioninfo        1.2.2      2021-12-06 [?] CRAN (R 4.4.0)
# P showtext         * 0.9-7      2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb       * 3.0        2020-06-04 [?] CRAN (R 4.4.0)
# P skimr            * 2.1.5      2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase          0.11.1     2023-08-27 [?] CRAN (R 4.4.0)
# P stats            * 4.4.0      2024-04-24 [?] local
# stringi            1.8.4      2024-05-06 [1] CRAN (R 4.4.0)
# P stringr          * 1.5.1      2023-11-14 [?] CRAN (R 4.4.0)
# P svglite            2.1.3      2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts         * 0.8.9      2024-03-02 [?] CRAN (R 4.4.0)
# systemfonts        1.1.0      2024-05-15 [1] CRAN (R 4.4.0)
# textshaping        0.4.0      2024-05-24 [1] CRAN (R 4.4.0)
# P tibble           * 3.2.1      2023-03-20 [?] CRAN (R 4.4.0)
# tidyr            * 1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect         1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
# P tidytuesdayR       1.0.3      2023-12-13 [?] CRAN (R 4.4.0)
# P tidyverse        * 2.0.0      2023-02-22 [?] CRAN (R 4.4.0)
# P timechange         0.3.0      2024-01-18 [?] CRAN (R 4.4.0)
# P tools              4.4.0      2024-04-24 [?] local
# P tzdb               0.4.0      2023-05-12 [?] CRAN (R 4.4.0)
# P usethis            2.2.3      2024-02-19 [?] CRAN (R 4.4.0)
# P utf8               1.2.4      2023-10-22 [?] CRAN (R 4.4.0)
# P utils            * 4.4.0      2024-04-24 [?] local
# P vctrs              0.6.5      2023-12-01 [?] CRAN (R 4.4.0)
# P vroom              1.6.5      2023-12-05 [?] CRAN (R 4.4.0)
# withr              3.0.1      2024-07-31 [1] CRAN (R 4.4.1)
# P xfun               0.43       2024-03-25 [?] CRAN (R 4.4.0)
# P xml2               1.3.6      2023-12-04 [?] CRAN (R 4.4.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/tidytuesday/renv/library/windows/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/d6ee0ff8
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# >


