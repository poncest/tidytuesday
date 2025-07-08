## Challenge: #TidyTuesday 2025 week 27
## Data:      The xkcd Color Survey Results
## Author:    Steven Ponce
## Date:      2025-07-08


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,  # Easily Install and Load the 'Tidyverse'
    ggtext,     # Improved Text Rendering Support for 'ggplot2'
    showtext,   # Using Fonts More Easily in R Graphs
    janitor,    # Simple Tools for Examining and Cleaning Dirty Data
    scales,     # Scale Functions for Visualization
    glue,       # Interpreted String Literals
    patchwork,  # The Composer of Plots
    effsize     # Efficient Effect Size Computation 
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2025, week = 27)

answers <- tt$answers |> clean_names()
users <- tt$users |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(answers)
glimpse(users)


## 4. TIDYDATA ----

# Data preparation
user_accuracy <- answers |>
  left_join(users, by = "user_id") |>
  filter(!is.na(y_chromosome), !is.na(colorblind)) |>
  group_by(user_id, y_chromosome, colorblind) |>
  summarise(
    avg_user_rank = mean(rank, na.rm = TRUE),
    accuracy_score = 1 / avg_user_rank,
    total_answers = n(),
    .groups = "drop"
  ) |>
  filter(total_answers >= 5)

# Plot 1 data
accuracy_by_groups <- user_accuracy |>
  mutate(
    chromosome_label = ifelse(y_chromosome == 1, "Males", "Females"),
    colorblind_label = ifelse(colorblind == 1, "Colorblind", "Normal Vision"),
    user_group = case_when(
      y_chromosome == 1 & colorblind == 0 ~ "Males\nNormal Vision",
      y_chromosome == 1 & colorblind == 1 ~ "Males\nColorblind",
      y_chromosome == 0 & colorblind == 0 ~ "Females\nNormal Vision",
      y_chromosome == 0 & colorblind == 1 ~ "Females\nColorblind"
    )
  ) |>
  filter(!is.na(user_group))

# Plot 2 data
group_stats <- accuracy_by_groups |>
  group_by(user_group) |>
  summarise(
    n = n(),
    mean_acc = mean(accuracy_score),
    median_acc = median(accuracy_score),
    se_acc = sd(accuracy_score) / sqrt(n()),
    ci_lower = mean_acc - 1.96 * se_acc,
    ci_upper = mean_acc + 1.96 * se_acc,
    .groups = "drop"
  ) |>
  mutate(
    gender = ifelse(str_detect(user_group, "Males"), "Males", "Females"),
    vision = ifelse(str_detect(user_group, "Colorblind"), "Colorblind", "Normal Vision")
  )

# key statistics

# overall mean for reference line
overall_mean <- mean(accuracy_by_groups$accuracy_score)

# t-test
gender_ttest <- t.test(accuracy_score ~ chromosome_label, data = accuracy_by_groups)

# Percentage difference
summary_stats <- accuracy_by_groups %>%
  group_by(chromosome_label, colorblind_label) %>%
  summarise(
    n = n(),
    mean_acc = mean(accuracy_score),
    .groups = "drop"
  )

male_mean <- summary_stats$mean_acc[summary_stats$chromosome_label == "Males" &
  summary_stats$colorblind_label == "Normal Vision"]
female_mean <- summary_stats$mean_acc[summary_stats$chromosome_label == "Females" &
  summary_stats$colorblind_label == "Normal Vision"]

percentage_diff <- ((male_mean - female_mean) / female_mean) * 100

# Effect size
gender_cohens_d <- cohen.d(
  accuracy_by_groups$accuracy_score,
  accuracy_by_groups$chromosome_label
)
cohens_d_value <- abs(gender_cohens_d$estimate)

# Descriptive variables
gender_sig <- ifelse(gender_ttest$p.value < 0.001, "p < 0.001",
                     ifelse(gender_ttest$p.value < 0.01, "p < 0.01",
                            ifelse(gender_ttest$p.value < 0.05, "p < 0.05", 
                                   paste("p =", round(gender_ttest$p.value, 3)))))

gender_effect_desc <- ifelse(cohens_d_value < 0.2, "very small",
                             ifelse(cohens_d_value < 0.5, "small", 
                                    ifelse(cohens_d_value < 0.8, "medium", "large")))



## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c("#DDA853", "#27548A")
)

### |- titles and caption ----
title_text <- str_glue("Gender Differences in Color Ranking Accuracy")

subtitle_text <- str_glue(
    "Analysis of ", scales::comma(nrow(accuracy_by_groups)), " users from the xkcd Color Survey.<br><br>",        
    "**Key Findings:**<br>", 
    "Males show ", round(percentage_diff, 1), "% higher accuracy. ",
    "Gender effect size is ", gender_effect_desc, " (", gender_sig, "). ",
    "Colorblindness has minimal impact."
    )

caption_text <- create_social_caption(
  tt_year = 2025,
  tt_week = 27,
  source_text =  "xkcd Color Survey SQLite database"
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

# Plot 1: Distribution plot
p1 <- ggplot(accuracy_by_groups, aes(x = accuracy_score, fill = chromosome_label)) +
  # Geoms
  geom_density(alpha = 0.65, size = 0.8) +
  # Annotations
  geom_richtext(
    data = tibble(
      accuracy_score = c(0.55, 0.6),
      y = c(6, 6),
      chromosome_label = c("Females", "Males"),
      label = c("More variable", "Slightly more accurate")
    ), aes(x = accuracy_score, y = y, label = label),
    inherit.aes = FALSE,
    size = 3.5, fontface = "italic",
    color = colors$palette
  ) +
  # Scales
  scale_fill_manual(
    values = colors$palette,
    name = "Group"
  ) +
  # Labs
  labs(
    title = "Distribution of Color Accuracy",
    subtitle = "Males show slight rightward shift (higher accuracy)",
    x = "Accuracy Score",
    y = "Density"
  ) +
  # Facets
  facet_wrap(~chromosome_label) +
  # Theme
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(
      size = rel(1.5),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      hjust = 0.5,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(0.9),
      family = fonts$subtitle,
      color = colors$subtitle,
      hjust = 0.5,
      lineheight = 1.2,
      margin = margin(t = 5, b = 20)
    ),
  )

# Plot 2: dot-range plot
p2 <- ggplot(group_stats, aes(x = mean_acc, y = reorder(user_group, mean_acc))) +
  # Geoms
  geom_segment(
    aes(
      x = ci_lower, xend = ci_upper,
      y = user_group, yend = user_group,
      color = gender
    ),
    size = 1.5, alpha = 0.8
  ) +
  geom_point(aes(color = gender), size = 4, alpha = 0.9) +
  geom_text(aes(label = paste0("n=", scales::comma(n))),
    hjust = 0.5, vjust = 2.5, size = 3, color = "gray30"
  ) +
  geom_text(aes(label = round(mean_acc, 3)),
    hjust = 0.5, vjust = -2, size = 3.5, fontface = "bold"
  ) +
  geom_vline(
    xintercept = overall_mean,
    linetype = "dashed", color = "gray50", alpha = 0.7
  ) +
  # Annotate
  annotate("text",
    x = overall_mean + 0.004, y = 0.5,
    label = paste0("Overall Mean\n(", round(overall_mean, 3), ")"),
    hjust = 0.5, vjust = 0, size = 3,
    color = "gray50", fontface = "italic"
  ) +
  # Scales
  scale_color_manual(
    values = colors$palette,
    name = "Gender"
  ) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.001),
    limits = c(
      min(group_stats$ci_lower) * 0.95,
      max(group_stats$ci_upper) * 1.05
    )
  ) +
  # Labs
  labs(
    title = "Mean Color Accuracy with 95% Confidence Intervals",
    subtitle = "Colorblindness has surprisingly minimal impact on ranking performance",
    x = "Mean Accuracy Score",
    y = NULL
  ) +
  # Theme
  theme(
    legend.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(
      size = rel(1.5),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      hjust = 0.5,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(0.9),
      family = fonts$subtitle,
      color = colors$subtitle,
      hjust = 0.5,
      lineheight = 1.2,
      margin = margin(t = 5, b = 5)
    ),
  )

# Combine plots 
combined_plot <- p1 / p2 +
  plot_layout(heights = c(0.8, 1.2)) 
  
combined_plot +  
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(1.75),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.9),
        family = fonts$subtitle,
        color = colors$subtitle,
        lineheight = 1.2,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size = rel(0.6),
        family = fonts$caption,
        color = colors$caption,
        hjust = 0.5,
        margin = margin(t = 10)
      )
    )
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-07-08
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────
# ! package      * version  date (UTC) lib source
# P annotater      0.2.3    2024-01-26 [?] CRAN (R 4.4.0)
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
# P effsize      * 0.8.1    2020-10-05 [?] CRAN (R 4.4.3)
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
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown       1.13     2024-06-04 [1] CRAN (R 4.4.2)
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
# ───────────────────────────────────────────────
# > 
