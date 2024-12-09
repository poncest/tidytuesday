
## Challenge: #TidyTuesday 2024
## Author:    Steven Ponce
## Date:      2024-12-09

## Goal:      Generate a bar plot summary using images of my #TidyTuesday contributions

# Section 1: Load Packages ----
pacman::p_load(
    tidyverse,      # Data manipulation and visualization
    ggtext,         # Text rendering in ggplot2
    showtext,       # Font handling
    camcorder,      # Plot recording
    scales,         # Scale functions
    ggtextures      # Image handling in ggplot2
)

# Section 2: Functions ----

# Get PNG files for a specific year
get_png_files <- function(year) {
    list.files(
        path = here::here(as.character(year)), 
        pattern = "\\.png$",
        full.names = TRUE, 
        recursive = TRUE
    )
}

# Create contribution theme
create_contribution_theme <- function(bkg_col, text_col, title_col, caption_col) {
    theme_minimal(base_size = 20, base_family = 'text') +
        theme(
            plot.title.position   = "plot",
            plot.caption.position = "plot",
            legend.position       = 'top',
            plot.background       = element_rect(fill = bkg_col, color = bkg_col),
            panel.background      = element_rect(fill = bkg_col, color = bkg_col),
            plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
            axis.line.x           = element_line(color = "black"),
            axis.ticks.x          = element_line(color = "black", unit(0.4,"cm")),
            axis.text             = element_text(
                family = 'text', 
                color = text_col, 
                size = 16
            ),
            axis.title = element_text(
                family = 'text', 
                color = text_col, 
                size = 20
            ),
            panel.grid = element_blank(),
            plot.title = element_text(
                family = 'title',
                color = title_col,
                face = "bold",
                size = 28,  
                margin = margin(t = 5, b = 5)
            ),
            plot.caption = element_markdown(
                family = 'caption',
                color = caption_col,
                lineheight = 0.6,
                size = 12,
                hjust = 0.5,
                halign = 0.5,
                margin = margin(t = 15, b = 5)
            )
        )
}

# Create plot
create_image_plot <- function(data, 
                              img_width = 1, 
                              img_height = 1,
                              colors = list(
                                  bkg = "#A1B0AB",
                                  title = "#0B132B",
                                  text = "#0B132B",
                                  caption = "#0B132B"
                              )) {
    
    # Create social media icons
    icons <- list(
        li = str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>"),
        gh = str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>"),
        bs = str_glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
    )
    
    # Create plot
    data |> 
        ggplot(aes(year, count, image = image_path)) +
        geom_isotype_col(
            img_width = grid::unit(img_width, "native"), 
            img_height = unit(img_height, "null"),
            ncol = NA, 
            nrow = 1, 
            hjust = 0, 
            vjust = 0.5, 
            fill = "#80808040"
        ) +
        scale_image_identity() +
        coord_flip() +
        labs(
            x = "",
            y = "Count",
            title = "#TidyTuesday Challenge Contributions",
            caption = str_glue(
                "{icons$li} stevenponce &bull; {icons$bs} sponce1 &bull; 
                {icons$gh} poncest &bull; #rstats #ggplot2"
            )
        ) +
        create_contribution_theme(
            bkg_col = colors$bkg,
            text_col = colors$text,
            title_col = colors$title,
            caption_col = colors$caption
        )
}

# Setup visualization
setup_visualization <- function() {
    # Set up fonts
    font_add('fa6-brands', here::here('fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf'))
    font_add_google("Rock Salt", family = "title")
    font_add_google("Lato", family = "text")
    font_add_google("PT Sans Narrow", family = "caption")
    showtext_auto(enable = TRUE)
    
    # Set up recording
    gg_record(
        dir = here::here("temp_plots"),
        device = "png",
        width = 10,
        height = 6,
        units = "in",
        dpi = 320
    )
    
    # Set resolution
    showtext_opts(dpi = 320)
}

# Section 3: Main Execution ----
main <- function() {
    # Setup
    setup_visualization()
    
    # Create data frame
    images_df <- tibble(
        year = rep(2022:2024, each = 1)
    ) |>
        mutate(
            image_path = map(year, get_png_files),
            year = as.factor(year)
        ) |>
        unnest(image_path) |>
        filter(file.exists(image_path)) |>
        arrange(year) |>
        mutate(count = 1)
    
    # Create and save plot
    images_df |>
        create_image_plot() |>
        ggsave(
            path = here::here("summary/"),
            filename = "image_plot.png",
            width = 10,
            height = 6,
            units = 'in',
            dpi = 320
        )
} 

# Run ----
main()


# Section 4: Session Info ---- 

# Save session info to file
session_info <- sessioninfo::session_info(include_base = TRUE) 
session_info

# if needed
# writeLines(capture.output(print(session_info)), "session_info.txt")

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
# date     2024-12-09
# rstudio  2024.09.1+394 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# P annotater     0.2.3    2024-01-26 [?] CRAN (R 4.4.0)
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P camcorder   * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi         1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext      * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# ggtextures  * 0.0.1    2024-05-02 [1] Github (clauswilke/ggtextures@7418e01)
# P gifski        1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# glue          1.8.0    2024-09-30 [1] CRAN (R 4.4.2)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# P gridtext      0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable        0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here          1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# labeling      0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# markdown      1.13     2024-06-04 [1] CRAN (R 4.4.2)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman        0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.3    2024-09-11 [?] CRAN (R 4.4.2)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P renv          1.0.7    2024-04-11 [?] CRAN (R 4.4.0)
# rlang         1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
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
# withr         3.0.1    2024-07-31 [1] CRAN (R 4.4.1)
# P xfun          0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────
# > 

