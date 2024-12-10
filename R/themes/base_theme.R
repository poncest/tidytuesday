## R/themes/base_theme.R

#' Get default theme colors
#' @param palette Optional custom color palette
#' @return List of theme colors
get_theme_colors <- function(palette = NULL) {
    list(
        background = "#f5f5f2",
        title      = "gray20",
        subtitle   = "gray30",
        text       = "gray30",
        caption    = "gray40",
        palette    = palette  # This can be overridden weekly
    )
}

#' Create base theme with consistent elements
#' @param colors List of colors from get_theme_colors()
#' @return ggplot theme object
create_base_theme <- function(colors = get_theme_colors()) {
    # Only include the truly fixed elements that don't change weekly
    theme_minimal(base_size = 14, base_family = fonts$text) +
        theme(
            plot.title.position   = "plot",
            plot.caption.position = "plot",
            legend.position       = "plot",
            
            # Background elements
            plot.background  = element_rect(fill = colors$background, color = colors$background),
            panel.background = element_rect(fill = colors$background, color = colors$background),
            
            # Common margins
            plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
            
            # Text elements that stay consistent
            axis.text  = element_text(
                family = fonts$text,
                size   = rel(0.79),
                color  = colors$text
            ),
            axis.title = element_text(
                family = fonts$text,
                size   = rel(0.93),
                face   = "bold",
                color  = colors$text
            )
            
            # Note: Weekly-specific theme elements should be added in the weekly scripts
        )
}

#' Extend base theme with weekly customizations
#' @param base_theme Existing theme to extend
#' @param weekly_theme_elements List of theme elements specific to this week
#' @return Modified ggplot theme
extend_weekly_theme <- function(base_theme, weekly_theme_elements) {
    base_theme + weekly_theme_elements
}
