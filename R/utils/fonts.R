## fonts.R

library(showtext) # Using Fonts More Easily in R Graphs
library(here)     # A Simpler Way to Find Your Files

# Font configuration function
setup_fonts <- function() {
    # Add Font Awesome
    font_add(
        "fa6-brands", 
        here::here("fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf")
    )
    
    # Add Google Fonts
    font_add_google("Oswald", regular.wt = 400, family = "title")
    # font_add_google("Merriweather Sans", regular.wt = 400, family = "subtitle")
    # font_add_google("Merriweather Sans", regular.wt = 400, family = "text")
    font_add_google("Inter", regular.wt = 400, family = "subtitle") 
    font_add_google("Inter", regular.wt = 400, family = "body") 
    font_add_google("Inter", regular.wt = 400, family = "text")
    font_add_google("Noto Sans", regular.wt = 400, family = "caption")
    
    # Enable showtext
    showtext_auto(enable = TRUE)
    showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)
}

# Font families accessory
get_font_families <- function() {
    list(
        title = "title",
        subtitle = "subtitle",
        text = "text",
        caption = "caption"
    )
}
