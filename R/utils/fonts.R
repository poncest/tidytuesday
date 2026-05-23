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
    
    # Add Google Fonts — Steven Ponce brand system (v1.0, May 2026)
    # Display / titles: Source Serif 4 — editorial, warm, authoritative
    # font_add_google("Source Serif 4", regular.wt = 400, family = "title")
    # 
    # # UI / chart labels / body: Source Sans 3 — clean, readable at small sizes
    # font_add_google("Source Sans 3", regular.wt = 400, family = "subtitle")
    # font_add_google("Source Sans 3", regular.wt = 400, family = "body")
    # font_add_google("Source Sans 3", regular.wt = 400, family = "text")
    
    # Display / titles: 
    font_add_google("Big Shoulders", regular.wt = 400, family = "title")
    
    # UI / chart labels / body: 
    font_add_google("DM Sans", regular.wt = 400, family = "subtitle")
    font_add_google("DM Sans", regular.wt = 400, family = "body")
    font_add_google("DM Sans", regular.wt = 400, family = "text")
    
    # Mono / captions / data labels: JetBrains Mono — tabular numerals, crisp
    font_add_google("JetBrains Mono", regular.wt = 400, family = "caption")
    
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
