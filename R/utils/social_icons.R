## social_icons.R

# Social media icons configuration
get_social_icons <- function() {
    list(
        linkedin = str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>"),
        github   = str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>"),
        bluesky  = str_glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
    )
}

# Create social media caption
create_social_caption <- function(tt_year, tt_week, source_text) {
    icons <- get_social_icons()
    
    tt_text <- str_glue("#TidyTuesday: {tt_year} Week {tt_week} &bull; Source: {source_text}<br>")
    social_text <- str_glue("{icons$linkedin} stevenponce &bull; {icons$bluesky} sponce1 &bull; {icons$github} poncest &bull; #rstats #ggplot2")
    
    str_glue("{tt_text} {social_text}")
}