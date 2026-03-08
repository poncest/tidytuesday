# snap.R
# Reliable plot recording helper for camcorder
# Works with ggplot2, patchwork, and fallback for other plot types
#
# Usage:
#   source(here::here("R/utils/snap.R"))
#   snap(my_plot)        # records a ggplot or patchwork object
#   snap(my_plot + p2)   # works with patchwork compositions
#
# Requires: camcorder (must call gg_record() first to initialize)

snap <- function(x) {
    if (!requireNamespace("camcorder", quietly = TRUE)) {
        stop("camcorder package is required. Install with: install.packages('camcorder')")
    }
    
    if (inherits(x, "patchwork")) {
        camcorder:::record_patchwork(x)
    } else if (inherits(x, "ggplot")) {
        camcorder:::record_ggplot(x)
    } else {
        print(x)
        camcorder::record_polaroid()
    }
    
    invisible(x)
}