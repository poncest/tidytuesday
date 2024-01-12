
#> Code modified from "Creating template files with R"
#> Nicola Rennie. August 22, 2023.
#> nrennie.rbind.io/blog/script-templates-r


#' Create a new folder and add tidytuesday R template and README.md files
#'
#' @param year_week_chr enter year_week as character
#' @param readme do you wanto to add a README.md file?
#'
#' @return tidytuesday R template and README.md
#' @export
#'
#' @examples
#' source(file = "utils/create_folder_and_add_tt_template.R")
#' create_folder_and_add_tt_template(year_week_chr = "2024_02", readme = TRUE)
create_folder_and_add_tt_template <- function(year_week_chr,
                                              readme = TRUE) {
    # get year and week from `year_week_chr`
    #year_week_chr <- year_week_chr
    yr_chr        <- stringr::str_extract(string = year_week_chr, pattern = "\\d{4}")
    week_chr      <- stringr::str_remove(string = year_week_chr, pattern = "2024_")
    
    # create new folder
    new_folder <- file.path(yr_chr, glue::glue("Week_", week_chr))
    if (!file.exists(new_folder)) {
        dir.create(new_folder, recursive = TRUE)
        message("Created new folder!")
    } else {
        print("Folder already exists!")
    }
    
    # specify folder path
    folder_path <- file.path(yr_chr, glue::glue("Week_", week_chr))
    
    # make new R file
    new_file <- file.path(folder_path, paste0(year_week_chr, ".R"))
    if (!file.exists(new_file)) {
        cat("# Your R code goes here\n", file = new_file)
        message("Created '.R' file")
    } else {
        print("File already exists!")
    }
    
    # copy lines to .R file
    r_txt <- readLines("utils/r-template.R")
    
    # replace placeholder text with variables
    r_txt <- gsub(
        pattern = "yr_chr",
        replacement = paste0("\"", yr_chr, "\""),
        x = r_txt
    )
    r_txt <- gsub(
        pattern = "week_chr",
        replacement = paste0("\"", week_chr, "\""),
        x = r_txt
    )
    
    # write to new file
    writeLines(r_txt, con = new_file)
    message("'.R' contents copied")
    
    # create new README.md
    if (readme) {
        # make new README file
        new_readme <- file.path(folder_path, "README.md")
        if (!file.exists(new_readme)) {
            file.create(new_readme)
            cat(glue::glue("![](", year_week_chr, ".png)"), file = new_readme)
            message("Created 'README.md' file")
        } else {
            print("File already exists!")
        }
    }
    
    message("Template successfully copied!")
}
