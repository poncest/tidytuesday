

# date_chr <- "2023-08-22"
# yr <- sub("-.*", "", date_chr)
# date_strip <- stringr::str_remove_all(date_chr, "-")


year_week_chr <- "2024_99"
yr_chr        <- str_extract(string = year_week_chr, pattern = "\\d{4}")
week_chr      <- str_remove(string = year_week_chr, pattern = "2024_")

# create new folder
new_folder <- file.path(yr_chr, glue::glue("Week_", week_chr))
if (!file.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
    message("Created new folder!")
} else {
    print("Folder already exists!")
}


# create new R file
folder_path <- "2024/Week_99/"

new_file <- file.path(folder_path, paste0(year_week_chr, ".R"))
if (!file.exists(new_file)) {
    cat("# Your R code goes here\n", file = new_file)
    message("Created '.R' file")
} else {
    print("File already exists!")
}


# create new README.md
new_readme <- file.path(folder_path, "README.md")
if (!file.exists(new_readme)) {
    file.create(new_readme)
    cat(glue::glue("![](", year_week_chr, ".png)"), file = new_readme)
    message("Created 'README.md' file")
} else {
    print("File already exists!")
}


# copy lines to .R file
r_txt <- readLines("utils/r-template.R")