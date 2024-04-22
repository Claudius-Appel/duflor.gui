#' retrieves image dimension via call to system interface
#'
#' @param path path to file to measure
#'
#' @return `list(width=dbl,height=dbl)`
#' @export
#'
get_image_dimensions <- function(path) {
    # get image dimensions.
    if(!file.exists(path))
        stop("No file found", call. = FALSE)

    # Ensure file ends with .png or .jpg or jpeg
    if (!grepl("\\.(png|jpg|jpeg)$", x = path, ignore.case = TRUE))
        stop("File must end with .png, .jpg, or .jpeg", call. = FALSE)

    # Get return of file system command
    s <- system(paste0("file ", str_c("'", path, "'")), intern = TRUE)
    # Extract width and height from string
    return(
        list(
            width = regmatches(s, gregexpr("(?<=, )[0-9]+(?=(x| x )[0-9]+,)", s, perl = TRUE))[[1]],
            height = regmatches(s, gregexpr(", [0-9]+(x| x )\\K[0-9]+(?=,)", s, perl = TRUE))[[1]]
        )
    )
}
