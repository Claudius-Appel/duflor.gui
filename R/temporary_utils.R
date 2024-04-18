duflor.check <- function(files) {
    ## for each file, one-by-one perform the following steps:
    ## fulfills naming conventions (decide how strict this must be later)
    ##
    return(files)
}
duflor.display <- function(cimg) {
    imager::display(cimg)
}
duflor.load_rgb_colorspace <- function(file) {
    return(imager::load.image(file))
}
duflor.load_hsv_colorspace <- function(file) {
    image <- imager::load.image(file)
    hsv <- imager::RGBtoHSV(imager::sRGBtoRGB(image))
    return(hsv)
}
duflor.get_image_dimensions <- function(path) {
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
