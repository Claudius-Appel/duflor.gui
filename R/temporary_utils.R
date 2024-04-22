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
