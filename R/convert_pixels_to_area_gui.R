
#' convert pixel counts to an area based on the known area of an identifier-dot.
#'
#' @param identifier_area declared size of the identifier area, in `[cm^2]`
#' @param pixel.counts list containing any of the following pixel-counts:
#'
#' - "bex_green_HSV"
#' - "bex_drought_HSV"
#' - "bex_complete_HSV"
#' - "bex_identifier_HSV"
#'
#' The pixel-count for `identifier` must be present, whereas the others are optional.
#' The calculated area is internally unit-less. However, as the default value for
#' `duflor.default_identifier_area` is `r getOption("duflor.default_identifier_area")` \[cm^2\], the areas computed by this
#' function is also in \[cm^2\].
#'
#' The assumed area of the identifier can be modified by excuting
#' `options(duflor.default_identifier_area = <value_in_square_centimeters>)` *prior* to calling this function
#'
#' @return list of computed areas for any key listed in `pixel.counts`.
#'   Additionally, two meta-values are returned as well:
#'   - the identifier-area used for calculating each area (whichever value is set in option `duflor.default_identifier_area`)
#'   - the area of a singe pixel
#'
#'   All values are in \[cm^2\]
#' @note
#' This is a restructured version of [duflor::convert_pixels_to_area], to be more suitable to the needs of this specific app.
#' @keywords internal
convert_pixels_to_area_gui <- function(pixel.counts, identifier_area)
{
    if (!any(grep("identifier",names(pixel.counts)))) {
        stop(simpleError("FATAL: Unknown identifier count. Is the identifier missing?"))
    }
    area_per_pixel = identifier_area/pixel.counts[[grep("identifier",names(pixel.counts))]]
    ret <- list(area_per_pixel = area_per_pixel)
    if (any(grep("identifier",names(pixel.counts)))) {
        idx <- grep("identifier",names(pixel.counts))
        ret[[names(pixel.counts)[idx]]] = identifier_area
    }
    for (each in c("green","drought","complete","root")) {
        if (any(grep(each,names(pixel.counts)))) {
            idx <- grep(each,names(pixel.counts))
            ret[[names(pixel.counts)[idx]]] <- area_per_pixel * pixel.counts[[idx]]
        }
    }
    return(ret)
}
