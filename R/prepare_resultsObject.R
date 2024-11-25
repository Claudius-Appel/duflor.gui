#' prepare the dataframe-object into which all results are to be merged.
#'
#' results requires fields
#' - full_path
#' - image_date
#' - image_name
#' - processed_width
#' - processed_height
#' - crop_x0/1\*
#' - crop_y0/1\*
#' - identifiercrop_x0/1\*
#' - identifiercrop_y0/1\*
#' - for each in  `names(DATA$spectrums$lower_bound)`:
#'     - %spectrum%_count
#'     - img.fraction (maybe not that valuable?)
#'     - %spectrum%_area (as results of duflor::convert_pixels_to_area())
#'     - %spectrum%_fraction (as defined as `%spectrum%_count/(sum(<all_plant_spectrums>_count)`)
#'     - if the distortion-correction is enabled, the de-distorted instances of
#'       these values are recorded as well. Otherwhise, these will be set to `NA`.
#'
#' @note
#' The results-fields `crop_x0`, `crop_x1`,... encode the x- and y-coordinates
#' of the selected cropping (for both image-crops and identifier-crops.)
#' Due to how [imager::grabRect()] functions, the **origin** of the coordinate-system
#' is the **top-left corner of the image**.
#'
#' @inheritParams .main_args
#'
#' @return results-csv-framework.
#' @keywords internal
#'
prepare_resultsObject <- function(input, DATA, DEBUGKEYS) {
    # static fields - always present
    column_names <- c(
        "full_path",
        "image_date",
        "image_name",
        "processed_width",
        "processed_height",
        "crop_x0",
        "crop_x1",
        "crop_y0",
        "crop_y1",
        "identifiercrop_x0",
        "identifiercrop_x1",
        "identifiercrop_y0",
        "identifiercrop_y1"
    )

    # Add dynamic fields for each spectrum
    for (spectrum in names(DATA$spectrums$lower_bound)) {
        column_names <- c(column_names, paste0(spectrum, "_count"),
                          paste0(spectrum, "_area"),
                          paste0(spectrum, "_fraction"),
                          paste0(spectrum, "_count_undistorted"),
                          paste0(spectrum, "_area_undistorted"),
                          paste0(spectrum, "_fraction_undistorted"))
    }

    # init a NA-filled df, assign the column-names and return it out of the func.
    df <- data.frame(matrix(NA, nrow = 0, ncol = length(column_names)))
    colnames(df) <- column_names
    return(df)
}
