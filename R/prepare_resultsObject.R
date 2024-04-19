#' prepare the dataframe-object into which all results are to be merged.
#'
#' results requires fields
#' - full_path
#' - date_of_analysis
#' - image_name
#' - image_width
#' - image_height
#' - for each in  `names(DATA$spectrums$lower_bound)`:
#'     - %spectrum%_count
#'     - img.fraction (maybe not that valuable?)
#'     - %spectrum%_area (as results of duflor::convert_pixels_to_area())
#'     - %spectrum%_fraction (as defined as `%spectrum%_count/(sum(<all_plant_spectrums>_count)`)
#'
#' @param input - shiny-app `input`-object
#' @param DATA - shiny-app `DATA`-reactives
#' @param DEBUGKEYS - shiny-app `DEBUGKEYS`-reactives
#'
#' @return results-csv-framework.
#' @keywords internal
#'
prepare_resultsObject <- function(input,DATA,DEBUGKEYS) {


    # static fields - always present
    column_names <- c("full_path", "date_of_analysis", "image_name", "image_width", "image_height")

    # Add dynamic fields for each spectrum
    for (spectrum in names(DATA$spectrums$lower_bound)) {
        column_names <- c(column_names, paste0(spectrum, "_count"),
                          paste0(spectrum, "_area"),
                          paste0(spectrum, "_fraction"))
    }

    # init a NA-filled df,
    df <- data.frame(matrix(NA, nrow = 0, ncol = length(column_names)))
    colnames(df) <- column_names
    # warning(simpleWarning("function not implemented: must prepare the csv-struct to be used for collecting results, and results will then have to be inserted each iter by function 'modify_resultsObject(resultsObject,results*)"))
    return(df)
}