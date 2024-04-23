#' execute analysis on a single image, and return its results
#'
#' For single-image analysis, see [execute_multiple()]
#'
#' @param file - file to analyse
#' @param input - `input` respective shiny-component
#' @param DATA - `DATA` respective shiny-component
#' @param DEBUGKEYS - `DEBUGKEYS` respective shiny-component
#' @param FLAGS - `FLAGS` respective shiny-component
#'
#' @return results_object, see [update_resultsObject()]
#' @keywords internal
#' @importFrom duflor extract_pixels_HSV
#' @importFrom imager width
#' @importFrom imager height
#'
execute_single <- function(file, input, DATA, DEBUGKEYS, FLAGS) {
    #### INIT RESULTS-OBJECT ####
    results_object <- prepare_resultsObject(input,DATA,DEBUGKEYS)
    # create a results-row to be merged into the `results_object`
    current_results <- data.frame(matrix(NA, nrow = 1, ncol = length(names(results_object))))
    colnames(current_results) <- names(results_object)
    #TODO: decide if these paths must be sanitized still?
    current_results$full_path <- file
    current_results$image_name <- basename(file)
    ## DATE_OF_ANALYSIS
    current_results$date_of_analysis <- input$date_of_image_shooting
    ## IMAGE DIMENSIONS
    image_dimensions <- as.integer(get_image_dimensions(file))
    ## LOAD IMAGE
    if (input$do_crop_image) {
        im <- load_image(
            image.path = file,
            subset_only = T,
            return_hsv = T,
            crop_left = input$x0,
            crop_right = image_dimensions[[1]] - input$x1,
            crop_top = input$y0,
            crop_bottom = image_dimensions[[2]] - input$y1
        )
    } else {
        im <- load_image(
            image.path = file,
            subset_only = F,
            return_hsv = T
        )
    }
    current_results$processed_width <- width(im)
    current_results$processed_height <- height(im)
    ## EXTRACT RESULTS
    hsv_results <- extract_pixels_HSV(
        pixel.array = im,
        lower_bound = DATA$spectrums$lower_bound,
        upper_bound = DATA$spectrums$upper_bound,
        fast_eval = T,
        bundle_pixelarray = F,
        check_value = T,
        use_single_iteration_cpp = T
    )
    ## CALCULATE AREA FROM PIXEL_COUNTS
    repackaged_pixel_counts <- list()
    for (name in names(hsv_results)) {
        repackaged_pixel_counts[[name]] <- hsv_results[[name]]$pixel.count
    }
    # we use the duflor.gui-version of this function because we need a different structure.
    areas <- convert_pixels_to_area_gui(repackaged_pixel_counts)
    for (name in names(hsv_results)) {
        current_results[[str_c(name,"_area")]] <- areas[[name]]
        current_results[[str_c(name,"_count")]] <- hsv_results[[name]]$pixel.count
        current_results[[str_c(name,"_fraction")]] <- hsv_results[[name]]$pixel.count/(prod(image_dimensions))
    }
    current_results$area_per_pixel <- areas$area_per_pixel
    ## UPDATE RESULTS_OBJECT
    results_object <- update_resultsObject(results_object,current_results)
}
