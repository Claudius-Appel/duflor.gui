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
    image_dimensions <- as.integer(duflor.get_image_dimensions(file))
    current_results$image_width <- image_dimensions[[1]]
    current_results$image_height <- image_dimensions[[2]]
    ## LOAD IMAGE
    if (input$do_crop_image) {
        im <- load_image(
            image.path = file,
            subset_only = T,
            return_hsv = T,
            crop_left = input$crop_left,
            crop_right = input$crop_right,
            crop_top = input$crop_top,
            crop_bottom = input$crop_bottom
        )
    } else {
        im <- load_image(
            image.path = file,
            subset_only = F,
            return_hsv = T
        )
    }
    ## EXTRACT RESULTS
    hsv_results <- duflor::extract_pixels_HSV(
        pixel.array = im,
        lower_bound = DATA$spectrums$lower_bound,
        upper_bound = DATA$spectrums$upper_bound,
        fast_eval = T,
        bundle_pixelarray = F,
        check_value = T,
        use_single_iteration_cpp = T
    )
}
