execute_multiple <- function(files, input, DATA, DEBUGKEYS, FLAGS) {
    #### INPUT VALIDATION ####
    if (any(!is.na(DATA$r__tbl_dir_files_selectedrow))) {
        showNotification(
            ui =    "All images will be evaluated, even though only image '",
            DATA$r__tbl_dir_files_selectedrow,
            "' was selected.",
            duration = DATA$notification_duration * 4,
            type = "warning"
        )
    }
    #### INIT RESULTS-OBJECT ####
    results_object <- prepare_resultsObject(input,DATA,DEBUGKEYS)
    #### ITERATE OVER IMAGES ####
    for (index in files$count) {
        # create a results-row to be merged into the `results_object`
        current_results <- data.frame(matrix(NA, nrow = 1, ncol = length(names(results_object))))
        colnames(current_results) <- names(results_object)
        ## NAME
        file <- files$images_filtered[index]
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
                crop_left = input$x0,
                crop_right = current_results$image_width - input$x1,
                crop_top = input$y0,
                crop_bottom = current_results$image_height - input$y1
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
        ## UPDATE RESULTS_OBJECT
        results_object <- update_resultsObject(results_object,current_results)
    }
    return(results_object)
}
