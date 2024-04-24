#' render mask based on input data
#'
#' @param input - `input` respective shiny-component
#' @param DATA - `DATA` respective shiny-component
#' @param FLAGS - `FLAGS` respective shiny-component
#'
#' @keywords internal
render_selected_mask <- function(input, DATA, FLAGS) {
    if (is.null(input$tbl_results_filtered_rows_selected)) {
        showNotification(
            ui = "No row selected.",
            duration = DATA$notification_duration * 4,
            type = "warning"
        )
        return()
    }
    if (!(input$reinspected_spectrums %in% names(DATA$spectrums$lower_bound))) {
        showNotification(
            ui = "This spectrum was not analysed for this image.",
            duration = DATA$notification_duration * 4,
            type = "warning"
        )
        return()
    }
    if (FLAGS$analyse_single_image) {
        file <- DATA$results$results$full_path
    } else {
        file <- DATA$r__tbl_dir_files$images_filtered[[input$tbl_results_filtered_rows_selected]]
    }
    image_dimensions <- as.integer(get_image_dimensions(file))
    ## LOAD IMAGE
    if (is.na(DATA$last_masked_image) || (DATA$last_masked_image!=file)) {
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
            DATA$last_masked_image <- file
            DATA$last_im <- im
        } else {
            im <- load_image(
                image.path = file,
                subset_only = F,
                return_hsv = T
            )
            DATA$last_masked_image <- file
            DATA$last_im <- im
        }
    } else {
        im <- DATA$last_im
    }
    mask <- input$reinspected_spectrums

    # get the spectrum's HSV scope
    lower_bound <- DATA$spectrums$lower_bound[[mask]]
    upper_bound <- DATA$spectrums$upper_bound[[mask]]

    # extract the spectrum & get its coordinates
    hsv_results <- extract_pixels_HSV(
        pixel.array = im,
        lower_bound = lower_bound,
        upper_bound = upper_bound,
        fast_eval = T,
        bundle_pixelarray = F,
        check_value = T,
        use_single_iteration_cpp = T
    )
    # LIMIT RANGE OF IDENTIFIER-HITS FROM CROPPED SEARCH REGION FOR ID-DOT
    if (input$do_crop_identifier_range) {
        hsv_results <- limit_identifier_coordinates(
            spectrums_object = hsv_results,
            image_dimensions = image_dimensions,
            identifiersearch_x0 = input$identifiersearch_x0,
            identifiersearch_x1 = input$identifiersearch_x1,
            identifiersearch_y0 = input$identifiersearch_y0,
            identifiersearch_y1 = input$identifiersearch_y1
        )
        # # update the repackaged pixel.counts, so that the area's are updated properly
        # repackaged_pixel_counts[[idx]] <- sum(condition)
        # repackaged_pixel_counts[[idx]] <- current_identifier_idx[condition,]
    }
    # make a mask
    mask <- apply_HSV_color_by_mask(
        pixel.array = im,
        pixel.idx = hsv_results[[1]]$pixel.idx,
        target.color = "red",
        mask_extreme = input$mask_extreme
    )
    # display the mask
    display(HSVtoRGB(mask))
}
