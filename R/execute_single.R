#' execute analysis on a single image, and return its results
#'
#' For single-image analysis, see [execute_multiple()]
#'
#' @inheritParams .main_args
#' @param file file to analyse
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
    bnf <- file_path_sans_ext(basename(file))
    current_results$image_name <- basename(file)
    ## DATE_OF_ANALYSIS
    current_results$date_of_analysis <- input$date_of_image_shooting
    ## IMAGE DIMENSIONS
    image_dimensions <- as.integer(get_image_dimensions(file))
    if (input$do_save_masks) {
        results_path <- normalizePath(str_c(
            dirname(file),
            "/results"
        ))
        if (isFALSE(dir.exists(results_path))) {
            dir.create(results_path)
        }
    }
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
    current_results$crop_x0 <- input$x0
    current_results$crop_x1 <- input$x1
    current_results$crop_y0 <- input$y0
    current_results$crop_y1 <- input$y1
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
    ## LIMIT RANGE OF IDENTIFIER-HITS FROM CROPPED SEARCH REGION FOR ID-DOT
    if (input$do_crop_identifier_range) {
        if (input$do_crop_image) {
            # coordinates of the identifier-dot has "moved" because the image
            # got cropped
            identifiersearch_x0 <- input$identifiersearch_x0 - input$x0
            identifiersearch_x1 <- input$identifiersearch_x1 - input$x0
            identifiersearch_y0 <- input$identifiersearch_y0 - input$y0
            identifiersearch_y1 <- input$identifiersearch_y1 - input$y0
        } else {
            identifiersearch_x0 = input$identifiersearch_x0
            identifiersearch_x1 = input$identifiersearch_x1
            identifiersearch_y0 = input$identifiersearch_y0
            identifiersearch_y1 = input$identifiersearch_y1
        }
        hsv_results <- limit_identifier_coordinates(
            spectrums_object = hsv_results,
            image_dimensions = image_dimensions,
            identifiersearch_x0 = identifiersearch_x0,
            identifiersearch_x1 = identifiersearch_x1,
            identifiersearch_y0 = identifiersearch_y0,
            identifiersearch_y1 = identifiersearch_y1
        )
    } else {
        # make sure that when not cropping the whole range is saved.
        identifiersearch_x0 <- input$x0
        identifiersearch_x1 <- input$x1
        identifiersearch_y0 <- input$y0
        identifiersearch_y1 <- input$y1
    }
    current_results$identifiercrop_x0 <- identifiersearch_x0
    current_results$identifiercrop_x1 <- identifiersearch_x1
    current_results$identifiercrop_y0 <- identifiersearch_y0
    current_results$identifiercrop_y1 <- identifiersearch_y1
    ## CALCULATE AREA FROM PIXEL_COUNTS
    repackaged_pixel_counts <- list()
    for (name in names(hsv_results)) {
        repackaged_pixel_counts[[name]] <- hsv_results[[name]]$pixel.count
    }
    # we use the duflor.gui-version of this function because we need a different structure.
    areas <- convert_pixels_to_area_gui(repackaged_pixel_counts, input$identifier_area)
    for (name in names(hsv_results)) {
        current_results[[str_c(name,"_area")]] <- areas[[name]]
        current_results[[str_c(name,"_count")]] <- hsv_results[[name]]$pixel.count
        current_results[[str_c(name,"_fraction")]] <- hsv_results[[name]]$pixel.count/(prod(image_dimensions))
        if (input$do_save_masks) {
            mask_path <- normalizePath(str_c(results_path, "/", bnf, "_", name, ".png"))
            save.image(RGBtosRGB(HSVtoRGB(
                apply_HSV_color_by_mask(
                    pixel.array = im,
                    pixel.idx = hsv_results[[name]]$pixel.idx,
                    target.color = "red",
                    mask_extreme = input$do_save_high_contrast_masks
                )
            )),file = mask_path)
        }
    }
    current_results$area_per_pixel <- areas$area_per_pixel
    ## UPDATE RESULTS_OBJECT
    results_object <- update_resultsObject(results_object,current_results)
}
