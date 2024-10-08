#' execute analysis on multiple images, and return their results
#'
#' For single-image analysis, see [execute_single()]
#'
#' @inheritParams .main_args
#' @param files files to analyse
#'
#' @return results_object, see [update_resultsObject()]
#' @keywords internal
#' @importFrom duflor extract_pixels_HSV
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom dplyr bind_rows
#' @importFrom imager width
#' @importFrom imager height
#' @importFrom imager save.image
#' @importFrom imager RGBtosRGB
#' @importFrom stringr str_c
#' @importFrom imager HSVtoRGB
#' @importFrom tools file_path_sans_ext
#'
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
    if (input$parallel_cores > 1) {
        date_of_image_shooting <- input$date_of_image_shooting
        x0 <- input$x0
        y0 <- input$y0
        x1 <- input$x1
        y1 <- input$y1
        do_crop_image <- input$do_crop_image
        do_crop_identifier_range <- input$do_crop_identifier_range
        spectrums <- DATA$spectrums
        identifiersearch_x0 = input$identifiersearch_x0
        identifiersearch_x1 = input$identifiersearch_x1
        identifiersearch_y0 = input$identifiersearch_y0
        identifiersearch_y1 = input$identifiersearch_y1
        identifier_area <- input$identifier_area
        do_save_masks <- input$do_save_masks
        if (do_save_masks) {
            results_path <- normalizePath(str_c(
                dirname(files$images_filtered[1]),
                "/results"
            ))
            if (isFALSE(dir.exists(results_path))) {
                dir.create(results_path)
            }
        }
        do_save_high_contrast_masks <- input$do_save_high_contrast_masks
        foreach_result <- foreach(index = 1:length(files$index),.packages = c("duflor","duflor.gui"), .verbose = T,.inorder = F) %dopar% {
        # stop(simpleError("parallelisation is not implemented yet. figure out how to do so!!"))
            current_results <- data.frame(matrix(NA, nrow = 1, ncol = length(names(results_object))))
            colnames(current_results) <- names(results_object)
            ## NAME
            file <- files$images_filtered[index]
            bnf <- file_path_sans_ext(basename(file))
            current_results$full_path <- file
            current_results$image_name <- basename(file)
            ## DATE_OF_ANALYSIS
            current_results$date_of_analysis <- date_of_image_shooting
            ## IMAGE DIMENSIONS
            image_dimensions <- as.integer(get_image_dimensions(file))
            ## LOAD IMAGE
            if (do_crop_image) {
                im <- load_image(
                    image.path = file,
                    subset_only = T,
                    return_hsv = T,
                    crop_left = x0,
                    crop_right = image_dimensions[[1]] - x1,
                    crop_top = y0,
                    crop_bottom = image_dimensions[[2]] - y1
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
            current_results$crop_x0 <- x0
            current_results$crop_x1 <- x1
            current_results$crop_y0 <- y0
            current_results$crop_y1 <- y1
            ## EXTRACT RESULTS
            hsv_results <- extract_pixels_HSV(
                pixel.array = im,
                lower_bound = spectrums$lower_bound,
                upper_bound = spectrums$upper_bound,
                fast_eval = T,
                bundle_pixelarray = F,
                check_value = T,
                use_single_iteration_cpp = T
            )
            ## LIMIT RANGE OF IDENTIFIER-HITS FROM CROPPED SEARCH REGION FOR ID-DOT
            if (do_crop_identifier_range) {
                if (do_crop_image) {
                    # coordinates of the identifier-dot has "moved" because the image
                    # got cropped
                    identifiersearch_x0_ <- identifiersearch_x0 - x0
                    identifiersearch_x1_ <- identifiersearch_x1 - x0
                    identifiersearch_y0_ <- identifiersearch_y0 - y0
                    identifiersearch_y1_ <- identifiersearch_y1 - y0
                } else {
                    identifiersearch_x0_ <- identifiersearch_x0
                    identifiersearch_x1_ <- identifiersearch_x1
                    identifiersearch_y0_ <- identifiersearch_y0
                    identifiersearch_y1_ <- identifiersearch_y1
                }
                hsv_results <- limit_identifier_coordinates(
                    spectrums_object = hsv_results,
                    image_dimensions = image_dimensions,
                    identifiersearch_x0 = identifiersearch_x0_,
                    identifiersearch_x1 = identifiersearch_x1_,
                    identifiersearch_y0 = identifiersearch_y0_,
                    identifiersearch_y1 = identifiersearch_y1_
                )
            } else {
                # make sure that when not cropping the whole range is saved.
                identifiersearch_x0_ <- x0
                identifiersearch_x1_ <- x1
                identifiersearch_y0_ <- y0
                identifiersearch_y1_ <- y1
            }
            current_results$identifiercrop_x0 <- identifiersearch_x0_
            current_results$identifiercrop_x1 <- identifiersearch_x1_
            current_results$identifiercrop_y0 <- identifiersearch_y0_
            current_results$identifiercrop_y1 <- identifiersearch_y1_
            ## CALCULATE AREA FROM PIXEL_COUNTS
            repackaged_pixel_counts <- list()
            for (name in names(hsv_results)) {
                repackaged_pixel_counts[[name]] <- hsv_results[[name]]$pixel.count
            }
            # we use the duflor.gui-version of this function because we need a different structure.
            areas <- convert_pixels_to_area_gui(repackaged_pixel_counts, identifier_area)
            for (name in names(hsv_results)) {
                current_results[[str_c(name,"_area")]] <- areas[[name]]
                current_results[[str_c(name,"_count")]] <- hsv_results[[name]]$pixel.count
                current_results[[str_c(name,"_fraction")]] <- hsv_results[[name]]$pixel.count/(prod(image_dimensions))
                if (do_save_masks) {
                    mask_path <- normalizePath(str_c(results_path, "/", bnf, "_", name, ".png"))
                    save.image(RGBtosRGB(HSVtoRGB(
                        apply_HSV_color_by_mask(
                            pixel.array = im,
                            pixel.idx = hsv_results[[name]]$pixel.idx,
                            target.color = "red",
                            mask_extreme = do_save_high_contrast_masks
                        )
                    )),file = mask_path)
                }
            }
            ## UPDATE RESULTS_OBJECT
            # results_object <- update_resultsObject(results_object,current_results)
            current_results$area_per_pixel <- areas$area_per_pixel
            return(current_results)
        }
        results_object <- bind_rows(foreach_result,.id = NULL)
    } else {
        if (input$do_save_masks) {
            results_path <- normalizePath(str_c(
                dirname(files$images_filtered[1]),
                "/results"
            ))
            if (isFALSE(dir.exists(results_path))) {
                dir.create(results_path)
            }
        }
        for (index in files$index) {
            # create a results-row to be merged into the `results_object`
            current_results <- data.frame(matrix(NA, nrow = 1, ncol = length(names(results_object))))
            colnames(current_results) <- names(results_object)
            ## NAME
            file <- files$images_filtered[index]
            bnf <- file_path_sans_ext(basename(file))
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
            ## UPDATE RESULTS_OBJECT
            current_results$area_per_pixel <- areas$area_per_pixel
            results_object <- update_resultsObject(results_object,current_results)
        }
    }
    return(results_object)
}
