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
        do_correct_distortion <- input$do_correct_distortion
        barrel_correction_factor <- input$barrel_correction_factor
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
        foreach_result <- foreach(index = 1:length(files$index),.packages = c("duflor","duflor.gui"), .verbose = T,.inorder = T,.errorhandling = "pass") %dopar% {
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
            # Calculating the area based on pixel counts for each spectrum vs. the identifier-spectrum
            # but first, ensure calculations are possible:
            for (name in names(hsv_results)) {
                if (isTRUE(do_correct_distortion)) { # experimental distortion-correction.
                    hsv_results[[name]]$pixel.count_undistorted <- correct_distortion(
                        hsv_results[[name]]$pixel.idx,
                        distortions = list("barrel" = barrel_correction_factor),
                        image_dimensions = image_dimensions,
                        do_crop_image = do_crop_image,
                        x0 = x0,
                        y0 = y0
                    )
                } else {
                    hsv_results[[name]]$pixel.count_undistorted <- NA
                }
            }
            repackaged_pixel_counts <- list()
            repackaged_pixel_counts_undistorted <- list()
            for (name in names(hsv_results)) {
                repackaged_pixel_counts[[name]] <- hsv_results[[name]]$pixel.count
                repackaged_pixel_counts_undistorted[[name]] <- hsv_results[[name]]$pixel.count_undistorted
            }
            # Calculating the area based on pixel counts for each spectrum vs. the identifier-spectrum
            # but first, ensure calculations are possible:
            #TODO: figure out why this fix fails in parallel instance
            if (repackaged_pixel_counts[[grep("identifier",names(repackaged_pixel_counts))]]==0) {
                # note: because we cannot warn here, the warning is performed
                # after foreach has concluded and the results are combined.
                # Unfortunately this means that executing multiple images via
                # parallelisation and without parallelisation will yield
                # different output data - I think?
                # TODO: verify the above
            } else {
                # in this case, the identifier has at least 1 pixel, so we can actually calculate areas now.


                # we use the duflor.gui-version of this function because we need a different structure.
                areas <- convert_pixels_to_area_gui(repackaged_pixel_counts, identifier_area)
                for (name in names(hsv_results)) {
                    current_results[[str_c(name,"_area")]] <- areas[[name]]
                    current_results[[str_c(name,"_count")]] <- hsv_results[[name]]$pixel.count
                    current_results[[str_c(name,"_fraction")]] <- hsv_results[[name]]$pixel.count/(prod(image_dimensions))
                    if (do_save_masks) {
                        mask_path <- normalizePath(str_c(results_path, "/", bnf, "_", name, ".png"))
                        # only save images for masks which matched at least
                        # 1 pixel
                        if (hsv_results[[name]]$pixel.count>0) {
                            save.image(RGBtosRGB(HSVtoRGB(
                                apply_HSV_color_by_mask(
                                    pixel.array = im,
                                    pixel.idx = hsv_results[[name]]$pixel.idx,
                                    target.color = ifelse(str_count(name,"identifier"),"white","red"),
                                    mask_extreme = do_save_high_contrast_masks
                                )
                            )),file = mask_path)
                        } else {
                            message(str_c("No mask saved for spectrum '",name,"' of image '",bnf,"': 0 Hits."))
                        }
                    }
                }
                ## UPDATE RESULTS_OBJECT
                # results_object <- update_resultsObject(results_object,current_results)
                current_results$area_per_pixel <- areas$area_per_pixel
            }
            ## UNDISTORTED RESULTS
            # if distortion-fixing is disabled, we skip this section; and inherit the entries in 'current_results' be 'NA'
            if (isTRUE(do_correct_distortion)) {
                if (repackaged_pixel_counts_undistorted[[grep("identifier",names(repackaged_pixel_counts_undistorted))]]==0) {
                    return(current_results)
                } else {
                    # we use the duflor.gui-version of this function because we need a different structure.
                    areas_undistorted <- convert_pixels_to_area_gui(repackaged_pixel_counts_undistorted, identifier_area)
                    for (name in names(hsv_results)) {
                        current_results[[str_c(name,"_area_undistorted")]] <- areas_undistorted[[name]]
                        current_results[[str_c(name,"_count_undistorted")]] <- hsv_results[[name]]$pixel.count_undistorted
                        current_results[[str_c(name,"_fraction_undistorted")]] <- hsv_results[[name]]$pixel.count_undistorted/(prod(image_dimensions))
                        if (do_save_masks) {
                            if (hsv_results[[name]]$pixel.count>0) {
                                message(str_c("The undistorted mask for spectrum '",name,"' of image '",bnf,"' cannot be saved."))
                            } else {
                                message(str_c("No mask saved for spectrum '",name,"' of image '",bnf,"': 0 Hits."))
                            }
                        }
                    }
                    current_results$area_per_pixel_undistorted <- areas_undistorted$area_per_pixel
                    ## UPDATE RESULTS_OBJECT
                    return(current_results)
                }
            } else {
                return(current_results)
            }
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
                for (name in names(hsv_results)) {
                    if (isTRUE(input$do_correct_distortion)) { # experimental distortion-correction.
                        hsv_results[[name]]$pixel.count_undistorted <- correct_distortion(
                            hsv_results[[name]]$pixel.idx,
                            distortions = list("barrel" = input$barrel_correction_factor),
                            image_dimensions = image_dimensions,
                            do_crop_image = input$do_crop_image,
                            x0 = input$x0,
                            y0 = input$y0
                        )
                    } else {
                        hsv_results[[name]]$pixel.count_undistorted <- NA
                    }
            }
            repackaged_pixel_counts <- list()
            repackaged_pixel_counts_undistorted <- list()
            for (name in names(hsv_results)) {
                repackaged_pixel_counts[[name]] <- hsv_results[[name]]$pixel.count
                repackaged_pixel_counts_undistorted[[name]] <- hsv_results[[name]]$pixel.count_undistorted
            }
            # Calculating the area based on pixel counts for each spectrum vs. the identifier-spectrum
            # but first, ensure calculations are possible:
            if (repackaged_pixel_counts[[grep("identifier",names(repackaged_pixel_counts))]]==0) {
                # Mote: because the warning cannot happen in this place for the
                # parallelised case, it is also not performed here. Instead,
                # checking and warning is performed at the end, before returning
                # out of this function.
            } else {
                # in this case, the identifier has at least 1 pixel, so we can actually calculate areas now.


                # we use the duflor.gui-version of this function because we need a different structure.
                areas <- convert_pixels_to_area_gui(repackaged_pixel_counts, input$identifier_area)
                for (name in names(hsv_results)) {
                    current_results[[str_c(name,"_area")]] <- areas[[name]]
                    current_results[[str_c(name,"_count")]] <- hsv_results[[name]]$pixel.count
                    current_results[[str_c(name,"_fraction")]] <- hsv_results[[name]]$pixel.count/(prod(image_dimensions))
                    if (input$do_save_masks) {
                        mask_path <- normalizePath(str_c(results_path, "/", bnf, "_", name, ".png"))
                        # only save images for masks which matched at least
                        # 1 pixel
                        if (hsv_results[[name]]$pixel.count>0) {
                            save.image(RGBtosRGB(HSVtoRGB(
                                apply_HSV_color_by_mask(
                                    pixel.array = im,
                                    pixel.idx = hsv_results[[name]]$pixel.idx,
                                    target.color = ifelse(str_count(name,"identifier"),"white","red"),
                                    mask_extreme = input$do_save_high_contrast_masks
                                )
                            )),file = mask_path)
                        } else {
                            message(str_c("No mask saved for spectrum '",name,"' of image '",bnf,"': 0 Hits."))
                        }
                    }
                }
                ## UPDATE RESULTS_OBJECT
                current_results$area_per_pixel <- areas$area_per_pixel
            }
            ## UNDISTORTED RESULTS
            # if distortion-fixing is disabled, we skip this section; and inherit the entries in 'current_results' be 'NA'
            if (isTRUE(input$do_correct_distortion)) {
                if (repackaged_pixel_counts_undistorted[[grep("identifier",names(repackaged_pixel_counts_undistorted))]]==0) {

                } else {
                    # we use the duflor.gui-version of this function because we need a different structure.
                    areas_undistorted <- convert_pixels_to_area_gui(repackaged_pixel_counts_undistorted, input$identifier_area)
                    for (name in names(hsv_results)) {
                        current_results[[str_c(name,"_area_undistorted")]] <- areas_undistorted[[name]]
                        current_results[[str_c(name,"_count_undistorted")]] <- hsv_results[[name]]$pixel.count_undistorted
                        current_results[[str_c(name,"_fraction_undistorted")]] <- hsv_results[[name]]$pixel.count_undistorted/(prod(image_dimensions))
                        if (input$do_save_masks) {
                            if (hsv_results[[name]]$pixel.count>0) {
                                message(str_c("The undistorted mask for spectrum '",name,"' of image '",bnf,"' cannot be saved."))
                            } else {
                                message(str_c("No mask saved for spectrum '",name,"' of image '",bnf,"': 0 Hits."))
                            }
                        }
                    }
                    ## UPDATE RESULTS_OBJECT
                    current_results$area_per_pixel_undistorted <- areas_undistorted$area_per_pixel
                }
            }
            results_object <- update_resultsObject(results_object,current_results)
        }
    }
    message("Analysis itself finished; checking for images w/o identifiers")
    images_without_identifier_pixels_count <- 0
    for (each in 1:nrow(results_object)) {
        if (any(is.na(results_object[each, grep("identifier.*count", names(results_object))]))) {
                str_no_ID_pixels_warning <- str_c(
                    "Image '",
                    results_object[each, "image_name"],
                    "' contains no pixels for the identifier.",
                    "\n'NA's are returned for pixel-count, area and image-fraction for all spectra of this image."
                )
                warning(simpleWarning(str_no_ID_pixels_warning))
                images_without_identifier_pixels_count <- images_without_identifier_pixels_count + 1
        }
    }
    if (images_without_identifier_pixels_count>0) {
        showNotification(
            ui = "No identifier found in some images. See console for more details.",
            id = "no_id.multiple",
            duration = NULL,
            type = "error"
        )
    }
    return(results_object)
}
