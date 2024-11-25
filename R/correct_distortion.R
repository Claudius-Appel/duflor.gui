#'
#'
#' @param pixel.idx natrixmatrix of
#' @param distortions list of distortion-factors to apply. Values must be numeric, on a scale from -100 <> +100
#' Currently, only `barrel`-distortion can be applied.
#' @param image_dimensions actual dimensions of the image, without cropping.
#' @param do_crop_image boolean to check if the image was cropped.
#' @param x0 coordinates of the cropping top left corner. Necessary to correct coordinate-space for cropping.
#' @param y0 coordinates of the
#'
#' @return sum of distortion-corrected pixels, rounded to the closest integer value.
#' @keywords internal
#'
correct_distortion <- function(pixel.idx, distortions, image_dimensions, do_crop_image, x0, y0) {

    # calculate the euclidean distance vectorized
    euclidean_distance <- function(p1, p2) {
        sqrt((p2[, 1] - p1[1])^2 + (p2[, 2] - p1[2])^2)
    }

    # Image dimensions and center
    center_coords <- c(round(image_dimensions[1] / 2), round(image_dimensions[2] / 2))
    euclidean_distance_edge <- sqrt((image_dimensions[1] - center_coords[1])^2 +
                                        (image_dimensions[2] - center_coords[2])^2)

    # correct coordinates if cropped
    if (do_crop_image) {
        pixel.idx_real_coords <- pixel.idx + c(x0, y0)
    } else {
        pixel.idx_real_coords <- pixel.idx
    }

    # calculate Euclidean distances for all pixels
    euclidean_distances <- euclidean_distance(center_coords, pixel.idx_real_coords)

    # normalize distances
    normalized_distances <- euclidean_distances / euclidean_distance_edge

    # apply barrel distortion
    distortion_percentage <- distortions$barrel / 100  # Convert to decimal
    if (distortion_type == "barrel") {
        distortion_factors <- 1 + (distortion_percentage * normalized_distances)
    } else {
        # suggested factor calculation for pincushion:
        # distortion_factors <- 1 - (distortion_percentage * normalized_distances)


        stop("Invalid distortion type. Implemented types: 'barrel'.")
    }

    # calculate area change (distortion factor squared)
    area_changes <- distortion_factors^2

    # return the sum of corrected areas
    return(round(sum(area_changes)))
}
