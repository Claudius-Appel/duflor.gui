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
correct_distortion <- function(pixel.idx, distortions, image_dimensions, do_crop_image,x0,y0) {

    # calculates the euclidean distance
    euclidean_distance <- function(p1, p2) {
        sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
    }

    radius <- max(image_dimensions)

    # Correct coordinates if cropped, so that the distance actually works.
    if (do_crop_image) {
        pixel.idx_real_coords <- pixel.idx + c(x0, y0)
    } else {
        pixel.idx_real_coords <- pixel.idx + c(0, 0)
    }

    center_coords <- c(round(image_dimensions[1] / 2), round(image_dimensions[2] / 2))

    # 2. Get maximum possible Euclidean distance from the center to the edge
    euclidean_distance_edge <- euclidean_distance(center_coords, c(image_dimensions[1], image_dimensions[2]))

    distortion_sum <- numeric(nrow(pixel.idx_real_coords))
    distortion_percentage <- distortions$barrel / 100  # Convert percentage to decimal (e.g., -1.2% -> -0.012)

    # 3. Calculate distortion for each pixel
    for (i in seq(1, nrow(pixel.idx_real_coords))) {
        # Calculate the Euclidean distance from the image center to the current pixel
        euclidean_distance_pixel <- euclidean_distance(center_coords, c(as.numeric(pixel.idx_real_coords[i, "x"]), as.numeric(pixel.idx_real_coords[i, "y"])))

        # normalise distance to 0->1
        normalised_distance <- euclidean_distance_pixel / euclidean_distance_edge

        # apply distortion
        distortion_factor <- 1 + (distortion_percentage * normalised_distance)

        # get its area (must be validated that this is correct)
        area_change <- distortion_factor^2

        # and store the correction magnitude
        distortion_sum[i] <- area_change
    }

    return(round(sum(distortion_sum)))  # Return the sum of the distortion-adjusted area for each pixel
}
