#' restrict area in which `identifier`-pixels are considered "valid"
#'
#' This function is only applied to the `identifier`-range.
#' The intention is to exclude random pixels which **definitely** should not be
#' attributed to the identifier-data.
#'
#'
#' @param spectrums_object return object of [extract_pixels_HSV()]
#' @param image_dimensions dimensions of image
#' @param identifiersearch_x0 respective coordinate of the search bounding rectangle
#' @param identifiersearch_x1 respective coordinate of the search bounding rectangle
#' @param identifiersearch_y0 respective coordinate of the search bounding rectangle
#' @param identifiersearch_y1 respective coordinate of the search bounding rectangle
#'
#' @return modified `spectrums_object`
#' @keywords internal
#'
limit_identifier_coordinates <- function(spectrums_object, image_dimensions, identifiersearch_x0, identifiersearch_x1, identifiersearch_y0, identifiersearch_y1) {
    if (length(spectrums_object)==1) { # single image
        current_identifier_idx <- spectrums_object[[1]]$pixel.idx
        condition <- ((current_identifier_idx[,1] >= identifiersearch_x0)
                      & (current_identifier_idx[,1]<=identifiersearch_x1)
                      & (current_identifier_idx[,2]>=identifiersearch_y0)
                      & (current_identifier_idx[,2]<=identifiersearch_y1)
        )
        # update pixel.count, img.fraction & pixel.idx,
        # as they are inserted into the `current_results`
        # and the `spectrums_objectobject`
        spectrums_object[[1]]$pixel.count <- sum(condition)
        spectrums_object[[1]]$img.fraction <- spectrums_object[[1]]$pixel.count/(prod(image_dimensions))
        spectrums_object[[1]]$pixel.idx <- current_identifier_idx[condition,]
    } else { # multiple images
        for (name in names(spectrums_object)) {
            if (any(grep("identifier",names(spectrums_object)))) { # get position of identifier-object
                idx <- grep("identifier",names(spectrums_object))
                if (names(spectrums_object)[[idx]]==name) {
                    current_identifier_idx <- spectrums_object[[idx]]$pixel.idx
                    condition <- ((current_identifier_idx[,1] >= identifiersearch_x0)
                                  & (current_identifier_idx[,1]<=identifiersearch_x1)
                                  & (current_identifier_idx[,2]>=identifiersearch_y0)
                                  & (current_identifier_idx[,2]<=identifiersearch_y1)
                    )
                    # update pixel.count, img.fraction & pixel.idx,
                    # as they are inserted into the `current_results`
                    # and the `spectrums_objectobject`
                    spectrums_object[[name]]$pixel.count <- sum(condition)
                    spectrums_object[[name]]$img.fraction <- spectrums_object[[name]]$pixel.count/(prod(image_dimensions))
                    spectrums_object[[name]]$pixel.idx <- current_identifier_idx[condition,]
                }
            }
        }
    }
    return(spectrums_object)
}
