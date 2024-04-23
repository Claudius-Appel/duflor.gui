#' trims numbers from groups from character-vector
#'
#' @param names .
#'
#' @return .
#' @keywords internal
#'
get_group <- function(names) {
    prefix <- gsub("\\d", "", names)
    return(prefix)
}
