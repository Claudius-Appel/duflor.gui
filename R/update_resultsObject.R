#' adds a line of values `newline` to a df `resultsObject`
#'
#' This function does not validate its inputs!.
#'
#' @param resultsObject existing dataframe
#' @param new_line data-vector to add
#'
#' @return updated `resultsObject`
#' @keywords internal
#'
update_resultsObject <- function(resultsObject,new_line) {
    resultsObject <- rbind(resultsObject,new_line)
    return(resultsObject)
}
