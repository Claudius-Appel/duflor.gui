#' replace values outside of boundaries with respective  boundary
#'
#' This function limits only values which lie outside of
#' `[replace_lower, replace_upper`]. Values which do not fall outside this range
#' are not modified.
#'
#'
#' @param value vector to normalise
#' @param replace_lower value to insert in place of elements of `value` below this bound.
#' @param replace_upper see `replace_lower`
#'
#' @return vector, normalised to range `[replace_lower, replace_upper]`
#' @keywords internal
#'
#' @examples
#' result <- duflor.gui:::limit_to_range(c(0,100,255),0,255)
#' result2 <- duflor.gui:::limit_to_range(c(0,100,256),0,255)
#' print(result)
#' print(result2)
limit_to_range <- function (x, replace_lower, replace_upper)
{
    if (isFALSE(is.numeric(x))) {
        stop(simpleError("Input 'x' must be numeric."))
    }
    if (isFALSE(is.numeric(replace_lower))) {
        stop(simpleError("Input 'replace_lower' must be numeric."))
    }
    if (isFALSE(is.numeric(replace_upper))) {
        stop(simpleError("Input 'replace_upper' must be numeric."))
    }
    x <- pmax(replace_lower, pmin(x, replace_upper))
    return(x)
}
