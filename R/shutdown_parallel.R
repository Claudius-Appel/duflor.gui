#' shut down existing parallel cluster
#'
#' @keywords internal
#' @importFrom doParallel stopImplicitCluster
#' @importFrom shiny showNotification
#'
shutdown_parallel <- function() {
    message("Parallel backend was shut down.")
    stopImplicitCluster()
    showNotification(
        ui = "Parallel backend was shut down.",
        duration = 6.5,
        type = "warning"
    )
}
