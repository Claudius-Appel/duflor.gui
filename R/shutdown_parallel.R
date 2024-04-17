#' shut down existing parallel cluster
#'
#' @keywords internal
#' @importFrom doParallel stopImplicitCluster
#'
shutdown_parallel <- function() {
    print("THIS MUST SHUT DOWN THE PAR CLUST")
    stopImplicitCluster()
}
