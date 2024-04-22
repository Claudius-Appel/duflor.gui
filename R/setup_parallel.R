#' setup parallel backend for a given number of cores.
#'
#' @param used_cores number of cores to be designated to the cluster
#' @param cluster_type either `PSOCK` or `FORK`. Generally `FORK` is recommended above `PSOCK`, however it is not available on all systems. See note below
#'
#' @note
#' On Windows, only the `PSOCK`-cluster type is available.
#'
#' @return .
#' @keywords internal
#' @importFrom foreach getDoParWorkers
#' @importFrom foreach getDoParRegistered
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom shiny showNotification
#'
setup_parallel <- function(used_cores, cluster_type) {
    print("THIS MUST SET UP THE PAR CLUST")
    cluster_type <- NA
    if (is.na(cluster_type)) {
        if (.Platform$OS.type == "windows") {
            cluster_type <- "PSOCK"
        } else {
            cluster_type <- "FORK"
        }
    }
    cl <- makeCluster(spec = used_cores,type = cluster_type)
    registerDoParallel(cl)
    if (getDoParRegistered()) {
        set_up_workers <- getDoParWorkers()
        if (set_up_workers == used_cores) {
            message("Successfully set up parallel '",cluster_type,"' back-end with ",set_up_workers," workers.")
            showNotification(
                ui = str_c("Successfully set up parallel '",cluster_type,"' back-end with ",set_up_workers," workers."),
                id = "backend.setup",
                duration = NULL,
                type = "message"
            )
        } else {
            showNotification(
                ui = str_c("Failed to set up ",set_up_workers," workers on a parallel '",cluster_type,"' back-end with ",used_cores," assigned cores."),
                id = "backend.setup",
                duration = NULL,
                type = "error"
            )
            # stop("Failed to set up ",set_up_workers," workers on a parallel '",cluster_type,"' back-end with ",used_cores," assigned cores.")
        }
    } else {
        stop("Parallel '",cluster_type,"' back-end could not be set up.")
    }
}
