execute_analysis <- function(input, DATA, DEBUGKEYS, FLAGS) {
    #### SETUP PARALLELISATION ####
    if (input$parallel_cores > 1) {
        if (.Platform$OS.type == "windows") {
            cluster_type <- "PSOCK"
        } else {
            cluster_type <- "FORK"
        }
        if (getDoParRegistered()) {
            # shut down existing cluster first (?)
            shutdown_parallel()
        }
        setup_parallel(input$parallel_cores, cluster_type)
    } else {
        if (getDoParRegistered()) {
            # shut down existing cluster first (?)
            shutdown_parallel()
        }
    }

    #### TEAR DOWN PARALLELISATION ####
    if (getDoParRegistered()) {
        # finally, shutdown the cluster if work was performed in parallel
        shutdown_parallel()
    }
}
