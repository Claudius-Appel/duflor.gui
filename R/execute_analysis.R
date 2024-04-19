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
    #### CALL RESPECTIVE ANALYSIS-FUNCTION ####
    if (FLAGS$analyse_single_image) {
        isolate(DATA$r__tbl_dir_files)
        file <- DATA$r__tbl_dir_files$images_filtered[[input$tbl_dir_files_rows_selected]]
        file <- duflor.check(file)
        execute_single(file,input,DATA, DEBUGKEYS, FLAGS)
    } else {
        valid_files <- duflor.check(DATA$r__tbl_dir_files)
        execute_multiple(valid_files,input, DATA, DEBUGKEYS, FLAGS)
        ## do nothing?
    }

    #### TEAR DOWN PARALLELISATION ####
    if (getDoParRegistered()) {
        # finally, shutdown the cluster if work was performed in parallel
        shutdown_parallel()
    }
}
