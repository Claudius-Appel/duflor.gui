#' wrapper-function facilitating single- and multi-image analysis.
#'
#' @param input - `input` respective shiny-component
#' @param DATA - `DATA` respective shiny-component
#' @param DEBUGKEYS - `DEBUGKEYS` respective shiny-component
#' @param FLAGS - `FLAGS` respective shiny-component
#'
#' @return list with components
#' - `results`: results-data passed-through from [execute_multiple()] or [execute_single()]
#' @export
#'
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
        results <- execute_single(file, input, DATA, DEBUGKEYS, FLAGS)
    } else {
        files <- duflor.check(DATA$r__tbl_dir_files)
        results <- execute_multiple(files, input, DATA, DEBUGKEYS, FLAGS)
    }
    results_path <- str_c(dirname(results$full_path[[1]]),"/results")
    out <- store_results_to_file(results = results,results_path = results_path,save_to_xlsx = input$save_as_xlsx)
    ## save the results
    if (isFALSE(out$success)) {
        showNotification(
            ui = "The results could not be written to file.",
            results$file_state$results_path,
            "'",
            duration = DATA$notification_duration * 4,
            type = "error"
        )
    }
    #### TEAR DOWN PARALLELISATION ####
    if (getDoParRegistered()) {
        # finally, shutdown the cluster if work was performed in parallel
        shutdown_parallel()
    }
    return(list(results = results, file_state = out))
}
