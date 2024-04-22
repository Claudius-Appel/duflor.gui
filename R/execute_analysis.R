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

    #### CALL RESPECTIVE ANALYSIS-FUNCTION ####
    if (FLAGS$analyse_single_image) {
        isolate(DATA$r__tbl_dir_files)
        file <- DATA$r__tbl_dir_files$images_filtered[[input$tbl_dir_files_rows_selected]]
        file <- duflor.check(file)
        results <- execute_single(file, input, DATA, DEBUGKEYS, FLAGS)
        showNotification(
            ui = "should single-eval runs even be saved to file? Should it even be considered relevant for the sake of displaying in the results-tab?",
            duration = DATA$notification_duration * 4,
            type = "warning"
        )
    } else {
        files <- duflor.check(DATA$r__tbl_dir_files)
        results <- execute_multiple(files, input, DATA, DEBUGKEYS, FLAGS)
    }
    return(list(results = results))
}
