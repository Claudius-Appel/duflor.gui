#' Handle saving application-state to file
#'
#' - assumes `input$save_state` to be properly formatted, as returned by [shinyFiles::shinyFileSave()]
#' - assumes `volumes` to be the return-value of [shinyFiles::getVolumes()]
#'
#' @param input .
#' @param DATA .
#' @param DEBUGKEYS .
#' @param FLAGS .
#' @param volumes .
#' @param error error-object
#' @param errordir_path directory to write error-replication state to
#' @keywords internal
#' @return path to error-state file which may be used to load the error-triggering settings.
#'
#' @importFrom shiny req
#' @importFrom stringr str_count
#' @importFrom stringr str_c
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shinyFiles shinyFileSave
#' @importFrom shinyFiles parseSavePath
#' @importFrom shinyFiles parseDirPath
#'
save_error_state <- function(input, DATA, DEBUGKEYS, FLAGS, volumes, error, errordir_path = NA, erroneous_callback = "") {
    if (is.na(errordir_path)) {
        errordir_path <- tempdir()
        showNotification(
            ui = str_c(
                "Error-state will be written to '",
                errordir_path,
                "' as no folder was selected yet.'"
            ),
            duration = NA,
            type = "error"
        )
    }
    req(dir.exists(errordir_path))
    time <- Sys.time()

    fpath <- file.path(errordir_path,str_c("duflor_gui - error_state (",erroneous_callback," - ",format(Sys.time(),'%Y_%m_%d__%H_%M'),").rds"))
    cat("\nSaving  error-state to '",fpath,"'")
    saveRDS(list(
        input = input,
        DATA = DATA,
        DEBUGKEYS = DEBUGKEYS,
        FLAGS = FLAGS
    ),
    file =  fpath)
    return(fpath)
}
