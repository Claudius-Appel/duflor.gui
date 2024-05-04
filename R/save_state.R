#' Title
#'
#' @param input .
#' @param DATA .
#' @param DEBUGKEYS .
#' @param FLAGS .
#' @param volumes .
#'
#' @return .
#' @keywords internal
#' @importFrom shinyFiles parseDirPath
#' @importFrom shiny req
#'
save_state <- function(input, DATA, DEBUGKEYS, FLAGS, volumes) {
    savedir_path <- parseDirPath(roots = volumes, input$save_state)
    req(dir.exists(savedir_path))
    saveRDS(
        list(
            input = input,
            DATA = DATA,
            DEBUGKEYS = DEBUGKEYS,
            FLAGS = FLAGS
        ),
        file =  file.path(savedir_path, "state.rds")
    )
}
