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
#'
#' @keywords internal
#' @importFrom shiny req
#' @importFrom stringr str_count
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shinyFiles shinyFileSave
#' @importFrom shinyFiles parseSavePath
#' @importFrom shinyFiles parseDirPath
#'
save_state <- function(input, DATA, DEBUGKEYS, FLAGS, volumes) {
    shinyFileSave(
        input,
        "save_state",
        roots = volumes,
        session = getDefaultReactiveDomain(),
        allowDirCreate = T
    )
    savedir_path <- parseDirPath(roots = volumes, selection = input$save_state)
    req(dir.exists(savedir_path))
    req(isFALSE(is.numeric(input$save_state[[1]])))
    savefile_path <- parseSavePath(roots = volumes, selection = input$save_state)
    fpath <- file.path(savefile_path$datapath)
    print(fpath)
    saveRDS(list(
        input = input,
        DATA = DATA,
        DEBUGKEYS = DEBUGKEYS,
        FLAGS = FLAGS
    ),
    file =  fpath)
}