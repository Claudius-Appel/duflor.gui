#' Handle saving application-state to file
#'
#' - assumes `input$save_state` to be properly formatted, as returned by [shinyFiles::shinyFileSave()]
#' - assumes `volumes` to be the return-value of [shinyFiles::getVolumes()]
#'
#' @param input - `input` respective shiny-component
#' @param DATA - `DATA` respective shiny-component
#' @param DEBUGKEYS - shiny-app `DEBUGKEYS`-reactives
#' @param volumes - volume letters available
#' @return path to the saved state-file
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
    savefile_path <- parseSavePath(roots = volumes, selection = input$save_state)
    fpath <- file.path(savefile_path$datapath)
    cat("\nSaving state to '",fpath,"'")
    # but first we must remove some values which are not to be saved to ensure filesize is minimal:
    # - DATA$last_im (which caches the last-loaded image of the 'render_selected_mask'-subroutine)
    DATA2 <- DATA
    DATA2$last_im <- NULL
    saveRDS(list(
        input = input,
        DATA = DATA2,
        DEBUGKEYS = DEBUGKEYS,
        FLAGS = FLAGS
    ),
    file =  fpath)
    DATA2 <- NULL
    return(fpath)
}
