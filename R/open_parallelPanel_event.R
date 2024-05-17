#' internal callback function for the checkbox opening the parallelisation-panel
#'
#' @param input - `input` respective shiny-component
#' @param DATA - `DATA` respective shiny-component
#' @param FLAGS - `FLAGS` respective shiny-component
#' @param use_logical_cores - internal flag
#' @param session - shiny session
#' @param STARTUP - `STARTUP` respective shiny-component
#'
#' @keywords internal
#' @importFrom stringr str_c
#' @importFrom shiny showNotification
#' @importFrom shiny updateNumericInput
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom parallel detectCores
#'
open_parallelPanel_event <- function(input, DATA, FLAGS, use_logical_cores, session, STARTUP, default_init_cores = 2) {
    # controls whether or not the number of cores can be selected.
    # Unchecking this will set the number of used cores to `1`
    if (input$open_parallelPanel) {
        show("PARALLEL_PANEL")
        showNotification(
            ui = str_c(
                "Enabled parallelisation, please select the number of used cores.",
                "\nThe system has ",
                detectCores(all.tests = T, logical = use_logical_cores),
                " available cores, of which up to ",
                detectCores(all.tests = T, logical = use_logical_cores) - 1,
                " cores may be used by this program."
            ),
            duration = DATA$notification_duration * 5,
            type = "warning"
        )
        if (isFALSE(FLAGS$restoring_state)) {
            updateNumericInput(session,inputId = "parallel_cores",value = 2)
        }
    } else {
        hide("PARALLEL_PANEL")
        updateNumericInput(session,inputId = "parallel_cores",value = 1)
        # I am terribly sorry for you, that you are reading this.
        # It's a horrible solution that _WILL_ bite me/you in the ass
        # if this app is extended at some point. Sadly, this conditional
        # was the only way I could figure out how do resolve this
        # properly.
        # For the full details, checkout the Event-callback for
        # `input%do_crop_image`, which must remain the last
        # event-callback to contain a message which should not show on
        # startup.
        if (isFALSE(STARTUP$startup)) {
            showNotification(
                ui = str_c("Disabled parallelisation, program will utilise 1 core."),
                duration = DATA$notification_duration,
                type = "warning"
            )
        }
    }
}
