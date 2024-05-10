#' gui component for adding HSV-range
#'
#' @param input - `input` respective shiny-component
#' @seealso [submit_added_HSV_range()]
#' @return .
#' @keywords internal
#'
add_HSV_range_gui_comp <- function(input) {
    showModal(modalDialog(
        tags$h3('Add additional spectra to analyse'),
        footer = tagList(
            textInput(inputId = "added_HSV_range_name", label = "Insert name of new spectrum"),
            numericInput(inputId = "lower_new_H", label = "H_0",value = 0, min = 0, max = 359, step = 0.01),
            numericInput(inputId = "lower_new_S", label = "S_0",value = 0, min = 0, max = 1, step = 0.01),
            numericInput(inputId = "lower_new_V", label = "V_0",value = 0, min = 0, max = 1, step = 0.0001),
            numericInput(inputId = "upper_new_H", label = "H_1",value = 0, min = 0, max = 359, step = 0.01),
            numericInput(inputId = "upper_new_S", label = "S_1",value = 0, min = 0, max = 1, step = 0.01),
            numericInput(inputId = "upper_new_V", label = "V_1",value = 0, min = 0, max = 1, step = 0.0001),
            actionButton('submit_added_HSV_range', 'Submit choices'),
            modalButton('cancel')
        )
    ))
}
