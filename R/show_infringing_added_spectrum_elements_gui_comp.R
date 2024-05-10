#' generate and call a modal-dialogue which displays the prospective changes in HSV-spectrums
#'
#' Additionally, it queries whether or not they should be applied, or reset to previous value.
#'
#' @param input - `input` respective shiny-component
#' @param DATA - `DATA` respective shiny-component
#' @param changes - results-object from [validate_custom_HSV_values()]
#'
#' @return .
#' @keywords internal
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom shiny h3
#' @importFrom shiny h4
#' @importFrom shiny h6
#' @importFrom shiny tagList
#' @importFrom shiny actionButton
#' @importFrom stringr str_c
show_infringing_added_spectrum_elements_gui_comp <- function(input, DATA, changes) {
    # decide which info to show
    H0_is_invalid <- ((input$lower_new_H<changes$expected_bounds$lower$H[[1]]) || (input$lower_new_H>changes$expected_bounds$lower$H[[2]]))
    S0_is_invalid <- ((input$lower_new_S<changes$expected_bounds$lower$S[[1]]) || (input$lower_new_S>changes$expected_bounds$lower$S[[2]]))
    V0_is_invalid <- ((input$lower_new_V<changes$expected_bounds$lower$V[[1]]) || (input$lower_new_V>changes$expected_bounds$lower$V[[2]]))

    H1_is_invalid <- ((input$upper_new_H<changes$expected_bounds$upper$H[[1]]) || (input$upper_new_H>changes$expected_bounds$upper$H[[2]]))
    S1_is_invalid <- ((input$upper_new_S<changes$expected_bounds$upper$S[[1]]) || (input$upper_new_S>changes$expected_bounds$upper$S[[2]]))
    V1_is_invalid <- ((input$upper_new_V<changes$expected_bounds$upper$V[[1]]) || (input$upper_new_V>changes$expected_bounds$upper$V[[2]]))
    # build the modal asking whether or not the changes are to be reversed, or if the range-limits should be used as bounds (as suggested)
    showModal(modalDialog(
        tags$h3('Provided values are invalid.'),
        tags$h4('Lower Bound: Invalid values must be replaced: '),
        tags$h6(ifelse(
            H0_is_invalid,
            str_c(
                "H: Range: [",
                changes$expected_bounds$lower$H[[1]],
                " - ",
                changes$expected_bounds$lower$H[[2]],
                "], Current H_0: '",
                input$lower_new_H,
                "', Previous Value: '",
                DATA$spectrums$lower_bound[[input$added_HSV_range_name]][[1]],
                "'"
            ),
            ""
        )),
        tags$h6(ifelse(
            S0_is_invalid,
            str_c(
                "S: Range: [",
                changes$expected_bounds$lower$S[[1]],
                " - ",
                changes$expected_bounds$lower$S[[2]],
                "], Current S_0: '",
                input$lower_new_S,
                "', Previous Value: '",
                DATA$spectrums$lower_bound[[input$added_HSV_range_name]][[2]],
                "'"
            ),
            ""
        )),
        tags$h6(ifelse(
            V0_is_invalid,
            str_c(
                "V: Range: [",
                changes$expected_bounds$lower$V[[1]],
                " - ",
                changes$expected_bounds$lower$V[[2]],
                "], Current V_0: '",
                input$lower_new_V,
                "', Previous Value: '",
                DATA$spectrums$lower_bound[[input$added_HSV_range_name]][[3]],
                "'"
            ),
            ""
        )),
        tags$h4('Upper Bound: Invalid values must be replaced: '),
        tags$h6(ifelse(
            H1_is_invalid,
            str_c(
                "H: Range: [",
                changes$expected_bounds$lower$H[[1]],
                " - ",
                changes$expected_bounds$lower$H[[2]],
                "], Current H_1: '",
                input$upper_new_H,
                "', Previous Value: '",
                DATA$spectrums$upper_bound[[input$added_HSV_range_name]][[1]],
                "'"
            ),
            ""
        )),
        tags$h6(ifelse(
            V1_is_invalid,
            str_c(
                "S: Range: [",
                changes$expected_bounds$lower$S[[1]],
                " - ",
                changes$expected_bounds$lower$S[[2]],
                "], Current S_1: '",
                input$upper_new_S,
                "', Previous Value: '",
                DATA$spectrums$upper_bound[[input$added_HSV_range_name]][[2]],
                "'"
            ),
            ""
        )),
        tags$h6(ifelse(
            V1_is_invalid,
            str_c(
                "V: Range: [",
                changes$expected_bounds$lower$V[[1]],
                " - ",
                changes$expected_bounds$lower$V[[2]],
                "], Current V_1: '",
                input$upper_new_V,
                "', Previous Value: '",
                DATA$spectrums$upper_bound[[input$added_HSV_range_name]][[3]],
                "'"
            ),
            ""
        )),
        # tags$h5('As a result, all images will be processed at full resolution. This is safer, but slower.'),
        footer=tagList(
            actionButton('confirm_coerced_added_HSV_values_modal', 'Set infringing values to closest limit'),
            actionButton('discard_coerced_added_HSV_values_modal', "Discard new changes")
        )
    ))
}
