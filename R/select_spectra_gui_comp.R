#' generate and call a modal-dialogue which lets you select which spectrums to analyse.
#'
#' Additionally, it queries the component `radio_analysis_type` to preselect common results for different types of analyses
#'
#' @param input `input`-shiny-component
#'
#' @return .
#' @keywords internal
#'
select_spectra_gui_comp <- function(input) {
    all_choices <- names(getOption("duflor.default_hsv_spectrums")$upper_bound)
    if (input$radio_analysis_type==1) { # GFA
        choices <- c("bex_identifier_dot","bex_green_HSV","bex_drought_HSV","bex_complete_HSV")
    } else {                            # WFA
        choices <- c("bex_identifier_dot","bex_root_HSV")
    }
    showModal(modalDialog(
        tags$h3('Choose which ranges to analyse'),
        # tags$h5('As a result, all images will be processed at full resolution. This is safer, but slower.'),
        footer=tagList(
            checkboxGroupInput("selected_spectra","Select spectra to analyse",choices = all_choices,selected = choices),
            actionButton('submit_selected_spectra', 'Submit choices'),
            modalButton('cancel')
        )
    ))
}
