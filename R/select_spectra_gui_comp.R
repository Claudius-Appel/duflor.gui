#' generate and call a modal-dialogue which lets you select which spectrums to analyse.
#'
#' Additionally, it queries the component `radio_analysis_type` to preselect common results for different types of analyses
#'
#' @inheritParams .main_args
#'
#' @return .
#' @keywords internal
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom shiny h3
#' @importFrom shiny tagList
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny actionButton
#' @importFrom shiny modalButton
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
            checkboxInput("do_save_masks","Save the spectrum-masks?"),
            checkboxInput("do_save_high_contrast_masks","Save the high-contrast-masks instead?"),
            actionButton('submit_selected_spectra', 'Submit choices'),
            modalButton('cancel')
        )
    ))
}
