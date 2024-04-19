select_spectra_gui_comp <- function(input) {
    all_choices <- names(getOption("duflor.default_hsv_spectrums")$upper_bound)
    if (input$radio_analysis_type==1) { # GFA
        choices_GFA <- c("bex_identifier_dot","bex_green_HSV","bex_drought_HSV","bex_complete_HSV")
        showModal(modalDialog(
            tags$h3('Choose which ranges to analyse'),
            # tags$h5('As a result, all images will be processed at full resolution. This is safer, but slower.'),
            footer=tagList(
                checkboxGroupInput("selected_spectra","Select spectra to analyse",choices = all_choices,selected = choices_GFA),
                actionButton('submit_selected_spectra', 'Submit choices'),
                modalButton('cancel')
            )
        ))
    } else {
        choices_WFA <- c("bex_identifier_dot","bex_root_HSV")
        showModal(modalDialog(
            tags$h3('Choose which ranges to analyse'),
            # tags$h5('As a result, all images will be processed at full resolution. This is safer, but slower.'),
            footer=tagList(
                checkboxGroupInput("selected_spectra","Select spectra to analyse",choices = all_choices,selected = choices_WFA),
                actionButton('submit_selected_spectra2', 'Submit choices'),
                modalButton('cancel')
            )
        ))
    }
}
