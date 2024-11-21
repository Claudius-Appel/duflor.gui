#' restore application state from stored `.RDS`-file
#'
#' This function is responsible for
#'
#' @inheritParams .main_args
#' @param state_file filepath for to-be-loaded state_file
#' @keywords internal
#' @return path to the loaded file.
#'
#' @importFrom shiny updateRadioButtons
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateActionButton
#' @importFrom shiny updateCheckboxInput
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @importFrom shiny updateNumericInput
#' @importFrom shiny updateDateInput
#' @importFrom shiny renderText
#' @importFrom DT renderDataTable
#' @importFrom shinyFiles parseDirPath
#'
restore_state <- function(input, output, DATA, FLAGS, DEBUGKEYS, session, volumes, state_file) {
    input_state <- readRDS(state_file)
    ## Restore DATA
    DATA <- input_state$DATA
    DATA$r__tbl_dir_files <- input_state$DATA$r__tbl_dir_files
    DEBUGKEYS <- input_state$DEBUGKEYS
    FLAGS <- input_state$FLAGS


    ## restore imagetype
    updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "image_file_suffix",
        choices = c("JPG","PNG"),
        selected = input_state$input$image_file_suffix
    )

    ## restore analysis_type
    updateRadioButtons(inputId = "radio_analysis_type",
                       choices = list("GFA" = 1,
                                      "WFA" = 2),
                       selected = input_state$input$radio_analysis_type)
    ## restore HSV ranges
    updateSelectInput(
        session = getDefaultReactiveDomain(),inputId = "selected_HSV_spectrum",input_state$input$selected_HSV_spectrum
    )
    updateSelectInput(
        session = getDefaultReactiveDomain(),inputId = "selected_HSV_spectrum2",input_state$input$selected_HSV_spectrum2
    )
    ### restore folder_text
    output$ctrl_current_folder <- renderText({
        file_selected <- parseDirPath(roots = volumes, input_state$input$folder)
    })
    ## setup tbl_dir_files
    # we cannot restore from `input_state$input$folder` because the button might not have been pressed yet, and thus will return '1'
    loaded_path <- input_state$DATA$folder_path
    cat("\nLoading '",loaded_path,"'")
    buttons_to_toggle <- c(
        "render_plant",
        "select_crops",
        "select_identifiercrops",
        "execute_analysis",
        "execute_analysis_single"
    )
    for (each in buttons_to_toggle) {
        updateActionButton(session = getDefaultReactiveDomain(),inputId = each,disabled = FALSE)
    }
    ## restore bound_X
    DATA$spectrums <- input_state$DATA$spectrums
    spectrums <- input_state$DATA$spectrums
    updateNumericInput(session = getDefaultReactiveDomain(), inputId = "lower_bound_H", value = spectrums$lower_bound[[input$selected_HSV_spectrum]][1])
    updateNumericInput(session = getDefaultReactiveDomain(), inputId = "lower_bound_S", value = spectrums$lower_bound[[input$selected_HSV_spectrum]][2])
    updateNumericInput(session = getDefaultReactiveDomain(), inputId = "lower_bound_V", value = spectrums$lower_bound[[input$selected_HSV_spectrum]][3])
    updateNumericInput(session = getDefaultReactiveDomain(), inputId = "upper_bound_H", value = spectrums$upper_bound[[input$selected_HSV_spectrum]][1])
    updateNumericInput(session = getDefaultReactiveDomain(), inputId = "upper_bound_S", value = spectrums$upper_bound[[input$selected_HSV_spectrum]][2])
    updateNumericInput(session = getDefaultReactiveDomain(), inputId = "upper_bound_V", value = spectrums$upper_bound[[input$selected_HSV_spectrum]][3])
    ## restore CROPPING
    if (input_state$input$do_crop_image) {
        show("CROPPING_PANEL")
        updateNumericInput(session = getDefaultReactiveDomain(),"x0",value = input_state$input$x0)
        updateNumericInput(session = getDefaultReactiveDomain(),"x1",value = input_state$input$x1)
        updateNumericInput(session = getDefaultReactiveDomain(),"y1",value = input_state$input$y1)
        updateNumericInput(session = getDefaultReactiveDomain(),"y0",value = input_state$input$y0)
        updateCheckboxInput(session = getDefaultReactiveDomain(),inputId = "do_crop_image",value = input_state$input$do_crop_image)
    } else {
        hide("CROPPING_PANEL")
        updateCheckboxInput(session = getDefaultReactiveDomain(),inputId = "do_crop_image",value = input_state$input$do_crop_image)
    }
    ## restore IDENTCROPPING
    if (input_state$input$do_crop_identifier_range) {
        show("IDENTIFIERCROPPING_PANEL")
        updateNumericInput(session = getDefaultReactiveDomain(),"identifiersearch_x0",value = input_state$input$identifiersearch_x0)
        updateNumericInput(session = getDefaultReactiveDomain(),"identifiersearch_x1",value = input_state$input$identifiersearch_x1)
        updateNumericInput(session = getDefaultReactiveDomain(),"identifiersearch_y1",value = input_state$input$identifiersearch_y1)
        updateNumericInput(session = getDefaultReactiveDomain(),"identifiersearch_y0",value = input_state$input$identifiersearch_y0)
        updateCheckboxInput(session = getDefaultReactiveDomain(),inputId = "do_crop_identifier_range",value = input_state$input$do_crop_identifier_range)
    } else {
        hide("IDENTIFIERCROPPING_PANEL")
        updateCheckboxInput(session = getDefaultReactiveDomain(),inputId = "do_crop_identifier_range",value = input_state$input$do_crop_identifier_range)
    }

    ## restore PARALLEL
    FLAGS$restoring_state <- TRUE
    if (input_state$input$open_parallelPanel) {
        show("PARALLEL_PANEL")
        updateCheckboxInput(session = getDefaultReactiveDomain(),inputId = "open_parallelPanel",value = input_state$input$open_parallelPanel)
        updateNumericInput(session = getDefaultReactiveDomain(),inputId = "parallel_cores",value = input_state$input$parallel_cores)
    } else {
        hide("PARALLEL_PANEL")
        updateCheckboxInput(session = getDefaultReactiveDomain(),inputId = "open_parallelPanel",value = input_state$input$open_parallelPanel)
        updateNumericInput(session = getDefaultReactiveDomain(),inputId = "parallel_cores",value = 1)
    }
    ## restore DISTORTION
    FLAGS$restoring_state <- TRUE
    if (input_state$input$do_correct_distortion) {
        show("DISTORTION_PANEL")
        updateCheckboxInput(session = getDefaultReactiveDomain(),inputId = "do_correct_distortion",value = input_state$input$do_correct_distortion)
        updateNumericInput(session = getDefaultReactiveDomain(),inputId = "barrel_correction_factor",value = input_state$input$barrel_correction_factor)
    } else {
        hide("DISTORTION_PANEL")
        updateCheckboxInput(session = getDefaultReactiveDomain(),inputId = "do_correct_distortion",value = input_state$input$do_correct_distortion)
        updateNumericInput(session = getDefaultReactiveDomain(),inputId = "barrel_correction_factor",value = 1)
    }
    ## restore dateofshooting
    updateDateInput(
        session = getDefaultReactiveDomain(),
        inputId = "date_of_image_shooting",
        value = input_state$input$date_of_image_shooting
    )
    ## restore identifierarea
    updateNumericInput(
        session = getDefaultReactiveDomain(),
        inputId = "identifier_area",
        value = input_state$input$identifier_area
    )
    return(loaded_path)
}
