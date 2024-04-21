#' Start the duflor-gui via this function
#'importFrom shiny
#' importFrom stringr str_
#' importFrom stringr str_
#'
#'
#' @importFrom shiny fluidPage
#' @importFrom shiny titlePanel
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny selectInput
#' @importFrom shinyFiles shinyDirButton
#' @importFrom shiny shinyApp
#' @importFrom shiny h4
#' @importFrom shiny h5
#' @importFrom shiny radioButtons
#' @importFrom shiny actionButton
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny showModal
#' @importFrom shiny removeModal
#' @importFrom shiny numericInput
#' @importFrom shiny updateNumericInput
#' @importFrom shiny passwordInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny dateInput
#' @importFrom shiny textOutput
#' @importFrom shiny mainPanel
#' @importFrom shiny tabsetPanel
#' @importFrom shiny tabPanel
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny reactiveValues
#' @importFrom shiny reactive
#' @importFrom shiny observeEvent
#' @importFrom shiny debounce
#' @importFrom shiny tagList
#' @importFrom shiny tags
#' @importFrom shiny req
#' @importFrom shiny renderText
#' @importFrom shiny showNotification
#' @importFrom shiny removeNotification
#' @importFrom shiny isolate
#' @importFrom shiny conditionalPanel
#' @importFrom shiny a
#' @importFrom shinyFiles shinyDirButton
#' @importFrom shinyFiles shinyDirChoose
#' @importFrom shinyFiles parseDirPath
#' @importFrom shinyFiles getVolumes
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @importFrom stringr str_count
#' @importFrom stringr str_split
#' @importFrom stringr str_c
#' @importFrom stringr str_flatten_comma
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_replace_all
#' @importFrom DT renderDataTable
#' @importFrom DT dataTableOutput
#' @importFrom parallel detectCores
#' @importFrom foreach getDoParRegistered
#' @importFrom stats df
#' @importFrom imager draw_rect
#' @importFrom imager grabRect
#' @importFrom duflor load_image
#' @return .
#' @export
#'
duflor_gui <- function() {
    use_logical_cores <- F
    ##### UI ####
        ui <- fluidPage(
        # App title
        titlePanel("Hello Shiny!"),
        useShinyjs(),
        # Sidebar layout with input and output definitions
        sidebarLayout(

            # Sidebar panel for inputs
            sidebarPanel(
                ## FILES
                h4("Select folder containing images"),
                shinyDirButton(id = 'folder', label = 'Select a folder',title = 'Please select a folder',buttonType = FALSE),
                selectInput(inputId = "image_file_suffix",label = "Select imagetype to process",choices = c("JPG","PNG")),
                "Current Folder:",
                textOutput(outputId = "ctrl_current_folder"),
                ## ANALYSIS_TYPE
                radioButtons(inputId = "radio_analysis_type",
                           h4("Type of Analysis"),
                           choices = list("GFA" = 1,
                                          "WFA" = 2),
                           selected = 1),
                ## BUTTONS_1
                actionButton(inputId = "render_plant",label = "Render Plant/Select subset area"),
                actionButton(inputId = "open_edit_HSV_ranges_conditionalPanel",label = "Edit HSV Ranges"),
                ## CONFIGURE_HSV_BOUNDS
                conditionalPanel(
                    condition = "input.open_edit_HSV_ranges_conditionalPanel %% 2 == 1", # Condition to open the panel
                    id = "HSV_PANEL",
                    selectInput("selected_HSV_spectrum", "Select spectrum to edit.", choices = names(getOption("duflor.default_hsv_spectrums")$lower_bound)),
                    numericInput(inputId = "lower_bound_H",label = "Lower Bound (H)", value = 0, min = 0, max = 360, step = 0.01),
                    numericInput(inputId = "lower_bound_S",label = "Lower Bound (S)", value = 0, min = 0, max = 1, step = 0.01),
                    numericInput(inputId = "lower_bound_V",label = "Lower Bound (V)", value = 0, min = 0, max = 1, step = 0.0001),
                    numericInput(inputId = "upper_bound_H",label = "Upper Bound (H)", value = 0, min = 0, max = 360, step = 0.01),
                    numericInput(inputId = "upper_bound_S",label = "Upper Bound (S)", value = 0, min = 0, max = 1, step = 0.01),
                    numericInput(inputId = "upper_bound_V",label = "Upper Bound (V)", value = 0, min = 0, max = 1, step = 0.0001),
                    actionButton("close_edit_HSV_ranges_conditionalPanel", "Submit changed HSV-spectra"),
                    useShinyjs() # Enable shinyjs inside the conditional panel
                ),
                ## CROPPING TO_BE_ANALYSED MATRIX
                h4("Crop Image"),
                checkboxInput(inputId = "do_crop_image",label = "Do you want to analyse only a cropped section?"),
                conditionalPanel(
                    condition = "input.do_crop_image %% 2 == 1",
                    id = "CROPPING_PANEL",
                    actionButton(inputId = "reset_crops", label = "Reset"),
                    numericInput(inputId = "x0",label = "x0",value = 0,min = 0),
                    numericInput(inputId = "x1",label = "x1",value = 0,min = 0),
                    numericInput(inputId = "y0",label = "y0",value = 0,min = 0),
                    numericInput(inputId = "y1",label = "y1",value = 0,min = 0),
                ),


                ## PARALLELISATION
                h4("Parallel Processing"),
                checkboxInput(inputId = "open_parallelPanel",label = "Run analysis in parallel?"),
                conditionalPanel(
                    condition = "input.open_parallelPanel %% 2 == 1",
                    id = "PARALLEL_PANEL",
                    numericInput(inputId = "parallel_cores",label = "Designate number of cores",value = 1, min = 1,max = (detectCores(logical = use_logical_cores) - 1)),
                ),
                ## BUTTONS_2
                actionButton(inputId = "execute_analysis",label = "Execute Analysis"),
                actionButton(inputId = "execute_analysis_single",label = "Execute Analysis (single)"),
                ## MISCELLANEOUS STUFF
                h5("Misc"),
                passwordInput(inputId = "dev_pass",label = "Dev-console",placeholder = "enter '-h' for a list of valid commands"),
                dateInput(inputId = "date_of_image_shooting",label = "Select date the images were shot",value = NULL,format = "yyyy-mm-dd",weekstart = 1,startview = "month",language = "en",autoclose = T)
            ),

            # Main panel for displaying outputs
            mainPanel(
                tabsetPanel(id = "tabset_panel",
                  tabPanel("Image Files",verbatimTextOutput("Image Files"),dataTableOutput("tbl_dir_files")),
                  tabPanel("Results",verbatimTextOutput("Analytics (red dot deviations?)"),dataTableOutput("tbl_results"))
                  #tabPanel("Analytics (misc1)",verbatimTextOutput("TAB3")),
                  #tabPanel("Analytics (misc2)",verbatimTextOutput("TAB4")),
                  #tabPanel("Analytics (misc3)",verbatimTextOutput("TAB5"))
                )

            ),
        )
    )
    #### SERVER ####
    server <- function(input, output,session) {
        #### INIT VARIABLES ####
        DATA <- reactiveValues(          #  nomenclature: reactives start with "r__"
            r__tbl_dir_files  = NA,
            r__img_type = "PNG",
            r__KPI_type = 1,
            r__tbl_dir_files_selectedrow = NA,
            # r__render_plant = 0,
            preview_img = NA,
            spectrums = getOption("duflor.default_hsv_spectrums"),
            notification_duration = 1.300
        )
        DEBUGKEYS <- reactiveValues(
            force.prints = FALSE,
            force.log = FALSE,
            set.author = FALSE

        )
        FLAGS <- reactiveValues(
            analyse_single_image = FALSE
        )
        ranges <- reactiveValues(
            x = NULL,
            y = NULL
        )
        # to make values only trigger reactives after x seconds of non-interaction, first assign them reactive.
        # next, assign a debounce-expression with a set timout after which the value is hnaded onwards to the reactive-pipeline
        # finally, refer to the debounce-expression via `expression()` instead of the input-value `input$value` in callbacks.
        volumes <- getVolumes()()
        #### DISPLAYING AND RENDERING FILES AND STUFF ####
        image_files <- reactive({ # image_files is a list of filepaths, which gets set reactively.
            req(input$folder,input$image_file_suffix,input$radio_analysis_type)
            shinyDirChoose(input = input, 'folder', roots=volumes)
            folder_path <- parseDirPath(roots = volumes,input$folder) # this is how you conver thte shinydirselection-objet to a valid path. cf: https://search.r-project.org/CRAN/refmans/shinyFiles/html/shinyFiles-parsers.html
            req(folder_path) ## make sure the rest of this react is only executed if 'folder_path' is set
            if (dir.exists(folder_path)) {
                images_ <- list.files(folder_path,pattern = paste0("*.(",str_to_lower(input$image_file_suffix),"|",str_to_upper(input$image_file_suffix),")"),recursive = F,full.names = T)
                if (length(images_)>0) {
                    images_filtered <- images_[!str_count(basename(images_),"_")]
                    ret <- as.data.frame(images_filtered) # TODO: see here for paginated tables in shiny-apps https://stackoverflow.com/questions/50043152/r-shiny-how-to-add-pagination-in-dtrenderdatatable
                    ret$count <- c(1:1:dim(ret)[1])
                    DATA$r__tbl_dir_files <- ret
                    return(ret)
                } else {
                    showNotification(
                        ui = str_c(
                            "No '.",
                            input$image_file_suffix,
                            "'-files found in folder '",
                            folder_path,"'."
                        ),
                        duration = DATA$notification_duration * 5,
                        type = "warning"
                    )
                    ret <- data.frame(images_filtered = character(),
                                                  count = numeric(),
                                                  stringsAsFactors = FALSE)
                    return(ret)
                }
            }
            return(df()) # return empty df in case no folder was selected (yet)
        })
        output$ctrl_current_folder <- renderText({
            file_selected <- parseDirPath(roots = volumes, input$folder)
        })

        #TODO: figure out how to make selecting rows in this table responsive:
        # after executing 'execute_analysis', the plot-area above should render the
        # parameter set in 'r__KPI_type', by default for all entries
        # selecting entries in the table should render the respective KPI for these
        # images only.
        output$tbl_dir_files <- renderDataTable({
            image_files()},
            server = TRUE,
            selection = "single",
            options = list(
                paging = TRUE,
                pageLength = 15,
                autoWidth = TRUE
            )
        )
        ### selected elements of the DT::renderDataTable() can be accessed in server via `input$tableID_rows_selected` - cf. https://clarewest.github.io/blog/post/making-tables-shiny/

        #### HIDE_PANELS_BY_DEFAULT ####
        hide("HSV_PANEL")
        hide("CROPPING_PANEL")
        hide("PARALLEL_PANEL")
        #### DEV TOGGLES ####
        observeEvent(input$dev_pass, {
            # add valid keys here
            # private, undocumentable keys must be prefixed with 3 `-`
            Arr <- str_split("--force-prints,--force-log,---set-author,-h",",")
            # then add them to the reactive 'DEBUGKEYS' so that it can be accessed elsewhere as well.
            Keys <- str_split(input$dev_pass,",")
            for (each in Keys) {
                if (each %in% Arr[[1]]) {
                    if (each=="-h") {
                        showNotification(
                            ui = str_c(
                                "Available dev Keys (see documentation): ",
                                str_flatten_comma(Arr[[1]][!str_count(Arr[[1]], "---")])
                            ),
                            duration = DATA$notification_duration,
                            type = "message"
                        )
                    } else {
                        key <- str_replace_all(str_replace_all(each,"---",""),"--","")
                        key <- str_replace_all(key,"-",".")
                        DEBUGKEYS[[key]] <- !DEBUGKEYS[[key]]
                        showNotification(
                            ui = str_c("DEBUG KEY ", " ", each, " set to ", DEBUGKEYS[[key]]),
                            duration = DATA$notification_duration,
                            type = "message"
                        )
                    }
                }

            }
        })
        #### SETUP PARALLELISATION ####
        observeEvent(input$open_parallelPanel, {
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
            } else {
                hide("PARALLEL_PANEL")
                updateNumericInput(session,inputId = "parallel_cores",value = 1)
                showNotification(
                    ui = str_c("Disabled parallelisation, program will utilise 1 core."),
                    duration = DATA$notification_duration,
                    type = "warning"
                )
            }
        })
        #### EDIT CROPPING ####
        observeEvent(input$do_crop_image, {
            #TODO: edit HSV ranges loaded from duflor-package
            print(input$do_crop_image)
            if (input$do_crop_image) {
                show("CROPPING_PANEL")
                showNotification(
                    ui = str_c(
                        "Enabled cropping. After being loaded, the image-matrix will be cropped by the values selected below before being processed."
                    ),
                    action = a(href = "https://www.google.com", "google"),
                    type = "message"
                )
            } else {
                hide("CROPPING_PANEL")
                showNotification(
                    ui = str_c(
                        "Disabled cropping. After being loaded, the complete image-matrix will be processed."
                    ),
                    type = "message"
                )
            }
        })
        observeEvent(input$reset_crops, {
            showModal(modalDialog(
                tags$h3('Do you want to reset the croppings?'),
                tags$h5('As a result, all images will be processed at full resolution. This is safer, but slower.'),
                footer=tagList(
                    actionButton('submit_reset_crops', 'Reset'),
                    modalButton('cancel')
                )
            ))
        })
        observeEvent(input$submit_reset_crops, {
            removeModal()
            showNotification(ui = str_c("not implemented: reset crops to 0/0/0/0"),
                             type = "message")
            updateNumericInput(session,"x0",value = 0)
            updateNumericInput(session,"x1",value = 0)
            updateNumericInput(session,"y1",value = 0)
            updateNumericInput(session,"y0",value = 0)
        })
        #### EDIT HSV RANGES ####
        observeEvent(input$open_edit_HSV_ranges_conditionalPanel, {
            #TODO: edit HSV ranges loaded from duflor-package
            show("HSV_PANEL")
            default_HSV_spectrums <- getOption("duflor.default_hsv_spectrums")
            # 1. create a modal for each of these spectrums
            # 2.
        })
        observeEvent(input$selected_HSV_spectrum, {
            ## load spectrums in
            spectrums <- DATA$spectrums
            updateNumericInput(session, inputId = "lower_bound_H", value = spectrums$lower_bound[[input$selected_HSV_spectrum]][1])
            updateNumericInput(session, inputId = "lower_bound_S", value = spectrums$lower_bound[[input$selected_HSV_spectrum]][2])
            updateNumericInput(session, inputId = "lower_bound_V", value = spectrums$lower_bound[[input$selected_HSV_spectrum]][3])
            updateNumericInput(session, inputId = "upper_bound_H", value = spectrums$upper_bound[[input$selected_HSV_spectrum]][1])
            updateNumericInput(session, inputId = "upper_bound_S", value = spectrums$upper_bound[[input$selected_HSV_spectrum]][2])
            updateNumericInput(session, inputId = "upper_bound_V", value = spectrums$upper_bound[[input$selected_HSV_spectrum]][3])
        })
        observeEvent(input$reset_HSV_ranges, {

        })
        observeEvent(input$close_edit_HSV_ranges_conditionalPanel, {
            hide("HSV_PANEL")
            DATA$spectrums$lower_bound[[input$selected_HSV_spectrum]] <- c(input$lower_bound_H,input$lower_bound_S,input$lower_bound_V)
            DATA$spectrums$upper_bound[[input$selected_HSV_spectrum]] <- c(input$upper_bound_H,input$upper_bound_S,input$upper_bound_V)
            showNotification(
                ui = str_c(
                    "Updated values for spectrum '",
                    input$selected_HSV_spectrum,
                    "'"
                ),
                type = "message"
            )

        })
        #### RENDER PLOT ####
        observeEvent(input$render_plant, {
            req(DATA$r__tbl_dir_files,input$tbl_dir_files_rows_selected)

            selectedrowindex <- as.numeric(input$tbl_dir_files_rows_selected[length(input$tbl_dir_files_rows_selected)])
            DATA$r__tbl_dir_files_selectedrow <- selectedrow <- (DATA$r__tbl_dir_files[selectedrowindex,])
            showNotification(
                ui = str_c("loading ", " ", selectedrow$images_filtered),
                duration = DATA$notification_duration,
                type = "message"
            )

            im <- load_image(selectedrow$images_filtered,subset_only = F,return_hsv = F)
            dims <- dim(im)
            if ((input$x0!=0) && (input$x1!=0)  && (input$y0!=0)  && (input$y1!=0))  { # add previously selected rect to new image

                im <- draw_rect(
                    im,
                    x0 = input$x0,
                    x1 = input$x1,
                    y0 = input$y0,
                    y1 = input$y1,
                    color = "red",
                    opacity = 0.25,
                    filled = T
                )
            }
            rect <- grabRect(im)
            if (sum(rect)>0) {
                # DATA$rect <- rect
                showNotification(
                    ui = str_c(
                        "Only pixels within the rectange defined below will be analysed:",
                        "\nx0: ",
                        rect["x0"],
                        " x1: ",
                        rect["x1"],
                        "\n",
                        "y0: ",
                        rect["y0"],
                        " y1: ",
                        rect["y1"]
                    ),
                    duration = DATA$notification_duration * 5,
                    type = "warning"
                )
                cl <- rect["x0"]
                cr <- rect["x1"]
                ct <- rect["y0"]
                cb <- rect["y1"]
                updateNumericInput(session,"x0",value = as.integer(cl))
                updateNumericInput(session,"x1",value = as.integer(cr))
                updateNumericInput(session,"y1",value = as.integer(cb))
                updateNumericInput(session,"y0",value = as.integer(ct))
            }
        })
        #### MAIN CALLBACK>EXECUTE DUFLOR_PACKAGE HERE ####
        observeEvent(input$execute_analysis_single, {
            isolate(FLAGS$analyse_single_image)
            FLAGS$analyse_single_image <- TRUE
            showNotification(
                ui = str_c(
                    "not implemented: in this scenario, we might consider displaying the resulting masks.\nIn normal execution, we do not display anything but the results at the end."
                ),
                duration = DATA$notification_duration,
                type = "warning"
            )
            select_spectra_gui_comp(input)
        })
        observeEvent(input$execute_analysis, {  ## to access output-variables at all, you must ingest them into a reactive-object before retrieving them from there.
            isolate(FLAGS$analyse_single_image)
            FLAGS$analyse_single_image <- FALSE
            select_spectra_gui_comp(input)
        })
        observeEvent(input$submit_selected_spectra, {
            removeModal()
            spectrums <- DATA$spectrums
            spectrums$lower_bound <- duflor:::remove_key_from_list(DATA$spectrums$lower_bound,names(DATA$spectrums$lower_bound)[!(names(DATA$spectrums$lower_bound) %in% input$selected_spectra)])
            spectrums$upper_bound <- duflor:::remove_key_from_list(DATA$spectrums$upper_bound,names(DATA$spectrums$lower_bound)[!(names(DATA$spectrums$lower_bound) %in% input$selected_spectra)])
            DATA$spectrums <- spectrums
            #TODO: add modal "analysis is ongoing, please wait"
            showNotification(
                ui = "Analysis ongoing.",
                id = "analysis.ongoing",
                duration = NULL,
                type = "warning"
            )
            results <- execute_analysis(input,DATA,DEBUGKEYS,FLAGS)
            removeNotification(id = "analysis.ongoing")
            if (results$file_state$success) {
                showNotification(
                    ui = "Analysis completed. Results have been written to '",
                    results$file_state$results_path,
                    "'",
                    duration = DATA$notification_duration,
                    type = "message"
                )
            } else {
                showNotification(
                    ui = "Analysis could not be completed successfully, and results could not be successfully written to",
                    results$file_state$results_path,
                    "'",
                    duration = DATA$notification_duration * 4,
                    type = "warning"
                )
            }
            #TODO: remove modal "analysis is ongoing, please wait", add modal (analysis has finished, please inspect results)
            # RENDER RESULTS OBJECT
            output$tbl_results <- renderDataTable({
                results$results},
                server = TRUE,
                selection = "single",
                options = list(
                    paging = TRUE,
                    pageLength = 15,
                    autoWidth = TRUE
                )
            )
        })
    }
    shinyApp(ui = ui, server = server)
}
