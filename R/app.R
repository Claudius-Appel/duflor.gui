#' Start the duflor-gui via this function
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
#' @importFrom shiny fileInput
#' @importFrom shiny setBookmarkExclude
#' @importFrom shiny updateNumericInput
#' @importFrom shiny updateActionButton
#' @importFrom shiny updateSelectInput
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shiny textInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny dateInput
#' @importFrom shiny textOutput
#' @importFrom shiny mainPanel
#' @importFrom shiny tabsetPanel
#' @importFrom shiny tabPanel
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny reactiveValues
#' @importFrom shiny reactive
#' @importFrom shiny observeEvent
#' @importFrom shiny debounce
#' @importFrom shiny tagList
#' @importFrom shiny tags
#' @importFrom shiny req
#' @importFrom shiny renderText
#' @importFrom shiny renderPlot
#' @importFrom shiny showNotification
#' @importFrom shiny removeNotification
#' @importFrom shiny isolate
#' @importFrom shiny conditionalPanel
#' @importFrom shiny a
#' @importFrom shiny onBookmark
#' @importFrom shinyFiles shinyFileSave
#' @importFrom shinyFiles shinySaveButton
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
#' @importFrom DT renderDataTable
#' @importFrom DT dataTableOutput
#' @importFrom parallel detectCores
#' @importFrom foreach getDoParRegistered
#' @importFrom foreach getDoParWorkers
#' @importFrom stats df
#' @importFrom imager draw_rect
#' @importFrom imager grabRect
#' @importFrom imager display
#' @importFrom duflor load_image
#' @importFrom duflor extract_pixels_HSV
#' @importFrom duflor apply_HSV_color_by_mask
#' @importFrom duflor HSVtoRGB
#' @importFrom utils packageDescription
#' @return .
#' @export
#'
duflor_gui <- function() {
    use_logical_cores <- F
    ##### UI ####
    ui <- function(request) {
        fluidPage(
            # App title
            titlePanel(str_c("duflor frontend v.",packageDescription("duflor.gui")$Version),windowTitle = str_c("duflor_gui v.",packageDescription("duflor.gui")$Version)),
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
                    actionButton(inputId = "open_edit_HSV_ranges_conditionalPanel",label = "Edit HSV Ranges"),
                    ## CONFIGURE_HSV_BOUNDS
                    conditionalPanel(
                        condition = "input.open_edit_HSV_ranges_conditionalPanel %% 2 == 1", # Condition to open the panel
                        id = "HSV_PANEL",
                        selectInput("selected_HSV_spectrum", "Select spectrum to edit.", choices = names(getOption("duflor.default_hsv_spectrums")$lower_bound)),
                        numericInput(inputId = "lower_bound_H",label = "Lower Bound (H_0)", value = 0, min = 0, max = 359, step = 0.01),
                        numericInput(inputId = "lower_bound_S",label = "Lower Bound (S_0)", value = 0, min = 0, max = 1, step = 0.01),
                        numericInput(inputId = "lower_bound_V",label = "Lower Bound (V_0)", value = 0, min = 0, max = 1, step = 0.0001),
                        numericInput(inputId = "upper_bound_H",label = "Upper Bound (H_1)", value = 0, min = 0, max = 359, step = 0.01),
                        numericInput(inputId = "upper_bound_S",label = "Upper Bound (S_1)", value = 0, min = 0, max = 1, step = 0.01),
                        numericInput(inputId = "upper_bound_V",label = "Upper Bound (V_1)", value = 0, min = 0, max = 1, step = 0.0001),
                        actionButton(inputId = "reset_HSV_ranges", label = "Reset"),
                        actionButton("close_edit_HSV_ranges_conditionalPanel", "Submit changed spectra"),
                        useShinyjs() # Enable shinyjs inside the conditional panel
                    ),
                    ## CROPPING TO_BE_ANALYSED MATRIX
                    h4("Crop Image"),
                    checkboxInput(inputId = "do_crop_image",label = "Do you want to analyse only a cropped section?"),
                    conditionalPanel(
                        condition = "input.do_crop_image %% 2 == 1",
                        id = "CROPPING_PANEL",
                        actionButton(inputId = "select_crops",label = "Select area to analyse"),
                        actionButton(inputId = "reset_crops", label = "Reset"),
                        numericInput(inputId = "x0",label = "x0",value = 0,min = 0),
                        numericInput(inputId = "x1",label = "x1",value = 0,min = 0),
                        numericInput(inputId = "y0",label = "y0",value = 0,min = 0),
                        numericInput(inputId = "y1",label = "y1",value = 0,min = 0),
                    ),
                    ## LIMIT SEARCH-RANGE FOR IDENTIFIER_DOT
                    h4("Limit area searched for identifier-dot"),
                    checkboxInput(inputId = "do_crop_identifier_range",label = "Do you want to limit the area in which to search for the identifier-dot?"),
                    conditionalPanel(
                        condition = "input.do_crop_identifier_range %% 2 == 1",
                        id = "IDENTIFIERCROPPING_PANEL",
                        actionButton(inputId = "select_identifiercrops",label = "Select area to analyse"),
                        actionButton(inputId = "reset_identifiercrops", label = "Reset"),
                        numericInput(inputId = "identifiersearch_x0",label = "x0",value = 0,min = 0),
                        numericInput(inputId = "identifiersearch_x1",label = "x1",value = 0,min = 0),
                        numericInput(inputId = "identifiersearch_y0",label = "y0",value = 0,min = 0),
                        numericInput(inputId = "identifiersearch_y1",label = "y1",value = 0,min = 0),
                    ),


                    ## PARALLELISATION
                    h4("Parallel Processing"),
                    checkboxInput(inputId = "open_parallelPanel",label = "Run analysis in parallel?"),
                    conditionalPanel(
                        condition = "input.open_parallelPanel %% 2 == 1",
                        id = "PARALLEL_PANEL",
                        numericInput(inputId = "parallel_cores",label = "Designate number of cores",value = 1, min = 1,max = (detectCores(logical = use_logical_cores) - 1)),
                    ),
                    ## MISCELLANEOUS STUFF
                    h5("Misc"),
                    textInput(inputId = "dev_pass",label = "Dev-console",placeholder = "enter '-h' for a list of valid commands"),
                    dateInput(inputId = "date_of_image_shooting",label = "Select date the images were shot",value = NULL,format = "yyyy-mm-dd",weekstart = 1,startview = "month",language = "en",autoclose = T),
                    numericInput(inputId = "identifier_area", label = "insert area of identifier-dot in cm^2", value = 0.503,min = 0,step = 0.00001),
                    ## BUTTONS_2
                    actionButton(inputId = "execute_analysis",label = "Execute Analysis"),
                    actionButton(inputId = "execute_analysis_single",label = "Execute Analysis (single)"),
                    ## BOOKMARKING
                    h5("BOOKMARKING"),
                    fileInput(inputId = "restore_state",label = "Load State", multiple = F, accept = c(".rds",".RDS")),
                    shinySaveButton(id = "save_state", label = "Save State",title = "Select filename",filetype = list(state = c(".rds",".RDS")))
                ),
                # Main panel for displaying outputs
                mainPanel(
                    tabsetPanel(
                        id = "tabset_panel",
                        tabPanel(
                            "Image Files"
                            ,dataTableOutput("tbl_dir_files")
                            ,actionButton(inputId = "render_plant",label = "Render Plant",disabled = TRUE)
                        ),
                        tabPanel(
                            "Results - complete"
                           ,dataTableOutput("tbl_results")
                           ,checkboxInput(inputId = "save_as_xlsx",label = "Save results as xlsx?",value = FALSE)
                           ,actionButton(inputId = "save_results","Save results",disabled = TRUE)
                        ),
                      tabPanel(
                          "Results - inspect"
                           ,selectInput(inputId = "reinspected_spectrums",label = "Select spectrum to inspect",choices = c())
                           ,dataTableOutput("tbl_results_filtered")
                           ,checkboxInput(inputId = "mask_extreme", label = "Do a high-contrast mask?", value = FALSE)
                           ,actionButton(inputId = "render_selected_mask",label = "Render masks for selected image",disabled = TRUE)
                        ),
                      tabPanel(
                          "Results - plots"
                           ,selectInput(inputId = "reinspected_spectrums2",label = "Select spectrum to inspect",choices = c())
                           ,selectInput(inputId = "reinspected_type2",label = "Select KPI to inspect",choices = c("_fraction","_count","_area","area_per_pixel"))
                           ,plotOutput("results_visualisation_plot")
                           ,actionButton(inputId = "save_visualisation_plot", label = "Save plot", disabled = TRUE)
                        ),
                    )
                )
            )
        )
    }
    #### SERVER ####
    server <- function(input, output,session) {
        #### STARTUP MESSAGE ####
        options(shiny.maxRequestSize=300*1024^2) # 300 MB of upload request size
        showNotification(
            ui = "App startup",
            id = "startup.notice",
            type = "message"
        )
        #### INIT VARIABLES ####
        DATA <- reactiveValues(          #  nomenclature: reactives start with "r__"
            r__tbl_dir_files  = NA,
            r__img_type = "PNG",
            r__KPI_type = 1,
            r__tbl_dir_files_selectedrow = NA,
            # r__render_plant = 0,
            preview_img = NA,
            spectrums = getOption("duflor.default_hsv_spectrums"),
            notification_duration = 1.300,
            results = NA,
            last_masked_image = NA,
            last_im = NA,
            folder_path = NA
        )
        DEBUGKEYS <- reactiveValues(
            force.prints = FALSE,
            force.log = FALSE,
            set.author = TRUE

        )
        FLAGS <- reactiveValues(
            analyse_single_image = FALSE,
            restoring_state = FALSE
        )
        image_files <- reactiveValues(image_files = data.frame(
            images_filtered = character(),
            index = numeric(),
            stringsAsFactors = FALSE
        ))
        # to make values only trigger reactives after x seconds of non-interaction, first assign them reactive.
        # next, assign a debounce-expression with a set timout after which the value is hnaded onwards to the reactive-pipeline
        # finally, refer to the debounce-expression via `expression()` instead of the input-value `input$value` in callbacks.
        volumes <- getVolumes()()
        #### DISPLAYING AND RENDERING FILES AND STUFF ####
        image_files_ <- reactive({ # image_files is a list of filepaths, which gets set reactively.
            folder_path <- DATA$folder_path # this is how you conver thte shinydirselection-objet to a valid path. cf: https://search.r-project.org/CRAN/refmans/shinyFiles/html/shinyFiles-parsers.html
            # all buttons listed here must be disabled by default
            # they will be enabled if the `list.files()` returns a non-empty vector of files.
            buttons_to_toggle <- c(
                    "render_plant",
                    "select_crops",
                    "select_identifiercrops",
                    "execute_analysis",
                    "execute_analysis_single"
                )
            for (each in buttons_to_toggle) {
                updateActionButton(session = getDefaultReactiveDomain(),inputId = each,disabled = TRUE)
            }
            if (dir.exists(folder_path)) {
                ## we do not recurse to force all input-files to be in the same level
                images_ <- list.files(folder_path,pattern = paste0("*.(",str_to_lower(input$image_file_suffix),"|",str_to_upper(input$image_file_suffix),")"),recursive = F,full.names = T)
                if (length(images_)>0) {
                    images_filtered <- images_ ##[!str_count(basename(images_),"_")]# BUG: WHY WAS THIS HERE?
                    ret <- as.data.frame(images_filtered)
                    ret$index <- c(1:1:dim(ret)[1])
                    DATA$r__tbl_dir_files <- ret
                    for (each in buttons_to_toggle) {
                        updateActionButton(session = getDefaultReactiveDomain(),inputId = each,disabled = FALSE)
                    }
                    image_files$image_files <- ret
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
                                                  index = numeric(),
                                                  stringsAsFactors = FALSE)
                    image_files$image_files <- ret
                }
            }
        })
        output$ctrl_current_folder <- renderText({
            file_selected <- parseDirPath(roots = volumes, input$folder)
        })
        output$tbl_dir_files <- renderDataTable({
            image_files$image_files},
            server = TRUE,
            selection = "single",
            options = list(
                paging = TRUE,
                pageLength = 15,
                autoWidth = TRUE
            )
        )
        observeEvent(input$image_file_suffix, {
            req(isFALSE(is.na(DATA$folder_path)))
            req(dir.exists(DATA$folder_path))
            req(input$image_file_suffix) # image_files_
            image_files_()
        })
        observeEvent(input$folder, {
            req(input$folder[[1]],input$image_file_suffix)
            shinyDirChoose(input = input, 'folder', roots=volumes)
            folder_path <- parseDirPath(roots = volumes,input$folder) # this is how you conver thte shinydirselection-objet to a valid path. cf: https://search.r-project.org/CRAN/refmans/shinyFiles/html/shinyFiles-parsers.html
            req(dir.exists(folder_path))
            DATA$folder_path <- folder_path
            image_files_()
        })
        #### REACTIVE - RESULTS_TABLE/BARPLOT, FILTERED BY SPECTRUM ####
        filtered_results <- reactive({
            req(input$reinspected_spectrums)
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            tryCatch({
                if (is.na(DATA$results)) { # handle empty DATA$results (this cannot be done via `req()` because `DATA$results` is initialised at startup)
                    ret <- data.frame(image_name = character(),
                                      stringsAsFactors = FALSE)
                    return(ret)
                }
                RESULTS <- DATA$results
                available_columns <- names(DATA$results$results)
                static_columns <- c("image_name","date_of_analysis","processed_width","processed_height","area_per_pixel")
                dynamic_columns_idx <- grep(input$reinspected_spectrums,available_columns)
                dynamic_columns <- available_columns[dynamic_columns_idx]
                total_columns <- c(static_columns,dynamic_columns)
                filtered_table <- RESULTS$results[,total_columns]
                return(filtered_table)
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "filtered_results"
                )
                showNotification(
                    ui = str_c("Error occured during reactive 'filtered_results'. The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
        })
        output$tbl_results_filtered <- renderDataTable({
            filtered_results()},
            server = TRUE,
            selection = "single",
            options = list(
                paging = TRUE,
                pageLength = 15,
                autoWidth = TRUE
            )
        )
        #### PLOT OUTPUT RESULTS ####
        filtered_plot <- reactive({
            req(input$reinspected_spectrums2,input$reinspected_type2,hasName(DATA$results,"results"))
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            tryCatch({
                KPI <- get_KPI_plot(input, DATA)
                DATA$current_KPI_key <- KPI$key
                DATA$current_KPI_plot <- KPI$plt
                return(KPI$plt)
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "filtered_plot"
                )
                showNotification(
                    ui = str_c("Error occured during reactive 'filtered_plot'. The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
        })
        output$results_visualisation_plot <- renderPlot({
            filtered_plot()
        })
        observeEvent(input$save_visualisation_plot, {
            req(DATA$current_KPI_plot)
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            tryCatch({
                store_KPI_plot_to_file(input, DATA)
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "save_visualisation_plot"
                )
                showNotification(
                    ui = str_c("Error occured during callback 'input$save_visualisation_plot'. The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
        })
        ### selected elements of the DT::renderDataTable() can be accessed in server via `input$tableID_rows_selected` - cf. https://clarewest.github.io/blog/post/making-tables-shiny/

        #### HIDE_PANELS_BY_DEFAULT ####
        hide("HSV_PANEL")
        hide("CROPPING_PANEL")
        hide("PARALLEL_PANEL")
        #### DEV TOGGLES ####
        observeEvent(input$dev_pass, {
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            tryCatch({
                DEBUGKEYS <- dev_key_handler(input, DATA, DEBUGKEYS, session, use_logical_cores)
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "dev_pass"
                )
                showNotification(
                    ui = str_c("Error occured during callback 'input$dev_pass'. The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
        })
        #### SETUP PARALLELISATION ####
        observeEvent(input$open_parallelPanel, {
            open_parallelPanel_event(input, DATA, FLAGS, use_logical_cores, session, STARTUP)
            FLAGS$restoring_state <- FALSE
        })
        #### EDIT CROPPING ####
        observeEvent(input$do_crop_image, {
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
                if (isFALSE(STARTUP$startup)) {
                    showNotification(
                        ui = str_c(
                            "Disabled cropping. After being loaded, the complete image-matrix will be processed."
                        ),
                        type = "message"
                    )
                }
            }
        })
        observeEvent(input$select_crops, {
            if (is.null(input$tbl_dir_files_rows_selected)) {
                showNotification(
                    ui = str_c(
                        "Please select an image in the 'Image Files'-Tab first."
                    ),
                    duration = DATA$notification_duration * 5,
                    type = "warning"

                )
                return()
            }
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
        #### EDIT CROPPING OF SEARCHRANGE FOR IDENTIFIER-DOT ####
        observeEvent(input$do_crop_identifier_range, {
            if (input$do_crop_identifier_range) {
                show("IDENTIFIERCROPPING_PANEL")
                showNotification(
                    ui = str_c(
                        "Limited search-range for the identifier-dot. After being loaded, only pixels within this range may be checked for the HSV-ranges of the identifier-dot. All other spectrums are unaffected."
                    ),
                    action = a(href = "https://www.google.com", "google"),
                    type = "message"
                )
            } else {
                hide("IDENTIFIERCROPPING_PANEL")
                if (isTRUE(STARTUP$startup)) {
                    STARTUP$startup <- FALSE
                    # this line must be executed instead of the *last* message
                    # which you want to suppress on startup. To be more precise,
                    # if another `conditionalPanel` is added, the event-callback
                    # for its notifications should be placed _above_ this
                    # event-callback.
                } else {
                    showNotification(
                        ui = str_c(
                            "Disabled cropping. All pixels will be considered for the size of the identifier."
                        ),
                        type = "message"
                    )
                }
            }
        })
        observeEvent(input$select_identifiercrops, {
            if (is.null(input$tbl_dir_files_rows_selected)) {
                showNotification(
                    ui = str_c(
                        "Please select an image in the 'Image Files'-Tab first."
                    ),
                    duration = DATA$notification_duration * 5,
                    type = "warning"

                )
                return()
            }
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
                updateNumericInput(session,"identifiersearch_x0",value = as.integer(cl))
                updateNumericInput(session,"identifiersearch_x1",value = as.integer(cr))
                updateNumericInput(session,"identifiersearch_y1",value = as.integer(cb))
                updateNumericInput(session,"identifiersearch_y0",value = as.integer(ct))
            }
        })
        observeEvent(input$reset_identifiercrops, {
            showModal(modalDialog(
                tags$h3('Do you want to reset the search-range for the identifier-range?'),
                tags$h5('As a result, the entire image will be processed at full resolution. Pi'),
                footer=tagList(
                    actionButton('submit_reset_identifiercrops', 'Reset'),
                    modalButton('cancel')
                )
            ))
        })
        observeEvent(input$submit_reset_identifiercrops, {
            removeModal()
            showNotification(ui = str_c("not implemented: reset crops to 0/0/0/0"),
                             type = "message")
            updateNumericInput(session,"identifiersearch_x0",value = 0)
            updateNumericInput(session,"identifiersearch_x1",value = 0)
            updateNumericInput(session,"identifiersearch_y0",value = 0)
            updateNumericInput(session,"identifiersearch_y1",value = 0)
        })
        #### EDIT HSV RANGES ####
        observeEvent(input$open_edit_HSV_ranges_conditionalPanel, {
            show("HSV_PANEL")
        })
        observeEvent(input$selected_HSV_spectrum, {
            ## render selected spectrum's current values
            spectrums <- DATA$spectrums
            updateNumericInput(session, inputId = "lower_bound_H", value = spectrums$lower_bound[[input$selected_HSV_spectrum]][1])
            updateNumericInput(session, inputId = "lower_bound_S", value = spectrums$lower_bound[[input$selected_HSV_spectrum]][2])
            updateNumericInput(session, inputId = "lower_bound_V", value = spectrums$lower_bound[[input$selected_HSV_spectrum]][3])
            updateNumericInput(session, inputId = "upper_bound_H", value = spectrums$upper_bound[[input$selected_HSV_spectrum]][1])
            updateNumericInput(session, inputId = "upper_bound_S", value = spectrums$upper_bound[[input$selected_HSV_spectrum]][2])
            updateNumericInput(session, inputId = "upper_bound_V", value = spectrums$upper_bound[[input$selected_HSV_spectrum]][3])
        })
        observeEvent(input$reset_HSV_ranges, {
            ## reset selected spectrum's values to default settings in `duflor.default_hsv_spectrums`
            default_HSV_spectrums <- getOption("duflor.default_hsv_spectrums")
            updateNumericInput(session, inputId = "lower_bound_H", value = default_HSV_spectrums$lower_bound[[input$selected_HSV_spectrum]][1])
            updateNumericInput(session, inputId = "lower_bound_S", value = default_HSV_spectrums$lower_bound[[input$selected_HSV_spectrum]][2])
            updateNumericInput(session, inputId = "lower_bound_V", value = default_HSV_spectrums$lower_bound[[input$selected_HSV_spectrum]][3])
            updateNumericInput(session, inputId = "upper_bound_H", value = default_HSV_spectrums$upper_bound[[input$selected_HSV_spectrum]][1])
            updateNumericInput(session, inputId = "upper_bound_S", value = default_HSV_spectrums$upper_bound[[input$selected_HSV_spectrum]][2])
            updateNumericInput(session, inputId = "upper_bound_V", value = default_HSV_spectrums$upper_bound[[input$selected_HSV_spectrum]][3])
            ## and reset the changes for that specific spectrum in the DATA-object
            DATA$spectrums$lower_bound[[input$selected_HSV_spectrum]][1] <- default_HSV_spectrums$lower_bound[[input$selected_HSV_spectrum]][1]
            DATA$spectrums$lower_bound[[input$selected_HSV_spectrum]][2] <- default_HSV_spectrums$lower_bound[[input$selected_HSV_spectrum]][2]
            DATA$spectrums$lower_bound[[input$selected_HSV_spectrum]][3] <- default_HSV_spectrums$lower_bound[[input$selected_HSV_spectrum]][3]
            DATA$spectrums$upper_bound[[input$selected_HSV_spectrum]][1] <- default_HSV_spectrums$upper_bound[[input$selected_HSV_spectrum]][1]
            DATA$spectrums$upper_bound[[input$selected_HSV_spectrum]][2] <- default_HSV_spectrums$upper_bound[[input$selected_HSV_spectrum]][2]
            DATA$spectrums$upper_bound[[input$selected_HSV_spectrum]][3] <- default_HSV_spectrums$upper_bound[[input$selected_HSV_spectrum]][3]
            showNotification(
                ui = str_c(
                    "Reset values for spectrum '",
                    input$selected_HSV_spectrum,
                    "'"
                ),
                type = "message"
            )
            hide("HSV_PANEL")
        })
        observeEvent(input$close_edit_HSV_ranges_conditionalPanel, {
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            tryCatch({
                ## check if submitted values are valid (i.e. do they exceed boundaries by being inputted as numbers manually?)
                changes <- validate_custom_HSV_values(input, DATA, getDefaultReactiveDomain())
                DATA$spectrum_changes <- changes
                ## assemble the vectors of updated HSV-bounds
                adjustments_lower <- c(input$lower_bound_H,input$lower_bound_S,input$lower_bound_V)
                adjustments_upper <- c(input$upper_bound_H,input$upper_bound_S,input$upper_bound_V)
                if (length(changes$return_obj)==0) { # just commit the changes (no infringing values were found - all changes were within valid range-limits)
                    DATA$spectrums$lower_bound[[input$selected_HSV_spectrum]] <- adjustments_lower
                    DATA$spectrums$upper_bound[[input$selected_HSV_spectrum]] <- adjustments_upper
                    showNotification(
                        ui = str_c(
                            "Updated values for spectrum '",
                            input$selected_HSV_spectrum,
                            "'"
                        ),
                        type = "message"
                    )
                    hide("HSV_PANEL")
                } else { ## ask the user if the changes are okay?
                    ## set values which exceed their bounds to the respective bound
                    for (each in names(changes$return_obj)) {
                        if (str_count(each,str_c("lower_bound"))) {
                            if (str_count(each,str_c("bound_H"))) {
                                adjustments_lower[[1]] <- changes$return_obj[[each]]
                            }
                            if (str_count(each,str_c("bound_S"))) {
                                adjustments_lower[[2]] <- changes$return_obj[[each]]
                            }
                            if (str_count(each,str_c("bound_V"))) {
                                adjustments_lower[[3]] <- changes$return_obj[[each]]
                            }
                        }
                        if (str_count(each,str_c("upper_bound"))) {
                            if (str_count(each,str_c("bound_H"))) {
                                adjustments_upper[[1]] <- changes$return_obj[[each]]
                            }
                            if (str_count(each,str_c("bound_S"))) {
                                adjustments_upper[[2]] <- changes$return_obj[[each]]
                            }
                            if (str_count(each,str_c("bound_V"))) {
                                adjustments_upper[[3]] <- changes$return_obj[[each]]
                            }
                        }
                    }
                    # make a copy to use when confirming the modalDialogue
                    DATA$coerced_spectrums <- DATA$spectrums
                    DATA$coerced_spectrums <- DATA$spectrums
                    # then modify it, so that the changes may be applied.
                    DATA$coerced_spectrums$lower_bound[[input$selected_HSV_spectrum]] <- adjustments_lower
                    DATA$coerced_spectrums$upper_bound[[input$selected_HSV_spectrum]] <- adjustments_upper
                    show_infringing_spectrum_elements_gui_comp(input, DATA, changes)
                }
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "save_visualisation_plot"
                )
                showNotification(
                    ui = str_c("Error occured during callback 'input$close_edit_HSV_ranges_conditionalPanel'. The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
        })
        observeEvent(input$confirm_coerced_HSV_values_modal, {
            ## user wants to use the range-limits for the respective HSV-parameters as their respective bounds
            DATA$spectrums$lower_bound <- DATA$coerced_spectrums$lower_bound
            DATA$spectrums$upper_bound <- DATA$coerced_spectrums$upper_bound
            removeModal()
            showNotification(
                ui = str_c(
                    "Updated values for spectrum '",
                    input$selected_HSV_spectrum,
                    "'"
                ),
                type = "message"
            )
            hide("HSV_PANEL")
        })
        observeEvent(input$discard_coerced_HSV_values_modal, {
            ## user wants to use the default values for the respective HSV-spectrum
            default_HSV_spectrums <- getOption("duflor.default_hsv_spectrums")
            current_lower <- DATA$spectrums$lower_bound[[input$selected_HSV_spectrum]]
            current_upper <- DATA$spectrums$upper_bound[[input$selected_HSV_spectrum]]
            map <- c("H","S","V")
            for (each in names(DATA$spectrum_changes$return_obj)) {
                index_of_change <- which(map == sub(".*_(.)$", "\\1", each))
                if (str_count(each,"lower_bound")>0) {
                    DATA$spectrums$lower_bound[[input$selected_HSV_spectrum]][[index_of_change]] <- current_lower[[index_of_change]]
                } else {
                    DATA$spectrums$upper_bound[[input$selected_HSV_spectrum]][[index_of_change]] <- current_upper[[index_of_change]]
                }
            }
            updateNumericInput(session, inputId = "lower_bound_H", value = DATA$spectrums$lower_bound[[input$selected_HSV_spectrum]][1])
            updateNumericInput(session, inputId = "lower_bound_S", value = DATA$spectrums$lower_bound[[input$selected_HSV_spectrum]][2])
            updateNumericInput(session, inputId = "lower_bound_V", value = DATA$spectrums$lower_bound[[input$selected_HSV_spectrum]][3])
            updateNumericInput(session, inputId = "upper_bound_H", value = DATA$spectrums$upper_bound[[input$selected_HSV_spectrum]][1])
            updateNumericInput(session, inputId = "upper_bound_S", value = DATA$spectrums$upper_bound[[input$selected_HSV_spectrum]][2])
            updateNumericInput(session, inputId = "upper_bound_V", value = DATA$spectrums$upper_bound[[input$selected_HSV_spectrum]][3])
            showNotification(
                ui = str_c(
                    "Reset values for spectrum '",
                    input$selected_HSV_spectrum,
                    "'"
                ),
                type = "message"
            )
            removeModal()
            hide("HSV_PANEL")
        })
        #### RENDER PLOT ####
        observeEvent(input$render_plant, {
            req(DATA$r__tbl_dir_files,input$tbl_dir_files_rows_selected)
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            tryCatch({
                selectedrowindex <- as.numeric(input$tbl_dir_files_rows_selected[length(input$tbl_dir_files_rows_selected)])
                DATA$r__tbl_dir_files_selectedrow <- selectedrow <- (DATA$r__tbl_dir_files[selectedrowindex,])
                showNotification(
                    ui = str_c("loading ", " ", selectedrow$images_filtered),
                    duration = DATA$notification_duration,
                    type = "message"
                )
                if (is.na(DATA$last_masked_image) || (DATA$last_masked_image!=selectedrow$images_filtered)) {
                    im <- load_image(selectedrow$images_filtered,subset_only = F,return_hsv = F)
                } else {
                    im <- DATA$last_im
                }
                dims <- dim(im)
                if ((input$x0!=0) && (input$x1!=0)  && (input$y0!=0)  && (input$y1!=0))  { # add selected cropping_rect to image
                    im <- draw_rect(
                        im,
                        x0 = input$x0,
                        x1 = input$x1,
                        y0 = input$y0,
                        y1 = input$y1,
                        color = "lightblue",
                        opacity = 0.25,
                        filled = T
                    )
                }
                if ((input$identifiersearch_x0!=0) && (input$identifiersearch_x1!=0)  && (input$identifiersearch_y0!=0)  && (input$identifiersearch_y1!=0))  { # add selected identcroping_rect to image
                    im <- draw_rect(
                        im,
                        x0 = input$identifiersearch_x0,
                        x1 = input$identifiersearch_x1,
                        y0 = input$identifiersearch_y0,
                        y1 = input$identifiersearch_y1,
                        color = "yellow",
                        opacity = 0.25,
                        filled = T
                    )
                }
                display(im)
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "render_plant"
                )
                showNotification(
                    ui = str_c("Error occured during callback 'input$render_plant'. The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
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
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            tryCatch({
                if (is.null(input$selected_spectra)) {
                    showNotification(
                        ui = str_c(
                            "Please select at least one option."
                        ),
                        duration = DATA$notification_duration * 5,
                        type = "warning"

                    )
                    return()
                }
                removeModal()
                spectrums <- DATA$spectrums
                spectrums$lower_bound <- duflor:::remove_key_from_list(DATA$spectrums$lower_bound,names(DATA$spectrums$lower_bound)[!(names(DATA$spectrums$lower_bound) %in% input$selected_spectra)])
                spectrums$upper_bound <- duflor:::remove_key_from_list(DATA$spectrums$upper_bound,names(DATA$spectrums$lower_bound)[!(names(DATA$spectrums$lower_bound) %in% input$selected_spectra)])
                # update the spectrum-selection DDLs in tabs `Results - inspect` and `Results - plots`
                updateSelectInput(session = getDefaultReactiveDomain(), inputId = "reinspected_spectrums",label = "Select spectrum to inspect",choices = names(spectrums$lower_bound))
                updateSelectInput(session = getDefaultReactiveDomain(), inputId = "reinspected_spectrums2",label = "Select spectrum to inspect",choices = names(spectrums$lower_bound))
                DATA$spectrums <- spectrums
                #### SETUP PARALLELISATION ####
                if (input$parallel_cores > 1) {
                    if (.Platform$OS.type == "windows") {
                        cluster_type <- "PSOCK"
                    } else {
                        cluster_type <- "FORK"
                    }
                    if (getDoParRegistered()) {
                        # a cluster is already spun up, so check if it can be reused
                        current_workers <- getDoParWorkers()
                        if (current_workers!=input$parallel_cores) {
                            # desired number of workers have changed, so we need to
                            # shut down the old cluster and build a new one
                            shutdown_parallel()
                            setup_parallel(input$parallel_cores, cluster_type)
                        }
                    } else {
                        # no preexisting cluster, must init one
                        setup_parallel(input$parallel_cores, cluster_type)

                    }
                } else {
                    if (getDoParRegistered()) {
                        # shut down existing cluster first (?)
                        shutdown_parallel()
                    }
                }
                #### EXECUTE ANALYSIS ####
                removeNotification(id = "analysis.completed")
                showNotification(
                    ui = str_c("Analysis ongoing since ", Sys.time(), "."),
                    id = "analysis.ongoing",
                    closeButton = F,
                    duration = NULL,
                    type = "warning"
                )
                execution_time <- system.time(
                    results <- execute_analysis(input, DATA, DEBUGKEYS, FLAGS)
                )
                removeNotification(id = "analysis.ongoing")
                showNotification(
                    ui = str_c("Analysis finished in ",round(x = execution_time[[3]],digits = 4)," seconds."),
                    id = "analysis.completed",
                    duration = NULL,
                    type = "message"
                )
                DATA$results <- results
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
                if (isFALSE(FLAGS$analyse_single_image)) { ## disallow save-to-file when running single-analysis
                    updateActionButton(session = getDefaultReactiveDomain(),inputId = "save_results",disabled = FALSE)
                }
                updateActionButton(session = getDefaultReactiveDomain(),inputId = "render_selected_mask",disabled = FALSE)
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "submit_selected_spectra"
                )
                showNotification(
                    ui = str_c("Error occured during analysis (callback 'input$submit_selected_spectra'). The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
        })
        #### RERUN ANALYSIS TO RENDER PLOTS ####
        observeEvent(input$render_selected_mask, {
            req(input$reinspected_spectrums)
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            tryCatch({
                render_selected_mask(input, DATA, FLAGS)
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "render_selected_mask"
                )
                showNotification(
                    ui = str_c("Error occured during callback 'input$render_selected_mask'. The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
        })
        #### SAVE RESULTS BTN ####
        observeEvent(input$save_results, {
            req(DATA$results)
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            tryCatch({
                if (is.na(DATA$results)) {
                    #TODO: add warning: results are empty, could not save.
                    return()
                } else {
                    # shinyDirChoose() #TODO: do I want to allow choosing of output-directory?

                    results_path <- str_c(
                        dirname(DATA$results$results$full_path[[1]]),
                        "/results/results_",
                        input$date_of_image_shooting
                    )
                    out <- store_results_to_file(
                        results = DATA$results,
                        results_path = results_path,
                        save_to_xlsx = input$save_as_xlsx,
                        set_author_xlsx =  DEBUGKEYS$set.author
                    )
                    ## verify save was successfull
                    if (out$success) {
                        showNotification(
                            ui = "Analysis completed. Results have been written to '",
                            out$results_path,
                            "'",
                            duration = DATA$notification_duration,
                            type = "message"
                        )
                    } else {
                        showNotification(
                            ui = "Analysis could not be completed successfully, and results could not be successfully written to",
                            out$results_path,
                            "'",
                            duration = DATA$notification_duration * 4,
                            type = "warning"
                        )
                    }
                }
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "save_results"
                )
                showNotification(
                    ui = str_c("Error occured callback 'input$save_results'. The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
        })
        #### FINISH STARTUP ####
        STARTUP <- reactiveValues(startup = TRUE)
        showNotification(
            ui = "Finished startup",
            id = "startup.notice",
            type = "message",
            duration = 1.3
        )
        #### MANUAL BOOKMARKING ####
        # the input-folder is relative to the system, and thush shouldn't be
        # bookmarked.
        setBookmarkExclude(
            c(
                "folder",
                "folder_modal",
                "folder-modal",
                "restore_folder",
                "restore_folder-modal",
                "restore_folder_modal"
            )
        )
        onBookmark(function(state) {

        })
        # Load button action
        observeEvent(input$restore_state, {
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            req(input$restore_state$datapath!="") # restore_state()
            req(file.exists(input$restore_state$datapath)) # restore_state()
            # req(isFALSE(is.na(DATA$folder_path)))
            # folder_path <- DATA$folder_path  # image_files_ # this is how you conver thte shinydirselection-objet to a valid path. cf: https://search.r-project.org/CRAN/refmans/shinyFiles/html/shinyFiles-parsers.html
            # req(dir.exists(folder_path)) # image_files_
            req(input$image_file_suffix) # image_files_
            tryCatch({
                showNotification(
                    ui = "State is being restored.",
                    id = "restore_state.ongoing",
                    duration = NA,
                    type = "warning"
                )
                state_file <- input$restore_state$datapath
                loaded_path <- restore_state(
                    input = input,
                    output = output,
                    DATA = DATA,
                    FLAGS = FLAGS,
                    DEBUGKEYS = DEBUGKEYS,
                    session = getDefaultReactiveDomain(),
                    volumes = getVolumes(),
                    state_file = state_file
                )
                DATA$folder_path <- loaded_path
                image_files_()
                removeNotification(id = "restore_state.ongoing")
                if (file.exists(loaded_path)) {
                    showNotification(
                        ui = str_c("State successfully restored from '",loaded_path,"'."),
                        id = "restore_state.done",
                        duration = DATA$notification_duration * 4,
                        type = "message"
                    )
                } else {
                    showNotification(
                        ui = str_c("State was not successfully restored from '",loaded_path,"'."),
                        id = "restore_state.error",
                        duration = DATA$notification_duration * 4,
                        type = "error"
                    )
                }
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "restore_state"
                )
                showNotification(
                    ui = str_c("Error occured while restoring state (during callback `input$restore_state`). The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
        })
        # Save directory selection
        observeEvent(input$save_state, {
            input_mirror <- input ## mirror input so that the error-trycatch can pass it to save_state
            shinyFileSave(
                input,
                "save_state",
                roots = volumes,
                session = getDefaultReactiveDomain(),
                allowDirCreate = T
            )
            savedir_path <- parseDirPath(roots = volumes, selection = input$save_state)
            req(isFALSE(is.numeric(input$save_state[[1]])))
            req(dir.exists(savedir_path))
            tryCatch({
                showNotification(
                    ui = "State is being saved.",
                    id = "save_state.ongoing",
                    duration = NA,
                    type = "warning"
                )
                # but first we must remove some values which are not to be saved to ensure filesize is minimal:
                # - DATA$last_im (which caches the last-loaded image of the 'render_selected_mask'-subroutine)
                saved_state_path <- save_state(
                    input = input,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes()
                )
                removeNotification(id = "save_state.ongoing")
                if (file.exists(saved_state_path)) {
                    showNotification(
                        ui = str_c("State successfully saved to '",saved_state_path,"'."),
                        id = "save_state.done",
                        duration = DATA$notification_duration * 4,
                        type = "message"
                    )
                } else {
                    showNotification(
                        ui = str_c("State was not successfully saved to '",saved_state_path,"'."),
                        id = "save_state.error",
                        duration = DATA$notification_duration * 4,
                        type = "error"
                    )
                }
            }, error = function(e) {
                DATA$stacktrace = traceback(1, 1)
                error_state_path <- save_error_state(
                    input = input_mirror,
                    DATA = DATA,
                    DEBUGKEYS = DEBUGKEYS,
                    FLAGS = FLAGS,
                    volumes = getVolumes(),
                    error = e,
                    errordir_path = DATA$folder_path,
                    erroneous_callback = "save_state"
                )
                showNotification(
                    ui = str_c("Error occured while saving state (during callback 'input$save_state'). The configuration which triggered this error was stored to '",error_state_path,"'."),
                    id = "error_state_generated.done",
                    duration = NULL,
                    type = "error"
                )
            })
        })
    }
    #### LAUNCH APP ####
    shinyApp(ui = ui, server = server, enableBookmarking = "server")
}
