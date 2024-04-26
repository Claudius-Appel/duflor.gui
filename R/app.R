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
        ui <- fluidPage(
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
                    numericInput(inputId = "lower_bound_H",label = "Lower Bound (H_0)", value = 0, min = 0, max = 360, step = 0.01),
                    numericInput(inputId = "lower_bound_S",label = "Lower Bound (S_0)", value = 0, min = 0, max = 1, step = 0.01),
                    numericInput(inputId = "lower_bound_V",label = "Lower Bound (V_0)", value = 0, min = 0, max = 1, step = 0.0001),
                    numericInput(inputId = "upper_bound_H",label = "Upper Bound (H_1)", value = 0, min = 0, max = 360, step = 0.01),
                    numericInput(inputId = "upper_bound_S",label = "Upper Bound (S_1)", value = 0, min = 0, max = 1, step = 0.01),
                    numericInput(inputId = "upper_bound_V",label = "Upper Bound (V_1)", value = 0, min = 0, max = 1, step = 0.0001),
                    actionButton("close_edit_HSV_ranges_conditionalPanel", "Submit changed HSV-spectra"),
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
                ## BUTTONS_2
                actionButton(inputId = "execute_analysis",label = "Execute Analysis"),
                actionButton(inputId = "execute_analysis_single",label = "Execute Analysis (single)"),
            ),

            # Main panel for displaying outputs
            mainPanel(
                tabsetPanel(id = "tabset_panel",
                  tabPanel("Image Files"
                           ,dataTableOutput("tbl_dir_files")
                           ,actionButton(inputId = "render_plant",label = "Render Plant",disabled = TRUE)
                           ),
                  tabPanel("Results - complete"
                           ,dataTableOutput("tbl_results")
                           ,checkboxInput(inputId = "save_as_xlsx",label = "Save results as xlsx?",value = FALSE)
                           ,actionButton(inputId = "save_results","Save results",disabled = TRUE)
                           ),
                  tabPanel("Results - inspect"
                           ,selectInput(inputId = "reinspected_spectrums",label = "Select spectrum to inspect",choices = c())
                           ,dataTableOutput("tbl_results_filtered")
                           ,checkboxInput(inputId = "mask_extreme", label = "Do a high-contrast mask?", value = FALSE)
                           ,actionButton(inputId = "render_selected_mask",label = "Render masks for selected image",disabled = TRUE)
                           ),
                  tabPanel("Results - plots"
                           ,selectInput(inputId = "reinspected_spectrums2",label = "Select spectrum to inspect",choices = c())
                           ,selectInput(inputId = "reinspected_type2",label = "Select KPI to inspect",choices = c("_fraction","_count","_area"))
                           ,plotOutput("results_visualisation_plot")
                           ,actionButton(inputId = "save_visualisation_plot", label = "Save plot", disabled = TRUE)
                           ,)
                  #tabPanel("Analytics (misc2)",verbatimTextOutput("TAB4")),
                  #tabPanel("Analytics (misc3)",verbatimTextOutput("TAB5"))
                )

            ),
        )
    )
    #### SERVER ####
    server <- function(input, output,session) {
        #### STARTUP MESSAGE ####
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
            last_im = NA
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
            req(input$folder[[1]],input$image_file_suffix)
            shinyDirChoose(input = input, 'folder', roots=volumes)
            folder_path <- parseDirPath(roots = volumes,input$folder) # this is how you conver thte shinydirselection-objet to a valid path. cf: https://search.r-project.org/CRAN/refmans/shinyFiles/html/shinyFiles-parsers.html
            updateActionButton(session = getDefaultReactiveDomain(),inputId = "render_plant",disabled = TRUE)
            req(folder_path) ## make sure the rest of this react is only executed if 'folder_path' is set
            if (dir.exists(folder_path)) {
                ## we do not recurse to force all input-files to be in the same level
                images_ <- list.files(folder_path,pattern = paste0("*.(",str_to_lower(input$image_file_suffix),"|",str_to_upper(input$image_file_suffix),")"),recursive = F,full.names = T)
                if (length(images_)>0) {
                    images_filtered <- images_ ##[!str_count(basename(images_),"_")]# BUG: WHY WAS THIS HERE?
                    ret <- as.data.frame(images_filtered) # TODO: see here for paginated tables in shiny-apps https://stackoverflow.com/questions/50043152/r-shiny-how-to-add-pagination-in-dtrenderdatatable
                    ret$index <- c(1:1:dim(ret)[1])
                    DATA$r__tbl_dir_files <- ret
                    updateActionButton(session = getDefaultReactiveDomain(),inputId = "render_plant",disabled = FALSE)
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
                                                  index = numeric(),
                                                  stringsAsFactors = FALSE)
                    updateActionButton(session = getDefaultReactiveDomain(),inputId = "render_plant",disabled = TRUE)
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
        #### REACTIVE - RESULTS_TABLE/BARPLOT, FILTERED BY SPECTRUM ####
        filtered_results <- reactive({
            req(input$reinspected_spectrums)
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
            KPI <- get_KPI_plot(input, DATA)
            DATA$current_KPI_key <- KPI$key
            DATA$current_KPI_plot <- KPI$plt
            return(KPI$plt)
        })
        output$results_visualisation_plot <- renderPlot({
            filtered_plot()
        })
        observeEvent(input$save_visualisation_plot, {
            req(DATA$current_KPI_plot)
            store_KPI_plot_to_file(input, DATA)
        })
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
                updateNumericInput(session,inputId = "parallel_cores",value = 2)
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
                    color = "lightblue",
                    opacity = 0.25,
                    filled = T
                )
            }
            if ((input$x0!=0) && (input$x1!=0)  && (input$y0!=0)  && (input$y1!=0))  { # add previously selected rect to new image

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

            # update the spectrum-selection DDLs in tabs `Results - inspect` and `Results - plots`
            updateSelectInput(session = getDefaultReactiveDomain(), inputId = "reinspected_spectrums",label = "Select spectrum to inspect",choices = names(spectrums$lower_bound))
            updateSelectInput(session = getDefaultReactiveDomain(), inputId = "reinspected_spectrums2",label = "Select spectrum to inspect",choices = c(names(spectrums$lower_bound),"area_per_pixel"))
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
            #TODO: add modal "analysis is ongoing, please wait"
            removeNotification(id = "analysis.completed")
            showNotification(
                ui = "Analysis ongoing since ", Sys.time(), ".",
                id = "analysis.ongoing",
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
            ## TODO: check out the evaluation-functions outlined in the `dev`-folder of `duflor`-package
            ## andeside which ones of those I want to display in the GUI.
        })
        #### RERUN ANALYSIS TO RENDER PLOTS ####
        observeEvent(input$render_selected_mask, {
            req(input$reinspected_spectrums)
            render_selected_mask(input, DATA, FLAGS)
        })
        #### SAVE RESULTS BTN ####
        observeEvent(input$save_results, {
            req(DATA$results)
            if (is.na(DATA$results)) {
                #TODO: add warning: results are empty, could not save.
                return()
            } else {
                # shinyDirChoose() #TODO: do I want to allow choosing of output-directory?

                results_path <- str_c(dirname(DATA$results$results$full_path[[1]]),"/results/results_",input$date_of_image_shooting)
                out <- store_results_to_file(results = DATA$results,results_path = results_path,save_to_xlsx = input$save_as_xlsx)
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
        })
        #### FINISH STARTUP ####
        STARTUP <- reactiveValues(startup = TRUE)
        showNotification(
            ui = "Finished startup",
            id = "startup.notice",
            type = "message",
            duration = 1.3
        )
    }
    shinyApp(ui = ui, server = server)
}
