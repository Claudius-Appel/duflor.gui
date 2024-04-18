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
#' @importFrom shinyFiles shinyDirChoose
#' @importFrom shinyFiles parseDirPath
#' @importFrom shinyFiles getVolumes
#' @importFrom shiny renderText
#' @importFrom shiny showNotification
#' @importFrom shiny isolate
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
    ui <- fluidPage(
        # App title ----
        titlePanel("Hello Shiny!"),

        # Sidebar layout with input and output definitions ----
        sidebarLayout(

            # Sidebar panel for inputs ----
            sidebarPanel(

                h4("Select folder containing images"),
                shinyDirButton(id = 'folder', 'Select a folder', 'Please select a folder', FALSE),
                selectInput(inputId = "image_file_suffix",label = "Select filetype to import",choices = c("JPG","PNG")),
                "Current Folder:",
                textOutput(outputId = "ctrl_current_folder"),
                radioButtons(inputId = "radio_analysis_type",
                           h4("Type of Analysis"),
                           choices = list("GFA" = 1,
                                          "WFA" = 2),
                           selected = 1),
                actionButton(inputId = "execute_analysis",label = "Execute Analysis"),
                actionButton(inputId = "execute_analysis_single",label = "Execute Analysis (single)"),
                actionButton(inputId = "render_plant",label = "Render Plant"),
                ## works only post 'execute_analysis'
                radioButtons(inputId = "KPI_type",
                           h4("Select KPI to render"),
                           choices = list("red-dot" = 1,
                                          "TBD1" = 2,
                                          "TBD2" = 3,
                                          "TBD3" = 4),
                           selected = 1),
                h4("Crop Image"),
                actionButton(inputId = "reset_crops", label = "Reset"),
                numericInput(inputId = "crop_left",label = "Crop Left",value = 0,min = 0),
                numericInput(inputId = "crop_right",label = "Crop Right",value = 0,min = 0),
                numericInput(inputId = "crop_top",label = "Crop Top",value = 0,min = 0),
                numericInput(inputId = "crop_bottom",label = "Crop Bottom",value = 0,min = 0),


                h5("Parallel Processing"),
                checkboxInput(inputId = "parallelise",label = "Run analysis in parallel?"),
                numericInput(inputId = "parallel_cores",label = "Designate number of cores",value = 1, min = 1,max = (detectCores() - 1)),
                h5("Misc"),
                passwordInput(inputId = "dev_pass",label = "Dev-console",placeholder = "enter '-h' for a list of valid commands")







            ),

            # Main panel for displaying outputs ----
            mainPanel(
                # plotOutput(outputId = "distPlot"),
                # plotOutput(
                #     outputId = "image_offset_plot",
                #     dblclick = "image_offset_dblclick",
                #     brush = brushOpts(
                #         id = "image_offset_plot_brush",
                #         resetOnNew = TRUE
                #     )
                #     ),
                #tableOutput(outputId = "tbl_dir_files"),
                tabsetPanel(id = "tabset_panel",
                  tabPanel("Image Files",verbatimTextOutput("Image Files"),dataTableOutput("tbl_dir_files")),
                  tabPanel("Analytics (red dot deviations?)",verbatimTextOutput("Analytics (red dot deviations?)"),dataTableOutput("tbl_dir_files_selectedrow"))
                  #tabPanel("Analytics (misc1)",verbatimTextOutput("TAB3")),
                  #tabPanel("Analytics (misc2)",verbatimTextOutput("TAB4")),
                  #tabPanel("Analytics (misc3)",verbatimTextOutput("TAB5"))
                )

            ),
        )
    )

    # Define server logic required to draw a histogram ----
    server <- function(input, output,session) {
        #### INIT VARIABLES ####
        DATA <- reactiveValues(          #  nomenclature: reactives start with "r__"
            r__tbl_dir_files  = NA,
            r__img_type = "PNG",
            r__KPI_type = 1,
            r__tbl_dir_files_selectedrow = NA,
            # r__render_plant = 0,
            preview_img = NA,
        )
        DEBUGKEYS <- reactiveValues(
            force.prints = FALSE,
            force.log = FALSE,
            set.author = FALSE

        )
        ranges <- reactiveValues(
            x = NULL,
            y = NULL
        )

        # to make values only trigger reactives after x seconds of non-interaction, first assign them reactive.
        # next, assign a debounce-expression with a set timout after which the value is hnaded onwards to the reactive-pipeline
        # finally, refer to the debounce-expression via `expression()` instead of the input-value `input$value` in callbacks.
        r__crop_left <- reactive(input$crop_left)
        r__crop_right <- reactive(input$crop_right)
        r__crop_top <- reactive(input$crop_top)
        r__crop_bottom <- reactive(input$crop_bottom)
        d__crop_left <- r__crop_left %>% debounce(1300)
        d__crop_right <- r__crop_right %>% debounce(1300)
        d__crop_top <- r__crop_top %>% debounce(1300)
        d__crop_bottom <- r__crop_bottom %>% debounce(1300)
        volumes <- getVolumes()()
        # Histogram of the Old Faithful Geyser Data ----
        # with requested number of bins
        # This expression that generates a histogram is wrapped in a call
        # to renderPlot to indicate that:
        #
        # 1. It is "reactive" and therefore should be automatically
        #    re-executed when inputs (input$bins) change
        # 2. Its output type is a plot
        # output$distPlot <- renderPlot({
        #
        #     req(input$folder,image_files()) # if you must require the result of a different ctrl, make sure to append the () to it.
        #
        #     x    <- faithful$waiting
        #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
        #
        #     hist(x, breaks = bins, col = "#007bc2", border = "white",
        #          xlab = "Waiting time to next eruption (in mins)",
        #          main = "Histogram of waiting times")
        #
        # })
        # TODO: change this to render instead of the histogram.
        # The selection works, so when selecting an image, populate the space
        # above. Then react-grab the contents of the cutoff-select and display as
        # red bars offset from the edges

        # output$image_offset_plot <- renderPlot({
        #     #TODO: reset render_plant
        #     req(DATA$r__tbl_dir_files,input$tbl_dir_files_rows_selected,DATA$preview_img)
        #     # print(stringr::str_c("offset_plot: preview_img: ",DATA$preview_img))
        #     if (isTRUE(preview_is_loaded)) {
        #         # isolate({
        #             preview_is_loaded <<- FALSE
        #         # })
        #         if (is.null(ranges$x)) {
        #             xmin <- 0
        #         } else {
        #             xmin <- ranges$x[1]
        #         }
        #         if (is.null(ranges$x)) {
        #             xmax <- 6000
        #         } else {
        #             xmax <- ranges$x[2]
        #         }
        #         if (is.null(ranges$y)) {
        #             ymin <- 0
        #         } else {
        #             ymin <- ranges$y[1]
        #         }
        #         if (is.null(ranges$y)) {
        #             ymax <- 4000
        #         } else {
        #             ymax <- ranges$y[2]
        #         }
        #
        #         plot(NULL,xlim = c(xmin,xmax),ylim = c(ymin,ymax))
        #         rasterImage(DATA$preview_img,xmin,ymin,xmax,ymax,interpolate = F)
        #         showNotification(str_c("plotted "," ", (DATA$r__tbl_dir_files[input$tbl_dir_files_rows_selected,]$images_filtered)),duration = 3)
        #     # d__crop_left
        #     # d__crop_right
        #     # d__crop_top
        #     # d__crop_bottom
        #         rect(xleft = 0, ybottom = 0, xright = d__crop_left(), ytop = ymax,col = "red",density = 5) # CL  ## works
        #         rect(xleft = xmax - d__crop_right(), ybottom = 0, xright = xmax, ytop = ymax, col = "red",density = 5) # CR
        #         rect(xleft = xmin, ybottom = ymax - d__crop_top(), xright = xmax, ytop = ymax, col = "red",density = 5) # CT
        #         rect(xleft = xmin, ybottom = 0, xright = xmax, ytop = 0 + d__crop_bottom(), col = "red",density = 5) # CB
        #     }
        # })
        ##### DISPLAYING AND RENDERING FILES AND STUFF ####
        image_files <- reactive({ # image_files is a list of filepaths, which gets set reactively.
            req(input$folder,input$image_file_suffix,input$radio_analysis_type)
            shinyDirChoose(input = input, 'folder', roots=volumes)
            folder_path <- parseDirPath(roots = volumes,input$folder) # this is how you conver thte shinydirselection-objet to a valid path. cf: https://search.r-project.org/CRAN/refmans/shinyFiles/html/shinyFiles-parsers.html
            req(folder_path) ## make sure the rest of this react is only executed if 'folder_path' is set
            if (dir.exists(folder_path)) {
                images_ <- list.files(folder_path,pattern = paste0("*.",input$image_file_suffix),recursive = F,full.names = T)
                images_ <- list.files(folder_path,pattern = paste0("*.(",str_to_lower(input$image_file_suffix),"|",str_to_upper(input$image_file_suffix),")"),recursive = F,full.names = T)
                images_filtered <- images_[!str_count(basename(images_),"_")]
                ret <- as.data.frame(images_filtered) # TODO: see here for paginated tables in shiny-apps https://stackoverflow.com/questions/50043152/r-shiny-how-to-add-pagination-in-dtrenderdatatable
                ret$count <- c(1:1:dim(ret)[1])
                DATA$r__tbl_dir_files <- ret
                return(ret)
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
            image_files()
        }, server = TRUE,
        selection = "single",
        options = list(
            paging = TRUE,
            pageLength = 15,
            autoWidth = TRUE
        )
        )
        ### selected elements of the DT::renderDataTable() can be accessed in server via `input$tableID_rows_selected` - cf. https://clarewest.github.io/blog/post/making-tables-shiny/

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
                        showNotification(str_c("Available dev Keys (see documentation): ",str_flatten_comma(Arr[[1]][!str_count(Arr[[1]],"---")])),duration = 5)
                    } else {
                        key <- str_replace_all(str_replace_all(each,"---",""),"--","")
                        key <- str_replace_all(key,"-",".")
                        DEBUGKEYS[[key]] <- !DEBUGKEYS[[key]]
                        showNotification(str_c("DEBUG KEY "," ",each, " set to ", DEBUGKEYS[[key]]))
                    }
                }

            }
        })
        #### MISC ####
        observeEvent(input$image_offset_dblclick, {
            #TODO: the brush shall not reset the ranges. else we fuck up a lot.
            #instead, make it modify the crop_X inputs - or, as a simpler first
            #solution, give us the vaues of its corners first
            brush <- input$image_offset_plot_brush
            if (!is.null(brush)) {
                # ranges$x <- c(brush$xmin, brush$xmax)
                # ranges$y <- c(brush$ymin, brush$ymax)
                showNotification(str_c("Bounding Coordinates: X(",brush$xmin,brush$xmax,"),Y(",brush$ymin,brush$ymax,")"),duration = 3)
                updateNumericInput(session,"crop_left",value = as.integer(brush$xmin))
                updateNumericInput(session,"crop_right",value = as.integer(brush$xmax))
                updateNumericInput(session,"crop_bottom",value = as.integer(brush$ymin))
                updateNumericInput(session,"crop_top",value = as.integer(brush$ymax))
                ## TODO: get the dimensions of the first image loaded, then fix the values above so that the right and bottom edges of the reported rectangle are correctly displayed?
                ## Right now, it seems like the program is miscalculating the edges somehow ¯\_(ツ)_/¯
            } else {
                # ranges$x <- NULL
                # ranges$y <- NULL
            }
        })

        #### RESET CROPPING ####
        observeEvent(input$reset_crops, {
            showModal(modalDialog(
                tags$h3('Do you want to reset the croppings?'),
                footer=tagList(
                    actionButton('submit_reset_crops', 'Reset'),
                    modalButton('cancel')
                )
            ))
        })
        observeEvent(input$submit_reset_crops, {
            removeModal()
            showNotification(str_c("not implemented: reset crops to 0/0/0/0"))
            updateNumericInput(session,"crop_left",value = 0)
            updateNumericInput(session,"crop_right",value = 0)
            updateNumericInput(session,"crop_bottom",value = 0)
            updateNumericInput(session,"crop_top",value = 0)
        })
        #### RENDER PLOT ####
        observeEvent(input$render_plant, {
            req(DATA$r__tbl_dir_files,input$tbl_dir_files_rows_selected)
            print("passed render_plant reqs")
            # DATA$r__render_plant <- DATA$r__render_plant + 1
            selectedrowindex <- as.numeric(input$tbl_dir_files_rows_selected[length(input$tbl_dir_files_rows_selected)])
            DATA$r__tbl_dir_files_selectedrow <- selectedrow <- (DATA$r__tbl_dir_files[selectedrowindex,])
            # selectedrow
            showNotification(str_c("loading "," ", selectedrow$images_filtered),duration = 3)
            showNotification(str_c(input$crop_left),duration = 3)
            # DATA$preview_img <- readJPEG(selectedrow$images_filtered)

            im <- load_image(selectedrow$images_filtered,subset_only = F,return_hsv = F)
            dims <- dim(im)
            if ((input$crop_left!=0) && (input$crop_right!=0)  && (input$crop_top!=0)  && (input$crop_bottom!=0))  { # add previously selected rect to new image

                im <- draw_rect(im,
                                        x0 = input$crop_left,
                                        x1 = dims[[1]] - input$crop_right,
                                        y0 = input$crop_top,
                                        y1 = input$crop_bottom,color = "red",opacity = 0.25,filled = T
                )
            }
            rect <- grabRect(im)
            if (sum(rect)>0) {
                # DATA$rect <- rect
                showNotification(str_c("x0: ",rect["x0"],"X1: ",rect["x1"],"\n","Y0: ",rect["y0"],"Y1: ",rect["y1"]))
                cl <- rect["x0"]
                cr <- rect["x1"]
                ct <- rect["y0"]
                cb <- rect["y1"]
                updateNumericInput(session,"crop_left",value = as.integer(cl))
                updateNumericInput(session,"crop_right",value = as.integer(dims[[1]] - cr))
                updateNumericInput(session,"crop_bottom",value = as.integer(cb))
                updateNumericInput(session,"crop_top",value = as.integer(ct))
            }
        })
        #### MAIN CALLBACK>EXECUTE DUFLOR_PACKAGE HERE ####
        observeEvent(input$execute_analysis_single, {
            showNotification(str_c("not implemented: in this scenario, we might consider displaying the resulting masks.\nIn normal execution, we do not display anything but the results at the end."))
        })
        observeEvent(input$execute_analysis, {  ## to access output-variables at all, you must ingest them into a reactive-object before retrieving them from there.
            ## file-check: only pass through files that are considered valid
            valid_files <- duflor.check(DATA$r__tbl_dir_files)
            print(input$parallel_cores)
            if (input$parallel_cores>1) {
                if (.Platform$OS.type == "windows") {
                    cluster_type <- "PSOCK"
                } else {
                    cluster_type <- "FORK"
                }
                if (getDoParRegistered()) { # shut down existing cluster first (?)
                    shutdown_parallel()
                }
                setup_parallel(input$parallel_cores,cluster_type)
            } else {
                if (getDoParRegistered()) { # shut down existing cluster first (?)
                    shutdown_parallel()
                }
            }
            #TODO: insert dev keys to toggle under-the-hood behaviour
            isolate(input$dev_pass) # retrieve reactive input value without reevaluating all other reacts?
            DATA$r__img_type <- input$radio_analysis_type
            DATA$r__KPI_type <- input$KPI_type
            for (index in valid_files$count) {
                file <- valid_files$images_filtered[index]
                ## duflor.init()
                ## duflor.validateSettings() # load in the selected thresholds for pinkdot, analysis-type etc
                ## duflor.identify_pixels_of_dot() # use the thresholds and extract the pixels which fulfill it
                ## respective subset of:
                ## duflor.identify_pixels_of_green_area(thresholds.green,image_file) # if settings.gfa
                ## duflor.identify_pixels_of_drought_area(thresholds.drought,image_file) # if settings.include_drought (must be added to GUI first)
                ## duflor.identify_pixels_of_root_area(thresholds.root,image_file) # if settings.wfa
                ## duflor
                ##
                image_dimensions <- duflor.get_image_dimensions(file) ## get image dimensions
                #red_dot_pixels <- duflor.identify_pixels_of_dot(file)
                #bm <- readbitmap::read.bitmap(file,native = FALSE)
                used_color_space <- "HSV" # TODO: wait for response in https://github.com/asgr/imager/issues/14
                if (used_color_space=="HSV") {
                    # image2 <- duflor.load_hsv_colorspace(file)
                    image <- duflor.load_rgb_colorspace(file)
                } else if (used_color_space=="RGB") {
                    image <- duflor.load_rgb_colorspace(file)
                }
                duflor.display(image)
                if (input$radio_analysis_type==1) { # GFA
                    color_ranges <- list(
                        Lower <- c(1,2,3),
                        Higher <- c(4,5,6)
                    )
                } else if (input$radio_analysis_type==2) { # WFA
                    color_ranges <- list(
                        Lower <- c(1,2,3),
                        Higher <- c(4,5,6)
                    )
                }
                print(file)
                print(DATA$r__img_type)
            }
            if (getDoParRegistered()) { # finally, shutdown the cluster if work was performed in parallel
                shutdown_parallel()
            }
        })
        ## CALLBACK FOR SELECTING FILES IN THE `IMAGE FILES`-TBL:
        ##
        ## this
    }
    shinyApp(ui = ui, server = server)
}



