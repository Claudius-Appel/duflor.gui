#' handles the setting and unsetting of debug-keys
#'
#' Keys which begin with 2 `-` are considered boolean flags, and must be provided
#' in the format `--key=<boolean>`, where for `boolean` the following values are
#' allowed:
#' - `T`/`TRUE`/`1`
#' - `F`/`FALSE`/`0`
#'
#' Keys which begin with 3 `-` are expecting non-boolean, custom input, which will differ
#' based on the respective key.
#'
#' @inheritParams .main_args
#' @param use_logical_cores internal flag
#'
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @importFrom stringr str_c
#' @importFrom stringr str_flatten_comma
#' @importFrom stringr str_count
#' @importFrom shiny showNotification
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom shiny h3
#' @importFrom shiny h5
#' @importFrom shiny h6
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom utils packageDescription
#'
#' @return modified `DEBUGKEYS`?
#' @keywords internal
#'
dev_key_handler <- function(input, DATA, DEBUGKEYS, session, use_logical_cores) {
    # add valid keys here
    keys_array <- str_split("---set-cores,-h,-v,---ca,---sf",",")
    # then add them to the reactive 'DEBUGKEYS' (see 'app.R', search for 'DEBUGKEYS <- reactiveValues(')
    # so that it can be accessed elsewhere as well.
    Keys <- unlist(str_split(input$dev_pass,","))
    for (each in Keys) {
        each_ <- str_remove_all(each,"=(1|0|FALSE|TRUE)")
        each_ <- str_remove_all(each,"=(F|T)")
        each__ <- str_remove_all(each,"=.*")
        if ((each %in% keys_array[[1]]) || (each_ %in% keys_array[[1]]) || (each__ %in% keys_array[[1]])) { # BUG: this bool returns an array if `each` is a char-vector itself.
            if (each=="-h") {
                showNotification(
                    ui = str_c(
                        "Available dev Keys (see documentation): ",
                        str_flatten_comma(keys_array[[1]])
                    ),
                    duration = DATA$notification_duration,
                    type = "message"
                )
            } else if (each=="---ca") {
                key_handle_identifierArea(input,DATA,session)
            } else if (each=="---sf") {
                key_handle_setSearchFolder(input,DATA,session)
            } else if (each=="-v") {
                version1 <- str_c("duflor.gui v.",packageDescription("duflor.gui")$Version)
                version2 <- str_c("duflor v.",packageDescription("duflor")$Version)
                showModal(modalDialog(
                    tags$h3('Software versions:'),
                    tags$h5(version1),
                    tags$h5(version2),
                    tags$h6('developed by Claudius Appel'),
                    footer=tagList(
                        modalButton('cancel')
                    )
                ))
            } else {
                if (str_count(each,"---")) { # non-boolean flags
                    key <- str_replace_all(str_replace_all(each,"---",""),"--","")
                    key <- str_replace_all(key,"-",".")
                    if (str_count(key,"=")) {
                        value <- str_split(key,"=")[[1]][[2]]
                        key <- str_split(key,"=")[[1]][[1]]
                        if (value=="") {
                            next
                        }
                        if (key == "set.cores") {
                            key_handle_cores(value, DATA, use_logical_cores, session)
                        } else {
                            DEBUGKEYS[[key]] <- value
                            showNotification(
                                ui = str_c("DEBUG KEY ", " ", each, " set to ", DEBUGKEYS[[key]]),
                                duration = DATA$notification_duration,
                                type = "message"
                            )
                        }
                    } else {
                        showNotification(
                            ui = str_c("Please provide proper value for flag. Flags beginning with 3 '-' receive non-boolean arguments"),
                            duration = DATA$notification_duration,
                            type = "warning"
                        )
                    }
                } else {
                    key <- str_replace_all(each,"--","")
                    key <- str_replace_all(key,"-",".")
                    if (str_count(key,"=")) {
                        value <- str_split(key,"=")[[1]][[2]]
                        key <- str_split(key,"=")[[1]][[1]]
                        if (value %in% c("F","FALSE",F,FALSE,0)) {
                            value <- FALSE
                        } else if (value %in% c("T","TRUE",T,TRUE,1)) {
                            value <- TRUE
                        }
                        DEBUGKEYS[[key]] <- value
                        showNotification(
                            ui = str_c("DEBUG KEY ", " ", each, " set to ", DEBUGKEYS[[key]]),
                            duration = DATA$notification_duration,
                            type = "message"
                        )
                    } else {
                        showNotification(
                            ui = str_c("Please provide boolean-esque value for flag, either of '1'/'T'/'TRUE'/'0'/'F'/'FALSE'. Other values are not allowed"),
                            duration = DATA$notification_duration,
                            type = "warning"
                        )
                    }
                }
            }
        }
    }
    return(DEBUGKEYS)
}

#' internal function handling dev-key `---set.cores=XX`
#'
#' @inheritParams .main_args
#' @param value number of cores to assign
#' @param use_logical_cores whether or not to use logical cores
#'
#' @importFrom stringr str_c
#' @importFrom stringr str_trim
#' @importFrom shiny showNotification
#' @importFrom shiny updateNumericInput
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom parallel detectCores
#' @keywords internal

key_handle_cores <- function(value, DATA, use_logical_cores, session) {
    value <- str_trim(value)
    value <- as.integer(value)
    if (isFALSE(is.na(value)) && isTRUE(is.integer(value))) {
        if (value == 1) {
            updateNumericInput(session,inputId = "parallel_cores",value = 1)
            hide("PARALLEL_PANEL")
            showNotification(
                ui = str_c("Disabled parallelisation, program will utilise 1 core."),
                duration = DATA$notification_duration,
                type = "warning"
            )
        } else if (value > 1) {
            if (value> (detectCores(all.tests = T, logical = use_logical_cores) - 1)) {
                value <- detectCores(all.tests = T, logical = use_logical_cores) - 1
            }
            updateNumericInput(session,inputId = "parallel_cores",value = as.numeric(value))
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
        }
    }
}
#' internal function handling dev-key `---ca`
#'
#' @inheritParams .main_args
#'
#' @importFrom stringr str_c
#' @importFrom shiny updateNumericInput
#'
#' @noRd
#' @keywords internal
#'
key_handle_identifierArea <- function(input, DATA, session) {
    current_id_area <- input$identifier_area
    if (current_id_area==0.503) {
        v <- as.numeric(14.535)
        updateNumericInput(session,inputId = "identifier_area",value = v)
    } else {
        v <- as.numeric(0.503)
        updateNumericInput(session,inputId = "identifier_area",value = v)
    }
    showNotification(
        ui = str_c("The identifier-area has been set to '",v,"'"),
        duration = DATA$notification_duration,
        type = "warning"
    )
}

#' internal function exposing the custom search-root for myself.
#'
#' @inheritParams .main_args
#'
#' @noRd
#' @keywords internal
#'
key_handle_setSearchFolder <- function(input, DATA, session) {
    DATA$search_root <- normalizePath("D:\\Dokumente neu\\Obsidian NoteTaking\\BE31-Thesis-quarto\\assets\\data")
    showNotification(
        ui = str_c("Changed 'Search_root'"),
        duration = DATA$notification_duration,
        type = "warning"
    )
}
