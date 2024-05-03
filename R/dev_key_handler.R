#' handles the setting and unsetting of debug-keys
#'
#' @param input - shiny-app `input`-object
#' @param DATA - shiny-app `DATA`-reactives
#' @param DEBUGKEYS - shiny-app `DEBUGKEYS`-reactives
#'
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @importFrom stringr str_c
#' @importFrom stringr str_flatten_comma
#' @importFrom stringr str_count
#' @importFrom shiny showNotification
#'
#' @return modified `DEBUGKEYS`?
#' @keywords internal
#'
dev_key_handler <- function(input, DATA, DEBUGKEYS) {
    # add valid keys here
    # private, undocumentable keys must be prefixed with 3 `-`
    Arr <- str_split("--force-prints,--force-log,---set-author,-h",",")
    # then add them to the reactive 'DEBUGKEYS' so that it can be accessed elsewhere as well.
    Keys <- unlist(str_split(input$dev_pass,","))
    for (each in Keys) {
        each_ <- str_remove_all(each,"=(1|0|FALSE|TRUE)")
        each_ <- str_remove_all(each,"=(F|T)")
        if ((each %in% Arr[[1]]) || (each_ %in% Arr[[1]])) { # BUG: this bool returns an array if `each` is a char-vector itself.
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
                if (str_count(each,"---")) { # non-boolean flags
                    key <- str_replace_all(str_replace_all(each,"---",""),"--","")
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
