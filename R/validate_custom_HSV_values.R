#' ensure that custom HSV values are within required range
#'
#' For `HUE`, value must be within `[0, 359]`
#' For `SATURATION`, value must be within `[0, 1]`
#' For `VALUE`, value must be within `[0, 1]`
#'
#' @inheritParams .main_args
#'
#' @return list containing
#' - expected bounds
#' - keys for which a specific bound was violated, along its violated value
#' @keywords internal
#' @importFrom stringr str_c
#' @importFrom shiny showNotification
#' @importFrom shiny updateNumericInput
#'
validate_custom_HSV_values <- function(input, DATA, session) {
    expected_bounds <- list(lower = list(H = c(0,359),
                                         S = c(0,1),
                                         V = c(0,1)),
                            upper = list(H = c(0,359),
                                         S = c(0,1),
                                         V = c(0,1)))
    return_obj <- list()
    bound_type <- c("lower","upper")
    for (type in bound_type) {
        for (parameter in c("H","S","V")) {
            current_key <- str_c(type,"_bound_", parameter)
            if (hasName(input,current_key)) {
                current_value <- input[[current_key]]
                current_check <- expected_bounds[[type]][[parameter]]
                string <- ""
                if (current_value<current_check[[1]]) {
                    string <- str_c(
                        "The '",
                        parameter,
                        "'-component of the '",
                        type,
                        "_bound' was below '",
                        current_check[[1]],
                        "'. Its value was coerced to '",
                        current_check[[1]],
                        "'."
                    )
                    updateNumericInput(session = session,inputId = current_key, value = current_check[[1]])
                    return_obj[[current_key]] <- current_check[[1]]
                }
                if (current_value>current_check[[2]]) {
                    string <- str_c(
                        "The '",
                        parameter,
                        "'-component of the '",
                        type,
                        "_bound' was above '",
                        current_check[[2]],
                        "'. Its value was coerced to '",
                        current_check[[2]],
                        "'."
                    )
                    updateNumericInput(session = session,inputId = current_key, value = current_check[[2]])
                    return_obj[[current_key]] <- current_check[[2]]
                }
                if (string!="") {
                    showNotification(
                        ui = string,
                        duration = DATA$notification_duration * 5,
                        type = "warning"
                    )
                }
            }
        }
    }
    return(list(return_obj = return_obj, expected_bounds = expected_bounds))
}
