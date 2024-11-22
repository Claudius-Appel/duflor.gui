#' generate a ggplot for a given KPI
#'
#' Function generates a ggplot-object base on the selected choices in the DDLs
#'
#' @inheritParams .main_args
#'
#' @return ggplot-object for the respective KPI
#' @keywords internal
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom utils hasName
#' @importFrom shiny updateActionButton
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shiny removeNotification
#' @importFrom shiny showNotification
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#'
get_KPI_plot <- function(input, DATA) {
    if (input$reinspected_type2 == "area_per_pixel") {
        key <- input$reinspected_type2
    } else {
        key <- str_c(input$reinspected_spectrums2, input$reinspected_type2)
    }

    key2 <- NULL
    if (input$do_correct_distortion) {
        if (input$reinspected_type2 == "area_per_pixel") {
            key2 <- str_c(input$reinspected_type2, "_undistorted")
        } else {
            key2 <- str_c(input$reinspected_spectrums2, input$reinspected_type2, "_undistorted")
        }
    }

    if (!hasName(DATA$results$results, key)) {
        showNotification(
            ui = "This spectrum was not analysed for this image.",
            id = "spectrum.not.found.ggplot",
            duration = NA,
            type = "warning"
        )
        updateActionButton(session = getDefaultReactiveDomain(), inputId = "save_visualisation_plot", disabled = TRUE)
        return(ggplot())
    }

    filtered_data <- DATA$results$results[[key]]
    filtered_data_undistorted <- NULL
    if (!is.null(key2) && hasName(DATA$results$results, key2)) {
        filtered_data_undistorted <- DATA$results$results[[key2]]
    }

    names <- DATA$results$results$image_name
    group <- get_group(names)
    x_label <- "Files"

    if (str_count(key, "_count") > 0) {
        y_label <- str_c(key, " [/]")
    } else if (str_count(key, "_fraction") > 0) {
        y_label <- str_c(key, " [%]")
        filtered_data <- filtered_data * 100
        if (!is.null(filtered_data_undistorted)) {
            filtered_data_undistorted <- filtered_data_undistorted * 100
        }
    } else if (str_count(key, "_area") > 0) {
        y_label <- str_c(key, " [cm^2]")
    } else if (str_count(key, "_per_pixel") > 0) {
        y_label <- str_c(key, " [cm^2]")
    }

    # Construct data
    data <- data.frame(
        names = names,
        value = filtered_data,
        group = group,
        type = "Distorted"
    )

    if (!is.null(filtered_data_undistorted)) {
        data_undistorted <- data.frame(
            names = names,
            value = filtered_data_undistorted,
            group = group,
            type = "Undistorted"
        )
        data <- rbind(data, data_undistorted)
    }

    if (!is.null(filtered_data_undistorted)) {
        plt <- ggplot(data, aes(x = names, y = value, fill = type)) +
            geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
            labs(x = x_label, y = y_label) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
        plt <- ggplot(data, aes(x = names, y = filtered_data, fill = group)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = x_label, y = y_label) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

    updateActionButton(session = getDefaultReactiveDomain(), inputId = "save_visualisation_plot", disabled = FALSE)
    return(list(plt = plt, key = key))
}
