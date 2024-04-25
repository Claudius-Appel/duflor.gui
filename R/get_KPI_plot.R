#' Title
#'
#' @return ggplot-object for the respective KPI
#' @keywords internal
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom utils hasName
#' @importFrom shiny updateActionButton
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shiny removeNotification
#' @importFrom shiny showNotification
#'
get_KPI_plot <- function(input, DATA) {
    if (input$reinspected_spectrums2=="area_per_pixel") {
        key <- input$reinspected_spectrums2
    } else {
        key <- str_c(input$reinspected_spectrums2, input$reinspected_type2)
    }
    if (hasName(DATA$results$results, key)) {
        removeNotification("spectrum.not.found.ggplot")
        filtered_data <- DATA$results$results[[key]]
    } else {
        # handle spectrums not present in results-set
        showNotification(
            ui = "This spectrum was not analysed for this image.",
            id = "spectrum.not.found.ggplot",
            duration = NA,
            type = "warning"
        )
        return(ggplot())
    }
    names <- DATA$results$results$image_name
    group <- get_group(names)
    # construct data
    data <-
        data.frame(filtered_data = filtered_data,
                   names = names,
                   group = group)
    plt <- ggplot(data, aes(x = names, y = filtered_data, fill = group)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Names", y = "Filtered Datra") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(list(plt = plt, key = key))
}
