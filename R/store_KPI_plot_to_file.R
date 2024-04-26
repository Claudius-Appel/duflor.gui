#' save KPI-plot to file
#'
#' @param input - `input` respective shiny-component
#' @param DATA - `DATA` respective shiny-component
#'
#' @keywords internal
#' @importFrom shiny showNotification
#' @importFrom ggplot2 ggsave
#' @importFrom stringr str_c
#'
store_KPI_plot_to_file <- function(input, DATA) {
    results_path <- str_c(dirname(DATA$results$results$full_path[[1]]),"/results/")
    dir <- dirname(results_path)
    if (isFALSE(dir.exists(dir))) {
        dir.create(dir)
    }
    if (input$reinspected_spectrums2=="area_per_pixel") {
        filename <- str_c(DATA$current_KPI_key,".png")
    } else {
        filename <- str_c(DATA$current_KPI_key,".png")

    }
    ggsave(filename = filename,plot = DATA$current_KPI_plot,create.dir = T,path = results_path,bg = "white",dpi = 300,width = 7,height = 5)
    ## verify save was successfull
    if (file.exists(str_c(results_path,"/",filename))) {
        showNotification(
            ui = "KPI-Plot has been written to '",
            str_c(results_path,filename),
            "'",
            duration = DATA$notification_duration,
            type = "message"
        )
    } else {
        showNotification(
            ui = "Analysis could not be completed successfully, and KPI-plot could not be successfully written to '",
            str_c(results_path,filename),
            "'",
            duration = DATA$notification_duration * 4,
            type = "warning"
        )
    }
}
