#' write results of the analysis to file
#'
#' @param results object to write.
#' @param results_path path to write file to, sans the filesuffix.
#' @param save_to_xlsx should results be written into a `.xlsx`-file? If false, it will be written into a `.csv`-file instead.
#' @param set_author_xlsx when writing to an xlsx-file, should the author be set to `generated with 'duflor' via 'duflor.gui', on user_machine XX`?
#'
#' @note
#' `XLSX`-files are the suggested output if any additional postprocessing is required.
#' If the data will not be modified again, writing it to `csv` will save space.
#'
#' @return list containing
#' - boolean check whether or not file was written to disk successfully
#' - complete path to the resulting file.
#'
#' @importFrom openxlsx write.xlsx
#' @importFrom utils write.csv2
#' @importFrom stringr str_count
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_c
#' @keywords internal
#'
store_results_to_file <- function(results, results_path, save_to_xlsx=FALSE, set_author_xlsx=FALSE) {
    ## 1. find free csv-name next to image-source-files
    ## 2. save df as csv if `save_to_xlsx==F`, else save to xlsx
    if (str_count(results_path, "\\.(xlsx|csv)")) {
        results_path <- str_replace_all(
            string = results_path,
            pattern = "\\.(xlsx|csv)",
            replacement = "")
    }
    if (isTRUE(save_to_xlsx)) {
        results_path <- str_c(results_path,".xlsx")
        dir <- dirname(results_path)
        if (isFALSE(dir.exists(dir))) {
            dir.create(dir)
        }
        write.xlsx(
            results,
            asTable = T,
            file = results_path,
            creator = ifelse(
                set_author_xlsx,
                str_c(
                    "generated with 'duflor' via 'duflor.gui', on user_machine ",
                    Sys.getenv("USERNAME")
                )
                ,
                ""
            )
        ) ## sign the file with being created by this username on this machine.
    } else {
        results_path <- str_c(results_path,".csv")
        dir <- dirname(results_path)
        if (isFALSE(dir.exists(dir))) {
            dir.create(dir)
        }
        write.csv2(x = results,file = results_path,col.names = F,row.names = T,fileEncoding = "UTF-8")
    }
    return(list(success = file.exists(results_path),results_path = results_path))
}
