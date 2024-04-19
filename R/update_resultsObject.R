update_resultsObject <- function(resultsObject,new_line) {
    resultsObject <- rbind(resultsObject,new_line)
    return(resultsObject)
}
