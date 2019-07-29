.natoempty <- function(test) {
    if (is.character(test) | sum(is.na(test)) == length(test)) {
        return(ifelse(is.na(test), "", test))
    } else {
        return(test)
    }
}

#' Read Data in with Empty Cells to Empty Strings
#'
#' @param ... Arguments to be passed to read.csv
read.csv3 <- function(...) {
    temp <- read.csv(...)
    for (col in colnames(temp)) {
        temp[,col] <- .natoempty(temp[,col])
    }
    return(temp)
}


