#' Sets environment variables from a file
#' 
#' @param file The path to the file containing the variables to add. File must be a list of variables formatted "varname=varvalue" with no quotes, one per line
setEnvVarsFromFile <- function(file) {
    s <- suppressWarnings(readLines(file))
    splits <- regexpr('=', s)
    vars <- cbind(
        substr(s, start = 1, stop = splits - 1), 
        substr(s, start = splits+1, stop = nchar(s))
    )
    args <- list()
    for (i in 1:nrow(vars)) {
        args[vars[i, 1]] = vars[i, 2]
    }
    
    do.call(Sys.setenv, args)
}
