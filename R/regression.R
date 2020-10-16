#' Build a regression formula for the dependent variable by the covariates, 
#' parsing form text form
#'
#' @param depVar a string, the dependent variable
#' @param covariates a string or vector of strings, the covariates
#' @return The regression formula
regFormula <- function(depVar, covariates) {
    eval(
        parse(
            text = paste(
                depVar,
                '~',
                paste(covariates, collapse = ' + ')
                )
            ),
        envir = parent.env(environment())
        )
}
