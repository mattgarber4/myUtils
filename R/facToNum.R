#' Coerse Factor to Numeric
#'
#' @param x a factor
#' @return The factor's names as numerics.
#' @note This is simply a shorthand for a standard converstion to numeric.
facToNum <- function(x) {
    as.numeric(as.character(x))
}
