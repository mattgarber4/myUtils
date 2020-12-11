.abb <- c(state.abb, 'DC')
.name <- c(state.name, 'District of Columbia')
.nametrans <- function(n) {gsub("Of", "of", capitalize(tolower(n), all = T))}
.abbtrans <- function(n) {toupper(n)}

.transStateNameAbb <- function(states, from.vec, to.vec, from.fun, to.fun) {
    out <- to.vec[match(from.fun(states), from.vec)]
    orig.idx <- is.na(out) & to.fun(states) %in% to.vec
    out[orig.idx] <- to.fun(states[orig.idx])
    out
}



#' Translate between states (plus DC) and their postal abbreviations
#'
#' @param states a character
#' @return States translated to postal abbreviations, or reverse
stateNameToAbb <- function(states) {
    .transStateNameAbb(states, from.vec = .name, to.vec = .abb, from.fun = .nametrans, to.fun = .abbtrans)
}

#' @aliases stateNameToAbb
stateAbbToName <- function(states) {
    .transStateNameAbb(states, from.vec = .abb, to.vec = .name, from.fun = .abbtrans, to.fun = .nametrans)
}




