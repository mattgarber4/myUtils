#' A 2D map from 2D input to any output class. Note that keys must be numerics, and values are typechecked.
#'@name refClassMap
#'@field .class The class of the output
#'
library(hash)
library(aoos)
refClassMap <- setRefClass("refClassMap",
                           fields = list(
                               .hashTable = "environment",
                               .class = "character"
                           ),
                           contains = "Private",
                           methods = list(
                               set = function(row, col, ref) {
                                   "Add element at this row/col location. Returns previous element"
                                   if (!is.numeric(row) | !is.numeric(col)) {
                                       stop("Must only use numeric keys")
                                   }
                                   key <- paste(row, col, sep = ",")
                                   if (is.null(ref)) {
                                       .hashTable$del(key)
                                       return(out)
                                   }

                                   if (class(ref) != .class) {
                                       stop(paste0("May only add ", class, " instances"))
                                   }

                                   out <- .hashTable[[key]]
                                   .set(.hashTable, key, ref)
                                   return(out)

                               },
                               get = function(row, col) {
                                   "Return element(s) at this location(s), or null "
                                   key <- paste(row, col, sep = ",")
                                   if (length(key) > 1) {
                                       return(sapply(key, function(k) .hashTable[[k]]))
                                   } else {
                                       return(.hashTable[[key]])
                                   }

                               },
                               size = function() {
                                   "Number of elements in map"
                                   return(.hashTable$length())
                               },
                               initialize = function(class) {
                                   "Generate new instance mapping to given class"
                                   .class <<- class
                                   .hashTable <<- hash()
                               }

                           ))

