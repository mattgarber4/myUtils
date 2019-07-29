# uniqueID
uniqueID <- function(x) {
    if(is.vector(x)){
        return(length(unique(x)) == length(x))
    } else if(is.matrix(x) | is.data.frame(x)) {
        return(dim(unique(x))[1] == dim(x)[1])
    } else {
        print("not a data frame, matrix, or vector")
    }
}




