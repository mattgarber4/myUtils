myBind <- function(x, left = 1, right = length(x), type = "df.row") {
    # execute helper function from top of data
    if (type == "vec") {
        return(.myBindHelper(x, left = left, right = right, type = type))
    }

    return(data.frame(.myBindHelper(x, left = left,
                                    right = right,
                                    type = type)))
}



.myBindHelper <- function(x, left, right, type) {
    if (type == "vec") {
        binder <- c
        if (!is.vector(x[[1]])) {
            if  (1 %in% dim(x[[1]]) & length(dim(x[[1]])) <= 2) {
                x <- lapply(x, function(y) {
                    if (dim(x[[1]])[1] == 1) {
                        y <- as.vector(y)
                        names(y) <- NULL
                        return(y)
                    } else {
                        y <- as.vector(t(y))
                        names(y) <- NULL
                        return(y)
                    }

                    })
            } else {
                stop("Not a vector and not coercable to a vector.")
            }

        }
    } else if (type == "df.row") {
        binder <- rbind
    } else if (type == "df.col") {
        binder <- cbind
    } else {
        stop("Improper binding method.")
    }

    return(.recHelp(x, left, right, binder))
}

.recHelp <- function(x, left, right, binder) {
    if (left == right) {
        return(x[[left]])
    }

    mid <- floor((left + right) / 2)
    return(binder(.recHelp(x, left, mid, binder),
                  .recHelp(x, mid + 1, right, binder)))
}


myBindVec <- function(x, left = 1, right = length(x)) {
    # recursive function: implements "divide and conquer" merge algorithm
    myBind(x, left, right, "vec")
}






