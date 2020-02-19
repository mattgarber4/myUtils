# writing combination cases with k-tuples over a cardinality n set
# helper function assigns the jth column the proper set of repetitions
.colassign <- function(setn, k, j, as.num = FALSE){
    txt <- c()
    for(i in 1:length(setn)){
        repNumb <- length(setn)^(k - j)
        txt[i] <- paste('rep("',
                        setn[i],
                        '", ',
                        repNumb,
                        ')',
                        sep = "")
    }

    txtOut <- paste(txt, collapse = ", ")
    txtOut <- paste("c(", txtOut, ")", sep = "")
    block <- eval(parse(text = txtOut))
    out <- rep(block, length(setn)^(j-1))

    if(as.num == TRUE) {
        out <- as.numeric(out)
    }

    return(out)
}


tupleGen <- function(setn, k, as.num = FALSE, unique = FALSE, collapsed = FALSE){
    setn <- setn[!duplicated(setn)]
    n <- length(setn)
    if (identical(setn, as.numeric(setn))) {
        as.num <- TRUE
    }
    test <- matrix(nrow = n^k, ncol = k)
    for(i in 1:k){
        test[,i] <- .colassign(setn, k, i, as.num)
    }

    if(unique == TRUE) {
        test <- test[!apply(test,1,.repeated),]
    }

    if (collapsed) {
        return(.toVec(test))
    } else {
        return(test)
    }
}


permute <- function(setn, as.num = FALSE) {
    out <- .permuter(setn)

    if (as.num) {
        for (i in 1:dim(out)[1]) {
            out[, i] <- as.numeric(out[, i])
        }
    }

    colnames(out) <- NULL
    as.matrix(out)
}

.permuter <- function(setn) {
    if (length(setn) == 1) {
        return(setn[1])
    }

    len <- factorial(length(setn) - 1)
    lapply(1:length(setn), function(i) {
        cbind(
            rep(setn[i], len),
            .permuter(setn[-i])
        )
    }) %>% myBind(type = "df.row")
}


# function to concatinate rows into words (matrix to vector)
.toVec <- function(matrix){
    outVec <- apply(matrix,1,.easyCollapse)
    return(outVec)
}

.easyCollapse <- function(x){
    return(paste(x,collapse = ""))
}

# helper function to satisfy no duplicates case
.repeated <- function(x){
    return(1 %in% duplicated(x))
}

