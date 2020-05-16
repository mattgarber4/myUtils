#' Find the Last Word of Each String
#'
#' @param string a character or character vector
#' @return A character or character vector containing the last word of each entry
#' @examples
#' findLastWord("hello world") # "world"
#' findLastWord(c("hello world", "hello mom")) # c("world", "mom")
findLastWord <- function(string) {
    y <- trimws(string)
    ll <- gregexpr(" ", y)
    accum <- mapply(function(ll, y) {
        lastSpace <- ll[length(ll)]
        if (!(lastSpace %in%  c(0, -1))) {
            substr(y, start = lastSpace + 1, stop = nchar(y))
        } else {
            y
        }
    },
    ll,
    y)

    # accum is a matrix; diagonal is where both indeces are the same
    return(accum)
}


#' Capitalize a Character Vector
#'
#' @param string a character or character vector
#' @param all logical, defauls to \code{FALSE}. If \code{TRUE}, will capitalize all words in each entry. Otherwise, only capitalizes first word.
#' @return A character or character vector with either the first or all words capitalized.
#' @examples
#' capitalize("hello world") # "Hello world"
#' capitalize(c("hello world", "hello mom")) # c("Hello world", "Hello mom")
#' capitalize(c("hello world"), all = T) # "Hello World"
#' capitalize(c("hello world", "hello mom"), all = T) # c("Hello World", "Hello Mom")
capitalize <- function(string, all = F) {
    # capitalize all words
    if (all) {
        # apply helper
        sapply(string, .help, USE.NAMES = F)

    } else { # capitalize first word (default)
        substr(string, 1, 1) <- toupper(substr(string, 1, 1))
        string
    }
}

# helper
.help <- function(string) {
    string.split <- unlist(strsplit(string, split = " "))
    capitalize(string.split) %>%
        paste(collapse = " ")
}

#' Collapse all white space blocks into a single space
#'
#' @param str a character or character vector
#' @return A character or character vector with only single spaces
#' @examples
#' collapseWS("hello    world") # "hello world"
#' @note This has a recursive implementation, so should not be used to remove spaces of extreme length
collapseWS <- function(str) {
    if (FALSE %!in% (gsub("  ", " ", str) == str)) {
        return(str)
    }
    return(collapseWS(gsub("  ", " ", str)))
}


#' Determine the number of words in a given string
#'
#' @param str a character
#' @return A numeric indicating how many words are in the input.
#' @examples
#' nwordsfun("hello world") # 2
nwordsfun <- function(str) {
    str <- trimws(str) %>% collapseWS()
    if (str == "") {
        return(0)
    }

    n <- gregexpr("\\s", str)[[1]]

    if (n[1] == -1) {
        return(1)
    }

    length(n) + 1

}


#' Fix annoying encoding issues
#'
#' @param txt a character
#' @return The same character but curly quotes and apostrophes subbed for their straight equivalents
replaceCurlies <- function(txt) {
    txt <- gsub("”|“", '"', txt)
    txt <- gsub("’|‘", "'", txt)
    # txt <- gsub("â€™", "'", txt)
    # txt <- gsub("â€œ|â€\u009", '"', txt)
    return(txt)
}
