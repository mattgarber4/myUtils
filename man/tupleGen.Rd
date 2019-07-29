\name{tuples}
\alias{tupleGen}
\alias{permute}
\title{Generate Tuples from Given Set}
\description{
Determines if the vector or matrix supplied is unique along the first dimension. Useful for preparing for merges.
}
\usage{
tupleGen(setn, k, as.num = FALSE, unique = FALSE, collapsed = FALSE)
permute(setn, as.num = FALSE)
}
\arguments{
\item{setn}{a vector, the set of elements of which to find combinations or permutations}
\item{k}{numeric, the size of the tuples}
\item{as.num}{logical, defaults to \code{FALSE}. Determines whether to coerce to numeric. If setn is a purely numeric input, \code{as.num} will be forced to \code{TRUE} and can only be coerced to a character after the function call.}
\item{unique}{
logical, defaults to \code{FALSE}. Determines if elements may be duplicated in output}
\item{collapsed}{ogical, defaults to \code{FALSE}. Determines whether or not to collapse each tuple to a string. Useful for random words or numbers.}
}
\value{For \code{tupleGen()}, a matrix or vector of the specified combinations. For \code{permute()}, a matrix of vector of the permutations of the given set.}
\section{Warning:}{These functions are currently implemented with a variety of loops, self-written code, and a top-down structure that creates and then searches all possibilities in order to subsets. Combinatorics generally grow rapidly with inputs, so be careful to manage memory. A faster implementation is most likely in the pipeline.}
\note{\code{permute()} is merely a shorthand for calling:\cr \code{tupleGen(setn, length(setn), as.num, unique = TRUE, collapsed = FALSE)}}
