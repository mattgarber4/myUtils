\name{uniqueID}
\alias{uniqueID}
\title{Unique ID}
\usage{
uniqueID(x)
}
\description{
Determines if the vector or matrix supplied is unique along the first dimension. Useful for preparing for merges.
}
\examples{
id1 <- c(1, 2, 5, 10)
id2 <- c(1, 2, 1, 10)
id3 <- cbind(id1, id2)
uniqueID(id1) ## TRUE
uniqueID(id2) ## FALSE
uniqueID(id3) ## TRUE
}
