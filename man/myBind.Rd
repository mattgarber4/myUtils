\name{myBind}
\alias{myBind}
\alias{myBindVec}
\title{Recursive Non-Complaining Bind Functions}
\usage{
myBind(x, left = 1, right = legnth(x), type = "df.row")
myBindVec(x, left = 1, right = length(x))
}
\arguments{
\item{x}{a list of dataframes, vectors, or matrices}
\item{left}{numeric, the index of the first element to include. Defaults to \code{1} so as to bind entire list by default.}
\item{right}{numeric, the index of the last element to include. Defaults to \code{length(x)} to bind entire list by default.}
\item{type}{character, indicates output. Defaults to \code{"df.row"}. \code{"vec"} binds to a vector. \code{"df.row"} binds by row to a dataframe. \code{"df.col"} binds by column to a dataframe.}
}
\description{
Binds elements of a list to each other as a data frame or a vector. \code{myBindVec} is depricated. Use \code{myBind{..., type = "vec"}}
}

