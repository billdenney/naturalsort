% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order.R, R/sort.R
\name{naturalorder}
\alias{naturalorder}
\alias{naturalsort}
\title{Natural Ordering Sort}
\usage{
naturalorder(text, decreasing = FALSE, na.last = TRUE,
  interpretation = c("natural", "integer", "real", "scientific"))

naturalsort(text, decreasing = FALSE, na.last = NA,
  interpretation = c("natural", "integer", "real", "scientific"))
}
\arguments{
\item{text}{a character vector to sort.}

\item{decreasing}{logical.}

\item{na.last}{logical. If \code{NA}, \code{NA}s will be removed of the result.}

\item{interpretation}{Interpret the numbers as one of \code{"natural"} numbers,
\code{"integers"} (including plus or minus before the number),
\code{"real"} numbers (including plus or minus and decimal points),
or \code{"scientific"} notation (real numbers with with an optional
upper or lower case "E", an optional plus or minus, and a natural
number).}
}
\value{
For \code{naturalorder}, the results are indices of vector elements in natural order.
For \code{naturalsort}, the results are sorted vectors.
}
\description{
Natural ordering is a kind of alphanumerical ordering.
\code{naturalorder} returns the order of the argument character #' vector in human natural ascending or descending order.
\code{naturalsort} returns the sorted vector.
}
\examples{
text <- c("a-1.png", "a-2.png", "a-10.png")
print(sort(text))
print(naturalsort(text))

}

