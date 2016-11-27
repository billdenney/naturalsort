#' Natural Ordering Sort
#' 
#' Natural ordering is a kind of alphanumerical ordering.
#' \code{naturalorder} returns the order of the argument character #' vector in human natural ascending or descending order.
#' \code{naturalsort} returns the sorted vector.
#' 
#' @param text
#' a character vector to sort.
#' @param decreasing
#' logical.
#' @param na.last
#' logical. If \code{NA}, \code{NA}s will be removed of the result.
#' @param interpretation
#' Interpret the numbers as one of \code{"natural"} numbers,
#' \code{"integers"} (including plus or minus before the number),
#' \code{"real"} numbers (including plus or minus and decimal points),
#' or \code{"scientific"} notation (real numbers with with an optional
#' upper or lower case "E", an optional plus or minus, and a natural
#' number).
#' 
#' @return
#' For \code{naturalorder}, the results are indices of vector elements in natural order.
#' For \code{naturalsort}, the results are sorted vectors.
#' 
#' @examples
#' text <- c("a-1.png", "a-2.png", "a-10.png")
#' print(sort(text))
#' print(naturalsort(text))
#'
#' @rdname naturalsort
#' @export
naturalorder <- function(text, decreasing=FALSE, na.last=TRUE,
                         interpretation=c("natural", "integer", "real", "scientific")) {  # different with base::order in order or arguments
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    decreasing <- as.logical(decreasing)[1]
  }
  if (is.na(decreasing)) {
    stop("'decreasing' must be either TRUE or FALSE")
  }
  if (!is.logical(na.last) || length(na.last) != 1) {
    na.last <- as.logical(na.last)[1]
  }
  interpretation <- match.arg(interpretation)
  if (!is.character(text)) {
    text <- as.character(text)
  }
  if (length(text) == 0L) {
    return(integer(0L))
  }
  # Save the positions that are NA
  idx.na <- (1:length(text))[is.na(text)]
  idx.not.na <- (1:length(text))[!is.na(text)]
  # Drop NA values for now
  text <- text[idx.not.na]

  patterns <- list(natural="\\d+",
                   integer="[+-]?\\d+",
                   real="[+-]?\\d*\\.?\\d+",
                   scientific="[+-]?\\d*\\.?\\d+([eE][+-]?\\d+)?")
  ## If strsplit is applied to an empty character, an empty character vector is returned.
  ## Therefore, if all elements in 'text' are empty, 'maxLength' will be 0.
  ## Otherwise, when there is at least one ordinal value or NA in 'text', 'maxLength' will be greater than 0.
  tokenMatches <- base::gregexpr(patterns[[interpretation]], text=text, perl=TRUE)
  tokenList <- base::lapply(
    seq_along(text),
    FUN=function(i, matches, text) {
      if (matches[[i]][1] == -1) {
        # The pattern did not match, so it is all text. Return the text 
        # unmodified.
        list(text[[i]])
      } else {
        starts <- matches[[i]]
        lengths <- attr(matches[[i]], "match.length")
        if (starts[[1]] == 1) {
          ret <- list()
        } else {
          # Get the text at the beginning of the string
          ret <- list(Inf, substr(text[[i]], 1, starts[[1]] - 1))
        }
        # Extract all the matches and the text between
        for (j in seq_along(starts)) {
          end <- starts[[j]] + lengths[[j]] - 1
          numbertext <- substr(text[[i]], starts[[j]], stop=end)
          ret <- append(ret, list(as.numeric(numbertext), numbertext))
          if (length(starts) > j &&
              end + 1 != starts[[j + 1]]) {
            # There is text after this number and before the next one.
            ret <- append(ret, substr(text[[i]], end + 1, starts[[j + 1]] - 1))
          }
        }
        # If there is text after the last match, grab it.
        if (end < nchar(text[[i]])) {
          ret <- append(ret, substr(text[[i]], end + 1, nchar(text[[i]])))
        }
        ret
      }
    },
    matches=tokenMatches,
    text=text)
  maxLength <- max(sapply(tokenList, length))
  if (maxLength == 0L) {  # all elements are empty ("").
    return(seq_along(text))
  }
  # Expand the lists so that they are all the same length
  sortableList <-
    lapply(tokenList,
           FUN=function(x, totalLength) {
             append(x, rep(NA, totalLength - length(x)))
           },
           totalLength=maxLength)
  # Make the lists each atomic vectors of either character or numeric
  # (alternating)
  sortableList <-
    unname(lapply(as.data.frame(do.call(rbind, sortableList)), unlist))
  # Find the ordered index of the remaining items in the lists (after
  # NAs have been removed)
  ret <- idx.not.na[do.call(order, append(sortableList, list(decreasing=decreasing, na.last=FALSE)))]
  if (!is.na(na.last)) {
    # Keep the NA values
    if (na.last) {
      ret <- c(ret, idx.na)
    } else {
      ret <- c(idx.na, ret)
    }
  }
  ret
}
