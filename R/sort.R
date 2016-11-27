#' @rdname naturalsort
#' @export
naturalsort <- function(text, decreasing=FALSE, na.last=NA,
                        interpretation=c("natural", "integer", "real", "scientific")) {
  interpretation <- match.arg(interpretation)
  text[naturalorder(text, decreasing=decreasing, na.last=na.last, interpretation=interpretation)]
}
