#' Natural Ordering Factor
#' 
#' \code{naturalfactor} creates a factor with levels in natural order.
#' 
#' @param x
#' a character vector.
#' @param levels
#' a character vector whose elements might be appeared in \code{x}.
#' @param ordered
#' logical flag that determines whether the factor is ordered.
#' @param interpretation
#' Passed to \code{\link{naturalorder}}
#' @param ...
#' arguments that are passed to \code{factor} function.
#' 
#' @rdname naturalfactor
#' @export
naturalfactor <- function(x, levels, ordered = TRUE,
                          interpretation=c("natural", "integer", "real", "scientific"), ...) {
   text <- as.character(x)
   interpretation <- match.arg(interpretation)
   if (missing(levels)) {
      levels <- unique(text)
   }
   levels <- naturalsort(levels, interpretation=interpretation)
   factor(text, levels = levels, ordered = ordered, ...)
}
