#' Confidence interval (lower 2.5\%)
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#'
#' @return 2.5% quantile of x
#'
#' @examples
#' x <- rnorm(1000, mean = 0, sd = 1)
#' ci_lower(x)
#' @export
ci_lower <- function(x, na.rm = FALSE) {
  stats::quantile(x, 0.025, na.rm = na.rm)
}

#' Confidence interval (upper 97.5\%)
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#'
#' @return 97.5% quantile of x
#'
#' @examples
#' x <- rnorm(1000, mean = 0, sd = 1)
#' ci_upper(x)
#' @export
ci_upper <- function(x, na.rm = FALSE) {
  stats::quantile(x, 0.975, na.rm = na.rm)
}
