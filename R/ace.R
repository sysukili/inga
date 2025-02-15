#' Average Causal Effect
#'
#' @param x a value
#' @param y a value
#'
#' @returns a value
#' @export
#'
#' @examples
#' ace(1:10, 11:20)
ace <- function(x, y) {
  mean(x) - mean(y)
}


