#' Average Causal Inference
#'
#' This function computes the average causal inference between two groups.
#'
#' @param treatment a numeric vector representing the treatment group
#' @param control a numeric vector representing the control group
#'
#' @return a numeric value representing the average causal inference
#' @export
#'
#' @examples
#' treatment <- c(5, 10, 15, 20)
#' control <- c(3, 8, 12, 18)
#' ace(treatment, control)
ace <- function(treatment, control) {
    mean(treatment) - mean(control)
}
