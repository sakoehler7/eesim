#' Simulate binary exposure data
#'
#' This function simulated binary exposure data.
#'
#' @param n A numeric value giving the number of days to simulate.
#' @param p A numeric value giving the baseline probability of exposure for
#'    a binary exposure.
#'
#' @return A numeric vector with simulated exposure of length n.
#'
#' @examples
#' season_binexp(5, 0.25)
#'
#' @export
season_binexp <- function(n, p){
  p <- p #Change this later to reflect probability varying by season using trends
  binexp <- sample(c(0,1), size = n, replace = T, prob = c(1-p, p))
  return(binexp)
}
