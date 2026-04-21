#' Power Function
#'
#' Raises 10 to the 1st through the nth power
#' @param x Highest power of 10 to use
#' @return Vector of 10 raised sequentially from the 1st to the nth power.
#' @keywords internal
power_function <- function(x) {
   10^(1:x)
}

# power_function <- function(x) {
#   as.integer(outer(c(1), 10^(1:x)))
# }
