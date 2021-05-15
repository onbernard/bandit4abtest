#' Generate a S matrix
#'
#' @description Generates the matrix used to store UCB computation variables.
#'   This is a 2 by K (number of arms) matrix with each columns corresponding to
#'   an arm. First row will store the estimated reward expectation, second row
#'   the number of times the arm was played.
#'
#'@param K Integer variable. Number of arms
#'
#'@return Numeric matrix
#'
#'@examples
#'K = 2
#'generate_matrix_S(K)
#'
#'@export
generate_matrix_S <- function(K) {
  S <- matrix(0,2,K)
  colnames(S) <- paste('arm', 1:K)
  rownames(S) <- c("average reward","trials")
  return(S)
}
