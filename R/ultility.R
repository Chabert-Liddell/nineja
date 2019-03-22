

#' Simulate an adjacency matrix
#'
#' @param n an integer: the number of nodes
#' @param K an integer: the number of cluster
#' @param alpha a matrix:
#' @param pi a vector of size K: the mixture parameter
#' @param directed Wether the graph is directed or not
#'
#' @return A n*n adjacency matrix
#' @export
#'
#' @examples
simulate_adjacency <- function(n, K, alpha, pi,  directed = FALSE) {
 Z <- sample(x = seq(K), size = n, replace = TRUE, prob = pi)
  X = matrix(0, n, n)
  for (i in 1:(n-1)){
    X[i, (i+1):n] <-  stats::rbinom(n-i, 1, alpha[Z[i], Z[(i+1):n]])
  }
  if (! directed) {
    X = X + t(X)
  } else {
    for (i in 2:n) {
      X[i, 1:(i-1)] <-
        stats::rbinom(i-1, 1, alpha[Z[i], Z[1:(i-1)]])
    }
  }
  return(X)
}
