#' Upper Confidence Bound computation function
#'
#'@description Computes, for each column of ucb computations variables matrix S
#'  an upper bound according to the Hoeffding inequality parameterized by
#'  exploration parameter alpha (default alpha = 1). Returns a vector containing
#'  each arm upper confidence bound.
#'
#'@param S     Numeric matrix. UCB computation variables.
#'@param iter  Numeric value. Index of iteration.
#'@param alpha Numeric value. Exploration parameter (optional)
#'
#'@return Numeric vector containing each arm upper confidence bound.
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'## Init the S Matrix
#'S <- generate_matrix_S(K=2)
#'S
#'## play arms uniformly
#'for(i in 1:nrow(visitor_reward)){
#'S <- play_arm(i,arm=(i%%K+1),S,visitor_reward)
#'}
#'## Results
#'S
#'proba_max_for_ucb(S=S,iter=i+1)
#'
#'@export
proba_max_for_ucb <- function(S, iter, alpha=1) {
  choice <- c()
  for (j in 1:ncol(S))
  {
    choice[j] <- S[1,j] + alpha * sqrt( (2*log(iter))/S[2,j])
  }
  return (choice)
}
