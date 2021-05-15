#'Update UCB computation variables matrix
#'
#' @description Updates the matrix S containing UCB computations variables according
#' a specific reward and iteration.
#' \itemize{ Retrieves the reward associated with the iter instant in the reward
#' dataframe
#' \item Updates the average reward of the chosen arm with the reward obtained
#' \item Increments the number of trial of said arm
#' \item Returns the updated S matrix }
#'
#'@param iter           Integer value. Index of iteration.
#'@param arm            Integer value. Index of chosen arm.
#'@param S              Numeric matrix. UCB computation variables.
#'@param visitor_reward Numeric matrix.
#'
#'@return S Updated UCB computation variables matrix.
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'## Number of arms
#'K=2
#'## Initialize the S Matrix
#'S <- generate_matrix_S(K)
#'S
#'## play arms uniformly
#'for(i in 1:nrow(visitor_reward)){
#'S <- play_arm(i,arm=(i%%K+1),S,visitor_reward)
#'}
#'## Results
#'S
#'@export
play_arm <- function(iter, arm, S, visitor_reward) {
  # mean
  S[1,arm] <- ((S[1,arm] * S[2,arm] + visitor_reward[iter,arm]) / (S[2,arm] + 1))
  # trials
  S[2,arm] = S[2,arm] + 1
  return (S)
}
