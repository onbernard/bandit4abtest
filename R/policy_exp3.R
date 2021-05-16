#'EXP3 algorithm
#'
#'@description Exponential  Weights  for  Exploration  and  Exploitation (EXP3)
#'  bandit strategy. It uses a list of weights which evolve according to arm's
#'  reward. The gamma parameter is a coefficient for balancing between
#'  exploitation and exploration.
#'
#'  When processing a dataframe of reward representing a bandit, the function
#'  keeps track of each arm estimated reward expectation and number of trials.
#'  These are returned at the end of the computation in addition to the arm
#'  played and its associated probability at each iteration, the actual reward
#'  expectations, the computation time and the final weights values.
#'
#'  See also  \code{\link{condition_for_exp3}}, \code{\link{generate_matrix_S}},
#'  and \code{\link{play_arm}}.
#'
#'  Reward input is checked for correct dimensions and values. These must be
#'  binary (either numeric or integer ones and zeros) ! See
#'  \code{\link{bandit_reward_control}} and \code{\link{control_binary}}.
#'
#'@param visitor_reward Dataframe of numeric values
#'@param gamma          Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S         : Means and trials matrix
#'  \item choice    : Choice history vector
#'  \item proba     : Max probability history vector
#'  \item time      : Computation time
#'  \item theta_hat : Final estimated reward expectation of each arm
#'  \item theta     : Actual reward expectation of each arm
#'  \item weight    : Final probability weight of each arm}
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'EXP3_alloc <- policy_exp3(visitor_reward)
#'EXP3_alloc$S
#'EXP3_alloc$time
#'EXP3_alloc$theta
#'EXP3_alloc$theta_hat
#'
#'@export
policy_exp3 <- function(visitor_reward, gamma=0.05){
  # Data control
  bandit_reward_control(visitor_reward)
  # Data formatting
  visitor_reward <- as.matrix(visitor_reward) * 1
  # More data control
  control_binary(visitor_reward)

  K <- ncol(visitor_reward)
  # Vector of probability weights
  weight <- rep(1, times=K)
  # Choice, reward and reward probability history vector
  choice <- c()
  reward <- c()
  proba <- c()
  # Means and trials matrix
  S <- generate_matrix_S(K)

  tictoc::tic()

  # Initialization : play each arm once
  for(i in 1:K) {
    S <- play_arm(iter=i, arm=i, S=S, visitor_reward)
    choice[i] <- i
    # Get reward
    reward[i] <- visitor_reward[i,i]
    # Update
    expectation <- (1 - gamma) * (1/K) + (gamma/K)
    proba[i] <- reward[i]/expectation
    weight[i] <- exp(gamma*proba[i]/K)
  }
  # Iterate over horizon
  for(i in (K+1):nrow(visitor_reward)) {
    # Compute vector of probabilities and sample it
    P <- (1 - gamma) * (weight/sum(weight)) + (gamma/K)
    choice[i] <- condition_for_exp3(K, P)
    # Get reward
    reward[i] <- visitor_reward[i,choice[i]]
    # Update
    S <- play_arm(iter=i, arm=choice[i], S, visitor_reward)
    proba[i] <- reward[i]/P[choice[i]]
    weight[choice[i]] <- weight[choice[i]]*exp(gamma*proba[i]/K)
  }

  time <- tictoc::toc()

  # Real and estimated coefficient estimates
  th = colMeans(visitor_reward)
  th_hat=S[1,]

  cat("th_hat", fill=2)
  cat(th_hat, fill=TRUE)
  cat("th real", fill=2)
  cat(th, fill=TRUE)

  return(list('S'=S,
              'proba'=proba,
              'time'=(time$toc - time$tic),
              'choice'= choice,
              'theta_hat'=th_hat,
              'theta'=th,
              'weight'=weight))
}

#'Condition for EXP3
#'
#'@description Samples the arm list according to a vector of probability weights
#'
#'@param K     Number of arms
#'@param proba Vector of probability weights
#'
#'@return Integer value
#'
#'@export
condition_for_exp3 <- function(K, proba) {
  return(sample(1:K, size=1, replace=TRUE, prob = proba))
}
