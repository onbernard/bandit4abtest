#' Epsilon Greedy algorithm
#'
#'@description The epsilon greedy algorithm is used in stochastic bandit with
#'  finitely many arms problems. At each iteration, a coin is flipped with
#'  success probability epsilon. If it succeeds arm is chosed at random. If not
#'  the arm with the highest observed reward expectation. This expectation is
#'  updated after observing the result of the draw.
#'
#'  Doing so, this function keeps track of each arm estimated reward expectation
#'  and number of trials. These are returned at the end of the computation in
#'  addition to the arm played and at each iteration, the actual reward
#'  expectations and the computation time.
#'
#'  See also \code{\link{condition_for_epsilon_greedy}},
#'  \code{\link{generate_matrix_S}}, and \code{\link{play_arm}}.
#'
#'  Reward input is checked for correct dimensions and values. See
#'  \code{\link{bandit_reward_control}}.
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param epsilon        Numeric value. Exploration parameter (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S         : Means and trials matrix
#'  \item choice    : Choice history vector
#'  \item proba     : Max probability history vector
#'  \item time      : Computation time
#'  \item theta_hat : Final estimated reward expectation of each arm
#'  \item theta     : Actual reward expectation of each arm}
#'
#'@examples
#'## Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run epsilon Greedy algorithm
#'epsilon_greedy_alloc  <- policy_epsilon_greedy(visitor_reward,epsilon  = 0.25)
#'epsilon_greedy_alloc$S
#'barplot(table(epsilon_greedy_alloc$choice),main = "Histogram of choices",
#'xlab="arm")
#'epsilon_greedy_alloc$time
#'epsilon_greedy_alloc$theta_hat
#'epsilon_greedy_alloc$theta
#'
#'@export
policy_epsilon_greedy <- function(visitor_reward, epsilon=0.25) {
  # data control
  bandit_reward_control(visitor_reward)
  # data formatting
  visitor_reward <- as.matrix(visitor_reward) * 1
  # choice history vector
  choice <- c()

  # means and trials matrix
  K <- ncol(visitor_reward)
  S <- GenerateMatrixS(K)

  tictoc::tic()
  # initialization : play each arm once
  for (j in 1:K) {
    S <- play_arm(iter=j, arm=j, S=S, visitor_reward)
    choice[1:K] <- c(1:K)
  }

  for (i in (K+1):nrow(visitor_reward)) {
    choice[i] <- condition_for_epsilon_greedy(S, epsilon)
    S <- play_arm(iter=i, arm=choice[i], S, visitor_reward)
  }
  time <- tictoc::toc()

  # Actual and estimated coefficients
  th = colMeans(visitor_reward)
  th_hat=S[1,]

  message("th_hat")
  message(th_hat)
  message("th real")
  message(th)

  return(list('S'=S,
              'choice'= choice,
              'time'=(time$toc - time$tic),
              'theta_hat'=th_hat,
              'theta'=th))
}


#' Condition for epsilon greedy
#'
#'@description Chooses the best with 1-epsilon probability. A random arm with
#'  probability epsilon.
#'
#'@param S        Numeric matrix of means and trials
#'@param epsilon  Numeric value (optional)
#'
#'@return Integer value
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'## Number of arms
#'K=2
#'## Init the S Matrix
#'S <- generate_matrix_S(K)
#'S
#'## play arms uniformly
#'for(i in 1:nrow(visitor_reward)){
#'S <- play_arm(i,arm=(i%%K+1),S,visitor_reward)
#'}
#'## Results
#'S
#'
#'@export
condition_for_epsilon_greedy <- function(S, epsilon=0.25) {
  # choose the best with 1 - espsilon probability. 1 : best arm , 2 : other arm
  res <- sample(c(1,2), 1, replace = TRUE, prob = c(1-epsilon,epsilon ))
  if(res==1){
    return(which.max(S[1,]))
  }
  else{
    return (sample(c(1:K)[-which.max(S[1,])] , 1))
  }
}
