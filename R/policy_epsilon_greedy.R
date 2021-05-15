#' Epsilon Greedy algorithm
#'
#'@description Control data in visitor_reward with
#'  \code{\link{bandit_reward_control}} Stop if something is wrong. Generates a
#'  matrix to save the results (S). At each iteration play the best arm with a
#'  probability of 1-epsilon and other arm with probability epsilon. Returns the
#'  calculation time.
#'  Returns the choice and probability history, computation time, estimated and
#'  real reward expectations.
#'
#'  See also \code{\link{condition_for_epsilon_greedy}},
#'  \code{\link{generate_matrix_S}}, and \code{\link{play_arm}}.
#'
#'@param visitor_reward Dataframe of numeric values
#'@param epsilon        Numeric value (optional)
#'
#'@return
#' \itemize{ List of elements :
#'  \item S         : numeric matrix of UCB parameters
#'  \item choice    : numeric vector of arm choice history
#'  \item proba     : numeric vector of max UCB history
#'  \item time      : total computation time
#'  \item theta_hat : estimated reward expectations
#'  \item theta     : real reward expectations
#'  }
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
  visitor_reward <- as.matrix(visitor_reward)

  # choice history vector
  choice <- c()

  # means and trials matrix
  S <- GenerateMatrixS(K)

  K <- ncol(visitor_reward)

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

  # real and estimated coefficients
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
