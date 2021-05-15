#'EXP3 algorithm
#'
#'@description Exponential  Weights  for  Exploration  and  Exploitation (EXP3)
#'  bandit strategy. Uses a list of weights which evolve according to arm's
#'  reward. The gamma parameter is a coefficient for balancing between
#'  exploitation and exploration.
#'
#'  Control data in visitor_reward with \code{\link{bandit_reward_control}} and
#'  \code{\link{control_binary}}.
#'
#'  Generates a matrix to save the results (S).
#'  \itemize{ At each iteration
#'  \item Update weight parameter for each arm
#'  \item Chooses randomly an arm according to the distribution of proba
#'  \item Receives a reward in visitor_reward for the arm and associated
#'  iteration
#'  \item Updates the means and trial matrix S. }
#'
#'  Returns the choice and probability history, computation time, estimated
#'  reward expectations, real reward expectations and weights.
#'
#'  See also  \code{\link{condition_for_exp3}}, \code{\link{generate_matrix_S}},
#'  and \code{\link{play_arm}}.
#'
#'@param visitor_reward Dataframe of numeric values
#'@param gamma          Numeric value (optional)
#'
#'@return
#' \itemize{ List of elements :
#'  \item S         : numeric matrix of UCB parameters
#'  \item choice    : numeric vector of arm choice history
#'  \item proba     : numeric vector of max UCB history
#'  \item time      : total computation time
#'  \item theta_hat : estimated reward expectations
#'  \item theta     : real reward expectations
#'  \item weight    : weight coefficient of each arm
#'  }
#'
#'
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

  weight <- rep(1, times=K)
  proba <- rep(0, times=K)
  choice <- c()
  reward <- c()
  estimated_reward <- c()
  S <- generate_matrix_S(K)
  K <- ncol(visitor_reward)

  tictoc::tic()

  # Initialization : play each arm once
  for(h in 1:K) {

    S <- play_arm(iter=h, arm=h, S=S, visitor_reward)

    weight_sum <- sum(weight)
    proba[h] <- (1 - gamma) * (weight[h]/weight_sum) + (gamma/K)

    # Get reward
    reward[h] <- visitor_reward[h,h]

    estimated_reward[h] <- reward[h]/proba[h]
    # Update weight
    weight[h] <- weight[h]*exp(gamma*estimated_reward[h]/K)

    choice[1:K] <- c(1:K)
  }

  for(i in (K+1):nrow(visitor_reward)) {
    for (j in 1:K){
      weight_sum <- sum(weight)
      proba[j] <- (1 - gamma) * (weight[j]/weight_sum) + (gamma/K)
    }

  choice[i] <- condition_for_exp3(S=S, proba = proba)
  reward[i] <- visitor_reward[i,choice[i]]
  S <- play_arm(iter=i, arm=choice[i], S, visitor_reward)

  estimated_reward[i] <- reward[i]/proba[choice[i]]

  # Update weight
  weight[choice[i]] <- weight[choice[i]]*exp(gamma*estimated_reward[i]/K)

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
#'@description Chooses randomly the next arm to play according to the list proba
#'
#'@param S     Numeric matrix
#'@param proba Numeric list
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
#'condition_for_exp3(S=S, proba = list(0.2, 0.8))
#'@export
condition_for_exp3 <- function(S, proba) {
  return(sample(1:K, size=1, replace=TRUE, prob = proba))
}
