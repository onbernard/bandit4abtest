#'Thompson Sampling algorithm
#'
#'@description A Thompson sampling (TS) bandit strategy implemented by sampling,
#'  in each round, averages from a posterior distribution
#'  \code{\link{condition_for_thompson_sampling}}, and choosing the action that
#'  maximizes the expected reward given the sampled average. Conceptually, this
#'  means that the player instantiates their beliefs randomly in each round, and
#'  then acts optimally according to them.
#'  Control data in visitor_reward with \code{\link{bandit_reward_control}} and
#'  \code{\link{control_binary}}.
#'  Generates a matrix to save the results (S).
#'  \itemize{ At each iteration
#'  \item Sample an averages from a posterior in S for each arm (beta
#'  distribution with alpha and beta parameters)
#'  \item Choose the arm with the highest average
#'  \item Receives a reward in visitor_reward for the arm and associated
#'  iteration
#'  \item Updates the results matrix S. }
#'
#'  Returns the choice and probability history, computation time, estimated and
#'  real reward expectations.
#'
#'  See also \code{\link{condition_for_thompson_sampling}},
#'  \code{\link{generate_matrix_S}}, and \code{\link{play_arm}}.
#'
#'@param visitor_reward Dataframe of numeric values
#'@param alpha          Numeric value (optional)
#'@param beta           Numeric value (optional)
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
#'
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'policy_thompson_sampling(visitor_reward)
#'
#'@export
policy_thompson_sampling  <- function(visitor_reward, alpha=1, beta=1) {
  # data control
  bandit_reward_control(visitor_reward)

  # data formatting
  visitor_reward <- as.matrix(visitor_reward) * 1
  K <- ncol(visitor_reward)

  # more data control
  control_binary(visitor_reward)

  # choice and max probability history vectors
  choice <- c()
  proba <- c()

  # means and trials matrix
  S <- generate_matrix_S(K)

  tictoc::tic()

  # initialization : play each arm once
  for (j in 1:K) {
    S <- play_arm(iter=j, arm=j, S=S, visitor_reward)
    choice[1:K] <- c(1:K)
    proba[1:K] <- 1/K
  }

  for (i in (K+1):nrow(visitor_reward)) {
    # sample an average from posterior distribution
    distrib <- condition_for_thompson_sampling(S, alpha, beta)
    # save the chosen arm
    choice[i] <- which.max(distrib)
    # save probability sampled
    proba[i] <- max(distrib)
    # update S
    S <- play_arm(iter=i, arm=choice[i], S, visitor_reward)
  }

  time <- tictoc::toc()

  # coefficients estimate
  th_hat=S[1,]

  # real coefficients
  th = colMeans(visitor_reward)

  message("th_hat")
  message(th_hat)
  message("th real")
  message(th)

  return (list('S'=S,
               'choice'= choice,
               'proba' = proba,
               'time'=(time$toc - time$tic),
               'theta_hat'=th_hat,
               'theta'=th))
}

#'Condition for thompson sampling
#'
#'@description Samples for each arm an average according to its probability
#'  distribution from the beta law (according to number of sucess and trials in
#'  S matrix). See also \code{\link{rbeta}}. Give the arm with the highest
#'  average score. Returns vector containing each arm sample.
#'
#'@param S          : Numerical matrix of means and trials
#'@param alpha,beta : Exploration parameters (optional)
#'
#'@return Vector containing each arm sample
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
#'## Init the S Matrix
#'S <- generate_matrix_S(K)
#'S
#'## play arms uniformly
#'for(i in 1:nrow(visitor_reward)){
#'S <- play_arm(i,arm=(i%%K+1),S,visitor_reward)
#'}
#'## Results
#'S
#'## Choose next arm with thompson sampling policy
#'condition_for_thompson_sampling(S)
#'#Density
#'plot(density( rbeta(100, 1 + S[1,1]*S[2,1], 1 + S[2,1] - S[1,1]*S[2,1])))
#'plot(density( rbeta(100, 1 + S[1,2]*S[2,2], 1 + S[2,2] - S[1,2]*S[2,2])))
#'
#'@export
condition_for_thompson_sampling <- function(S, alpha=1, beta=1) {

  distrib <- list()

  for (j in 1:K) {
    #Sample a mean from a beta distribution of means
    distrib[j] <- stats::rbeta(1, alpha +  S[1,j]*S[2,j], beta + S[2,j] - S[1,j]*S[2,j])
  }

  return(distrib)
}
