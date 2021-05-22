#'Thompson Sampling algorithm
#'
#'@description A Thompson sampling (TS) bandit strategy implemented by sampling,
#'  in each round, averages from a posterior distribution and choosing the
#'  action that maximizes the expected reward given the sampled average.
#'  Conceptually, this means that the player instantiates their beliefs randomly
#'  in each round, and then acts optimally according to them.
#'
#'  \itemize{ At each iteration
#'  \item Sample an averages from a prior for each arm (beta
#'  distribution with alpha and beta parameters)
#'  \item Choose the arm with the highest average
#'  \item Receives a reward in visitor_reward for the arm and associated
#'  iteration
#'  \item Update the prior according to the reward. }
#'
#'  The function keeps track of each arm estimated reward expectation and number
#'  of trials. These are returned at the end of the computation in addition to
#'  the arm played and at each iteration, the actual reward expectations and the
#'  computation time.
#'
#'  See also \code{\link{condition_for_thompson_sampling}},
#'  \code{\link{generate_matrix_S}}, and \code{\link{play_arm}}.
#'
#'  Reward input is checked for correct dimensions and values. These must be
#'  binary (either numeric or integer ones and zeros) ! See
#'  \code{\link{bandit_reward_control}} and \code{\link{control_binary}}.
#'
#'@param visitor_reward Dataframe of numeric values
#'@param alpha          Numeric value (optional)
#'@param beta           Numeric value (optional)
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
policy_thompson_sampling <- function(visitor_reward, alpha=1, beta=1, quiet=TRUE) {
  visitor_reward <- as.matrix(visitor_reward)
  K       <- ncol(visitor_reward)
  horizon <- nrow(visitor_reward)
  # Choice and max probability history vectors
  choice  <- c()
  proba   <- c()
  # Means and trials matrix
  S       <- generate_matrix_S(K)

  tictoc::tic()
  # Initialization : play each arm once
  choice[1:K] <- 1:K
  proba[1:K]  <- 1/K
  for (t in 1:K) {
    S <- play_arm(iter=t, arm=t, S=S, visitor_reward)
  }
  # Iterate over horizon
  for (t in (K+1):horizon) {
    # Sample an average from posterior distribution
    distrib   <- condition_for_thompson_sampling(S, alpha, beta)
    # Save the chosen arm
    choice[t] <- which.max(distrib)
    # Save probability sampled
    proba[t]  <- max(distrib)
    # Update means and trials
    S <- play_arm(iter=t, arm=choice[t], S, visitor_reward)
  }
  time <- tictoc::toc(quiet=quiet)

  if(!quiet){
    message("th_hat")
    message(th_hat)
    message("th real")
    message(th)
  }

  return (list('choice'    = choice,
               'proba'     = proba,
               'time'      = time$toc - time$tic,
               'trials'    = S[2,],
               'theta_hat' = S[1,],
               'theta'     = colMeans(visitor_reward)))
}

#'Condition for thompson sampling
#'
#'@description Samples for each arm an average according to its probability
#'  distribution from the beta law (according to number of sucess and trials in
#'  S matrix). See also \code{\link{stats::rbeta}}. Give the arm with the highest
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
  distrib <- c()
  for (j in 1:ncol(S)) {
    #Sample a mean from a beta distribution of means
    distrib[j] <- stats::rbeta(1, alpha +  S[1,j]*S[2,j], beta + S[2,j] - S[1,j]*S[2,j])
  }
  return(distrib)
}
