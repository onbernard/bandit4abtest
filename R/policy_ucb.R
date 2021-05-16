#'Upper Confidence Bound algorithm
#'
#'@description The Upper Confidence Bound algorithm is used in stochastic bandit
#'  with finitely many arms problems. It attributes to each arm an upper bound
#'  of its reward expectation according to past rewards and number of trials.
#'
#'  \itemize{ At each iteration
#'  \item Computes the upper confidence bounds
#'  \item Chooses the arm with the maximum upper bound (with alpha parameter)
#'  \item Receives a reward in visitor_reward for the arm and associated
#'  iteration
#'  \item Updates the means and number of trials
#'  }
#'
#'  The function keeps track of each arm estimated reward expectation and number
#'  of trials. These are returned at the end of the computation in addition to
#'  the arm played and at each iteration, the actual reward expectations and the
#'  computation time.
#'
#'  See also \code{\link{generate_matrix_S}}, \code{\link{proba_max_for_ucb}}
#'  and \code{\link{play_arm}}.
#'
#'  Reward input is checked for correct dimensions and values. See
#'  \code{\link{bandit_reward_control}}.
#'
#'@param visitor_reward Dataframe of numeric values
#'@param alpha          Numeric value. Exploration parameter (optional)
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
#'## Generate 10000 numbers from 2 binomial  distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Create a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run UCB algorithm
#'ucb_alloc  <- policy_ucb(visitor_reward,alpha = 10)
#'ucb_alloc$S
#'barplot(table(ucb_alloc$choice),main = "Histogram of choices",xlab="arm")
#'#Upper bound for arm 2 according iterations (red line is the real mean)
#'plot(x=c(1:length(ucb_alloc$choice[ucb_alloc$choice==2])),
#'   ucb_alloc$proba[ucb_alloc$choice==2],
#'   type='l',xlab = 'Time',ylab = 'Upper bound of arm 2')
#'   lines(c(1:length(ucb_alloc$choice[ucb_alloc$choice==2])),rep(mean(K2),
#'   length(ucb_alloc$choice[ucb_alloc$choice==2])),col="red")
#'ucb_alloc$time
#'ucb_alloc$theta_hat
#'ucb_alloc$theta
#'
#'@export
policy_ucb <- function(visitor_reward, alpha=1) {
  # Data control
  bandit_reward_control(visitor_reward)
  # Data formatting
  visitor_reward <- as.matrix(visitor_reward) * 1
  K <- ncol(visitor_reward)
  # Choice and max probability history vectors
  choice <- c()
  proba <- c()
  # Means and trials matrix
  S <- GenerateMatrixS(K)

  tictoc::tic()
  # Initialization : play each arm once
  for (j in 1:K) {
    # Compute upper confidence bounds
    upper_confidence_bounds <- proba_max_for_ucb(S, iter, alpha)
    # Save max value
    proba[j] <- max(upper_confidence_bounds)
    # Update mean and number of trial
    S <- play_arm(iter=j, arm=j, S=S, visitor_reward)
    # Store choice
    choice[j] <- j
  }

  for (i in (K+1):nrow(visitor_reward)) {
    # Compute upper confidence bounds and choose arm with the highest value
    upper_confidence_bounds <- proba_max_for_ucb(S, iter, alpha)
    choice[i] <- which.max(upper_confidence_bounds)
    # Store highest value
    proba[i] <-  max(upper_confidence_bounds)
    # Get reward and update means and trials accordingly
    S <- play_arm(iter=i, arm=choice[i], S, visitor_reward)
  }
  time <- tictoc::toc()

  # Estimated and actual reward expectations
  th_hat=S[1,]
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

#' Upper Confidence Bound computation function
#'
#'@description Computes, for each arm an upper bound according to the Hoeffding
#'  inequality parameterized by exploration parameter alpha (default alpha = 1).
#'  Returns a vector containing each arm upper confidence bound.
#'
#'@param S     Means and trials matrix
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
