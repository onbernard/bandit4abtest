#'Upper Confidence Bound algorithm
#'
#'@description Controls data in visitor_reward with
#'  \code{\link{bandit_reward_control}}. Generates the UCB computation variables
#'  in matrix S.
#'  \itemize{ At each iteration
#'  \item Computes the upper confidence bounds
#'  \item Choose the arm with the maximum upper bound (with alpha parameter)
#'  \item Receives a reward in visitor_reward for the arm and associated
#'  iteration
#'  \item Updates the UCB computation variables according to that reward
#'  }
#'  Returns the choice and probability history, computation time, estimated and
#'  real reward expectations.
#'  See also
#'  \code{\link{generate_matrix_S}},
#'  \code{\link{proba_max_for_ucb}} and
#'  \code{\link{play_arm}}.
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha          Numeric value. Exploration parameter(optional)
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
  bandit_reward_control(visitor_reward)

  # data formatting
  visitor_reward <- as.matrix(visitor_reward)

  # choice and max probability history vectors
  choice <- c()
  proba <- c()
  # UCB computation variables
  S <- GenerateMatrixS(K)

  tictoc::tic()

  # initialization : play each arm once
  for (j in 1:K) {
    # compute upper confidence bounds
    upper_confidence_bounds <- proba_max_for_ucb(S, iter, alpha)
    # store max value
    proba[j] <- max(upper_confidence_bounds)
    # update ucb computation variables
    S <- play_arm(iter=j, arm=j, S=S, visitor_reward)
    # store choice
    choice[j] <- j
  }

  for (i in (K+1):nrow(visitor_reward)) {
    # compute upper confidence bounds and choose arm with the highest value
    upper_confidence_bounds <- proba_max_for_ucb(S, iter, alpha)
    choice[i] <- which.max(upper_confidence_bounds)
    # store said highest value
    proba[i] <-  max(upper_confidence_bounds)
    # get reward and update computation variables accordingly
    S <- play_arm(iter=i, arm=choice[i], S, visitor_reward)

  }

  time <- tictoc::toc()


  # estimated reward expectations
  th_hat=S[1,]

  # real reward expectations
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
