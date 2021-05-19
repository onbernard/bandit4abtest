#'LINUCB algorithm
#'
#'@description The Linear Upper Confidence Bound algorithm is used to solve
#'  contextual bandit problem. It assumes that an unknown linear dependence exists
#'  between context and reward data. Said dependence is modeled through a linear
#'  regression when processing a dataframe of contexts and a dataframe of
#'  rewards representing a bandit.
#'
#'  The function keeps track of the arm choice and associated max probability at
#'  each iteration. These are returned at the end of the computation in addition
#'  to the actual and estimated coefficient of each arm and the computation time. See
#'  \code{\link{return_real_theta}}.
#'
#'  Reward and context inputs are checked for correct dimensions and values. See
#'  \code{\link{bandit_reward_control}} and
#'  \code{\link{data_control_context_reward}}.
#'
#'@param dt             Context data. Dataframe of integer or numeric values
#'@param visitor_reward Reward data. Dataframe of integer or numeric values
#'@param alpha          Exploration parameter. Numeric value (optional)
#'
#'
#'@return
#' \itemize{ List of element:
#'  \item choice    : Choice history vector
#'  \item proba     : Max probability history vector
#'  \item time      : Computation time
#'  \item theta_hat : Final estimated arm coefficients
#'  \item theta     : Actual arm coefficients}
#'
#'@examples
#'size.tot = 1000
#'# this makes the example exactly reproducible
#'set.seed(4649)
#'# you have 4, largely uncorrelated predictors
#'x1 = runif(size.tot, min=0, max=10)
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = crossprod(t(dt),arm_3)
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'policy_linucb(dt,visitor_reward)
#'
#'@export
policy_linucb <- function(dt, visitor_reward, alpha=1) {
  # Data control
  bandit_reward_control(visitor_reward)
  data_control_context_reward(dt, visitor_reward)
  # Data formatting
  visitor_reward <- as.matrix(visitor_reward) * 1
  K <- nrow(visitor_reward)
  # Context matrix
  D <- as.matrix(dt) * 1
  n <- nrow(dt)
  n_f <- ncol(D)
  # Keep the past choice for regression
  choices = c()
  rewards = c()
  proba = c()
  # Parameters to be modeled
  th_hat = array(0, c(K,n_f))
  colnames(th_hat) <- colnames(dt)
  rownames(th_hat) <- colnames(visitor_reward)
  # Regression variable
  b <- matrix(0, K, n_f)
  A <- array(diag(n_f), c(n_f, n_f, K))
  # Temporary variable
  p = rep(0, K)

  tictoc::tic()
  # Iterate over horizon
  for (i in 1:n) {
    x_i = D[i,]
    for (j in 1:K) {
      A_inv      = solve(A[,,j])
      th_hat[j,] = A_inv %*% b[j,]
      ta         = t(x_i) %*% A_inv %*%  x_i
      a_upper_ci = alpha * sqrt(ta)           # Upper part of variance interval
      a_mean     = th_hat[j,] %*% x_i         # Current estimate of mean
      p[j]       = a_mean + a_upper_ci        # Top CI
    }
    # Choose the highest,
    choices[i] = which.max(p)
    # Save probability
    proba[i] = max(p)
    # Get reward
    rewards[i] = visitor_reward[i,choices[i]]
    # Update the input vector
    # Covariance matrix
    A[,,choices[i]] = A[,,choices[i]] + x_i %*% t(x_i)
    b[choices[i],] = b[choices[i],] +  x_i * rewards[i]
  }
  time <- tictoc::toc()
  # TODO : handle logit in return_real_theta or maybe not given reward * 1
  th <- return_real_theta(dt=dt,visitor_reward=visitor_reward,option = "linear")
  return(list('proba' = proba,
              'theta_hat'=th_hat,
              'theta'=th,
              'choice'=choices,
              'time'=(time$toc - time$tic)))
}
