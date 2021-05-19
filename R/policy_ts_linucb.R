#'TSLINUCB algorithm
#'
#'@description The Thompson Sampling Linear Upper Confidence bound algorithm is
#'  used to solve contextual bandit problem. It assumes that an unknown linear
#'  dependence exists between context and reward data. Instead of computing an
#'  upper bound of the reward expectations of each arm , the TSLINUCB algorithm
#'  sample the arm distributions (assumed to be Normals) then choose the one
#'  that maximize the sample set. The linear dependence is for its part modeled
#'  through a linear regression.
#'
#'  \itemize{ At each iteration
#'  \item Sample a reward from a multivariate distribution (with covariance and
#'  means) for each arm.
#'  \item Chooses the arm with the highest expected reward
#'  \item Receives a reward in visitor_reward for the arm and associated
#'  iteration
#'  \item Updates its prior given that reward }
#'
#'  The function keeps track of the arm choice and associated max probability at
#'  each iteration. These are returned at the end of the computation in addition
#'  to the actual and estimated coefficient of each arm and the computation
#'  time. See \code{\link{return_real_theta}}.
#'
#'  Reward and context inputs are checked for correct dimensions and values. See
#'  \code{\link{bandit_reward_control}} and
#'  \code{\link{data_control_context_reward}}
#'
#'@param dt             Context data. Dataframe of integer or numeric values
#'@param visitor_reward Reward data. Dataframe of integer or numeric values
#'@param nsamp          Number of samples required. Integer value (optional)
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
#'# This makes the example exactly reproducible
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
#'policy_tslinucb(dt,visitor_reward)
#'
#'@export
policy_tslinucb <- function(dt, visitor_reward, alpha=1, nsamp = 10 ) {
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
  for (i in 1:n) {
    x_i = D[i,]
    for (j in 1:K) {
      A_inv       = solve(A[,,j])
      th_hat[j,]  = A_inv %*% b[j,]
      ta          = t(x_i) %*% A_inv %*%  x_i
      theta_tilde =  apply(MASS::mvrnorm(nsamp,
                                         th_hat[j,],
                                         ((0.2)^2 * A_inv)),
                           2,max)
      p[j]        =  x_i %*% theta_tilde
    }
    # Choose the highest,
    choices[i] = which.max(p)
    # Save probability
    proba[i] = max(p)
    # Get reward
    rewards[i] = visitor_reward[i,choices[i]]
    # Update the input vector
    A[,,choices[i]] = A[,,choices[i]] + x_i %*% t(x_i)
    b[choices[i],] = b[choices[i],] +  x_i * rewards[i]
  }
  time <- tictoc::toc()
  th <- return_real_theta(dt=dt,visitor_reward=visitor_reward)
  # Return  data , models, groups and results
  return (list('proba'     = proba,
               'theta_hat' = th_hat,
               'theta'     = th,
               'choice'    = choices,
               'time'      = (time$toc - time$tic)))
}
