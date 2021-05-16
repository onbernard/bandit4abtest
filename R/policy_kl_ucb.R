#'KL-UCB algorithm
#'
#'@description The Kullback-Leibler Upper Confidence Bound (KL-UCB) algorithm is
#'  used in stochastic bandit with finitely many arms problems.
#'
#'  When processing a dataframe of reward representing a bandit, the function
#'  keeps track of each arm estimated reward expectation and number of trials.
#'  These are returned at the end of the computation in addition to the arm
#'  played and its associated probability at each iteration, the actual reward
#'  expectations and the computation time.
#'
#'  See also \code{\link{condition_for_klucb}}, \code{\link{kl_bernoulli}},
#'  \code{\link{generate_matrix_S}}, and \code{\link{play_arm}}.
#'
#'  Reward input is checked for correct dimensions and values. See
#'  \code{\link{bandit_reward_control}}.
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param precision      Numeric value. Bisection method precision (optional)
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
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'klucb_alloc <- policy_kl_ucb(visitor_reward)
#'klucb_alloc$S
#'klucb_alloc$time
#'klucb_alloc$theta
#'klucb_alloc$theta_hat
#'
#'@export
policy_kl_ucb <- function(visitor_reward, precision=1e-6){
  bandit_reward_control(visitor_reward)
  K <- ncol(visitor_reward)
  # Data formatting
  visitor_reward <- as.matrix(visitor_reward) * 1

  # Choice history vector
  choice <- c()
  # Mean/Trials matrix
  S <- generate_matrix_S(K)

  tictoc::tic()

  # Initialization : play each arm once
  for (i in 1:K){
    choice[i] <- i
    S <- play_arm(iter=i, arm=i, S=S, visitor_reward)
  }
  # Iterate over horizon
  for (j in (K+1):nrow(visitor_reward)){
    t=sum(S[2,])
    indices <- condition_for_klucb(S,
                                  d=(log(t) + c*(log(log(t)))) / S[2,],
                                  precision=precision,
                                  max_iteration = 50)
    choice[j] <- which.max(indices)
    S <- play_arm(iter=j, arm=choice[j], S=S, visitor_reward)
  }

  time <- tictoc::toc()

  # Real and estimated reward expectations
  th = colMeans(visitor_reward)
  th_hat = S[1,]

  message("th_hat")
  message(th_hat)
  message("th real")
  message(th)

  return(list('S'=S,
              'time'=(time$toc - time$tic),
              'choice'= choice,
              'theta_hat'=th_hat,
              'theta'=th))
}

#'Arm indices according to the Kullback-Leibler upper confidence bound
#'
#' @description Computes for each arm a the value m (assumed to be the mean of a
#'   binomial distribution) that maximize {kl(mu_hat(a),m) < d} where kl is the
#'   binomial Kullback-Leibler divergence and mu_hat(a) the reward expectation
#'   of arm a. The value is computed using a bisection method.
#'
#' @param S              Means/Trials matrix
#' @param d              Upper bound of the divergence
#' @param precision      Precision of bisection method
#' @param max_iteration  Maximum number of iterations
#'
#' @return A vector containing the arms indices
#'
#' @export
condition_for_klucb <- function(S, d, precision, max_iteration){
  # Vectorized Kullback-Leibler divergence function
  kl_vec <- Vectorize(kl)
  # Reward expectations vector
  mu_hat <- S[1,]

  lower <- mu_hat
  upper <- min(1, mu_hat + sqrt(2 * d))
  count_iteration <- 0
  while (count_iteration < max_iteration && upper - lower > precision){
    # Middle : cut interval [lower,upper] in half
    middle <- (lower + upper) / 2
    # Middle value greater than the bound ? It is now the upper end of the
    # search interval, else nothing changes
    upper <- ifelse(kl_vec(mu_hat, middle) > d, middle, upper)
    # Middle value lower than the bound ? It is now the lower end of the search
    # interval, else nothing changes
    lower <- ifelse(kl_vec(mu_hat, middle) <= d, middle, lower)

    count_iteration <- count_iteration + 1
  }
  return ((lower + upper) / 2)
}

#'Kullback-Leibler divergence of two Bernoulli distributions
#'
#' @description The Kullback-Leibler divergence is a measure of how different
#'   two distributions are. Here two Bernouilli distributions parameterized by
#'   their means p and q.
#'
#' @param p Mean of first Bernouilli distribution
#' @param q Mean of second Bernouilli distribution
#'
#' @return The Kullback-Leibler divergence of the two distributions
kl_bernoulli <- function(p, q){
  epsilon <- 1e-16
  p = min(max(p, epsilon), 1 - epsilon)
  q = min(max(q, epsilon), 1 - epsilon)
  return(p * log(p/q) + (1 - p) * log((1 - p)/(1 - q)))
}
