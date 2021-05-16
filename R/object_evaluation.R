#'ctreeucbRejectionSamplingBanditObjectEvaluation
#'
#'Run a \code{\link{ctreeucb_rejection_sampling}} using visitor_reward and dt
#'values. Control data. Stop if something is wrong. Exclud any choices which not
#'corresponds to real exepriments in dataset. After execution of
#'ctreeucb_bandit, calculates the cumulative regret associated with the choices
#'made. Review the cumulative regret according iterations and an ctreeucb_bandit
#'object. See also \code{\link{ctreeucb}},
#'\code{\link{ctreeucb_rejection_sampling}} Require \code{\link{ctree}}
#'\code{\link{partykit}} library Require \code{\link{tic}} and \code{\link{toc}}
#'from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha numerical value (optional)
#'@param K Integer value (optional)
#'@param average Boolean value to evaluate the policy
#'
#'@return
#' \itemize{ List of element:
#'  \item ctreeucb_bandit_alloc: ctreeucb bandit object ,
#'  \item cum_reg_ctreeucb_bandit_alloc: List numeric.
#'  }
#'
#'@examples
#'##### Pairewise #####
#'set.seed(1234)
#'size.tot <- 10000
#'x <- seq(0, 5, 0.01)
#'x1<- sample(x, size.tot, replace = TRUE, prob = NULL)
#'arm_1 <-  as.vector(c(2,-1,1.5,0))
#'K1 <- (x1 < 1 ) * arm_1[4]  +
#'  (x1 >= 1 & x1 < 2 ) * arm_1[1]  +
#'  (x1 >= 2 & x1 < 3) * arm_1[2]  +
#'  (x1 >= 3 & x1 < 4) * arm_1[3]  +
#'  (x1 >= 4) * arm_1[4]
#'plot(x1, K1)
#'
#'arm_2 <-  as.vector(c(1.5,-0.5,1.25,0))
#'K2 <- (x1 < 1 ) * arm_2[4]  +
#'  (x1 >= 1 & x1 < 2 ) * arm_2[1]  +
#'  (x1 >= 2 & x1 < 3) * arm_2[2]  +
#'  (x1 >= 3 & x1 < 4) * arm_2[3]  +
#'  (x1 >= 4) * arm_2[4]
#'plot(x1, K2)

#'#covariate without interest
#'x2<- sample(x, size.tot, replace = TRUE, prob = NULL)
#'#Results for each variation
#'visitor_reward <-  data.frame(K1,K2 )
#'#remove data
#'temp_list <- sample(1:nrow(visitor_reward), as.integer(size.tot/2) ,
#'replace = FALSE, prob = NULL)
#'visitor_reward$K1[temp_list] <- NA
#'visitor_reward$K2[-temp_list] <- NA
#'summary(visitor_reward)
#'dt <- as.data.frame(cbind(x1,x2))
#'controle_param = ctreeucb_parameters_control_default(dt=dt,
#'visitor_reward=visitor_reward,learn_size=1500,  alpha=1,
#'ctree_control_val= partykit::ctree_control(teststat = "quadratic"))
#'ctreeucb_rejection_sampling_bandit =
#'ctreeucbRejectionSamplingBanditObjectEvaluation(dt=dt,visitor_reward,
#'ctree_parameters_control = controle_param )
#'#take data for online ab test for other algorithm
#'first <-
#'ctreeucb_rejection_sampling_bandit$
#'ctreeucb_rejection_sampling_bandit_alloc$first_train_element
#'last <- nrow(visitor_reward)
#'dt.abtest <- dt[first:last,]
#'visitor_reward.abtest <- visitor_reward[first:last,]
#'#compare with linucb bandit
#'linucb_rejection_sampling_bandit <-
#'LinucbRejectionSamplingBanditObjectEvaluation(dt.abtest,visitor_reward.abtest)
#'@import partykit
#'@export
#ctreeucb_rejection_sampling_bandit object evaluation
ctreeucbRejectionSamplingBanditObjectEvaluation <- function(dt, visitor_reward, K=ncol(visitor_reward), ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward)) {
  ctreeucb_rejection_sampling_bandit_alloc <- ctreeucb_rejection_sampling(dt,visitor_reward,K, ctree_parameters_control)
  cum_rew_ctreeucb_rejection_sampling_alloc <- reward_cumulative(choice=ctreeucb_rejection_sampling_bandit_alloc$choice,
                                                                 visitor_reward=visitor_reward[ctreeucb_rejection_sampling_bandit_alloc$first_train_element:nrow(visitor_reward),])

  plot(cum_rew_ctreeucb_rejection_sampling_alloc, type = 'l',ylab = "Cumulative Reward",xlab="Number of items")
  message(paste("average : "),max(cum_rew_ctreeucb_rejection_sampling_alloc)/length(cum_rew_ctreeucb_rejection_sampling_alloc), sep = " " )

  return (list('ctreeucb_rejection_sampling_bandit_alloc'=ctreeucb_rejection_sampling_bandit_alloc ,'cum_rew_ctreeucb_rejection_sampling_alloc'=cum_rew_ctreeucb_rejection_sampling_alloc))
}

#'dbactreeucbRejectionSamplingBanditObjectEvaluation
#'
#'Run a \code{\link{dbactreeucb_rejection_sampling}} using visitor_reward and dt
#'values. Control data. Stop if something is wrong. Exclud any choices which not
#'corresponds to real exepriments in dataset. After execution of
#'ctreeucb_bandit, calculates the cumulative regret associated with the choices
#'made. Review the cumulative regret according iterations and an ctreeucb_bandit
#'object. See also \code{\link{ctreeucb}},
#'\code{\link{ctreeucb_rejection_sampling}} Require \code{\link{ctree}}
#'\code{\link{partykit}} library Require \code{\link{tic}} and \code{\link{toc}}
#'from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha numerical value (optional)
#'@param K Integer value (optional)
#'@param average Boolean value to evaluate the policy
#'
#'@return
#' \itemize{ List of element:
#'  \item ctreeucb_bandit_alloc: ctreeucb bandit object ,
#'  \item cum_reg_ctreeucb_bandit_alloc: List numeric.
#'  }
#'
#'@examples
#'size.tot = 9000
#'set.seed(4649)              # this makes the example exactly reproducible
#'# you have 4, largely uncorrelated predictors
#'x1 = runif(size.tot, min=0, max=10)
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = crossprod(t(dt),arm_3)
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'size.tot = 9000
#'# Time series
#'alpha_list <- c(1,2,3)
#'beta_list <- c(0.5,0.1,-0.2)
#'theta_list <- c(0.8,0.2,0.5)
#'y <- as.data.frame(c(1))
#'colnames(y) = "ID"
#'temp=1
#'for (j in 1:3000){
#'  for (i in 1:length(alpha_list)){
#'    n = sample(1:100,1)
#'    t <- 1:n
#'    ts <- alpha_list[i] + beta_list[i] * t + arima.sim(
#'    list(ma = theta_list[i]), n = length(t))
#'    y[temp, "time_series"][[1]] <- list(ts)
#'    y[temp, "cluster"][[1]] <- i
#'    y$ID[temp] = temp
#'    temp = temp +1
#'  }
#'}
#'y <- y[sample(nrow(y)),]



#'dt <-  as.data.frame(cbind(x1,x2,x3,x4,y$time_series))
#'colnames(dt) <- c("x1","x2","x3","x4","time_series")

#'for(i in 1:nrow(dt)) {
#'  if(y$cluster[i] == 1) visitor_reward$K1[i] = 10
#'  if(y$cluster[i] == 2) visitor_reward$K2[i] = 20
#'  if(y$cluster[i] == 3) visitor_reward$K3[i] = 30
#'}

#'dt$cluster <- NULL
#'dt$x1 <- as.numeric(dt$x1)
#'dt$x2 <- as.numeric(dt$x2)
#'dt$x3 <- as.numeric(dt$x3)
#'dt$x4 <- as.numeric(dt$x4)
#'K=ncol(visitor_reward)
#'ctree_parameters_control=ctreeucb_parameters_control_default(dt,
#'visitor_reward)
#'listSerie = c("time_series")
#'listKCentroids=c(3)
#'dbactreeucbRejectionSamplingBanditObjectEvaluation(dt,visitor_reward,K,
#'listSerie, listKCentroids , ctree_parameters_control)
#'@import partykit
#'@export
#dbactreeucb_rejection_sampling_bandit object evaluation
dbactreeucbRejectionSamplingBanditObjectEvaluation <- function(dt,visitor_reward,K=ncol(visitor_reward), listSerie, listKCentroids , ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward)) {
  dbactreeucb_rejection_sampling_bandit_alloc <- dbactreeucb_rejection_sampling(dt,visitor_reward,K, listSerie, listKCentroids , ctree_parameters_control)
  cum_rew_dbactreeucb_rejection_sampling_alloc <- reward_cumulative(choice=dbactreeucb_rejection_sampling_bandit_alloc$choice,
                                                                    visitor_reward=visitor_reward[dbactreeucb_rejection_sampling_bandit_alloc$first_train_element:nrow(visitor_reward),])

  plot(cum_rew_dbactreeucb_rejection_sampling_alloc, type = 'l',ylab = "Cumulative Reward",xlab="Number of items")
  message(paste("average : "),max(cum_rew_dbactreeucb_rejection_sampling_alloc)/length(cum_rew_dbactreeucb_rejection_sampling_alloc), sep = " " )

  return (list('dbactreeucb_rejection_sampling_bandit_alloc'=dbactreeucb_rejection_sampling_bandit_alloc ,'cum_rew_dbactreeucb_rejection_sampling_alloc'=cum_rew_dbactreeucb_rejection_sampling_alloc))
}

#'EpsilonGreedyBanditObjectEvaluation
#'
#'Run the EpsilonGreedy algorithm using visitor_reward values with
#'\code{\link{EpsilonGreedy}} function. Stop if something is wrong. After
#'execution of EpsilonGreedy, calculates the cumulative regret associated with
#'the choices made. Review the cumulative regret according iterations and an
#'EpsilonGreedy object. See also \code{\link{EpsilonGreedy}},
#'\code{\link{CumulativeRegret}} Require \code{\link{tic}} and \code{\link{toc}}
#'from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param epsilon Numeric value (optional)
#'@param average Boolean value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item EpsilonGreedy_alloc: EpsilonGreedy object ,
#'  \item cum_reg_EpsilonGreedy_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run EpsilonGreedy algorithm with policy evaluation
#'EpsilonGreedyBanditObjectEvaluation(visitor_reward,epsilon = 0.25)
#'
#'@export
#EpsilonGreedy object evaluation
EpsilonGreedyBanditObjectEvaluation <- function(visitor_reward=visitor_reward, K=ncol(visitor_reward), epsilon=0.25,average = FALSE,IsRewardAreBoolean = TRUE,dt=NA,explanatory_variable=colnames(dt) ) {
  epsilon_greedy_alloc <- EpsilonGreedy(visitor_reward, epsilon=epsilon, K=K)

  if(average == FALSE) cum_reg_epsilon_greedy_alloc  <- cumulativeRegret(epsilon_greedy_alloc$choice,visitor_reward)
  if(average == TRUE) cum_reg_epsilon_greedy_alloc  <- cumulativeRegretAverage(epsilon_greedy_alloc$choice,
                                                                               visitor_reward = visitor_reward,
                                                                               dt=dt,
                                                                               IsRewardAreBoolean=IsRewardAreBoolean,
                                                                               explanatory_variable=explanatory_variable)




  return(list('epsilon_greedy_alloc'=epsilon_greedy_alloc ,'cum_reg_epsilon_greedy_alloc'=cum_reg_epsilon_greedy_alloc))
}

#'kernelucbBanditObjectEvaluation
#'
#'Run a \code{\link{kernelucb}} using visitor_reward and dt values. Control
#'data. Stop if something is wrong. After execution of kernelucb_bandit,
#'calculates the cumulative regret associated with the choices made. Review the
#'cumulative regret according iterations and an kernelucb_bandit object. See
#'also \code{\link{kernelucb}}, \code{\link{CumulativeRegret}} Require
#'\code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha numerical value (optional)
#'@param K Integer value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item kernelucb_bandit_alloc: kernelucb bandit object ,
#'  \item cum_reg_kernelucb_bandit_alloc: List numeric.
#'  }
#'
#'@examples
#'size.tot = 1000
#'set.seed(4649)        # this makes the example exactly reproducible
#'# you have 4, largely uncorrelated predictors
#'x1 = runif(size.tot, min=0, max=10)
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = 1/(1+exp(- crossprod(t(dt),arm_1))) # inverse logit transform of linear
#'predictor
#'K1 = vapply(K1, function(x) rbinom(1, 1, x), as.integer(1L))
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = 1/(1+exp(- crossprod(t(dt),arm_2))) # inverse logit transform of linear
#'predictor
#'K2 = vapply(K2, function(x) rbinom(1, 1, x), as.integer(1L))
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = 1/(1+exp(- crossprod(t(dt),arm_3)))
#'K3 = vapply(K3, function(x) rbinom(1, 1, x), as.integer(1L))
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'kernelucbBanditObjectEvaluation(dt=dt,visitor_reward)
#'kernelucbBanditObjectEvaluation(dt=dt,visitor_reward,update_val= 0)
#'kernelucbBanditObjectEvaluation(dt=dt,visitor_reward,average=TRUE,
#'IsRewardAreBoolean = TRUE)
#'@export
#kernelucb_bandit object evaluation
kernelucbBanditObjectEvaluation <- function(dt, visitor_reward, alpha=1, K=ncol(visitor_reward),update_val= 100, average=FALSE, IsRewardAreBoolean=FALSE, dt.reward = dt, explanatory_variable = colnames(dt.reward)) {
  kernelucb_bandit_alloc <- kernelucb(dt=dt, visitor_reward=visitor_reward, alpha=alpha, K = K,update_val= 100)

  if(average == FALSE) cum_reg_kernelucb_bandit_alloc <- cumulativeRegret(kernelucb_bandit_alloc$choice, visitor_reward)
  if(average == TRUE) cum_reg_kernelucb_bandit_alloc <- cumulativeRegretAverage(kernelucb_bandit_alloc$choice,
                                                                                visitor_reward = visitor_reward,
                                                                                dt=dt.reward,
                                                                                IsRewardAreBoolean=IsRewardAreBoolean,
                                                                                explanatory_variable=explanatory_variable)

  return (list('kernelucb_bandit_alloc'=kernelucb_bandit_alloc ,'cum_reg_kernelucb_bandit_alloc'=cum_reg_kernelucb_bandit_alloc))
}

#'LinucbBanditObjectEvaluation
#'
#'Run a \code{\link{LINUCB}} using visitor_reward and dt values. Control data.
#'Stop if something is wrong. After execution of linucb_bandit, calculates the
#'cumulative regret associated with the choices made. Review the cumulative
#'regret according iterations and an linucb_bandit object. See also
#'\code{\link{LINUCB}}, \code{\link{CumulativeRegret}} Require \code{\link{tic}}
#'and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha numerical value (optional)
#'@param K Integer value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item linucb_bandit_alloc: linucb bandit object ,
#'  \item cum_reg_linucb_bandit_alloc: List numeric.
#'  }
#'
#'@examples
#'size.tot = 1000
#'set.seed(4649)                # this makes the example exactly reproducible
#'# you have 4, largely uncorrelated predictors
#'x1 = runif(size.tot, min=0, max=10)
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = 1/(1+exp(- crossprod(t(dt),arm_1))) # inverse logit transform of linear
#'predictor
#'K1 = vapply(K1, function(x) rbinom(1, 1, x), as.integer(1L))
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = 1/(1+exp(- crossprod(t(dt),arm_2))) # inverse logit transform of linear
#'predictor
#'K2 = vapply(K2, function(x) rbinom(1, 1, x), as.integer(1L))
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = 1/(1+exp(- crossprod(t(dt),arm_3)))
#'K3 = vapply(K3, function(x) rbinom(1, 1, x), as.integer(1L))
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'LinucbBanditObjectEvaluation(dt,visitor_reward)
#'LinucbBanditObjectEvaluation(dt,visitor_reward,average = TRUE,
#'IsRewardAreBoolean = TRUE)
#'@export
#linucb_bandit object evaluation
LinucbBanditObjectEvaluation <- function(dt, visitor_reward, alpha=1, K=ncol(visitor_reward),average = FALSE,IsRewardAreBoolean = FALSE,dt.reward = dt ,explanatory_variable=colnames(dt.reward)) {
  linucb_bandit_alloc <- LINUCB(dt=dt, visitor_reward=visitor_reward, alpha=alpha, K=K)

  if(average == FALSE) cum_reg_linucb_bandit_alloc <- cumulativeRegret(linucb_bandit_alloc$choice, visitor_reward)
  if(average == TRUE) cum_reg_linucb_bandit_alloc <- cumulativeRegretAverage(linucb_bandit_alloc$choice,
                                                                             visitor_reward = visitor_reward,
                                                                             dt=dt.reward,
                                                                             IsRewardAreBoolean=IsRewardAreBoolean,
                                                                             explanatory_variable=explanatory_variable)


  return (list('linucb_bandit_alloc'=linucb_bandit_alloc ,'cum_reg_linucb_bandit_alloc'=cum_reg_linucb_bandit_alloc))
}

#'LinucbRejectionSamplingBanditObjectEvaluation
#'
#'Run the LINUCB algorithm with rejection sampling method using visitor_reward values with \code{\link{LINUCB}} function.
#'Exclude any choices which not corresponds to real exepriments in dataset
#'Stop if something is wrong.
#'After execution of LINUCB, calculates the cumulative reward
#'associated with the choices made.
#'Review the cumulative reward according iterations and an linucb rejection sampling object.
#'See also \code{\link{LINUCB_rejection_sampling}}, \code{\link{LINUCB_rejection_sampling}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'@param IsRewardAreBoolean (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item linucb_alloc: linucb object ,
#'  \item cum_reg_ucb_alloc: List numeric.
#'  }
#'
#'@examples
#'# Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)                         # this makes the example exactly reproducible
#'size.tot = 1000
#'x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'dt <- as.data.frame(dt)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'colnames(visitor_reward) = c("K1","K2")
#'#remove data
#'temp_list <- sample(1:nrow(visitor_reward), as.integer(nrow(visitor_reward)/2), replace = FALSE, prob = NULL)
#'visitor_reward$K1[temp_list] <- NA
#'visitor_reward$K2[-temp_list] <- NA
#'#run linucb on missing data
#'#'LinucbRejectionSamplingBanditObjectEvaluation(dt,visitor_reward,alpha = 1,IsRewardAreBoolean = FALSE)
#'@export
#Linucb object evaluation
LinucbRejectionSamplingBanditObjectEvaluation <- function(dt,visitor_reward=visitor_reward ,K=ncol(visitor_reward), alpha=1,IsRewardAreBoolean = FALSE) {

  linucb_rejection_sampling_bandit_alloc <- LINUCB_rejection_sampling(dt,visitor_reward, alpha = alpha, K=K,IsRewardAreBoolean = IsRewardAreBoolean)
  cum_rew_linucb_rejection_sampling_alloc <- reward_cumulative(choice=linucb_rejection_sampling_bandit_alloc$choice,
                                                               visitor_reward=visitor_reward)
  plot(cum_rew_linucb_rejection_sampling_alloc, type = 'l',ylab = "Cumulative Reward",xlab="Number of items")
  message(paste("average : "),max(cum_rew_linucb_rejection_sampling_alloc)/length(cum_rew_linucb_rejection_sampling_alloc), sep = " " )

  return (list('linucb_rejection_sampling_bandit_alloc'=linucb_rejection_sampling_bandit_alloc ,'cum_rew_linucb_rejection_sampling_alloc'=cum_rew_linucb_rejection_sampling_alloc))
}

#'LogitucbBanditObjectEvaluation
#'
#'Run a \code{\link{LOGITUCB}} using visitor_reward and dt values. Control data.
#'Stop if something is wrong. After execution of logitucb_bandit, calculates the
#'cumulative regret associated with the choices made. Review the cumulative
#'regret according iterations and an logitucb_bandit object. See also
#'\code{\link{LOGITUCB}}, \code{\link{CumulativeRegret}} Require
#'\code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha numerical value (optional)
#'@param K Integer value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item logitucb_bandit_alloc: logitucb bandit object ,
#'  \item cum_reg_logitucb_bandit_alloc: List numeric.
#'  }
#'
#'@examples
#'size.tot = 1000
#'set.seed(4649)              # this makes the example exactly reproducible
#'# you have 4, largely uncorrelated predictors
#'x1 = runif(size.tot, min=0, max=10)
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = 1/(1+exp(- crossprod(t(dt),arm_1))) # inverse logit transform of linear
#'predictor
#'K1 = vapply(K1, function(x) rbinom(1, 1, x), as.integer(1L))
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = 1/(1+exp(- crossprod(t(dt),arm_2))) # inverse logit transform of linear
#'predictor
#'K2 = vapply(K2, function(x) rbinom(1, 1, x), as.integer(1L))
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = 1/(1+exp(- crossprod(t(dt),arm_3)))
#'K3 = vapply(K3, function(x) rbinom(1, 1, x), as.integer(1L))
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'LogitucbBanditObjectEvaluation(dt=dt,visitor_reward)
#'@export
#logitucb_bandit object evaluation
LogitucbBanditObjectEvaluation <- function(dt, visitor_reward, alpha=1, K=ncol(visitor_reward),average = FALSE,IsRewardAreBoolean = TRUE,explanatory_variable=colnames(dt)) {
  logitucb_bandit_alloc <- LOGITUCB(dt=dt, visitor_reward=visitor_reward, alpha=alpha, K = K)

  if(average == FALSE) cum_reg_logitucb_bandit_alloc <- cumulativeRegret(logitucb_bandit_alloc$choice, visitor_reward)
  if(average == TRUE) cum_reg_logitucb_bandit_alloc <- cumulativeRegretAverage(logitucb_bandit_alloc$choice,
                                                                               visitor_reward = visitor_reward,
                                                                               dt=dt,
                                                                               IsRewardAreBoolean=IsRewardAreBoolean,
                                                                               explanatory_variable=explanatory_variable)


  return (list('logitucb_bandit_alloc'=logitucb_bandit_alloc ,'cum_reg_logitucb_bandit_alloc'=cum_reg_logitucb_bandit_alloc))
}

#'ThompsonSamplingBanditObjectEvaluation
#'
#'Run the Thompson Sampling algorithm using visitor_reward values with
#'\code{\link{ThompsonSampling}} function. Stop if something is wrong. After
#'execution of Thompson Sampling, calculates the cumulative regret associated
#'with the choices maded. Review the cumulative regret according iterations and
#'an thompson sampling object. See also \code{\link{ThompsonSampling}},
#'\code{\link{CumulativeRegret}} Require \code{\link{tic}} and \code{\link{toc}}
#'from \code{\link{tictoc}} library. Average option implies a regret based on
#'conditional inference tree model.
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'@param beta Numeric value (optional)
#'@param average Boolean value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item ThompsonSampling_alloc: ThompsonSampling object ,
#'  \item cum_reg_ucb_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run Thompson Sampling algorithm with policy evaluation
#'ThompsonSamplingBanditObjectEvaluation(visitor_reward,alpha = 1, beta = 1 )
#'ThompsonSamplingBanditObjectEvaluation(visitor_reward= visitor_reward,alpha=1,
#'beta = 1 ,average = TRUE,IsRewardAreBoolean = TRUE)
#'@export
#thompson sampling object evaluation
ThompsonSamplingBanditObjectEvaluation <- function(visitor_reward=visitor_reward, K=ncol(visitor_reward), alpha=1, beta=1,average = FALSE,IsRewardAreBoolean = TRUE,dt=NA,explanatory_variable=colnames(dt)) {
  ThompsonSampling_bandit_alloc <- ThompsonSampling(visitor_reward, alpha = alpha, beta = beta, K = K)


  if(average == FALSE) cum_reg_ThompsonSampling_bandit_alloc <- cumulativeRegret(ThompsonSampling_bandit_alloc$choice, visitor_reward)
  if(average == TRUE) cum_reg_ThompsonSampling_bandit_alloc <- cumulativeRegretAverage(ThompsonSampling_bandit_alloc$choice,
                                                                                       visitor_reward = visitor_reward,
                                                                                       dt=dt,
                                                                                       IsRewardAreBoolean=IsRewardAreBoolean,
                                                                                       explanatory_variable=explanatory_variable)




  return (list('ThompsonSampling_alloc'=ThompsonSampling_bandit_alloc ,'cum_reg_ThompsonSampling_alloc'=cum_reg_ThompsonSampling_bandit_alloc))
}

#'UcbRejectionSamplingBanditObjectEvaluation
#'
#'Run the UCB algorithm with rejection sampling method using visitor_reward
#'values with \code{\link{UCB}} function. Exclude any choices which not
#'corresponds to real exepriments in dataset Stop if something is wrong. After
#'execution of UCB, calculates the cumulative reward associated with the choices
#'made. Review the cumulative reward according iterations and an ucb rejection
#'sampling object. See also \code{\link{UCB_rejection_sampling}},
#'\code{\link{UCB_rejection_sampling}} Require \code{\link{tic}} and
#'\code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'@param average Boolean values to define the cumulative regret evaluation
#'  (simple:FALSE, average:TRUE)
#'
#'@return
#' \itemize{ List of element:
#'  \item ucb_alloc: ucb object ,
#'  \item cum_reg_ucb_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#remove data
#'temp_list <- sample(1:nrow(visitor_reward), 500, replace = FALSE, prob = NULL)
#'visitor_reward$K1[temp_list] <- NA
#'visitor_reward$K2[-temp_list] <- NA
#'#run ucb on missing data
#'UcbRejectionSamplingBanditObjectEvaluation(visitor_reward,alpha = 1)
#'@export
#UCB object evaluation
UcbRejectionSamplingBanditObjectEvaluation <- function(visitor_reward=visitor_reward ,K=ncol(visitor_reward), alpha=1) {
  ucb_rejection_sampling_bandit_alloc <- UCB_rejection_sampling(visitor_reward, alpha = alpha, K=K)
  cum_rew_ucb_rejection_sampling_alloc <- reward_cumulative(choice=ucb_rejection_sampling_bandit_alloc$choice,
                                                            visitor_reward=visitor_reward)

  plot(cum_rew_ucb_rejection_sampling_alloc, type = 'l',ylab = "Cumulative Reward",xlab="Number of items")
  message(paste("average : "),max(cum_rew_ucb_rejection_sampling_alloc)/length(cum_rew_ucb_rejection_sampling_alloc), sep = " " )
  return (list('ucb_rejection_sampling_bandit_alloc'=ucb_rejection_sampling_bandit_alloc ,'cum_rew_ucb_rejection_sampling_alloc'=cum_rew_ucb_rejection_sampling_alloc))
}

#'UcbBanditObjectEvaluation
#'
#'Run the UCB algorithm using visitor_reward values with \code{\link{UCB}}
#'function. Stop if something is wrong. After execution of UCB, calculates the
#'cumulative regret associated with the choices made. Review the cumulative
#'regret according iterations and an ucb object. See also \code{\link{UCB}},
#'\code{\link{CumulativeRegret}} Require \code{\link{tic}} and \code{\link{toc}}
#'from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'@param average Boolean values to define the cumulative regret evaluation
#'  (simple:FALSE, average:TRUE)
#'
#'@return
#' \itemize{ List of element:
#'  \item ucb_alloc: ucb object ,
#'  \item cum_reg_ucb_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run UCB algorithm with policy evaluation
#'UcbBanditObjectEvaluation(visitor_reward,alpha = 1)
#'UcbBanditObjectEvaluation(visitor_reward,alpha = 1,average = TRUE,
#'IsRewardAreBoolean = TRUE)
#'@export
#UCB object evaluation
UcbBanditObjectEvaluation <- function(visitor_reward=visitor_reward ,K=ncol(visitor_reward), alpha=1,average = FALSE,IsRewardAreBoolean = FALSE,dt.reward=NA,explanatory_variable=colnames(dt.reward)) {
  ucb_bandit_alloc <- UCB(visitor_reward, alpha = alpha, K=K)

  if(average == FALSE) cum_reg_ucb_bandit_alloc <- cumulativeRegret(ucb_bandit_alloc$choice, visitor_reward)
  if(average == TRUE) cum_reg_ucb_bandit_alloc <- cumulativeRegretAverage(ucb_bandit_alloc$choice,
                                                                          visitor_reward = visitor_reward,
                                                                          dt=dt.reward,
                                                                          IsRewardAreBoolean=IsRewardAreBoolean,
                                                                          explanatory_variable=explanatory_variable)

  return (list('ucb_alloc'=ucb_bandit_alloc ,'cum_reg_ucb_alloc'=cum_reg_ucb_bandit_alloc))
}
