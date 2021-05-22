# TODO : change prototype to remove NULL c()
setClass("ThompsonSampling",
         contains = "FMA_evaluation",
         slots = c(
           alpha = "numeric",
           beta  = "numeric"
         ),
         prototype = list(
           alpha = 1,
           beta = 1)
)

ThompsonSampling <- function(visitor_reward, alpha=1, beta=1){
  o <- new("ThompsonSampling",
           visitor_reward = visitor_reward,
           alpha          = alpha,
           beta           = beta,
           policy         = "ts")
  run(o)
}

setValidity("ThompsonSampling", function(object){
  bandit_reward_control(object@visitor_reward)
  control_binary(object@visitor_reward)
})

setMethod("run", "ThompsonSampling", function(o){
  ts_alloc  <- policy_thompson_sampling(visitor_reward(o), alpha(o), beta(o))
  choice(o) <- ts_alloc$choice
  proba(o)  <- ts_alloc$proba
  time(o)   <- ts_alloc$time
  th_hat(o) <- ts_alloc$theta_hat
  th(o)     <- ts_alloc$theta
  return(o)
})

#setGeneric("alpha", function(x) standardGeneric("alpha"))
#setMethod("alpha", "ThompsonSampling", function(x) x@alpha)
alpha <- function(x) x@alpha

setGeneric("alpha<-", function(x, value) standardGeneric("alpha<-"))
setMethod("alpha<-", "ThompsonSampling", function(x, value){
  x@alpha <-value
  x
})

setGeneric("beta", function(x) standardGeneric("beta"))
setMethod("beta", "ThompsonSampling", function(x) x@beta)
setGeneric("beta<-", function(x, value) standardGeneric("beta<-"))
setMethod("beta<-", "ThompsonSampling", function(x, value){
  x@beta <-value
  x
})
