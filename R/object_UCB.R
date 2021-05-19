# TODO : change prototype to remove NULL c()
setClass("UCB",
         contains = "FMA_evaluation",
         slots = c(
           alpha = "numeric"
         ),
         prototype = list(
           alpha = 1)
)

UCB <- function(visitor_reward, alpha = 1){
  o <- new("UCB",
           visitor_reward = visitor_reward,
           alpha = alpha,
           policy ="ucb")
  run(o)
}

setValidity("UCB", function(object){
  # TODO
  TRUE
})

setMethod("run", "UCB", function(o){
  ucb_alloc <- policy_ucb(visitor_reward(o), alpha(o))
  S(o)              <- ucb_alloc$S
  choice(o)         <- ucb_alloc$choice
  proba(o)          <- ucb_alloc$proba
  time(o)           <- ucb_alloc$time
  th_hat(o)         <- ucb_alloc$theta_hat
  th(o)             <- ucb_alloc$theta
  o
})

setGeneric("alpha", function(x) standardGeneric("alpha"))
setMethod("alpha", "UCB", function(x) x@alpha)
setGeneric("alpha<-", function(x, value) standardGeneric("alpha<-"))
setMethod("alpha<-", "UCB", function(x, value){
  x@alpha <-value
  x
})
