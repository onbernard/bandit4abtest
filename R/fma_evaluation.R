setClass("FMA_evaluation",
         slots = c(
           visitor_reward    = "data.frame",
           policy            = "character",
           S                 = "matrix",
           choice            = "numeric",
           proba             = "numeric",
           time              = "numeric",
           th_hat            = "numeric",
           th                = "numeric"),
         prototype = list(
           visitor_reward    = data.frame(),
           policy            = "",
           S                 = diag(1),
           choice            = c(1),
           proba             = c(1),
           time              = c(1),
           th_hat            = c(1),
           th                = c(1))
)

setValidity("FMA_evaluation", function(object){
  is_fma_type(object@policy)
  # TODO : check policy parameters and visitor_reward validity
})

# Generics
setGeneric("run", function(o) standardGeneric("run"))

# Methods
setMethod("plot", signature(x="FMA_evaluation"), function(x, type){
  if(type == "proba"){
    plot(x@proba)
  }
  else if(type == "regret"){
    plot(cumulative_regret(x@choice, x@visitor_reward))
  }
  else {
    hist(x@choice)
  }
})

# Getters and Setters
setGeneric("visitor_reward", function(x) standardGeneric("visitor_reward"))
setMethod("visitor_reward", "FMA_evaluation", function(x) x@visitor_reward)
setGeneric("visitor_reward<-", function(x, value) standardGeneric("visitor_reward<-"))
setMethod("visitor_reward<-", "FMA_evaluation", function(x, value){
  x@visitor_reward <-value
  x
})

setGeneric("policy", function(x) standardGeneric("policy"))
setMethod("policy", "FMA_evaluation", function(x) x@policy)
setGeneric("policy<-", function(x, value) standardGeneric("policy<-"))
setMethod("policy<-", "FMA_evaluation", function(x, value){
  x@policy <-value
  x
})

setGeneric("S", function(x) standardGeneric("S"))
setMethod("S", "FMA_evaluation", function(x) x@S)
setGeneric("S<-", function(x, value) standardGeneric("S<-"))
setMethod("S<-", "FMA_evaluation", function(x, value){
  x@S <-value
  x
})

setGeneric("choice", function(x) standardGeneric("choice"))
setMethod("choice", "FMA_evaluation", function(x) x@choice)
setGeneric("choice<-", function(x, value) standardGeneric("choice<-"))
setMethod("choice<-", "FMA_evaluation", function(x, value){
  x@choice <-value
  x
})

setGeneric("proba", function(x) standardGeneric("proba"))
setMethod("proba", "FMA_evaluation", function(x) x@proba)
setGeneric("proba<-", function(x, value) standardGeneric("proba<-"))
setMethod("proba<-", "FMA_evaluation", function(x, value){
  x@proba <-value
  x
})

setGeneric("time", function(x) standardGeneric("time"))
setMethod("time", "FMA_evaluation", function(x) x@time)
setGeneric("time<-", function(x, value) standardGeneric("time<-"))
setMethod("time<-", "FMA_evaluation", function(x, value){
  x@time <-value
  x
})

setGeneric("th_hat", function(x) standardGeneric("th_hat"))
setMethod("th_hat", "FMA_evaluation", function(x) x@th_hat)
setGeneric("th_hat<-", function(x, value) standardGeneric("th_hat<-"))
setMethod("th_hat<-", "FMA_evaluation", function(x, value){
  x@th_hat <-value
  x
})

setGeneric("th", function(x) standardGeneric("th"))
setMethod("th", "FMA_evaluation", function(x) x@th)
setGeneric("th<-", function(x, value) standardGeneric("th<-"))
setMethod("th<-", "FMA_evaluation", function(x, value){
  x@th <-value
  x
})
