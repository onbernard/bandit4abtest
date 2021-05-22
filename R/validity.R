#'Visitor rewards dataframe control
#'
#'@description Controls data for bandit algorithm. See also
#'  \code{\link{control_missing_data}} and \code{\link{control_size}}
#'
#'@details visitor_reward must be a dataframe without NA values, with a number
#'  of columns greater than or equal to 2 and a number of rows greater than or
#'  equal to the number of columns.
#'
#'@param visitor_reward Dataframe of numeric values
#'
#'@return Logical value
#'
#'@export
bandit_reward_control <- function(visitor_reward){
  if(is.data.frame(visitor_reward)) {
    "supplied visitor_reward is not a dataframe"
  }
  mat_visitor_reward <- as.matrix(visitor_reward)
  if(!is.numeric(mat_visitor_reward)) {
    "supplied visitor_reward must only contain numeric values"
  }
  if(ncol(mat_visitor_reward) < 2) {
    "number of columns in visitor_reward must be greater than or equal to 2"
  }
  if(nrow(mat_visitor_reward) < ncol(mat_visitor_reward)) {
    "number of rows in visitor_reward must be greater than or equal to the number of columns"
  }
  if(any(is.na(mat_visitor_reward))) {
    "visitor_reward must not contain NA values"
  }
  return(TRUE)
}

#'Are rewards binary values ?
#'
#'@description Checks if reward matrix contains only numeric binary values.
#'  Stops if not. Returns TRUE otherwise.
#'
#'@param visitor_reward Numeric matrix
#'
#'@return Logical value
#'
#'@export
control_binary <- function(visitor_reward){
  if(any(mapply(function(v)(v!=1 && v!=0),visitor_reward))){
    "values in visitor_reward data must be binaries"
  }
  return(TRUE)
}

# TODO : contextual
#'data_control_context_reward
#'
#'@description Controls whether the number of rows of dt and visitor_reward
#'  matches. Returns TRUE if it does, produces an error if not.
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param dt             Dataframe of integer numeric or factor values
#'
#'@return Logical value
#'
#'@export
data_control_context_reward <- function(dt, visitor_reward) {
  if (nrow(dt) != nrow(visitor_reward)) {
    stop("number of row in contextual data and rewards data are not equals")
  }
  return (TRUE)
}

is_fma_type <- function(type){
  if(type %in% c("ucb", "ts", "exp3", "grdy", "klucb"))
  {
    TRUE
  } else {
    "specified type must be of one of : ucb, ts, exp3, grdy, klucb"
  }
}

