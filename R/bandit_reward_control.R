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
bandit_reward_control <- function(visitor_reward) {
  stopifnot(is.data.frame(visitor_reward))
  control_size(visitor_reward)
  control_missing_data(visitor_reward)
  return(TRUE)
}

#'Missing data control of visitor reward dataframe
#'
#'@description Controls data for bandit. Check in a dataframe if there is some
#'  missing values. Stops if it's not respected. Else returns TRUE.
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'
#'@return Logical value
#'
#'@export
control_missing_data <- function(visitor_reward) {
  #no missing data
  if (any(is.na(visitor_reward))) {
    stop("missing data in arm results data")
  }
  return(TRUE)
}

#'Size control of visitor reward dataframe
#'
#'@description Controls number of arms for bandit. Check if the number of
#'  columns is greater than or equal to 2 and if the number of rows is greater
#'  than or equal to the number of columns. Stops if not respected. Else returns
#'  TRUE.
#'
#'@param visitor_reward Dataframe of numeric values
#'
#'@return Logical value
#'
#'@export
control_size <- function(visitor_reward) {
  # number of arms must be superior to 2
  if (ncol(visitor_reward) < 2) {
    stop("number of arms must be greater than or equal to 2")
  }

  if(nrow(visitor_reward) < ncol(visitor_reward)){
    stop("horizon must be at least equal to the number of arms")
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
  if(!is.numeric(visitor_reward)){
    stop("reward data must be numeric")
  }
  if(any(mapply(function(v)(v!=1 && v!=0),visitor_reward))){
    stop("reward data must be binary")
  }
  return(TRUE)
}
