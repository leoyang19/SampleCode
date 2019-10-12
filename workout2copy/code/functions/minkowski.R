#Minkowski Function

#' @title: Minkowski Distance
#' @description: This takes in two points, x and y, and returns the Minkowski distance of order p
#' @param: vector
#' @return: Numerical distance of order p (p-norm distance) for a point x and a point y

minkowski <- function(x, y, p = 1){
  if (length(x) != length(y)) {
    stop('x and y must have the same lengths')
  }
  else if(p =='max'){
    distance_vector = rep(NA, length(x))
    for (i in 1:length(x)) {
      distance_vector[i] = abs(x[i]-y[i])
      distance = max(distance_vector)
    }
  }
  else if (p < 1){
    stop('Invalid; p less that 1')
  }
  else {
    distance_vector = rep(NA, length(x))
    for (i in 1:length(x)) {
      distance_vector[i] = (abs(x[i] - y[i]))^1/p
      distance = (sum(distance_vector))^p
    }
  }
  return(distance)
}
