#' Backpropagation
#' @name backpropagation

backpropagation <- function(X, Y) {

}

#' Deep Learning
#' @name deeplearning

deeplearning <- function(X, Y) {

}

#' Gradient Descent
#' @name gradientdescent

gradientdescent <- function(X, Y) {

}

#' Recurrent Neural Network
#' @name recurrentneuralnetwork

recurrentneuralnetwork <- function(X, Y) {

}

#' Generate Backpropagation Data
#' @name backpropagation_data
#' @export

backpropagation_data <- function(n=50000) {
  # begin random numbers in the same spot
  set.seed(123)

  # generate
  x1 <- rbinom(50000, 1, 0.5)
  x2 <- rbinom(50000, 1, 0.5)
  x3 <- rbinom(50000, 1, 0.5)

  X <<- cbind(x1, x2, x3)

  Y <<- x1

  # print notice that objects X and Y are now available
  print("The objects X and Y are now available in the Global Environment.")
  print("Below the first few rows are printed together.")

  # print the top of the data
  return(head( cbind(Y,X) ))
}

#' Generate Deep Learning Data
#' @name deeplearning_data

deeplearning_data <- function(n=50000) {

}

#' Generate Gradient Descent Data
#' @name gradientdescent_data

gradientdescent_data <- function(n=50000) {

}

#' Generate Recurrent Neural Network Data
#' @name recurrentneuralnetwork_data

recurrentneuralnetwork_data <- function(n=50000) {

}
