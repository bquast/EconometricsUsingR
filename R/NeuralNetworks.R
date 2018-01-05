#' Backpropagation
#' @name backpropagation
#' @export

backpropagation <- function(X, Y) {
  n          = length(Y)
  input_dim  = dim(X)[2] # the number of columns or variables in X
  output_dim = 1 # the number of outcome variables, here 1

  # initialize weights randomly between -1 and 1, with mean 0
  synapse_0  = matrix( runif(n = input_dim*output_dim, min=-1, max=1),
                       nrow=input_dim,   # for dot multiplication, a %*% b, we need the number of rows of b to be equal to the number of columns of a (i.e. the number of input variables)
                       ncol=output_dim ) # for dot multiplication, b %*% c, we need the number of columns of b to be equal to the number of rows of c (i.e. the number of output variables)

  for (j in 1:n) {
    # Feed forward through layers 0 and 1
    layer_0 = X[j, , drop=FALSE]
    layer_1 = sigmoid( layer_0 %*% synapse_0 )

    # how much did we miss the target value?
    layer_1_error = Y[j] - layer_1

    if (j %% 10000 == 0)
      print(paste("Error:", mean(abs(layer_1_error))))

    # in what direction is the target layer_1?
    # were we really sure? if so, don't change too much.
    layer_1_delta = layer_1_error * sig_to_der(layer_1)

    # update weights
    synapse_0 = synapse_0 + t(layer_0) %*% layer_1_delta
  }

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

#' Sigmoid
#' @name sigmoid
sigmoid <- function(x)
  1 / ( 1+exp(-x) )

#' Sigmoid to Derivative
#' @name sig_to_der
sig_to_der <- function(x)
  x*(1-x)
