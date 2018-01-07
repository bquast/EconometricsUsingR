#' Generate Backpropagation Data
#' @name backpropagation_data
#' @family backpropagation
#' @family data generators
#' @export

backpropagation_data <- function(n=50000) {
  # begin random numbers in the same spot
  set.seed(123)

  # generate
  x1 <- rbinom(50000, 1, 0.5)
  x2 <- rbinom(50000, 1, 0.5)
  x3 <- rbinom(50000, 1, 0.5)

  # join x1, x2, x3 to X
  # and write X to the Global Environment
  X <<- cbind(x1, x2, x3)

  # define Y as being equal to x1
  # and write Y to the Global Environment
  Y <<- x1

  # print notice that objects X and Y are now available
  print("The objects X and Y are now available in the Global Environment.")
  print("Below the first few rows are printed together.")

  # print the top of the data
  return(head( cbind(Y,X) ))
}

#' Backpropagation Training
#' @name backpropagation
#' @family backpropagation
#' @family training
#' @export

backpropagation_training <- function(X, Y) {
  n          = length(Y)
  input_dim  = dim(X)[2] # the number of columns or variables in X
  output_dim = 1 # the number of outcome variables, here 1

  # initialize weights randomly between -1 and 1, with mean 0
  weights_0  = matrix( runif(n = input_dim*output_dim, min=-1, max=1),
                       nrow=input_dim,   # for dot multiplication, a %*% b, we need the number of rows of b to be equal to the number of columns of a (i.e. the number of input variables)
                       ncol=output_dim ) # for dot multiplication, b %*% c, we need the number of columns of b to be equal to the number of rows of c (i.e. the number of output variables)

  # loop through all observations
  for (j in 1:n) {
    # Feed forward through layers 0 and 1
    layer_0 = X[j, , drop=FALSE]
    layer_1 = sigmoid( layer_0 %*% weights_0 )

    # how much did we miss the target value?
    layer_1_error = Y[j] - layer_1

    if (j %% 10000 == 0)
      print(paste("Error:", mean(abs(layer_1_error))))

    # in what direction is the target layer_1?
    # were we really sure? if so, don't change too much.
    layer_1_delta = layer_1_error * sig_to_der(layer_1)

    # update weights
    weights_0 = weights_0 + t(layer_0) %*% layer_1_delta
  }

  # return?

}

#' Generate Deep Learning Data
#' @name deeplearning_data
#' @family data generators

deeplearning_data <- function(n=50000) {
  # begin random numbers in the same spot
  set.seed(123)

  # generate
  x1 <- rbinom(50000, 1, 0.5)
  x2 <- rbinom(50000, 1, 0.5)
  x3 <- rbinom(50000, 1, 0.5)

  # join x1, x2, x3 to X
  # and write X to the Global Environment
  X <<- cbind(x1, x2, x3)

  # define Y as the XOR of x1 and x2
  # and write Y to the Global Environment
  Y <<-- ifelse(x1 == x2, 0, 1)

  # print notice that objects X and Y are now available
  print("The objects X and Y are now available in the Global Environment.")
  print("Below the first few rows are printed together.")

  # print the top of the data
  return(head( cbind(Y,X) ))
}

#' Deep Learning Training
#' @name deeplearning
#' @family deep learning
#' @family training

deeplearning_training <- function(X, Y, hidden_dim=4) {
  n          = length(Y)
  input_dim  = dim(X)[2] # the number of columns or variables in X
  output_dim = 1 # the number of outcome variables, here 1

  # initialise weights
  weights_0 = matrix( runif(n = input_dim*hidden_dim, min=-1, max=1),
                      nrow=input_dim,
                      ncol=hidden_dim )
  weights_1 = matrix( runif(n = hidden_dim*output_dim, min=-1, max=1),
                      nrow=hidden_dim,
                      ncol=output_dim )

  # loop through all observations
  for (j in 1:n) {
    # Feed forward through layers 0, 1, and 2
    layer_0 = X[j, , drop=FALSE]
    layer_1 = sigmoid( layer_0 %*% weights_0 )
    layer_2 = sigmoid( layer_1 %*% weights_1 )

    # how much did we miss the target value?
    layer_2_error = Y[j] - layer_2
    if (j %% 10000 == 0)
      print(paste("Error:", mean(abs(layer_2_error))))

    # in what direction is the target value?
    # were we really sure? if so, don't change too much.
    layer_2_delta = layer_2_error * sig_to_der(layer_2)

    # how much did each layer_1 value contribute to
    # the layer_2 error (according to the weights)?
    layer_1_error = layer_2_delta %*% t(weights_1)

    # in what direction is the target layer_1?
    # were we really sure? if so, don't change too much.
    layer_1_delta = layer_1_error * sig_to_der(layer_1)

    # how much did layer_1 value contribute
    # to the error (according to the weights)?
    weights_1 = weights_1 + t(layer_1) %*% layer_2_delta
    weights_0 = weights_0 + t(layer_0) %*% layer_1_delta
  }

  # return?
}

#' Generate Gradient Descent Data
#' @name gradientdescent_data
#' @family data generators

gradientdescent_data <- deeplearning_data()


#' Gradient Descent Training
#' @name gradientdescent
#' @family gradient descent
#' @family training

gradientdescent_training <- function(X, Y, hidden_dim =4, alpha=0.1) {
  n          = length(Y)
  input_dim  = dim(X)[2] # the number of columns or variables in X
  output_dim = 1 # the number of outcome variables, here 1

  # initialise weights
  weights_0 = matrix( runif(n = input_dim*hidden_dim, min=-1, max=1),
                      nrow=input_dim,
                      ncol=hidden_dim )
  weights_1 = matrix( runif(n = hidden_dim*output_dim, min=-1, max=1),
                      nrow=hidden_dim,
                      ncol=output_dim )

  # loop through all observations
  for (j in 1:n) {
    # Feed forward through layers 0, 1, and 2
    layer_0 = X[j, , drop=FALSE]
    layer_1 = sigmoid( layer_0 %*% weights_0 )
    layer_2 = sigmoid( layer_1 %*% weights_1 )

    # how much did we miss the target value?
    layer_2_error = Y[j] - layer_2
    if (j %% 10000 == 0)
      print(paste("Error:", mean(abs(layer_2_error))))

    # in what direction is the target value?
    # were we really sure? if so, don't change too much.
    layer_2_delta = layer_2_error * sig_to_der(layer_2)

    # how much did each layer_1 value contribute to
    # the layer_2 error (according to the weights)?
    layer_1_error = layer_2_delta %*% t(weights_1)

    # in what direction is the target layer_1?
    # were we really sure? if so, don't change too much.
    layer_1_delta = layer_1_error * sig_to_der(layer_1)

    # how much did layer_1 value contribute
    # to the error (according to the weights)?
    weights_1 = weights_1 + t(layer_1) %*% layer_2_delta
    weights_0 = weights_0 + t(layer_0) %*% layer_1_delta
  }

  # return?
}

#' Generate Recurrent Neural Network Data
#' @name recurrentneuralnetwork_data
#' @family data generators

recurrentneuralnetwork_data <- function(n=10000, binary_dim=8) {

  # calculate possible values given binary_dim
  possible_values = 2^binary_dim

  # create sample inputs
  X1 = sample(0:(possible_values-1), n, replace=TRUE)
  X2 = sample(0:(possible_values-1), n, replace=TRUE)

  # create sample output
  Y <- X1 + X2

  # print notice that objects X and Y are now available
  print("The objects X1, X2, and Y are now available in the Global Environment.")
  print("Below the first few values of Y as integers and as binaries are printed.")

  # print integers
  print(head(Y))

  # convert to binary
  X1 <<- t( sapply(X1, int2binary) )
  X2 <<- t( sapply(X2, int2binary) )
  Y  <<- t( sapply(Y, int2binary) )

  # print the top of the data
  return(head(Y))

}

#' Recurrent Neural Network Training
#' @name recurrentneuralnetwork
#' @family recurrent neural network
#' @family training

recurrentneuralnetwork <- function(X1, X2, Y, hidden_dim=10, alpha=0.1) {
  # define dimenions manually
  input_dim  <- 2
  output_dim <- 1

  # initialize neural network weights
  weights_0 = matrix(runif(n = input_dim *hidden_dim, min=-1, max=1),
                     nrow=input_dim,
                     ncol=hidden_dim )
  weights_h = matrix(runif(n = hidden_dim*hidden_dim, min=-1, max=1),
                     nrow=hidden_dim,
                     ncol=hidden_dim )
  weights_1 = matrix(runif(n = hidden_dim*output_dim, min=-1, max=1),
                     nrow=hidden_dim,
                     ncol=output_dim )

  # create objects to store updates in
  weights_0_update = matrix(0, nrow = input_dim, ncol = hidden_dim)
  weights_h_update = matrix(0, nrow = hidden_dim, ncol = hidden_dim)
  weights_1_update = matrix(0, nrow = hidden_dim, ncol = output_dim)

  for (j in 1:n) {
    # select data
    a = X1[j,]
    b = X2[j,]

    # select true answer
    c = Y[j,]

    # where we'll store our best guesss (binary encoded)
    d = matrix(0, nrow = 1, ncol = binary_dim)
    overallError = 0
    layer_2_deltas = matrix(0)
    layer_1_values = matrix(0, nrow=1, ncol = hidden_dim)

    # moving along the positions in the binary encoding
    for (position in 1:binary_dim) {

      # generate input and output
      X = cbind( a[position], b[position] ) # rename X to layer_0?
      y = c[position]

      # hidden layer (input ~+ prev_hidden)
      layer_1 = sigmoid( (X%*%weights_0) +
                           (layer_1_values[dim(layer_1_values)[1],] %*% weights_h) )

      # output layer (new binary representation)
      layer_2 = sigmoid(layer_1 %*% weights_1)

      # did we miss?... if so, by how much?
      layer_2_error = y - layer_2
      layer_2_deltas = rbind(layer_2_deltas, layer_2_error * sig_to_der(layer_2))
      overallError = overallError + abs(layer_2_error)

      # decode estimate so we can print it out
      d[position] = round(layer_2)

      # store hidden layer
      layer_1_values = rbind(layer_1_values, layer_1)
    }

    future_layer_1_delta = matrix(0, nrow = 1, ncol = hidden_dim)

    for (position in binary_dim:1) {
      X = cbind(a[position], b[position])
      layer_1 = layer_1_values[dim(layer_1_values)[1]-(binary_dim-position),]
      prev_layer_1 = layer_1_values[dim(layer_1_values)[1]- ( (binary_dim-position)+1 ),]

      # error at output layer
      layer_2_delta = layer_2_deltas[dim(layer_2_deltas)[1]-(binary_dim-position),]

      # error at hidden layer
      layer_1_delta = (future_layer_1_delta %*% t(weights_h) +
                         layer_2_delta %*% t(weights_1)) * sig_to_der(layer_1)

      # let's update all our weights so we can try again
      weights_1_update = weights_1_update + matrix(layer_1) %*% layer_2_delta
      weights_h_update = weights_h_update + matrix(prev_layer_1) %*% layer_1_delta
      weights_0_update = weights_0_update + t(X) %*% layer_1_delta
      future_layer_1_delta = layer_1_delta
    }

    weights_0 = weights_0 + ( weights_0_update * alpha )
    weights_1 = weights_1 + ( weights_1_update * alpha )
    weights_h = weights_h + ( weights_h_update * alpha )

    weights_0_update = weights_0_update * 0
    weights_1_update = weights_1_update * 0
    weights_h_update = weights_h_update * 0

    if(j%%(n/5) == 0)
      print(paste("Error:", overallError))
  }

}


#' Sigmoid
#' @name sigmoid
sigmoid <- function(x)
  1 / ( 1+exp(-x) )

#' Sigmoid to Derivative
#' @name sig_to_der
sig_to_der <- function(x)
  x*(1-x)

#' Integer to Binary
#' @name int2binary
int2binary <- function(x)
  head(as.integer(intToBits(x) ), binary_dim)
