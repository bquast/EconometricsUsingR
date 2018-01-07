#' @name EconometricsUsingR
#' @docType package
#' @title Econometrics Using R
#' @author
#' Hrishikesh Vinod \email{vinod@@fordham.edu} (Editor), \cr
#'  ... , \cr
#' Bastiaan Quast \email{bquast@@gmail.com}
#' @description This R package accompanies the Handbook of Statistics: Econometrics Using R with code from the chapters.
#' The prefered viewing format is using the vignette which can be found by following this link \link{../doc/EconometricsUsingR.html}.
#' @section Chapter 1 (A. Nonymous):
#' In Chapter 1 "nonameyet", we can find functions a, b, and c.
#' @section Neural Networks (B.A. Quast):
#' The chapter "Neural Networks" contains several code snippets, which are evaluated in the book using \link{knitr}.
#'
#' Backpropagation \cr
#' - \code{\link{backpropagation_data}} \cr
#' - \code{\link{backpropagation}} \cr
#'
#' Deep Learning \cr
#' - \code{\link{deeplearning_data}} \cr
#' - \code{\link{deeplearning_training}} \cr
#'
#' Gradient Descent \cr
#' - \code{\link{gradientdescent_data}} \cr
#' - \code{\link{gradientdescent_training}} \cr
#'
#' Recurrent Neural Network \cr
#' - \code{\link{recurrentneuralnetwork_data.R}} \cr
#' - \code{\link{recurrentneuralnetwork_training.R}} \cr
#'
#' @references {Handbook of Statistics Volume XX (2018) Econometrics Using R; Edited by Hrishikesh D. Vinod and C.R. Rao}
#' @seealso \url{https://www.elsevier.com/books/computational-statistics-with-r/rao/978-0-444-63431-3}
"_PACKAGE"
.onLoad <- function(...){
  packageStartupMessage('HANDBOOK OF STATISTICS XX: ECONOMETRICS USING R

Welcome to the R package accompanying Econometrics Using R.

For an overview of the package, please use:

  vignette("EconometricsUsingR")

or alternatively:

  help("EconometricsUsingR")
')
}
