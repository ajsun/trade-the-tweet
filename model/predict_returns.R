predict_returns <- function(Y, returns, y) {
  # Given historical matrix of word frequencies and historical returns, computes
  # the predicted return
  # 
  # Args:
  #   Y: weighted term-document matrix
  #   returns: the matrix of historical returns
  #   y: the vector of term frequencies we'd like to predict
  #   
  # Returns:
  #   A vector of predicted returns
  source('solve_Y.R')
  library(tm)
  # hard-coded hyper parameters #
  d = 10
  a = .4
  lambda = a
  mu = 1-a
  rho = 10
  ###############################
  model.output <- learn_UW(trainR,trainY,d, lambda, mu, rho)
  predicted.return <- model.output$U %*% model.output$W %*% y
  return(predicted.return)
}