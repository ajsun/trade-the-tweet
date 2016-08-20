###########################################################################
# Script that produces predictions from day 503 to 1173 (2013-01-02 to 2015-08-31)
#
# Andrew Sun
#
########################################################################### 
source('learn_UW.R')
source('term_weighting.R')
library(tm)
library(parallel)
library(doParallel)
library(foreach)
library(iterators)


# variable 'Y' is the word frequency matrix dim are words x dates (1000 x 1173)
# reading in term document matrix
Y.dir = 'y_directory'
load(Y.dir)

# variable 'returns' is the return matrix dim are stocks x dates (420 x 1173)
# reading in matrix of returns
returns.dir = 'returns_directory'
load(returns.dir)

# R is the return matrix dim are stocks x dates (420 x 1173)
# Y is the word frequency matrix dim are words x dates (1000 x 1173w)

# number of days to train
n = 400
# total range of dates to test
start.T = 503
end.T = 1173

## parallel code
cl <- makeCluster(detectCores(), outfile='')
registerDoParallel(cl)
d = 10
a = .5
lambda = a
mu = 1-a
rho = 10
price.pred <- foreach(i=(start.T:end.T), .combine = 'cbind') %dopar% {
  range <- (i-n):(i-1)
  trainY <- weight(Y[,range-1, drop=FALSE], standard = FALSE)
  trainR <- returns[,range, drop=FALSE]
  model.out <- learn_UW(trainR,trainY,d, lambda, mu, rho)
  y.pred <- Y[,i-1,drop=FALSE]
  out <- model.out$U %*% model.out$W %*% y.pred
  return(out)
}

stopCluster(cl)

