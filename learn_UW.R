learn_UW <- function(R, Y, d, lambda, mu, rho, iter) {
  # Solves for U, W in the objective function from Wong et. al. (2014)
  # Code originally from Wong et. al. (2014) adapted for R
  # 
  #
  #   R: matrix of returns
  #   Y: matrix of terms
  #   d: latent factors
  #   lambda: sparse group lasso hyperparameter
  #   mu: sparse group lasso hyperparameter
  #   rho: sparse group lasso hyperparameter
  #   iter: number of iterations to run
  #   
  # Returns:
  #   U
  #   W
  #   obj: the objective value
  library('MASS')
  library('Matrix')
  source('solve_Y.R')
  source('vhessenberg.R')
  if (missing(iter)) {
    maxiter = 80;
  }
  # W is a d by m matrix, where m is the number of terms
  size_W = c(d, nrow(Y))
  # U is a n by d matrix, where n is the number of stocks
  size_U = c(nrow(R), d)
  
  n = prod(size_W)
  m = prod(size_U)
  
  ## initialize the Lagrangian multipliers
  #C = matrix(runif(m), size_U[1], size_U[2])
  C = matrix(0, size_U[1], size_U[2])
  D = matrix(0, size_W[1], size_W[2])
  
  
  # intializing a first guess of U and W using svd
  s = svd(R)
  # factors R into R = U %*% S %*% t(V)
  S = diag(s$d)
  
  # initialize U
  U = s$u[, 1:d] %*%  sqrt(S[1:d, 1:d])
  # if the term is too close to 0 or negative, set to 0.1 (so its not singular)
  eps = 0.1
  U[U<eps] = eps
  # intialize A = U
  A = U
  # find V
  V = sqrt(S[1:d, 1:d]) %*% t(s$v[,1:d])
  
  # intialize W as V/Y (since V = WY)
  # least squares approximation of V/Y (could also use generalized inverse)
  W = t(solve( qr(t(Y), LAPACK=TRUE) , t(V) ))
  
  # initialize B = W
  B = W
  # save calculation by computing this once (YYt = YY')
  YYt = Y %*% t(Y)
  # precompute the Schur decomposition of YYt
  schur = Schur(YYt)
  # R produces T Q T'
  # .s suffix stands for Hessenberg-Schur method variables
  S.s = schur$T
  V.s = schur$Q
  
  obj = rep(0,maxiter+1)
  obj[1] = 0.5*norm(R-(U %*% W %*% Y), "F")^2 + 
    lambda*sum(sqrt(colSums(W^2))) + mu * sum(abs(W))
  # begin the update steps
  for (i in 2:(maxiter+1)) {
    ### Updating A ###
    # numerator for A update step
    a = (R %*% t(Y) %*% t(B) - C + rho * U)
    # denominator for A update step
    b = (B %*% YYt %*% t(B) + rho * diag(d))
    # update A
    A = a %*% solve(b)
    #A = t(solve( qr(t(b), LAPACK=TRUE) , t(a) ))
    
    ### Updating B ###
    # need the form AXB + X = C
    # .s suffix stands for Hessenberg-Schur method variables
    A.s = (1 / rho) * t(A) %*% A 
    # B has been pre-calculated as YYt to save computation
    C.s = 1 / rho * (t(A) %*% R %*% t(Y) - D) + W
    B = solve_sylvester(A.s, S.s, V.s, C.s)
#     ## Applying the Hessenber Schur Method ##
#     ## H Y S' = F
#     # compute the hessenberg decomposition for H
#     print(i)
#     hess = Hess(A.s)
#     # function outputs Q H Q'
#     U.s = hess$Q
#     H.s = hess$H
#     # S has been computed already
#     # computing F
#     F.s = t(U.s) %*% C.s %*% V.s
#     # solve the HS equation
#     Y.s = solve_Y(H.s, S.s, F.s)
#     # obtain B by UYV'
#     B = U.s %*% Y.s %*% t(V.s)
    
    ### Updating U ###
    U = A + C / rho
    U[U < 0] = 0
    
    ### Updating W ###
    for (j in 1:ncol(W)) {
      v = B[, j, drop = FALSE] + D[, j, drop = FALSE] / rho
      W[, j] = update_wj(v, lambda, mu, rho)
    }
    
    ### Updating C ###
    C = C + rho * (A - U)
    ### Updating D ###
    D = D + rho * (B - W)
    
    obj[i] = 0.5*norm(R-(U %*% W %*% Y), "F")^2 + 
      lambda*sum(sqrt(colSums(W^2))) + mu * sum(abs(W))
    ## stopping criterion
#     if (i > 4) {
#       if (abs(obj[i] - obj[i-1])/abs(obj[i-1]) < tol) {
#         if (abs(obj[i-1] - obj[i-2])/abs(obj[i-2]) < tol) {
#           break
#         }
#       }
#     }
  }
  return(list(U = U, W = W, obj = obj))
}
solve_sylvester <- function(A, S, V, C) {
  hess = Hess(A)
  H = hess$H
  U = hess$Q
  
  #   schur = Schur(B)
  #   S = schur$T
  #   V = schur$Q
  
  f = t(U) %*% C %*% V
  Ys = solve_Y(H, S, f)
  Xs = U %*% Ys %*% t(V)
  return(Xs)
}

update_wj <- function(v, lambda, mu, rho) {
  # calulate the positive term
  vmax = abs(v) - mu/rho
  vmax[vmax<0] = 0
  # calculate w
  w = rho * sign(v) * vmax 
  # calculate the norm
  norm_w = sqrt(sum(w^2))
  # if the numerator is <= 0, return all 0's
  if (norm_w <= lambda) {
    u = matrix(0, nrow(v), ncol(v))
  }
  # else calculate the new w
  else {
    u = ((norm_w - lambda) / (rho * norm_w)) * w
  }
  return(u)
}