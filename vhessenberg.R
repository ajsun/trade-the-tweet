
vhessenberg <- function(x) {
  # Factors a square nxn matrix A into QHQ^T where H is Hessenberg and Q is orthogonal.
  # 
  #   x: matrix to be factored
  #
  #   
  # Returns:
  #   Q, H
  m = length(x)
  x = x/norm(x,"F")
  s = t(x[2:m]) %*% x[2:m]
  v = matrix(c(1, x[2:m]))
  if (s == 0) {
    beta = 0
  }
  else {
    mu = sqrt(x[1]^2+s)
    if (x[1] <= 0) {
      v[1] = x[1] - mu
    }
    else {
      v[1] = -s/(x[1] + mu)
    }
    beta = (2*v[1]^2)/(s+v[1]^2)
    v = v/v[1]
  }
  return(list(v=v, beta=beta))
}

Hess <- function(A) {
  n = max(dim(A))
  Q = diag(n)
  H = A
  if (n <= 2) {
    H = A
    Q = diag(n)
  }
  else {
    for (k in 1:(n-2)) {
      B = vhessenberg(as.matrix(H[(k+1):n,k]))
      v = B$v
      beta = B$beta
      I = diag(k)
      N = matrix(0, k, n-k)
      m = length(v)
      R = diag(m) - as.vector(beta)* v %*% t(v)
      H[(k+1):n,k:n] = R %*% H[(k+1):n, k:n]
      H[1:n, (k+1):n] = H[1:n, (k+1):n] %*% R
      P = rbind(cbind(I,N),cbind(t(N), R))
      Q = Q %*% P
    }
  }
  return(list(H = H, Q = Q))
}