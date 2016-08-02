solve_Y <- function(H, S, F) {
  # Solves for Y in HYS' + Y = F. Code rewritten from Wong et. al. (2014) to be
  # used in R
  #
  #   H: H is an upper Hessenberg matrix
  #   S: S is a quasi triangular matrix
  #   F: F is a matrix
  #   
  # Returns:
  #   Y
  dimF = dim(F)
  m = dimF[1]
  n = dimF[2]
  
  Y = matrix(0, m, n)
  
  k = n
  # take care of values close to 0
  S <- zapsmall(S)
  while (k>=1) {
    if ((k>1) && (S[k,k-1] != 0)) {
      B = F[,(k-1):k, drop=FALSE]
      # non-vectorized method is faster than the vectorized method
      Btmp2 = matrix(0,dim(B)[1],dim(B)[2])
      if (k<n) {
        for (j in (k+1):n) {
          Btmp2 = Btmp2 + cbind(S[k-1,j] * Y[,j,drop=FALSE], S[k,j] * Y[,j, drop=FALSE])
        }
      }
      B = B-H %*% Btmp2
      # vectorized method
      # if (k < n) {
      #   B = B - H %*% cbind(rowSums(Y[,(k+1):n,drop=FALSE] %*% diag(x = S[k-1,(k+1):n])), rowSums(Y[,(k+1):n,drop=FALSE] %*% diag(x = S[k,(k+1):n])))
      # }
      a = S[k-1,k-1] * H + diag(m)
      b = S[k-1,k] * H
      c = S[k,k-1] * H
      d = S[k,k] * H + diag(m)
      A = rbind(cbind(a,b), cbind(c,d))
      y = solve(A,rbind(B[,1,drop=FALSE],B[,2,drop=FALSE]))
      Y[,k-1] = y[1:m]
      Y[,k] = y[(m+1):(2*m)]
      k = k-1
    }
    else {
      B = F[,k,drop=FALSE]
      # non-vectorized method is faster than the vectorized method
      Btmp1 = matrix(0,dim(B)[1],dim(B)[2])
      if (k<n) {
        for (j in (k+1):n) {
          Btmp1 = Btmp1 + S[k,j] * Y[,j]
        } 
      }
      B = B - H %*% Btmp1
      # vectorized method
      # if (k < n) {
      #   B = B - H %*% rowSums(Y[,(k+1):n,drop=FALSE] %*% diag(x=S[k,(k+1):n],nrow = length(S[k,(k+1):n])))
      # }
      A = S[k,k] * H + diag(m)
      Y[,k] = solve(A,B)
    }
    k = k-1
  }
  return(Y)
}