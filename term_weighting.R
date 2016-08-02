weight <- function(Y, standard = TRUE) {
  # Takes in a matrix of terms and weights them according to cosine normalization and
  # Z - score
  # 
  #   Y: matrix of terms
  #   standard: boolean to determine whether to calculate Z-score or not
  #   
  # Returns:
  #   Y
  for (i in 1:ncol(Y)) {
    Y[,i] = Y[,i]/ sqrt(sum(Y[,i]^2))
  }
  
  if (standard == TRUE) {
    for (i in 1:nrow(Y)) {
        Y[i,] = (Y[i,] - mean(Y[i,]))/sd(Y[i,])
      
    }
  }
  
  Y[Y > 3] = 3
  Y[Y < 0] = 0
  Y[is.nan(Y)] = 0
  
  return(Y)
}





