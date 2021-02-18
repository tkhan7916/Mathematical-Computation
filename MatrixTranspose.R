# This function transposes a given matrix n x m, return a matrix m x n
trans <- function(A) {
  tr = matrix(NA, ncol(A), nrow(A))
  for (i in 1:ncol(A)) {
    for (j in 1:nrow(A)) {
      tr[i,j] = A[j,i]
    }
  }
  return(tr)
}