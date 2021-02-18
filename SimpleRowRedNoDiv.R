# Pivot below function using no division
pivot.below.nd <- function(A, row=1, col=1) {
  n = nrow(A)
  if (row < n) {
    for (i in 1:(n-row)) {
      A[row+i,] = (-1)*(A[row+i,col]/A[row,col])*A[row,] + A[row+i,]
    }
  }
  return(A)
}

# Half row reduction using the no division function above
simple.rr.no.division <- function(A) {
  n = nrow(A)
  for (i in 1:n) {
    A = pivot.below.nd(A, row=i, col=i)
  }
  print(A)
}
