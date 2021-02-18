# Reference from class
# The pivot below function
pivot.below <- function(A, row=1, col=1) {
  n = nrow(A)
  A[row,] = A[row,] / A[row,col]
  if (row < n) {
    for (i in 1:(n-row)) {
      A[row+i,] = A[row,]*(-1)*A[row+i,col] + A[row+i,]
    }
  }
  return(A)
}
# Simple row reduction function
simple.row.reduction <- function(A) {
  n = nrow(A)
  for (i in 1:n) {
    A = pivot.below(A, row=i, col=i)
  }
  print(A)
}

# Pivot above function
pivot.above <- function(A, row=nrow(A), col=ncol(A)) {
  A[row,] = A[row,] / A[row,col]
  if (row > 1) {
    for (i in 1:row) {
      A[row-i,] = A[row,]*(-1)*A[row-i,col] + A[row-i,]
    }
  }
  return(A)
}

# Complete form of simple row reduction
simple.rr.complete <- function(A) {
  n = nrow(A)
  for (i in 1:n) {
    A = pivot.below(A, row=i, col=i)
    A = pivot.above(A, row=i, col=i)
  }
  return(A)
}

# Generating Matrix
x = runif(40, -1, 1)
A = matrix(x, nrow=5, ncol=6)