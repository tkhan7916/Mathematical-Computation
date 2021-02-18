# this code executes a pivot operation below the specified row and col
# by filling those numbers with 0s and the number at the specified row and col
# is filled with 1
# the default pivot position is at row=1 and col=1
pivot.below = function(A, row=1, col=1) {
  n = nrow(A)
  A[row,] = A[row,] / A[row,col]
  if (row < n) {
    for (i in 1:(n-row)) {
      A[row+i,] = A[row,]*(-1)*A[row+i,col] + A[row+i,]
    }
  }
  return(A)
}

# this code executes row reduction with the assumption that all the pivots
# occur diagonally and there is no issue of division by zero in any of the steps
# technically this is only half of the row reduction, since we do will pivot
# above, so we will end up with a triangular matrix where the 0's are below
# the diagonal
simple.row.reduction = function(A) {
  n = nrow(A)
  for (i in 1:n) {
    A = pivot.below(A, row=i, col=i)
  }
  print(A)
}

x = runif(50, -1, 1)
A = matrix(x, nrow=5, ncol=10)
print(A)