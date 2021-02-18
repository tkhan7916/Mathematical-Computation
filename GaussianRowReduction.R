# once a row and col position is specified this code will search for the 
# maximum number below the position and bring it specified position
locate.pivot <- function(A, row=1, col=1) {
  n = nrow(A)
  x = A[row:n,col]
  x = abs(x)
  m = which.max(x)
  max.row = A[row+m-1,]
  initial.row = A[row,]
  A[row,] = max.row
  A[row+m-1,] = initial.row
  return(A)
}

# this code executes half row reduction only down the diagonal, but not up
# this row reduction solves the issue of zero division
row.reduce <- function(A) {
  n = nrow(A)
  m = ncol(A)
  p = 1
  q = 1
  for (i in 1:m) {
    if (p<= n & q <= m) {
      locate.pivot(A,p,q)
      if (A[p,q] != 0) {
        A = pivot.below(A,p,q)
        p = p + 1
        q = q + 1
      }
      else {
        q = q + 1
      }
    }
  }
  return(A)
}

x = rbinom(30, size=3, prob=.75)
A = matrix(x, 3, 10)