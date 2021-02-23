# Reference from class
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

simple.row.reduction <- function(A) {
  n = nrow(A)
  for (i in 1:n) {
    A = pivot.below(A, row=i, col=i)
  }
  print(A)
}

pivot.below.nd <- function(A, row=1, col=1) {
  n = nrow(A)
  if (row < n) {
    for (i in 1:(n-row)) {
      A[row+i,] = (-1)*(A[row+i,col]/A[row,col])*A[row,] + A[row+i,]
    }
  }
  return(A)
}

simple.rr.no.division <- function(A) {
  n = nrow(A)
  for (i in 1:n) {
    A = pivot.below.nd(A, row=i, col=i)
  }
  return(A)
}

simple.det <- function(A) {
  n = nrow(A)
  m = ncol(A)
  if (n != m) {
    print("Matrix is not a square!")
  }
  else {
    B = simple.rr.no.division(A)
    det = rep(NA,n)
    for (i in 1:n) {
      det[i] = B[i,i]
    }
    return(prod(det))
  }
}


A = rbind( c(3,4,-2), c(5,2,-3), c(6,3,-1))