# Calculating the matrix norms: 
#   1. Frobenius norm
#   2. 1-norm
#   3. inf-norm

# Frobenius norm function
F.norm <- function(A) {
  sum_squares = 0
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      sum_squares = sum_squares + A[i,j]^2
    }
  }
  return(sqrt(sum_squares))
}

# 1-norm function
one.norm <- function(A) {
  n = nrow(A)
  m = ncol(A)
  col_sums = vector(length=m)
  for (j in 1:m) {
    sum = 0
    for (i in 1:n) {
      sum = sum + abs(A[i,j])
    }
    col_sums[j] = sum
  }
  return(max(col_sums))
}

# Infinity norm function
inf.norm <- function(A) {
  n = nrow(A)
  m = ncol(A)
  row_sums = vector(length=n)
  for (i in 1:n) {
    sum = 0
    for (j in 1:m) {
      sum = sum + abs(A[i,j])
    }
    row_sums[i] = sum
  }
  return(max(row_sums))
}

# Main Matrix Norm function
mat.norm <- function(A, type=c("one", "inf", "F")) {
  switch (type,
    "one" = one.norm(A),
    "inf" = inf.norm(A),
    "F" = F.norm(A)
  )
}