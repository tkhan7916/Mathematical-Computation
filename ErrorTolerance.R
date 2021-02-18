# we will create an "error tolerance" program that will run through the
# values that make up a matrix and replace them by 0s whenever a threshold
# error is tolerated
et = function(A, eps=0.02) {
  n = nrow(A)
  m = ncol(A)
  for (i in 1:n) {
    for (j in 1:m) {
      if (abs((A[i,j]) < eps)) {
        A[i,j] = 0
      }
    }
  }
  return(A)
}