# this program will execute usual matrix multiplication
mat.prod = function(A, B) {
  n = nrow(A)
  m = ncol(A)
  p = nrow(B)
  q = ncol(B)
  if (m != p) {
    print("Matrix multiplication is not defined!!!")
  }
  if (m == p) {
    x = rep(NA, n*q)
    C = matrix(x, nrow=n, ncol=q)
    for (i in 1:n) {
      for (j in 1:q) {
        C[i,j] = sum(A[i,] * B[,j])
      }
    }
    return(C)
  }
}

x = rbinom(12, size=1, prob=0.5)
A = matrix(x, nrow=3, ncol=4)
print(A)
y = rbinom(12, size=1, prob=0.5)
B = matrix(x, nrow=4, ncol=3)
print(B)