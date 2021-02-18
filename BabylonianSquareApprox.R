# This function uses the Babylonian method for approximating square-roots
sq.seq <- function(x,s,n) {
  seq = array(NA, n)
  seq[1] = s
  for (i in 2:n) {
    seq[i] = (seq[i-1]^2 + x) / (2*seq[i-1])
  }
  return(seq[n])
}