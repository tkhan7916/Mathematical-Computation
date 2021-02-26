locate.root = function(f,a=0,b=1,n=20){
  
  for(i in 1:n){
    
    root = (a+b)/2
    
    if(f(a)*f(root) < 0){
      
      b = root
      
    }
    
    else{
      
      a = root
      
    }
    
  }
  
  root
  
}
roots = function(f,a=0,b=1,n=20,N=100){
  
  x = grid(a,b,N)
  
  signs = rep(NA,N+1)
  
  for(i in 1:(N+1)){
    
    if( f( x[i] ) > 0 ){
      
      signs[i] = "+"
      
    }
    
    else{
      
      signs[i] = "-"
      
    }
    
  }
  
  roots = NULL
  
  for(i in 1:N){
    
    if( signs[i] != signs[i+1]){
      
      roots = c(roots, locate.root(f,a = x[i],b=x[i+1],n))
      
    }
    
  }
  
  roots
  
}

# this searches for only real eigenvalues of a square matrix in a specified
# search interval from [a, b], where n is the number of steps used in the
# bisection algorithm and N is the number of sub-divisions applied to the 
# search interval
locate.eig <- function(A, a=0, b=1, n=20, N=100) {
  r = nrow(A)
  c = ncol(A)
  if (r != c) {
    print("This matrix is not square!")
  }
  else {
    I = diag(r)
    f <- function(lam) {det(A - lam*I)}
    return(roots(f,a,b,n,N))
  }
}


A = matrix(runif(16,min=-2,max=2),nrow=4,ncol=4)
print(A)

# Gershgorin Circle Theorem
# A subset set of R(n x n)
# if lambda is a real eigenvalue then
# |lambda| <= min(||A||_1, ||A||_inf)