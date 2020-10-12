#Start of file
library('class')

data.generate = function(mu1, mu2, s, n , p){
  
  y = as.factor(c(rep(1,n),rep(2,n)))
  M = matrix (NA,  nrow = 2*n, ncol = p, byrow = 1) 
  df = data.frame(y)
  for (i in c(1:p)) {
    M[,i] = c(rnorm(n,mu1,s), rnorm(n,mu2,s))
    #assign(paste("x", i, sep=""),c(rnorm(n,mu1[i],s), rnorm(n,mu2[i],s)) )
  }
  df = cbind(y,M)
  df = data.frame(df)
  return(df)
}
n = 500
df = data.generate(1,-1, 1, n, 2)
head(df)
