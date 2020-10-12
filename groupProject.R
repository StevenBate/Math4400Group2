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

#Sample and break into training and testing data
test.index = sample(c(1:dim(df)[1]), floor(n/2),replace = TRUE)
df.test = df[test.index,]
df.train = df[-test.index,]
train.X = df.train[,c("V2", "V3")]
test.X = df.test[,c("V2", "V3")]
train.Y = df.train[,c("y")]
test.Y = df.test[,c("y")]
