#Start of file
library('class')
library(pROC)
library('MASS')

#Generate data
data.generate = function(mu1, mu2, variance, sampleSize, numInputVariables){
  
  y = as.factor(c(rep(1, sampleSize),rep(2, sampleSize)))
  M = matrix (NA,  nrow = 2* sampleSize, ncol = numInputVariables, byrow = 1) 
  df = data.frame(y)
  for (i in c(1:numInputVariables)) {
    M[,i] = c(rnorm(sampleSize, mu1, variance), rnorm(sampleSize, mu2, variance))
    #assign(paste("x", i, sep=""),c(rnorm(n,mu1[i],s), rnorm(n,mu2[i],s)) )
  }
  df = cbind(y,M)
  df = data.frame(df)
  return(df)
}

#Split into train / test data
train_Test_Split = function(df, seed)
{
  set.seed(seed)
  test.index = sample(c(1:dim(df)[1]), floor(dim(df)[1]/2), replace = FALSE)
  
  assign("df.train", df[-test.index,], envir = .GlobalEnv)
  assign("df.test", df[test.index,], envir = .GlobalEnv)
  
  assign("train.X", df.train[,-1], envir = .GlobalEnv)
  assign("train.Y", df.train[,1], envir = .GlobalEnv)
  assign("test.X", df.test[,-1], envir = .GlobalEnv)
  assign("test.Y", df.test[,1], envir = .GlobalEnv)
  
  return(test.index)
}

#KNN algorythm
generate.knn = function(train.Y, test.X, train.X, test.Y){
  knn.pred = knn(train.X,test.X,train.Y,k=dim(train.X)[1]*0.1)
  
  knn.accuracy = sum(knn.pred == test.Y)/length(test.Y)
  
  return(knn.accuracy)
}

#Qda algorythm
generate.qda = function(df.train, df.test){
  qda.fit = qda(y ~ . - y, data = df.train)
  
  qda.pred = predict(qda.fit,df.test[,-1])
  qda.accuracy = mean(qda.pred$class==df.test[,1])
  
  return(qda.accuracy)
}

#Lda algorythm
generate.lda = function(df.train, df.test){
  lda.fit = lda(y ~ . - y, data = df.train)
  
  lda.pred = predict(lda.fit,df.test[,-1])
  lda.accuracy = mean(lda.pred$class==df.test[,1])
  
  return(lda.accuracy)
}

#Logistic Algorithm
generate.logistic = function(df.train, df.test){
  df.train$y = as.factor(df.train$y)
  df.test$y = as.factor(df.test$y)
  glm.fit = glm(as.factor(y) ~ . - as.factor(y),data=df.train, family=binomial, maxit = 100)
  
  glm.probs = predict(glm.fit, df.test[, -1], type = "response")
  glm.pred = ifelse(glm.probs > .5, "2", "1")
  logistic.accuracy = mean(glm.pred==df.test[, 1])
  
  return(logistic.accuracy)
}

scenario.one = function(){
  n = 50
  df = data.generate(1, -1, 1, n, 2)
  
  test.index = train_Test_Split(df, 1)
  knn.accuracy = generate.knn(train.Y, test.X, train.X, test.Y)
  qda.accuracy = generate.qda(df.train, df.test)
  lda.accuracy = generate.lda(df.train, df.test)
  logistic.accuracy = generate.logistic(df.train, df.test)
  
  assign("knn.accuracy.one", knn.accuracy, envir = .GlobalEnv)
  assign("qda.accuracy.one", qda.accuracy, envir = .GlobalEnv)
  assign("lda.accuracy.one", lda.accuracy, envir = .GlobalEnv)
  assign("logistic.accuracy.one", logistic.accuracy, envir = .GlobalEnv)
}

scenario.two = function(){
  n = 500
  df = data.generate(1,-1, 1, n, 2)
  
  for (col in colnames(df)){
    if (col != "y")
    {
      df[,col] = scale(df[,col])
    }
  }
  
  test.index = train_Test_Split(df, 1)
  knn.accuracy = generate.knn(train.Y, test.X, train.X, test.Y)
  qda.accuracy = generate.qda(df.train, df.test)
  lda.accuracy = generate.lda(df.train, df.test)
  logistic.accuracy = generate.logistic(df.train, df.test)
  
  assign("knn.accuracy.two", knn.accuracy, envir = .GlobalEnv)
  assign("qda.accuracy.two", qda.accuracy, envir = .GlobalEnv)
  assign("lda.accuracy.two", lda.accuracy, envir = .GlobalEnv)
  assign("logistic.accuracy.two", logistic.accuracy, envir = .GlobalEnv)
}

scenario.three = function(){
  n = 50
  df = data.generate(1, -1, 1, n, 20)
  
  test.index = train_Test_Split(df, 1)
  knn.accuracy = generate.knn(train.Y, test.X, train.X, test.Y)
  qda.accuracy = generate.qda(df.train, df.test)
  lda.accuracy = generate.lda(df.train, df.test)
  logistic.accuracy = generate.logistic(df.train, df.test)
  
  assign("knn.accuracy.three", knn.accuracy, envir = .GlobalEnv)
  assign("qda.accuracy.three", qda.accuracy, envir = .GlobalEnv)
  assign("lda.accuracy.three", lda.accuracy, envir = .GlobalEnv)
  assign("logistic.accuracy.three", logistic.accuracy, envir = .GlobalEnv)
}

scenario.four = function(){
  n = 500
  df = data.generate(1,-1, 1, n, 20)
  
  for (col in colnames(df)){
    if (col != "y")
    {
      df[,col] = scale(df[,col])
    }
  }
  
  test.index = train_Test_Split(df, 1)
  knn.accuracy = generate.knn(train.Y, test.X, train.X, test.Y)
  qda.accuracy = generate.qda(df.train, df.test)
  lda.accuracy = generate.lda(df.train, df.test)
  logistic.accuracy = generate.logistic(df.train, df.test)
  
  assign("knn.accuracy.four", knn.accuracy, envir = .GlobalEnv)
  assign("qda.accuracy.four", qda.accuracy, envir = .GlobalEnv)
  assign("lda.accuracy.four", lda.accuracy, envir = .GlobalEnv)
  assign("logistic.accuracy.four", logistic.accuracy, envir = .GlobalEnv)
}


#Main, run from here.
scenario.one()
scenario.two()
scenario.three()
scenario.four()

#used to determine which of the four models had the highest accuracy for each scenario. Useful
#if we run our scenarios with different seeds. Feel free to edit/optimize.
determine.max.accuracy = function(){
  knnAccuracy = c(knn.accuracy.one, knn.accuracy.two, knn.accuracy.three, knn.accuracy.four)
  qdaAccuracy = c(qda.accuracy.one, qda.accuracy.two, qda.accuracy.three, qda.accuracy.four)
  ldaAccuracy = c(lda.accuracy.one, lda.accuracy.two, lda.accuracy.three, lda.accuracy.four)
  logAccuracy = c(logistic.accuracy.one, logistic.accuracy.two, logistic.accuracy.three, logistic.accuracy.four)
  accuracyFrame = data.frame=cbind(c(1:4), knnAccuracy, qdaAccuracy, ldaAccuracy, logAccuracy)
   for(j in 1:4){
    maxMethod <- c(j)
    for(i in 2:5){  
      if(max(accuracyFrame[j, -1])==accuracyFrame[j, i]){
        maxMethod <- append(maxMethod, colnames(accuracyFrame)[i])
      }
    }
    print(maxMethod)
  }
}
determine.max.accuracy()
