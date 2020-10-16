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

plotter = function(){
  plot(variance)
}

generate_accuracy = function(){
  accuracies = c()
  knn.accuracy = c()
  qda.accuracy = c()
  lda.accuracy = c()
  logistic.accuracy = c()
  
  for (count in c(1:10)){
    test.index = train_Test_Split(df, count)
    knn.accuracy[count] = generate.knn(train.Y, test.X, train.X, test.Y)
    qda.accuracy[count] = generate.qda(df.train, df.test)
    lda.accuracy[count] = generate.lda(df.train, df.test)
    logistic.accuracy[count] = generate.logistic(df.train, df.test)
  }
  
  accuracies[1] = mean(knn.accuracy)
  accuracies[2] = mean(qda.accuracy)
  accuracies[3] = mean(lda.accuracy)
  accuracies[4] = mean(logistic.accuracy)
  
  return(accuracies)
}

scenario.one = function(df){
  
  accuracies = generate_accuracy()
  
  assign("knn.accuracy.one", accuracies[1], envir = .GlobalEnv)
  assign("qda.accuracy.one", accuracies[2], envir = .GlobalEnv)
  assign("lda.accuracy.one", accuracies[3], envir = .GlobalEnv)
  assign("logistic.accuracy.one", accuracies[4], envir = .GlobalEnv)
}

scenario.two = function(df){
  
  for (col in colnames(df)){
    if (col != "y")
    {
      df[,col] = scale(df[,col])
    }
  }
  
  accuracies = generate_accuracy()
  
  assign("knn.accuracy.two", accuracies[1], envir = .GlobalEnv)
  assign("qda.accuracy.two", accuracies[2], envir = .GlobalEnv)
  assign("lda.accuracy.two", accuracies[3], envir = .GlobalEnv)
  assign("logistic.accuracy.two", accuracies[4], envir = .GlobalEnv)
}

scenario.three = function(df){
  
  
  accuracies = generate_accuracy()
  
  assign("knn.accuracy.three", accuracies[1], envir = .GlobalEnv)
  assign("qda.accuracy.three", accuracies[2], envir = .GlobalEnv)
  assign("lda.accuracy.three", accuracies[3], envir = .GlobalEnv)
  assign("logistic.accuracy.three", accuracies[4], envir = .GlobalEnv)
}

scenario.four = function(df){
  
  for (col in colnames(df)){
    if (col != "y")
    {
      df[,col] = scale(df[,col])
    }
  }
  
  accuracies = generate_accuracy()
  
  assign("knn.accuracy.four", accuracies[1], envir = .GlobalEnv)
  assign("qda.accuracy.four", accuracies[2], envir = .GlobalEnv)
  assign("lda.accuracy.four", accuracies[3], envir = .GlobalEnv)
  assign("logistic.accuracy.four", accuracies[4], envir = .GlobalEnv)
}


#Main, run from here.
#Can just run these, not part of the assignment
sampleSize = 50
df = data.generate(1,-1, 1, sampleSize, 2)
scenario.one(df)

sampleSize = 500
df = data.generate(1,-1, 1, sampleSize, 2)
scenario.two(df)

sampleSize = 50
df = data.generate(1,-1, 1, sampleSize, 2)
scenario.three(df)

sampleSize = 500
df = data.generate(1,-1, 1, sampleSize, 2)
scenario.four(df)



#Graphs!----------------------------------------------------------------------------
#Here for different variances
variance.list = c(.5, 1, 1.5, 2, 2.5, 3)

#Scenario 1
accuracy.qda.list = c()
accuracy.lda.list = c()
accuracy.glm.list = c()
accuracy.knn.list = c()

counter = 1
for (variance in variance.list){
  print(variance)
  sampleSize = 500
  df = data.generate(1,-1, variance, sampleSize, 2)
  scenario.one(df)
  
  accuracy.qda.list[counter] = qda.accuracy.one
  accuracy.lda.list[counter] = lda.accuracy.one
  accuracy.glm.list[counter] = logistic.accuracy.one
  accuracy.knn.list[counter] = knn.accuracy.one
  counter = counter + 1
}
plot(variance.list, accuracy.qda.list,col = "blue", type = "o", ylab = "Accuracy", xlab = "Variance", main = "Variance vs Accuracy", ylim=c(0.55, 1.05))
lines(variance.list, accuracy.lda.list, col = "red", type = "o")
lines(variance.list, accuracy.glm.list, col = "yellow", type = "o")
lines(variance.list, accuracy.knn.list, col = "black", type = "o")

#Scenario 2
accuracy.qda.list = c()
accuracy.lda.list = c()
accuracy.glm.list = c()
accuracy.knn.list = c()

counter = 1
for (variance in variance.list){
  print(variance)
  sampleSize = 500
  df = data.generate(1,-1, variance, sampleSize, 2)
  scenario.two(df)
  
  accuracy.qda.list[counter] = qda.accuracy.two
  accuracy.lda.list[counter] = lda.accuracy.two
  accuracy.glm.list[counter] = logistic.accuracy.two
  accuracy.knn.list[counter] = knn.accuracy.two
  counter = counter + 1
}
plot(variance.list, accuracy.qda.list,col = "blue", type = "o", ylab = "Accuracy", xlab = "Variance", main = "Variance vs Accuracy", ylim=c(0.55, 1.05))
lines(variance.list, accuracy.lda.list, col = "red", type = "o")
lines(variance.list, accuracy.glm.list, col = "yellow", type = "o")
lines(variance.list, accuracy.knn.list, col = "black", type = "o")

#Scenario 3
accuracy.qda.list = c()
accuracy.lda.list = c()
accuracy.glm.list = c()
accuracy.knn.list = c()

counter = 1
for (variance in variance.list){
  sampleSize = 50
  df = data.generate(1, -1, variance, sampleSize, 20)
  scenario.three(df)
  
  accuracy.qda.list[counter] = qda.accuracy.three
  accuracy.lda.list[counter] = lda.accuracy.three
  accuracy.glm.list[counter] = logistic.accuracy.three
  accuracy.knn.list[counter] = knn.accuracy.three
  counter = counter + 1
}
plot(variance.list, accuracy.qda.list,col = "blue", type = "o", ylab = "Accuracy", xlab = "Variance", main = "Variance vs Accuracy", ylim=c(0.55, 1.05))
lines(variance.list, accuracy.lda.list, col = "red", type = "o")
lines(variance.list, accuracy.glm.list, col = "yellow", type = "o")
lines(variance.list, accuracy.knn.list, col = "black", type = "o")


#Scenario 4
accuracy.qda.list = c()
accuracy.lda.list = c()
accuracy.glm.list = c()
accuracy.knn.list = c()

counter = 1
for (variance in variance.list){
  sampleSize = 500
  df = data.generate(1, -1, variance, sampleSize, 20)
  scenario.four(df)
  accuracy.qda.list[counter] = qda.accuracy.four
  accuracy.lda.list[counter] = lda.accuracy.four
  accuracy.glm.list[counter] = logistic.accuracy.four
  accuracy.knn.list[counter] = knn.accuracy.four
  counter = counter + 1
}
plot(variance.list, accuracy.qda.list,col = "blue", type = "o", ylab = "Accuracy", xlab = "Variance", main = "Variance vs Accuracy", ylim=c(0.55, 1.05))
lines(variance.list, accuracy.lda.list, col = "red", type = "o")
lines(variance.list, accuracy.glm.list, col = "yellow", type = "o")
lines(variance.list, accuracy.knn.list, col = "black", type = "o")


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











