
generate.logistic = function(df.train, df.test){
 glm.fit= glm(y ~ . - y,data=df.train, family=binomial)
 
 glm.pred = predict(glm.fit, df.test[, -1])
 logistic.accuracy = mean(glm.pred$class==df.test[, 1])
 
 return(glm.accuracy)
   
}

scenario.three = function(){
  n = 50
  df = data.generate(1, -1, 1, n, 20)
  
  test.index = train_Test_Split(df, 1)
  qda.accuracy = generate.qda(df.train, df.test)
  lda.accuracy = generate.lda(df.train, df.test)
  logistic.accuracy = generate.logistic(df.train, df.test)
  
  assign("qda.accuracy.three", qda.accuracy, envir = .GlobalEnv)
  assign("lda.accuracy.three", lda.accuracy, envir = .GlobalEnv)
  assign("logistic.accuracy.three", logistic.accuracy, envir = .GlobalEnv)
}