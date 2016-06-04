source("DataPreparation.R")

#INCOMPLETE - NEEDS UPDATING
text.analysis <- function(dataset, textField, sentiment, ratio = 0.8, seed = 412) {
  library(RTextTools)
  library(e1071)
  
  splitter <- random.split(dataset, seed = 0)
  train <- splitter$train
  test <- splitter$test
  sortedData <- rbind(train, test)
  
  matrix <- create_matrix(sortedData$review, language="english", removeStopwords=FALSE, removeNumbers=TRUE, stemWords=FALSE)
  container <- create_container(matrix, as.numeric(as.factor(sortedData$sentiment)), 
                               trainSize=1:238, testSize=239:298,virgin=FALSE)
  
  models <- train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))
  results <- classify_models(container, models)
  create_analytics(container, results)
}