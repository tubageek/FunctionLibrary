source("DataPreparation.R")

lm.summary <- function(dataset, target, ratio = 0.8, seed = 412, features) {
  formulaStart <- paste(target, " ~ ", sep = "")
  formulaPredictors <- paste(features, collapse = " + ")
  wholeFormula = paste(formulaStart, formulaPredictors, sep = "")
  
  split <- random.split(dataset, ratio, seed)
  train <- split$train
  test <- split$test
  
  lmData <- lm(wholeFormula, data = train)
  actual <- test[target][,]
  predicted <- predict(lmData, test)
  rsq <- 1-sum((actual - predicted) ^ 2) / sum((actual - mean(actual))^2)
  rsme <- (sum((actual - predicted) ^ 2) / length(actual)) ^ (1/2)
  
  list(lm = lmData, summary = summary(lmData), rSquared = rsq, rsme = rsme)
}

lm.fit <- function(target, predictors, dataset, ratio = 0.8, seed = 412) {
  #TODO: check for valid data in "ratio"
  trainingSize <- floor(ratio * nrow(dataset))
  set.seed(seed)
  trainingIndexes <- sample(seq_len(nrow(dataset)), size = trainingSize)
  train = dataset[trainingIndexes, ]
  test = dataset[-trainingIndexes, ]
  
  local_formula <- paste(target, "~", predictors)
  
  regression <- lm(local_formula, data = train)
  predicted <- predict.lm(regression, test)
  list(RSS = sum((test[target] - predicted)^2), lm = regression)
}

