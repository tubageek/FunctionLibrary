lr_simple <- function(target, input) {
  sumproduct <- sum(target * input)
  numerator <- sumproduct / length(target) - mean(target) * mean(input)
  denominator <- mean(input^2) - mean(input)^2
  slope <- numerator / denominator
  intercept <- mean(target) - slope * mean(input)
  return (list(slope = slope, intercept = intercept))
}

lr_simple.predict <- function(target, lr_simple.object) {
  target * lr_simple.object$slope + lr_simple.object$intercept
}

lr_simple.predictinv <- function(target, lr_simple.object) {
  (target - lr_simple.object$intercept) / lr_simple.object$slope
}

lr_simple.RSS <- function(target, input, lr_simple.object) {
  predicted <- input * lr_simple.object$slope + lr_simple.object$intercept
  sum((predicted - target)^2)
}