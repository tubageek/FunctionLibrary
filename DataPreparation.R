random.split <- function (dataset, ratio = 0.8, seed = 412) {
  #TODO: check for valid data in "ratio"
  trainingSize <- floor(ratio * nrow(dataset))
  set.seed(seed)
  trainingIndexes <- sample(seq_len(nrow(dataset)), size = trainingSize)
  list(train = dataset[trainingIndexes, ], test = dataset[-trainingIndexes, ])
}