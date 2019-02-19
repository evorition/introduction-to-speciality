computeSquaredBatchSum <- function(sample, batchSize){

  size <- length(sample)
  squaredBatchSum <- array(data = 0, dim = size-batchSize+1)

  for(i in 1:(size-batchSize+1)){
    squaredBatchSum[i] <- (sum(sample[i:(i+batchSize-1)]))^2
  }

  return(squaredBatchSum)
}
