computeLRVKernelHat <- function(sample, kernelArray, batchSize){
  size <- length(sample)
  
  LRVArray <- sum(sample*kernelArray)/(batchSize*(size-batchSize+1))
  
  return(LRVArray)
}