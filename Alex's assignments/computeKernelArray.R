computeKernelArray <- function(sample, tPar, bandWidth){
  size <- length(sample)
  kernelArray <- array(data = 0, dim = size)

  for(i in 1:length(kernelArray)){
    kernelArray[i] <- dnorm((i/size - tPar)/bandWidth)/bandWidth
  }

  return(kernelArray)
}
