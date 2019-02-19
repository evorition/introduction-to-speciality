VarianceOfSumDependent <- function(n=10, mean=5, sd=2){
  xSample <- rnorm(n, mean, sd)
  variance <- var(xSample)
  makeHistogram(obj=xSample, nameOfFile="VarianceOfSumDependent_1.png", 
                title=paste("xSample <- rnorm(n =", n, "mean =", mean,
                            "sd =", sd, ") and variance", 
                            variance, sep=" "))
  
  ySample <- 2 * xSample
  variance <- var(ySample)
  makeHistogram(obj=ySample, nameOfFile="VarianceOfSumDependent_2.png", 
                title=paste("ySample <- 2 * xSample and variance", 
                            variance, sep=" "))
  
  makePlot(xSample=xSample, ySample=ySample, 
           nameOfFile="VarianceOfSumDependent_3.png")
  
  zSample <- xSample + ySample
  variance <- var(ySample)
  makeHistogram(obj=xSample, nameOfFile="VarianceOfSumDependent_4.png", 
                title=paste("zSample <- zSample <- xSample + ySample)
                            and variance", 
                            variance, sep=" "))
}