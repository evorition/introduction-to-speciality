CompareTwoHistograms <- function(n=10, mean=5, sd=6){
  xSample <- rnorm(n, mean, sd)
  variance <- var(xSample)
  makeHistogram(obj=xSample, nameOfFile="CompareTwoHistograms_1.png", 
                title=paste("xSample <- rnorm(n =", n, "mean =", mean,
                            "sd =", sd, ") and variance", 
                            variance, sep=" "))
  
  xBarArray <- c()
  for (value in seq(n)){
    xSample <- rnorm(n, mean, sd)
    xBar <- mean(xSample)
    xBarArray <- c(xBarArray, xBar)
  }
  
  variance <- var(xBarArray)
  makeHistogram(obj=xBarArray, nameOfFile="CompareTwoHistograms_2.png", 
                title=paste("xBarArray and variance", 
                            variance, sep=" "))
}