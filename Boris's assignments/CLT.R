main <- function(r=10000, n=200){
  samples <- runif(r * n)
  means <- calculateMeans(samples, r, n)
  plotTwoGraphics(samples, means)
}

calculateMeans <- function(samples, r, n){
  means <- rowMeans(matrix(samples, nrow=r, ncol=n))
  return(means)
}

plotTwoGraphics <- function(samples, means){
  path <- file.path(".", paste("CLT_hist.png", sep=""))
  png(path, width=1000, height=1000, units="px")
  par(mfrow=c(1, 2))
  hist(samples, col="red", xlab="Sample of Uniform distribution")
  hist(means, col="blue", xlab="Sample Mean")
  dev.off()
}