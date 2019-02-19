plotXBarAgainstExpectationExpansion <- function(n=100, mean=5, sd=1){
  sample1 <- rnorm(n, mean, sd)
  xBar <- c()
  temp <- c()
  
  for (value in sample1){
    temp <- c(temp, value)
    meanx <- sum(temp)/length(temp)
    xBar <- c(xBar, meanx)
  }
  
  path <- file.path("/home", "/maxim/", "experiments_with_R", 
                    paste("Expansion.png", sep=""))
  png(path, width=700, height=700, units="px")
  par(lwd=2)
  plot(xBar, type="l", col="red", main="Expansion", ylim = c(0, 10))
  axis(1, at=seq(0, n, by=n/10))
  abline(h=5, col="blue")
  legend('topleft', c("X bar", "Actual"), 
         col=c("red", "blue"), lty=1, cex=2)
  dev.off()
}