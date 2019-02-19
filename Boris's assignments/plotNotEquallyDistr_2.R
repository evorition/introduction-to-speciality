plotNotEquallyDistr_2 <- function(n=100){
  xBar <- c()
  temp <- c()
  
  for (value in seq(n)){
    set.seed(value)
    sample1 <- rnorm(n=1, mean=0, sd=sqrt(value))
    temp <- c(temp, sample1)
    meanx <- sum(temp)/length(temp)
    xBar <- c(xBar, meanx)
  }
  
  path <- file.path("/home", "/maxim/", "experiments_with_R", 
                    paste("not_equally_distr_2.png", sep=""))
  png(path, width=700, height=700, units="px")
  par(lwd=2)
  plot(xBar, type="l", col="red", main="Not Equally Disturbed (b)",
       ylim = c(-10, 10))
  axis(1, at=seq(0, n, by=n/10))
  abline(h=0, col="blue")
  legend('topleft', c("X bar", "Actual"), 
         col=c("red", "blue"), lty=1, cex=2)
  dev.off()
}