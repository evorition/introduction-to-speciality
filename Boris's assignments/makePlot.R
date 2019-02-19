makePlot <- function(xSample, ySample, nameOfFile){
  path <- file.path("/home", "/maxim/", "experiments_with_R", 
                    paste(nameOfFile, sep=""))
  png(path, width=700, height=700, units="px")
  plot(xSample, type="l", col="red", main="xSample ~ ySample",
       ylim = c(0, 20), ylab="value")
  lines(ySample, col="green")
  legend('topleft', c("xSample", "ySample"), 
         col=c("red", "green"), lty=1, cex=1.5)
  dev.off()
}