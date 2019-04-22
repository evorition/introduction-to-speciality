confidence_interval <- function(n = 10,
                                mean = 0,
                                sd = sqrt(1/3),
                                t = 1.96){
  X_norm <- rnorm(n = n, mean = mean, sd = sd)
  
  upper_bound_norm <- c()
  lower_bound_norm <- c()
  
  X_unif <- runif(n = n, min = -1, max = 1)
  
  upper_bound_unif <- c()
  lower_bound_unif <- c()
  
  for(i in seq(n)){
    # for normal distribuition
    X_subset_norm <- X_norm[1:i]
    a_hat_norm <- mean(X_subset_norm)
    sd_hat_norm <- sd(X_subset_norm)
    delta_norm <- t * sd_hat_norm / sqrt(i)
    upper <- a_hat_norm + delta_norm
    lower <- a_hat_norm - delta_norm
    upper_bound_norm <- c(upper_bound_norm, upper)
    lower_bound_norm <- c(lower_bound_norm, lower)
    
    # for uniform distribuition
    X_subset_unif <- X_unif[1:i]
    a_hat_unif <- mean(X_subset_unif)
    sd_hat_unif <- sd(X_subset_unif)
    delta_unif <- t * sd_hat_unif / sqrt(i)
    upper_unif <- a_hat_unif + delta_unif
    lower_unif <- a_hat_unif - delta_unif
    upper_bound_unif <- c(upper_bound_unif, upper_unif)
    lower_bound_unif <- c(lower_bound_unif, lower_unif)
    
  }
  
  svg("confidence_interval.svg")
  max_val <- max(max(na.omit(upper_bound_norm)), max(na.omit(lower_bound_norm)), 
                 max(na.omit(upper_bound_unif)), max(na.omit(lower_bound_unif)))
  min_val <- min(min(na.omit(upper_bound_norm)), min(na.omit(lower_bound_norm)), 
                 min(na.omit(upper_bound_unif)), min(na.omit(lower_bound_unif)))
  
  plot(upper_bound_norm, type = "l", col = "blue", ylim = c(min_val, max_val),
       ylab = "confidence interval for a")
  lines(lower_bound_norm, col = "blue")
  lines(upper_bound_unif, col = "red")
  lines(lower_bound_unif, col = "red")
  abline(h = 0, lwd = 2)
  legend("topright", legend = c("normal distribution", "uniform distribution"), 
         col = c("blue", "red"), lty = 1:1, bg="transparent")
  
  dev.off()
}