confidence_interval_replication_var <- function(n = 100,
                                                mean = 0,
                                                var = 100,
                                                r = 1000,
                                                gamma = 0.95){
  
  chi <- qchisq(p = gamma, df = n - 1, lower.tail = F)
  
  failed_intervals <- 0
  
  upper_bound <- c()
    
  for(counter in seq(r)){
    X <- rnorm(n = n, mean = mean, sd = sqrt(var))
    sd_hat <- sd(X)
    upper <- sd_hat * (1 + sqrt(n-1/chi) - 1)
    
    if(upper < var){
      failed_intervals <- failed_intervals + 1
    }
    
    upper_bound <- c(upper_bound, upper)
  }
  
  cat("Failed", failed_intervals, "\n")
}