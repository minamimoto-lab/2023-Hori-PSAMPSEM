map <- function(z){
  density(z)$x[which.max(density(z)$y)]
}

quntile95 <- function(x){
  quantile(x, 
           probs = c(0.025, 0.975), 
           names = TRUE)
}

ess_tail95 <- function(x){
  posterior::ess_quantile(x, 
                          probs = c(0.025, 0.975),
                          names = TRUE) %>% 
    min()
}

r_hat <- posterior::rhat



pd_out <- c("mean", "sd", "map", "quntile95", "ess_tail95", "r_hat")
