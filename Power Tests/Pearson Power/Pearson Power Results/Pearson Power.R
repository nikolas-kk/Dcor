source("~/Desktop/R FIles/Dcor/Type1 Error/Distributions&Type1_Partial.R")
library(parallel)

ncores <- detectCores() - 1

power_test_pearson <- function(distx, disty, n) {
  dist_x <- match.fun(distx)
  dist_y <- match.fun(disty)
  x <- dist_x(n)
  y <- numeric(n)
  id <- x <= mean(x)
  size1 <- sum(id)
  x1 <- scale(x[id]^2)
  x2 <- scale(sqrt(exp(x[!id])))
  y[id] <- x1
  y[!id] <- x2
  p <- Rfast::permcor(x, y)[2]
  as.numeric(p < 0.05)
}

for (n in seq(50, 500, by = 50)) {
  power_test_results <- mclapply(1:1000, function(i) {
    power_test_pearson('beta', 'mix_norm3', n)
  }, mc.cores = ncores)
  power_test_results <- Rfast::colmeans(do.call(rbind, power_test_results))
  save(power_test_results, file = paste0("Pearson_Power", n, ".RData"))
}

