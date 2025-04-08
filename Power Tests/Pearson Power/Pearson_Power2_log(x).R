source("Type1 Error/Dcor Type 1/Distributions&Type1_Partial.R")
library(parallel)

ncores <- detectCores() - 1

power_test_pearson <- function(distx, disty, n) {
  dist_x <- match.fun(distx)
  dist_y <- match.fun(disty)
  x <- dist_x(n)
  z <- z1(n)
  y <- log(x) +z
  p <- Rfast::permcor(x, y)[2]
  as.numeric(p < 0.05)
}

for (n in seq(50, 500, by = 50)) {
  power_test_results <- mclapply(1:1000, function(i) {
    power_test_pearson('beta', 'mix_norm3', n)
  }, mc.cores = ncores)
  power_test_results <- Rfast::colmeans(do.call(rbind, power_test_results))
  save(power_test_results, file = paste0("Pearson_Power2_", n, ".RData"))
}