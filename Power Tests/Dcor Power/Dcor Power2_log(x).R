source("Type1 Error/Dcor Type 1/Distributions&Type1_Partial.R")
library(parallel)

ncores <- detectCores() - 1

power_test <- function(distx, disty, n) {
  dist_x <- match.fun(distx)
  dist_y <- match.fun(disty)
  z <- z1(n)
  x <- dist_x(n)
  y <- log(x) + z
  pperm <- dcov::dcor.test(x, y, R = 500, type = 'U')$p.values
  stat <- n * dcov::dcor(x, y, type = 'U') + 1
  pas <- pchisq(stat, 1, lower.tail = FALSE)
  c(Permutation = as.numeric(pperm < 0.05),
    Asymptotic = as.numeric(pas < 0.05))
}

for (n in seq(50, 500, by = 50)) {
  power_test_results <- mclapply(1:1000, function(i) {
    power_test('beta', 'mix_norm3', n)
  }, mc.cores = ncores)
  power_test_results <- Rfast::colmeans(do.call(rbind, power_test_results))
  save(power_test_results, file = paste0("Dcor_Power2_", n, ".RData"))
}