source("~/Desktop/R FIles/Dcor/Type1 Error/Dcor Type 1/Distributions&Type1_Partial.R")
library(parallel)

ncores <- detectCores() - 1

power_partial_pearson <- function(n, distx, disty,nz=1) {
  dist_x <- match.fun(distx)
  dist_y <- match.fun(disty)
  
  if (nz == 1) {
    z <- z1(n)
  } else {
    z <- cbind(z1(n), z2(n))
  }
  y <- numeric(n)
  x <- numeric(n)
  z_bar <- mean(z)
  id <- z <= z_bar
  size1 <- sum(id)
  size2 <- n - size1
  z_1 <- z[id]
  z_2 <- z[!id]^2
  x[id] <- dist_x(size1) + z_1
  x[!id] <- dist_x(size2) + z_2
  y[id] <- dist_y(size1) + z_1
  y[!id] <- dist_y(size2) + z_2
  p <- corrfuns::partialcor(cor(cbind(x, y, z)), 1, 2, 3, n)[2]
  as.numeric(p < 0.05)
}

for (n in seq(50, 500, by = 50)) {
  results <- mclapply(1:1000, function(i) {
    power_partial_pearson(n, 'beta', 'mix_norm3',nz=1)
  }, mc.cores = ncores - 1)
  results <- Rfast::colmeans(do.call(rbind, results))
  save(results,file=paste0("PartialPearson_Power",n,".RData"))
}


