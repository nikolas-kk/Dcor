source('~/Desktop/Dcor/Pdcor1zfast.R')
source('~/Desktop/Dcor/Type1 Error/Distributions&Type1_Partial.R')
library(parallel)

#fix the function and use mclapply for linux
P <- 1000
power_test <- function(n,distx,disty,nz=1){
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
  
  pperm   <-pdcor.test3(x, y, z, R = 500)
  stat_as <- n * pdcor2(x,y,z) + 1
  pas    <- pchisq(stat_as, 1, lower.tail = FALSE)
  c(Permutation = as.numeric(pperm < 0.05), Asymptotic = as.numeric(pas < 0.05))
}
results <- mclapply(1:P, function(i) {
  power_test(n, distx, disty, nz)
}, mc.cores = detectCores() - 7)
results_matrix <-do.call(rbind,results)
results_matrix
colMeans(results_matrix)
