source('~/Desktop/Dcor/Pdcor1zfast.R')
source('~/Desktop/Dcor/Type1 Error/Distributions&Type1_Partial.R')
library(parallel)

#fix the function and use mclapply for linux
P <- 1000
type1error_testpartial <- function(n,distx,disty,nz=1){
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
  
  pperm   <-pdcor.test3(x,y,z)
  c(Permutation = as.numeric(pperm < 0.05))
}
microbenchmark::microbenchmark({results <- mclapply(1:1000, function(i) {
  type1error_testpartial(1000, 'cauchy','mix_skew_t', nz)
}, mc.cores = detectCores() - 1)},times=10)
results_matrix <-do.call(rbind,results)
colMeans(results_matrix)
pperm<-numeric(n)
pas<-numeric(n)


p_values <- numeric(1000)
for (i in 1:1000) {
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
  p_values[i] <- dcov::pdcor.test(x,y,z,type='U')$p.values
}
sum(p_values < 0.05) / 1000
Rfast::permcor(x,y)$p-value
