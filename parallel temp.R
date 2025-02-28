source('~/Desktop/Dcor/Pdcor1zfast.R')
source('~/Desktop/Dcor/Type1 Error/Distributions&Type1_Partial.R')

#fix the function and use mclapply for linux
P <- 1000
power_test <- function(P,n,distx,disty,nz=1){
  pperm <-numeric(P)
  pas <- numeric (P)
  dist_x <- match.fun(distx)
  dist_y <- match.fun(disty)
  for (i in 1:P){
  if (nz == 1) {
    z <- z1(n)
  } else {
    z <- cbind(z1(n), z2(n))
  }
  y <- numeric(n)
  x <- numeric(n)
  z_bar <- mean(z)
  id1 <- z <= z_bar
  id2 <- !id1
  size1 <- sum(id1)
  size2 <- n - size1
  z_1 <- z[id1]
  z_2 <- z[id2]^2
  x[id1] <- dist_x(size1) + z_1
  x[id2] <- dist_x(size2) + z_2
  y[id1] <- dist_y(size1) + z_1
  y[id2] <- dist_y(size2) + z_2
  
  pperm[i]   <-pdcor.test3(x, y, z, R = 500)
  stat_as <- n * pdcor2(x,y,z) + 1
  pas[i]    <- pchisq(stat_as, 1, lower.tail = FALSE)
  }
  c(mean(pperm<0.05),mean(pas<0.05))
}
microbenchmark::microbenchmark(power_test(1000,100,'beta','mix_norm'))
