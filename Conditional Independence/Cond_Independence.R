library(parallel)
source("~/Desktop/R Files/Dcor/Type1 Error/Dcor Type 1/Distributions&Type1_Partial.R")

cond_ind <- function(dist_z, n) {
  z <- match.fun(dist_z)(n)
  x <- log(abs(z)) + z^2 + rnorm(n)
  y <- sin(z) + log10(abs(z)) + rnorm(n)
  pas <- corrfuns::partialcor(cor(cbind(x, y, z)), 1, 2, 3, n)[2]
  pperm <- pdcor::pdcor.test(x, y, z)[-1]
  
  c(as.numeric(pperm < 0.05), as.numeric(pas < 0.05))
}

zdist <- c("beta","skew_normal",'Vonmises', "Gamma", 'cauchy','mix_norm', 'mix_norm3', 'mix_skew_t')
sizes <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000)

for (dist in zdist) {
  results <- c()
  for (n in sizes) {
    value <-mclapply(1:1000, function(i){
      cond_ind(dist, n)
    }, mc.cores = 11)
    value <- Rfast::colmeans(do.call(rbind, value))
    title <- paste0("n", n, "_", dist)
    names(value) <- paste0(title, c("_perm", "_as","_pear"))
    results <- c(results, value)
  }
  save (results, file = paste0('z~',dist,".RData"))
}

cond_ind('mix_skew_t', 100)
