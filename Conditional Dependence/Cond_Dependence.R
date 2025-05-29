library(parallel)
source("~/Desktop/R Files/Dcor/Type1 Error/Dcor Type 1/Distributions&Type1_Partial.R")
cond_dep <- function(dist_x, dist_y, n) {
  distx <- match.fun(dist_x)
  disty <- match.fun(dist_y)
  x <- distx(n)
  y <- disty(n)
  z <- log(abs(x)) + sin(y) + rnorm(n)
  
  pas <- corrfuns::partialcor(cor(cbind(x, y, z)), 1, 2, 3, n)[2]
  pperm <- pdcor::pdcor.test(x, y, z)[-1]
  
  c(as.numeric(pperm < 0.05), as.numeric(pas < 0.05))
}

xdist <- c("beta", "skew_normal", 'Vonmises', "Gamma", 'cauchy')
ydist <- c('mix_norm', 'mix_norm3', 'mix_skew_t')
sizes <- c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500)


for (k in xdist) {
  results <- c()
  for (j in ydist) {
    for (n in sizes) {
      value <- mclapply(1:1000, function(i) {
        cond_dep(k, j, n)
      }, mc.cores = 11)
      value <- Rfast::colmeans(do.call(rbind, value))
      title <- paste0("n", n, "_", j)
      names(value) <- paste0(title, c("_perm", "_as", "_pear"))
      results <- c(results, value)
    }
  }
  save(results, file = paste0("x~", k, ".RData"))
}
