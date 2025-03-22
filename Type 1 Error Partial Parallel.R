library(parallel)
source("~/Desktop/R Files/Dcor/Type1 Error/Dcor Type 1/Distributions&Type1_Partial.R")
type1_partial <- function(dist_x, dist_y, n) {
  distx <- match.fun(dist_x)
  disty <- match.fun(dist_y)
  z <- z1(n)
  x <- distx(n) + z
  y <- disty(n) + z
  
  pas <- corrfuns::partialcor(cor(cbind(x, y, z)), 1, 2, 3, n)[2]
  pperm <- pdcor::pdcor.test(x, y, z)[-1]
  
  c(as.numeric(pperm<0.05),as.numeric(pas<0.05))
}

xdist <- c('Vonmises', "Gamma", 'cauchy')
ydist <- c('mix_norm', 'mix_norm3', 'mix_skew_t')
sizes <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000)


for (k in xdist) {
  results <- c()
  for (j in ydist) {
    for (n in sizes) {
      value <- mclapply(1:1000,function(i){
        type1_partial(k,j,n)
      },mc.cores=9)
      value <- Rfast::colmeans(do.call(rbind, value))
      title<-paste0("n",n,"_",j)
      names(value) <- paste0(title, c("_perm", "_as","_pear"))
      results <- c(results, value)
    }
  }
  save(results,file=paste0("x~",k,".RData"))
}
