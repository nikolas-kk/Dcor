source("~/Desktop/R FIles/Dcor/Type1 Error/Dcor Type 1/Distributions&Type1_Partial.R")
library(parallel)
ncores<-detectCores()-1

type1_partial_pearson <- function (n, distx, disty, nz = 1) {
  dist_x <- match.fun(distx)
  dist_y <- match.fun(disty)
  x <- dist_x(n)
  y <- dist_y(n)
  z <- z1(n)
  p <- corrfuns::partialcor(cor(cbind(x, y, z)), 1, 2, 3, n)[2]
  as.numeric(p < 0.05)
}
xdist <- c('beta',"skew_normal",'Vonmises', "Gamma", 'cauchy')
ydist <- c('mix_norm', 'mix_norm3', 'mix_skew_t')
sizes <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000)

for (k in xdist) {
  results <- c()
  for (j in ydist) {
    for (n in sizes) {
      value <- mclapply(1:1000,function(i){
        type1_partial_pearson(n,k,j)
      },mc.cores=ncores)
      value <- Rfast::colmeans(do.call(rbind, value))
      title<-paste0("n",n,"_",j)
      results <-c(results,setNames(value,title))
    }
  }
  save(results,file=paste0("x~",k,".RData"))
}

