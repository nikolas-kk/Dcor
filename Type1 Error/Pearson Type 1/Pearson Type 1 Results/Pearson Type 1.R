source("~/Desktop/R FIles/Dcor/Type1 Error/Dcor Type 1/Distributions&Type1_Partial.R")

type1_pearson <- function(P, n, distx, disty) {
  dist_x <- match.fun(distx)
  dist_y <- match.fun(disty)
  x <- replicate(P, dist_x(n))
  y <- replicate(P, dist_y(n))
  p <- Rfast::corpairs(x, y, rho = c(0, 0))[, 3]
  sum(p < 0.05) / P
}

xdist<-c('beta','skew_normal','Vonmises','Gamma','cauchy')
ydist <- c('mix_norm','mix_norm3','mix_skew_t')
size <- c(50,100,200,500,1000,2000,5000,10000)

for (i in xdist){
  results<-c()
  for (j in ydist){
    for (n in size){
      title<-paste0("n",n,"_",j)
      value <- type1_pearson(1000, n, i, j)
      results <-c(results,setNames(value,title))
    }
    
  }
  save(results,file=paste0("x~",i,".RData"))
}


