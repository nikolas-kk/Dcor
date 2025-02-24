source('C:/Users/Nikolas/Desktop/Arxeia_R/Dcor/Type1 Error/Distributions&Type1_Partial.R')
source('C:/Users/Nikolas/Desktop/Arxeia_R/Dcor/Pdcor1zfast.R')
library(parallel)
library(doParallel)
library(foreach)
library(dcov)
library(Rfast2)
library(LaplacesDemon)
library(mixsmsn)

ncores <- detectCores() - 8
cl <- makeCluster(ncores)
registerDoParallel(cl)


type1_error_parallel <- function(P, n, distx, disty, nz) {
  dist_x <- match.fun(distx)
  dist_y <- match.fun(disty)
  
  results <- foreach(i = 1:P, .combine = rbind, .packages = c("dcov", "Rfast2", "LaplacesDemon", "mixsmsn"), 
                     .export = c("z1", "z2","pdcor.test3",'pdcor2')) %dopar% {
                       if (nz == 1) {
                         z <- z1(n)
                       } else {
                         z <- cbind(z1(n), z2(n))
                       }
                       
                       x <- dist_x(n)
                       y <- dist_y(n)
                       
                       pperm   <-pdcor.test2(x, y, z, R = 500)$
                       stat_as <- n * pdcor2(x,y,z) + 1
                       pas     <- pchisq(stat_as, 1, lower.tail = FALSE)
                       gc()
                       
                       c(Permutation = as.numeric(pperm < 0.05), Asymptotic = as.numeric(pas < 0.05))
                     }
  
  colMeans(results)
}

n_values <- c(10000)
distx <- "beta"
distributions <- c("mix_norm", "mix_norm3", "mix_skew_t")

results <- list()

for (disty in distributions) {
  for (n in n_values) {
    key <- paste0("n", n, "_", disty)
    results[[key]] <- type1_error_parallel(1000, n, distx, disty, nz = 1)
  }
}

stopCluster(cl)

save(results, file = "x~Beta_Partial_1z_parallel.RData")
