source("C:/Users/Nikolas/Desktop/Αρχεία R/Dcor/Type1 Error/Distributions&Type1 Partial.R")
library(parallel)
library(doParallel)
library(foreach)
library(dcov)
library(Rfast2)
library(LaplacesDemon)
library(mixsmsn)

ncores <- detectCores() - 6
cl <- makeCluster(ncores)
registerDoParallel(cl)


type1_error_parallel <- function(P, n, distx, disty, nz) {
  dist_x <- match.fun(distx)
  dist_y <- match.fun(disty)
  
  results <- foreach(i = 1:P, .combine = rbind, .packages = c("dcov", "Rfast2", "LaplacesDemon", "mixsmsn"), 
                     .export = c("z1", "z2")) %dopar% {
                       if (nz == 1) {
                         z <- z1(n)
                       } else {
                         z <- cbind(z1(n), z2(n))
                       }
                       
                       x <- dist_x(n)
                       y <- dist_y(n)
                       
                       pperm   <- dcov::pdcor.test(x, y, z, R = 500, type = 'U')$p.values
                       stat_as <- n * dcov::pdcor(x, y, z, type = "U") + 1
                       pas     <- pchisq(stat_as, 1, lower.tail = FALSE)
                       
                       c(Permutation = as.numeric(pperm < 0.05), Asymptotic = as.numeric(pas < 0.05))
                     }
  
  colMeans(results)
}

n_values <- c(50, 100, 200, 500,1000,2000,5000,10000)
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
