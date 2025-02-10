library(dcov)
library(Rfast)
library(Rfast2)
library(parallel)

beta <- function(size) {
  a <- Rfast2::Runif(1, 1, 50)
  b <- Rfast2::Runif(1, 1, 50)
  x <- rbeta(size, a, b)
  return(x)
}

mix_norm <- function(size) {
  y <- c()
  p <- Rfast2::Runif(1)
  mu1 <- Rfast2::Runif(1, -50, 50)
  sigma1 <- Rfast2::Runif(1, 0, 50)
  mu2 <- Rfast2::Runif(1, -50, 50)
  sigma2 <- Rfast2::Runif(1, 0, 50)
  for (i in 1:size) {
    y[i] <- p * Rfast::Rnorm(1, mu1, sigma1) + (1 - p) * Rfast::Rnorm(1, mu2, sigma2)
  }
  return(y)
}

sim_error <- function(dummy, n) {
  x <- beta(n)
  y <- mix_norm(n)
  pperm <- dcor.test(x, y, R = 1000, type = 'U')$p.values
  stat_as <- n * dcov::dcor(x, y, type = "U")^2 + 1
  pas <- pchisq(stat_as, 1, lower.tail = FALSE)
  c(perm = as.numeric(pperm < 0.05),
    asym = as.numeric(pas < 0.05))
}

type1_error_parallel <- function(P, n) {
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, {
    library(dcov)
    library(Rfast)
    library(Rfast2)
  })
  clusterExport(cl, varlist = c("beta", "mix_norm", "sim_error"), envir = environment())
  res_list <- parLapply(cl, 1:P, function(i) sim_error(i, n))
  stopCluster(cl)
  res <- do.call(rbind, res_list)
  type1 <- c("Permutation" = mean(res[, "perm"]),
             "Asymptotic"  = mean(res[, "asym"]))
  return(type1)
}

result <- type1_error_parallel(P = 1000, n = 100)
microbenchmark(type1_error_parallel(P = 1000, n = 100),1,unit='seconds')
