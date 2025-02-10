library(dcov)
library(Rfast)
library(Rfast2)
library(microbenchmark)

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
type1_error <- function(P, n) {
  count_perm <- 0
  count_as <- 0
  for (i in 1:P) {
    x <- beta(n)
    y <- mix_norm(n)
    pperm <- dcor.test(x, y, R = 1000, type = 'U')$p.values
    stat_as <- n * (dcov::dcor(x, y, type = "U")^2) + 1
    pas <- pchisq(stat_as, 1, lower.tail = FALSE)
    if (pperm < 0.05) {
      count_perm <- count_perm + 1
    }
    if (pas < 0.05) {
      count_as <- count_as + 1
    }
  }
  type1 <- c("Permutation" = count_perm / P, "Asymptotic" = count_as / P)
  return(type1)
}
type1_error(1000,500)
