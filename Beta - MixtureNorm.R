library(dcov)
library(Rfast)
library(Rfast2)
library(microbenchmark)
#----Συναρτήσεις--------------------
beta <- function(size) {
  a <- Rfast2::Runif(1, 1, 50)
  b <- Rfast2::Runif(1, 1, 50)
  x <- rbeta(size, a, b)
  return(x)
}

mix_norm <- function(size) {
  p <- Rfast2::Runif(1)
  q <- 1 - p
  mu1 <- Rfast2::Runif(1, -50, 50)
  v1 <- Rfast2::Runif(1, 0, 50)
  mu2 <- Rfast2::Runif(1, -50, 50)
  v2 <- Rfast2::Runif(1, 0, 50)
  v1 <- Rfast::Rnorm(size, mu1, v1)
  v2 <- Rfast::Rnorm(size, mu2, v2)
  y <- p * v1 + q * v2 # sigma1 =v1 and sigma2=v2
}


type1_error <- function(P, n) {
  count_perm<-0
  count_as<-0
  for (i in 1:P) {
    x <- beta(n)
    y <- mix_norm(n)
    pperm <- dcov::dcor.test(x, y, R = 500, type = 'U')$p.values
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
#--------------------------------------------------------------------------
n50<-type1_error(1000,50)
n100<-type1_error(1000,100)
n200<-type1_error(1000,200)
n500<-type1_error(1000,500)
n1000<-type1_error(1000,1000)
n2000<-type1_error(1000,2000)
n5000<-type1_error(1000,5000)
n10000<-type1_error(1000,10000)
