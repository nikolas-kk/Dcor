#Required packages: library(Rfast)
#                   library(Rfast2)
#                   library(MCMCpack)
#                   library(sn)
#                   library(dcov)
#                   library(LaplacesDemon)
########################### For Generating X ####################################

beta <- function(size) {
  #Beta Distribution
  a <- Rfast2::Runif(1, 1, 50)
  b <- Rfast2::Runif(1, 1, 50)
  x <- rbeta(size, a, b)
}

skew_normal <- function(size) {
  #Skew_normal Distribution
  loc <- Rfast2::Runif(1, -50, 50)
  scale <- Rfast2::Runif(1, 1, 50)
  a <- Rfast2::Runif(1, -20, 20)
  x <- sn::rsn(size,
               xi = loc,
               omega = scale,
               alpha = a)
}


Vonmises <- function(size) {
  m <- Rfast2::Runif(1, 0, 360)
  k <- Rfast2::Runif(1, 1, 10)
  x <- Rfast::rvonmises(size, m, k, rads = FALSE) #Check also rvmf
}

Gamma <- function(size) {
  #the shape parameter as approaches to zero becomes heavily right skewed and it explodes
  #so im setting the limit to 0.1
  shape <- Rfast2::Runif(1, 0.1, 10)
  scale <- Rfast2::Runif(1, 1, 10)
  x <- rgamma(size, shape, scale)
}

cauchy <- function(size) {
  loc <- Rfast2::Runif(1, -50, 50)
  scale <- Rfast2::Runif(1, 1, 50)
  x <- rcauchy(size, loc, scale)
}
############################## For Generating Y ################################

mix_norm <- function(size) {
  #Mixture of 2 normals
  p1 <- Rfast2::Runif(1)
  p<-c(p1,1-p1)
  mu1 <- Rfast2::Runif(1, -50, 50)
  v1 <- Rfast2::Runif(1, 0, 50)
  mu2 <- Rfast2::Runif(1, -50, 50)
  v2 <- Rfast2::Runif(1, 0, 50)
  m<-c(mu1,mu2)
  s<-c(v1,v2)
  y <- LaplacesDemon::rnormm(size,p,m,s)
}
mix_norm3 <- function(size) {
  #Mixture of 3 normals
  mu1 <- Rfast2::Runif(1, -50, 50)
  v1 <- Rfast2::Runif(1, 0, 50)
  mu2 <- Rfast2::Runif(1, -50, 50)
  v2 <- Rfast2::Runif(1, 0, 50)
  mu3 <- Rfast2::Runif(1, -50, 50)
  v3 <- Rfast2::Runif(1, 0, 50)
  v1 <- Rfast::Rnorm(size, mu1, v1)
  v2 <- Rfast::Rnorm(size, mu2, v2)
  v3 <- Rfast::Rnorm(size, mu3, v3)
  #rdirichlet generates 3 weights that sum to 1
  y <- tcrossprod(matrix(c(v1, v2, v3), size), MCMCpack::rdirichlet(1, c(1, 1, 1)))
}

mix_skew_t <- function(size) {
  p <- Rfast2::Runif(1)
  q <- 1 - p
  loc1 <- Rfast2::Runif(1, -50, 50)
  scale1 <- Rfast2::Runif(1, 1, 50)
  a1 <- Rfast2::Runif(1, -20, 20)
  loc2 <- Rfast2::Runif(1, -50, 50)
  scale2 <- Rfast2::Runif(1, 1, 50)
  a2 <- Rfast2::Runif(1, -20, 20)
  v1 <- sn::rst(
    size,
    xi = loc1,
    omega = scale1,
    alpha = a1,
    nu = 1
  )#DF =1
  v2 <- sn::rst(
    size,
    xi = loc2,
    omega = scale2,
    alpha = a2,
    nu = 1
  ) #DF = 1
  y <- p * v1 + q * v2
}
########################### Type 1 Error Function ####################################
type1_error <- function(P, n, distx, disty) {
  count_perm <- 0
  count_as <- 0
  for (i in 1:P) {
    x <- match.fun(distx)(n)
    y <- match.fun(disty)(n)
    pperm <- dcov::dcor.test(x, y, R = 500, type = 'U')$p.values
    stat_as <- n * dcov::dcor(x, y, type = "U") + 1
    pas <- pchisq(stat_as, 1, lower.tail = FALSE)
    if (pperm < 0.05) {
      count_perm <- count_perm + 1
    }
    if (pas < 0.05) {
      count_as <- count_as + 1
    }
  }
  type1 <- c("Permutation" = count_perm / P, "Asymptotic" = count_as / P)
}
