#Required packages: Rfast
#                   Rfast2
#                   MCMCpack
#                   sn


########################### for Generating X ####################################

beta <- function(size) {
  #Beta Distribution
  a <- Rfast2::Runif(1, 1, 50)
  b <- Rfast2::Runif(1, 1, 50)
  x <- rbeta(size, a, b)
  return(x)
}

skew_normal <- function(size) {
  #Skew_normal Distribution
  loc <- Rfast2::Runif(1, -50, 50)
  scale <- Rfast2::Runif(1, 1, 50)
  a <- Rfast2::Runif(1,-20, 20)
  x <- sn::rsn(size, xi = loc, omega = scale, alpha = a)
  return(x)
}

cauchy <- function(size) {
  loc <- Rfast2::Runif(1, -50, 50)
  scale <- Rfast2::Runif(1, 1, 50)
  x <- rcauchy(size, loc, scale)
  return(x)
}
############################## For Generating Y ################################

mix_norm <- function(size) {
  #Mixture of 2 normals
  p <- Rfast2::Runif(1)
  q <- 1 - p
  mu1 <- Rfast2::Runif(1, -50, 50)
  v1 <- Rfast2::Runif(1, 0, 50)
  mu2 <- Rfast2::Runif(1, -50, 50)
  v2 <- Rfast2::Runif(1, 0, 50)
  v1 <- Rfast::Rnorm(size, mu1, v1)
  v2 <- Rfast::Rnorm(size, mu2, v2)
  y <- p * v1 + q * v2 # sigma1 =v1 and sigma2=v2
  return(y)
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
  return(y)
}
