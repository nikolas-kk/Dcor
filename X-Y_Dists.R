
###These are the dist used in the examples 


###for X

beta <- function(size) {
  a <- Rfast2::Runif(1, 1, 50)
  b <- Rfast2::Runif(1, 1, 50)
  x <- rbeta(size, a, b)
  return(x)
}



### For Y
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



mix_norm3 <- function(size){
  mu1 <- Rfast2::Runif(1, -50, 50)
  v1 <- Rfast2::Runif(1, 0, 50)
  mu2 <- Rfast2::Runif(1, -50, 50)
  v2 <- Rfast2::Runif(1, 0, 50)
  mu3 <- Rfast2::Runif(1,-50,50)
  v3 <- Rfast2::Runif(1,0,50)
  v1 <- Rfast::Rnorm(size, mu1, v1)
  v2 <- Rfast::Rnorm(size, mu2, v2)
  v3 <- Rfast::Rnorm(size, mu3, v3)
  #### used the dirclet dist to generate weight for the mix of 3
  y <- tcrossprod(matrix(c(v1,v2,v3),size),rdirichlet(1,c(1,1,1)))
}



