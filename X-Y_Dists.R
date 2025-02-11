
###These are the dist used in the examples 


########################### for Generating X ####################################


##For the Beta  dist the packages needed to run the code are
# library(Rfast2)

beta <- function(size) {
  a <- Rfast2::Runif(1, 1, 50)
  b <- Rfast2::Runif(1, 1, 50)
  x <- rbeta(size, a, b)
  return(x)
}

##For the Skewed Normal dist the packages needed to run the code are
# library(Rfast2)
#library(fGarch)

skew_norm <- function(size) {
  mu1 <- Rfast2::Runif(1, -50, 50)
  v1  <- Rfast2::Runif(1, 0, 50)
  c   <- Rfast2::Runif(1,-50,50) # skewness parameter
  x   <- rsnorm(size, mu1, v1,c) 
  return(x)
}

##For the Von Mises dist the packages needed to run the code are
# library(Rfast)
# library(Rfast2)


Vonmises <- function(size){
  m <- Rfast2::Runif(1,0,360)
  k <- Rfast2::Runif(1,1,10)
  x <- Rfast::rvonmises(size,m,k,rads = FALSE) #Check also rvmf
}

##For the Gamma dist the packages needed to run the code are
# library(Rfast)
# library(Rfast2)



Gamma <- function(size){
  #the shape parameter as approaches to zero becomes heavily right skewed and it explodes
  #so im setting the limit to 0.1 
  shape <- Rfast2::Runif(1,0.1,10)
  #scale affects the variance here as the var becomes higher the var lowers and vice versa
  scale <- Rfast2::Runif(1,1,10)
  x <- rgamma(size,shape,scale)
}


############################## For Generating Y ################################


##For the mixture of 2 dist the packages needed to run the code are
# library(Rfast)
# library(Rfast2)

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

##For the mixture of 3 dist the packages needed to run the code are
# library(Rfast)
# library(Rfast2)
# library(MCMCpack)

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

mix_norm_skew_t <- function(size) {
  p <- Rfast2::Runif(1)
  q <- 1 - p
  mu1 <- Rfast2::Runif(1, -50, 50)
  v1 <- Rfast2::Runif(1, 0, 50)
  mu2 <- Rfast2::Runif(1, -50, 50)
  v2 <- Rfast2::Runif(1, 0, 50)
  v1 <- Rfast::Rnorm(size, mu1, v1,1)#DF =1 
  v2 <- Rfast::Rnorm(size, mu2, v2,1) #DF
  y <- p * v1 + q * v2 # sigma1 =v1 and sigma2=v2
}


