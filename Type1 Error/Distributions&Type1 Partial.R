#Required packages: library(Rfast)
#                   library(Rfast2)
#                   library(sn)
#                   library(dcov)
#                   library(LaplacesDemon)
#                   library(mixsmsn)
########################### For Generating X ####################################

beta <- function(size) {
  #Beta Distribution
  a <- Rfast2::Runif(1, 1, 50)
  b <- Rfast2::Runif(1, 1, 50)
  x <- rbeta(size, a, b)
}

skew_normal <- function(size) {
  #Skew_normal Distribution
  loc   <- Rfast2::Runif(1, -50, 50)
  scale <- Rfast2::Runif(1, 1, 50)
  a     <- Rfast2::Runif(1, -20, 20)
  x     <- sn::rsn(size,
                   xi = loc,
                   omega = scale,
                   alpha = a)
}


Vonmises <- function(size) {
  m <- Rfast2::Runif(1, 0, 2*pi)
  k <- Rfast2::Runif(1, 1, 10)
  x <- Rfast::rvonmises(size, m, k, rads = TRUE) #Check also rvmf
}

Gamma <- function(size) {
  #the shape parameter as approaches to zero becomes heavily right skewed and it explodes
  #so im setting the limit to 0.1
  shape <- Rfast2::Runif(1, 0.1, 10)
  scale <- Rfast2::Runif(1, 1, 10)
  x     <- rgamma(size, shape, scale)
}

cauchy  <- function(size) {
  loc   <- Rfast2::Runif(1, -50, 50)
  scale <- Rfast2::Runif(1, 1, 50)
  x     <- rcauchy(size, loc, scale)
}
############################## For Generating Y ################################

mix_norm <- function(size) {
  #Mixture of 2 normals
  p1  <- Rfast2::Runif(1)
  p   <- c(p1, 1 - p1)
  mu1 <- Rfast2::Runif(1, -50, 50)
  v1  <- Rfast2::Runif(1, 0, 50)
  mu2 <- Rfast2::Runif(1, -50, 50)
  v2  <- Rfast2::Runif(1, 0, 50)
  m   <- c(mu1, mu2)
  s   <- c(v1, v2)
  y   <- LaplacesDemon::rnormm(size, p, m, s)
}
mix_norm3 <- function(size) {
  #Mixture of 3 normals
  mu1 <- Rfast2::Runif(1, -50, 50)
  v1  <- Rfast2::Runif(1, 0, 50)
  mu2 <- Rfast2::Runif(1, -50, 50)
  v2  <- Rfast2::Runif(1, 0, 50)
  mu3 <- Rfast2::Runif(1, -50, 50)
  v3  <- Rfast2::Runif(1, 0, 50)
  m   <- c(mu1, mu2, mu3)
  s   <- c(v1, v2, v3)
  p <- sort(Rfast2::Runif(2))
  p1 <- p[1]
  p2 <- p[2] - p[1]
  p3 <- 1 - p[2]
  p <- c(p1, p2, p3)
  p <- p / sum(p)
  y <- LaplacesDemon::rnormm(size, p, m, s)
}

mix_skew_t <- function(size) {
  p      <- Rfast2::Runif(1)
  q      <- 1 - p
  loc1   <- Rfast2::Runif(1, -50, 50)
  loc2   <- Rfast2::Runif(1, -50, 50)
  scale1 <- Rfast2::Runif(1, 1, 50)
  scale2 <- Rfast2::Runif(1, 1, 50)
  a1     <- Rfast2::Runif(1, -20, 20)
  a2     <- Rfast2::Runif(1, -20, 20)
  p      <- c(p, q)
  # mu = loc , sigma2 = scale , shape =  skew , nu = must be df
  arg1   <- c(
    mu = loc1,
    sigma2 = scale1,
    shape  = a1 ,
    nu = 1
  ) #nu = DF = 1
  arg2   <- c(
    mu = loc2,
    sigma2 = scale2,
    shape  = a2 ,
    nu = 1
  )
  arg    <- list(arg1, arg2)
  mixsmsn::rmix(size, p, family = "Skew.t", arg, cluster = FALSE)
}
#-----------z for the Partial Correlation------------
z1 <- function(n) { #mix exp - weibull
  z <- numeric(n)
  p <- Rfast2::Runif(1)
  r1 <- Rfast2::Runif(1, 1, 50)
  shape1 <- Rfast2::Runif(1, 1, 50)
  scale1 <- Rfast2::Runif(1, 1, 50)
  q <- Rfast2::Runif(n)
  exp_ind <- q <= p
  weibull_ind <- !exp_ind
  z[exp_ind] <- rexp(sum(exp_ind), r1)
  z[weibull_ind] <- rweibull(sum(weibull_ind), shape1, scale1)
  z
}

z2 <- function(n) { #mix laplace - x^2
  z <- numeric(n)
  p <- Rfast2::Runif(1)
  df1 <- Rfast2::Runif(1, 1, 50)
  loc1 <- Rfast2::Runif(1, -50, 50)
  scale1 <- Rfast2::Runif(1, 1, 50)
  q <- Rfast2::Runif(n)
  x_2_ind <- q <= p
  laplace_ind <- !x_2_ind
  z[x_2_ind] <- rchisq(sum(x_2_ind), df1)
  z[laplace_ind] <- diagL1::rlaplace(sum(laplace_ind), loc1, scale1)
  z
}

########################### Type 1 Error Function for 1 z ####################################
type1_error_partial  <- function(P, n, distx, disty, nz) {
  count_perm <- 0
  count_as   <- 0
  dist_x <- match.fun(distx)
  dist_y <- match.fun(disty)
  for (i in 1:P) {
    if (nz == 1) {
      z <- z1(n)
    } else{
      z <- cbind(z1(n), z2(n))
    }
    x       <- dist_x(n)
    y       <- dist_y(n)
    pperm   <- dcov::pdcor.test(x, y, z , R = 500, type = 'U')$p.values
    stat_as <- n * dcov::pdcor(x, y, z, type = "U") + 1
    pas     <- pchisq(stat_as, 1, lower.tail = FALSE)
    if (pperm < 0.05) {
      count_perm <- count_perm + 1
    }
    if (pas < 0.05) {
      count_as <- count_as + 1
    }
  }
  type1 <- c("Permutation" = count_perm / P, "Asymptotic" = count_as / P)
}
