library(dcov)

beta <- function(size) {    #Βάζω τυχαίες παραμέτρους για όλες τις κατανομές
  a <- runif(1, 1, 50)
  b <- runif(1, 1, 50)
  x <- rbeta(size, a, b)
  return(x)
}
mix_norm <- function(size) {
  y <- c()
  p <- runif(1) #Επιλέγω τυχαία πιθανότητα p για τις τιμές από τη 1η κανονική κατανομή
  mu1 <- runif(1, -50, 50)
  sigma1 <- runif(1, 0, 50)
  mu2 <- runif(1, -50, 50)
  sigma2 <- runif(1, 0, 50)
  for (i in 1:size) {
    q <- runif(1)
    if (q <= p) {
      y[i] <- rnorm(1, mu1, sigma1)
    } else{
      y[i] <- rnorm(1, mu2, sigma2)
    }
  }
  return(y)
}


type1_error <- function(P) {
  count_perm <- 0
  count_as <- 0
  for (i in 1:P) {
    x <- beta(50)
    y <- mix_norm(50)
    pperm <- dcor.test(x, y, R = 1000, type = 'U')$p.values
    stat_as <- 50 * dcor(x, y, type = "U")^2 + 1
    pas <- pchisq(stat_as, 1, lower.tail = FALSE)
    if (pperm < 0.05) {
      count_perm <- count_perm + 1
    }
    if (pas < 0.05) {
      count_as <- count_as + 1
    }
  }
  type1 <- c("Pemutation" = count_perm / P, "Asymptotic" = count_as / P)
  return(type1)
}
type1_error(1000)
