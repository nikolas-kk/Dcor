pdcor.test3 <- function(x, y, z, R = 500) {
  n <- length(x)
  
  if (is.matrix(z)) {
    a1 <- dcov::dcor(x, y, type = "U")
    a2 <- Rfast::dcor(x, z)$dcor
    a3 <- Rfast::dcor(y, z)$dcor
    up <- a1 - a2 * a3
    down <- sqrt(1 - a2^2) * sqrt(1 - a3^2)
    stat <- up / down
    pstat <- numeric(R)
    for (i in 1:R) {
      id <- sample(n, n)
      a1 <- dcov::dcor(x[id], y, type = "U")
      a2 <- dcov::dcor(x[id], z, type = "U")
      up <- a1 - a2 * a3
      down <- sqrt(1 - a2^2) * sqrt(1 - a3^2)
      pstat[i] <- up / down
    }
    
  } else {
    a1 <- dcov::dcor(x, y, type = "U")
    a2 <- dcov::dcor(x, z, type = "U")
    a3 <- dcov::dcor(y, z, type = "U")
    up <- a1 - a2 * a3
    down <- sqrt(1 - a2^2) * sqrt(1 - a3^2)
    stat <- up / down
    x <- replicate(R, Rfast2::Sample(x, n))
    a1 <- dcov::mdcor(y, x, type = 'U')
    a2 <- dcov::mdcor(z, x, type = 'U')
    up <- a1 - a2 * a3
    down <- sqrt(1 - a2^2) * sqrt(1 - a3^2)
    pstat <- up / down
  }
  (sum(pstat > stat) + 1) / (R + 1)
}
pdcor.test4 <- function(x, y, z, R = 500) {
  n <- length(x)
  a1 <- dcov::dcor(x, y, type = "U")
  a2 <- dcov::dcor(x, z, type = "U")
  a3 <- dcov::dcor(y, z, type = "U")
  up <- a1 - a2 * a3
  down <- sqrt(1 - a2^2) * sqrt(1 - a3^2)
  stat <- up / down
  x <- replicate(R, Rfast2::Sample(x, n))
  a1 <- dcov::mdcor(y, x, type = 'U')
  z <- as.matrix(z)
  a2 <- numeric(ncol(z))
  for (i in 1:ncol(z)) {
    a2[i] <- dcov::mdcor(z[, i], x, type = 'U')
  }
  up <- a1 - a2 * a3
  down <- sqrt(1 - a2^2) * sqrt(1 - a3^2)
  pstat <- up / down
  (sum(pstat > stat) + 1) / (R + 1)
}


pdcor2 <- function (x, y, z) {
  if (is.matrix(z)) {
    a1 <- dcov::dcor(x, y, type = "U")
    a2 <- Rfast::dcor(x, z)$dcor
    a3 <- Rfast::dcor(y, z)$dcor
  } else {
    a1 <- dcov::dcor(x, y, type = "U")
    a2 <- dcov::dcor(x, z, type = "U")
    a3 <- dcov::dcor(y, z, type = "U")
  }
  up <- a1 - a2 * a3
  down <- sqrt(1 - a2^2) * sqrt(1 - a3^2)
  up / down
}