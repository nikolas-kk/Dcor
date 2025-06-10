sizes <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000)

for (size in sizes) {
  x <- rnorm(size)
  y <- rnorm(size)
  z <- rnorm(size)
  cat("\n", "Size: ", size, "\n")
  cat("Permuation-based Correlations", "\n")
  print(
    microbenchmark::microbenchmark(
      pdcor::pdcor.test(x, y, z, type = 1, R = 500),
      dcov::dcor.test(x, y, R = 500, type = 'U'),
      times = 10,
      unit = "seconds"
    )
  )
  
  cat("\n", "Simple Correlations", "\n")
  print(
    microbenchmark::microbenchmark(
      dcov::pdcor(x, y, z, type = 'U'),
      dcov::dcor(x, y, type = "U"),
      times = 10,
      unit = "seconds"
    )
  )
}