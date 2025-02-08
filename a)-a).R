

library(dcov)
library(ggplot2)
#auto einai ena testaki


#normal distrubtion
x <- rnorm(50)

#mixture from two normals(same mean difrrent sd )
y <- 0.5*rnorm(50) + 0.5*rnorm(50,0,2)

xy <- matrix(c(x,y),ncol=2)
colnames(xy) <- c("xx","yy")

ggplot(xy,aes(x = xx, y = yy)) +
  geom_point()

dcor(x,y)
cor(x,y)


