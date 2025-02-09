

library(dcov)
library(ggplot2)
#auto einai ena testaki


#Generate normal distribution with 50 samples 
x <- rnorm(50)

#Generate a mixture from two normals(same mean different sd ) with 50 samples
y <- 0.5*rnorm(50) + 0.5*rnorm(50,0,2)

#assign them into a matrix so it can be used in the ggplot library
xy <- matrix(c(x,y),ncol=2)
colnames(xy) <- c("xx","yy")

#Plot the distributions
ggplot(xy,aes(x = xx, y = yy)) +
  geom_point()

# Calculate Distance correlation
dcor(x,y)
# Calculate Pearson correlation
cor(x,y)


