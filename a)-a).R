

library(dcov)
library(ggplot2)
#auto einai ena testaki


#Generate normal distribution with 50 samples 
x <- rnorm(50)



#Generate a 50:50 mixture from two normals(same mean different sd ) with 50 samples
y1 <- 0.5*rnorm(50)
y2 <-  0.5*rnorm(50,0,2)
y  <-  y1 + y2

#Create a data frame for plotting the data 
ymat <- c(y,y1,y2)
group <- c(rep("A", 50), rep("B", 50), rep("AB", 50))
df <- data.frame(ymat, group)

cols <- c("#F76D5E", "#FFFFBF", "#72D8FF")

# Basic density plot for plotting the 3 distributions 
ggplot(df, aes(x = ymat, fill = group)) +
  geom_density(alpha = 0.3) + 
  scale_fill_manual(values = cols)


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

#Creates a random vector from two 
CocktailNorm <- function(n,p=0.5){
  x <- (1-p)*rnorm(n,runif(1,1,10),runif(1,1,10)) 
                   + p*(rnorm(n,runif(1,1,10),runif(1,1,10)))
  return(x)
}

aa <- CocktailNorm(100)
df <- data.frame(aa)

ggplot(df, aes(x = aa)) +
  geom_density()

data.frame(CocktailNorm(10))
