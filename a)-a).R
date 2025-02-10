

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

d <-replicate(1000,CocktailNorm(500,runif(1,0,1)))
y <-replicate(1000,rnorm(500,runif(1,1,10),runif(1,1,10)))

pperm <- dcor.test(d, y, R = 1000, type = 'U')$p.values
dcor.te
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