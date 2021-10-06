x1 <- c(4,4,3,2,1,6,4,4,1,1,3,4)
x2 <- c(4,3.6,3.1,3.2,3.0,3.8,3.8,2.9,3.8,2.8,3.4,2.8)
y  <- c(37.8,22.5,17.1,10.8,7.2,42.3,30.2,19.4,14.8,9.5,32.4,21.6)
plot(x1,y)

sxy <- 0 
sxx <- 0
syy <- 0
for(i in 1:12){
  sxy <- sxy + (x1[i] - mean(x1))*(y[i] - mean(y))
  sxx <- sxx + (x1[i] - mean(x1))*(x1[i] - mean(x1))
}
b11<-cov(x1,y)/var(x1)
b1 <- sxy/sxx
b0 <- mean(y) - b1*mean(x1)
abline(b0,b1)
err <- cbind(y - b0 - b1*x1)

#methode du prof 

mod1 <- lm(y~x1)
summary(mod1)
anova(mod1)
