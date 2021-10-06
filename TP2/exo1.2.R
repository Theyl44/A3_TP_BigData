x <- c(420,380,350,400,440,380,450,420)
y <- c(5.5,6,6.5,6,5,6.5,4.5,5)
plot(x,y)
sxy <- 0 
sxx <- 0
syy <- 0
for(i in 1:8){
  sxy <- sxy + (x[i] - mean(x))*(y[i] - mean(y))
  sxx <- sxx + (x[i] - mean(x))*(x[i] - mean(x))
}
b1 <- sxy/sxx
b0 <- mean(y) - b1*mean(x)
abline(b0,b1)

err <- cbind(y - b0 - b1*x) #demander comment les afficher sur les plot ? si possible

model <- lm(y~x)
coef(model)
#questions : qst 3 ?? et 6 et 7 ?? pas compris la question (excel ? )
