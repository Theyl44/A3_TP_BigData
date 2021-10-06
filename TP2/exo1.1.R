x <- c(420,380,350,400,440,380,450,420)
y <- c(5.5,6,6.5,6,5,6.5,4.5,5)
plot(x,y)

#on cherche Y :B0 + B1x + epsilon
x <- cbind(ité = rep(1,8),x)
y <- cbind(y) 
matB <- solve(t(x)%*%x, t(x)%*%y)
test <- matB[2]
abline(matB[1],matB[2])


