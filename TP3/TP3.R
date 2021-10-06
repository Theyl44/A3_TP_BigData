attach(anesthesie)
library(stats)
reg = glm(Y ~ X1, family = binomial)
summary(reg)

