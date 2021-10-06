#reg log 
#faut include le txt 
attach(anesthesie)

reglog1 = glm(Y~X1, family=binomial(link=logit))#car erreur est entre 0 & 1 
summary(reglog1)
plot(X1,Y)

logit_ypredit=-5.57*anesthesie$X1+6.47
ypredit=exp(logit_ypredit)/(1+ exp(logit_ypredit)) # transfo inverse de logit

o=order(anesthesie$X1)
points(anesthesie$X1[o],ypredit[o], col="red", type="l", lwd=2)


newdata = data.frame(X1 = 1.25)
predict(reglog1, newdata, type="response")
#----------------------------------
#correction du prof s
# reponse qst 3 s
#on trouve un Y =0,3799 donc on en conclu qu'il n'a pas bougé


#le curve du prof est bien 

