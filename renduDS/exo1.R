#EXERCICE 1 
#on rentre les données de l'énoncer
vol = c(0.152,0.284,0.187,0.350,0.416,0.230,0.242,0.276,0.383,0.140)
aire = c(297,595,372,687,790,520,473,585,762,232)
#on rentre nos valeur sous forme de data frame
data <- data.frame(vol,aire)
#plot pour visualiser les données 
plot(data$vol,data$aire)
#ensuite on fait la regression linéaire avec la fonction lm (=Linear Models) pour trouver les coefficient de la droite des mondre carrées
model1 = lm(data$vol~data$aire)
#ensuite la fontion coef va nous donner les coef B0 et B1 dans cet ordre 
coef(model1)

#nous allons predire l'aire d'un abre avec la regression linaire  
var = predict(model1,data.frame(465), type = "response")
var = data.frame(var)
mean(var$var)


#########################################
#EXERCICE 2 
#pour cet exercice avec le bouton "import dataset" je vais créer ma data "ronfle" avec toutes mes valeurs 
#il faut telecharger le fichier .txt ou autre pour faire cette manipulation 

attach(ronfle)
library(stats)
#on va regarder les tableau de toutes les regressions logistique qu'on peut faire pour eliminer des paramètre 
plot(ronfle$AGE,ronfle$RONFLE)
plot(ronfle$POIDS,ronfle$RONFLE)
plot(ronfle$TAILLE,ronfle$RONFLE)
plot(ronfle$ALCOOL,ronfle$RONFLE)
plot(ronfle$SEXE,ronfle$RONFLE)
plot(ronfle$TABA,ronfle$RONFLE)

####age 
#on suit la methode faite dans le TP3 
reg1 = glm(RONFLE ~ ronfle$AGE, family = binomial)
summary(reg1)
pred.prob1 = predict(reg1, type = "response")
pred.prob1
pred.prob1 = factor(ifelse(pred.prob1 >0.5, "1", "0"))
pred.prob1

mc1 = table(RONFLE, pred.prob1)
mc1 
t1  = (mc1[1,2]+mc1[2,1])/sum(mc1)
t1


####poids 
reg2 = glm(RONFLE ~ ronfle$POIDS, family = binomial)
summary(reg2)
pred.prob2 = predict(reg2, type = "response")
pred.prob2
pred.prob2 = factor(ifelse(pred.prob2 >0.5, "1", "0"))
pred.prob2

mc2 = table(RONFLE, pred.prob2)
mc2
t2  = (mc2[1,2]+mc2[2,1])/sum(mc2)
t2


####taille 
reg3 = glm(RONFLE ~ ronfle$TAILLE, family = binomial)
summary(reg3)
pred.prob3 = predict(reg3, type = "response")
pred.prob3
pred.prob3 = factor(ifelse(pred.prob3 >0.5, "1", "0"))
pred.prob3

mc3 = table(RONFLE, pred.prob3)
mc3 
t3  = (mc3[1,2]+mc3[2,1])/sum(mc3)
t3

###conclusion et test pour le nvx patient 
#parametre du patien 
newpat = data.frame(IDEN = "POOX", AGE = 71, POIDS = 76, TAILLE = 164, ALCOOL = 4, SEXE = 0, TABA = 0)
#notre prediction 
pred = predict(reg3, newpat, type = "response", add = TRUE)
pred

