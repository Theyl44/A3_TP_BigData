data(iris)

tab1 <- split(iris, iris$Species)#pour faire les listes en fct de la race de la plante
tabexo1 <- data.frame(SL = (iris$Sepal.Length - mean(iris$Sepal.Length))/sqrt(var(iris$Sepal.Length)),
                      SW = (iris$Sepal.Width - mean(iris$Sepal.Width))/sqrt(var(iris$Sepal.Width)), 
                      PL = (iris$Petal.Length - mean(iris$Petal.Length))/sqrt(var(iris$Petal.Length)),
                      PW = (iris$Petal.Width - mean(iris$Petal.Width))/sqrt(var(iris$Petal.Width)),
                      Species = (iris$Species)
                      )
vplot(tabexo1$PL, tabexo1$PW, col=c("red", "blue", "green")[tabexo1$Species])


