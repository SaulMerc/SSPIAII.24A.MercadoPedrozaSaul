#Saúl Mercado Pedroza#
source("./Datasets & Lib/LibPreprocess.r")

set.seed(2002)
options(scipen = 999)

#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")
library(rpart)
library(rpart.plot)
library(randomForest)

df.Salaries <- read.csv("./Datasets & Lib/Position_Salaries.csv", header = T, stringsAsFactors = F)
df.Salaries$Position <- NULL
mod <- rpart(formula = Salary ~ Level, data = df.Salaries, minsplit = 1)

summary(mod)

rpart.plot(mod)

x <- seq(min(df.Salaries[,1])-1, max( df.Salaries[,1])+1, by = 0.1)
new.Data <- as.data.frame(x)
colnames(new.Data) <- "Level"

graphic <- ggplot() +
    geom_point(aes(x = df.Salaries$Level, y = df.Salaries$Salary), colour = "blue") +
    geom_line(aes(x = df.Salaries$Level, y = predict(mod, newdata = df.Salaries)), colour = "gold") +
    geom_line(aes(x = new.Data$Level, y = predict(mod, newdata = new.Data)), colour = "peru") + theme_minimal()

graphic
#Dataset a utilizar Social Network ADS#

##Parte 2##
df.Social <- read.csv("./Datasets & Lib/Social_Network_Ads.csv", header = T, stringsAsFactors = F)
df.Social$User.ID <- NULL
df.Social$Gender <-NULL

cor.Test <- cor(df.Social)
cor.Test

#Escalado por variable
df.Social$Age <- scale(df.Social$Age)
df.Social$EstimatedSalary <- scale(df.Social$EstimatedSalary)
colnames(df.Social) <- c("Age", "EstimatedSalary", "Purchased")
df.Social$Age <- as.numeric(df.Social$Age)
df.Social$EstimatedSalary <- as.numeric(df.Social$EstimatedSalary)
df.Social$Purchased <- as.numeric(df.Social$Purchased)

#Modelo de árbol de decisión
##Muestra las diferentes opciones y sus consecuencias. Los puntos en los que hay que tomar decisiones se muestran como nodos, las ramas unen estos nodos y las decisiones últimas son las hojas, donde el camino termina (también se denominan nodos terminales).
##Los árboles de decisión consideran todas las variables y seleccionan la que mejor separa las clases
mdl.Tree <- rpart(formula = Purchased ~ ., data = df.Social, minsplit = 3)
rpart.plot(mdl.Tree)

predict.Tree <- predict(mdl.Tree, newdata = df.Social)
Y.pred.Tree <- ifelse(predict.Tree > 0.5, 1, 0)

#Modelo Random Forest
##El método de Random Forest  utiliza una serie de árboles de decisión, con el fin de mejorar la tasa de clasificación
##Random Forest solo selecciona un subconjunto de características
mdl.Forest <- randomForest(formula = Purchased ~ ., data = df.Social, ntree = 50)

predict.Forest <- predict(mdl.Forest, newdata = df.Social)
Y.pred.Forest <- ifelse(predict.Forest > 0.5, 1, 0)
##Matrices de confusión##

confusionMatrix(as.factor(Y.pred.Tree), as.factor(df.Social$Purchased), mode = "everything", positive = "0")
confusionMatrix(as.factor(Y.pred.Forest), as.factor(df.Social$Purchased), mode = "everything", positive = "0")

##De acuerdo con el conjunto de datos proporcionado resulto ser más exacto el modelo Random Forest ya que indica un valor
##F1 para la clase 0 de 0.95, mientras que el modelo de árbol de decisión indica un valor de 0.94

#Graficar#
graphics <- function(dataframe, model) {
    set <- dataframe

    X1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.1)
    X2 <- seq(min(set$EstimatedSalary) -1, max(set$EstimatedSalary) + 1, by = 0.1)

    grid.set <- expand.grid(X1, X2)

    colnames(grid.set) <- c("Age", "EstimatedSalary")

    prob.set <- predict(model, newdata = grid.set)
    Y.grid <- ifelse(prob.set > 0.5, 1, 0)
    #Graficar#
    plot(set[,-3], main = "Gráfico de Clasificación", xlab = "Edad", ylab = "Comprado", xlim = range(X1), ylim = range(X2)) 
    contour(X1,X2, matrix(as.numeric(Y.grid), length(X1), length(X2)), add = TRUE)

    points(grid.set, pch = "_", col = ifelse(Y.grid == 1, "gold", "red"))

    points(set, pch = 21, bg = ifelse(set$Purchased == 1, "orange", "green"))
}

graphics(df.Social, mdl.Forest)
graphics(df.Social, mdl.Tree)
