#getwd()
#setwd("./SSPIAII.24A.MercadoPedrozaSaul")
source("./Datasets & Lib/LibPreprocess.r")

set.seed(2205200)
options(scipen = 999)

df.Social <- read.csv("./Datasets & Lib/Social_Network_Ads.csv", header = T, stringsAsFactors = T)

df.Social$User.ID <- NULL
summary(df.Social)

#Factores
#df.Social$Gender <- factor(df.Social$Gender, levels = c("Male", "Female", labels = c(1, 0)))
df.Social$Gender <-as.numeric(df.Social$Gender)

df.Social$Gender <- NULL

cor.Test <- cor(df.Social)
#View(cor.Test)

#Escalado por variable
df.Social$Age <- scale(df.Social$Age)
df.Social$EstimatedSalary <- scale(df.Social$EstimatedSalary)
colnames(df.Social) <- c("Age", "EstimatedSalary", "Purchased")
df.Social$Age <- as.numeric(df.Social$Age)
df.Social$EstimatedSalary <- as.numeric(df.Social$EstimatedSalary)
df.Social$Purchased <- as.numeric(df.Social$Purchased)


#División de datos
Split <- sample.split(df.Social$Purchased, SplitRatio = 0.80)
df.Social.Train <- subset(df.Social, Split == T)
df.Social.Test <- subset(df.Social, Split == F)


#Regresión logística#
mdl.Rlog <- glm(formula = Purchased ~ ., data = df.Social.Train, family = binomial)

summary(mdl.Rlog)

#Predicciones
predict.Rlog <- predict(mdl.Rlog, newdata = df.Social.Test, type = "response")

#View(predict.Rlog)

Y.pred <- ifelse(predict.Rlog > 0.5, 1, 0)

#Test, Train, Línea#

#plot(df.Social.Train$Age, df.Social.Train$Purchased, color = "blue")

plt.Social <- ggplot(df.Social.Train, aes(x = Age, y = Purchased)) + theme_gray() + 
                geom_point() +
                geom_smooth(method = "glm", method.args = list(family = "binomial"))

plot(plt.Social)

#ggsave(filename = "Rlog.png", plot = plt.Social, units = "in", height = 7, width = 14)

#Colores
set <- df.Social.Test

X1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.1)
X2 <- seq(min(set$EstimatedSalary) -1, max(set$EstimatedSalary) + 1, by = 0.1)

grid.set <- expand.grid(X1, X2)

colnames(grid.set) <- c("Age", "EstimatedSalary")

prob.set <- predict(mdl.Rlog, type = "response", newdata = grid.set)

prob.set

Y.grid <- ifelse(prob.set > 0.5, 1, 0)
#Graficar#
plot(set[,-3], main = "Gráfico de Clasificación", xlab = "Edad", ylab = "Salario estimado", xlim = range(X1), ylim = range(X2)) 
contour(X1,X2, matrix(as.numeric(Y.grid), length(X1), length(X2)), add = TRUE)

points(grid.set, pch = "-", col = ifelse(Y.grid == 1, "gold", "black"))

points(set, pch = 21, bg = ifelse(set$Purchased == 1, "orange", "green"))

#Evaluación del modelo#
#Check imbalanced#
summary(as.factor(df.Social.Train$Purchased))

#Matriz de confusión#
matriz <- table(df.Social.Test$Purchased, Y.pred)
matriz

#Obetener los coeficientes#
TP <- matriz[2,2]
TN <- matriz[2,1]
FP <- matriz[1,1]
FN <- matriz[1,2]

#Fórmulas#
##Accuracy##
accuracy <- (TP + TN) / (TP + TN + FP + FN)
accuracy
##Presicion##
presicion <- TP / (TP + FP)
presicion
##Recall##
recall <- TP / (TP + FN)
recall
##F1 Score##
F1 <- (2 * presicion * recall) / (presicion + recall)
F1

#Uso de Caret para el cálculo de Accuracy, Presicion, Recall, F1 Score#
confusionMatrix(as.factor(Y.pred), as.factor(df.Social.Test$Purchased), mode = "everything", positive = "1")
confusionMatrix(matriz, positive = "1")$byClass

#Conclusión
# El realizar los cálculos de Accuracy, Presicion, Recall y F1 Score permiten tener una idea acerca de la calidad del modelo
# resulta curioso ver como los cálculos realizados de manera manual varian mucho con respecto la que realiza la función de caret confusionMatrix
# pudiendo ser el resultado de cosas como la implementación de las fórmulas o el ajuste de umbral que realiza caret para los datos
#
#
