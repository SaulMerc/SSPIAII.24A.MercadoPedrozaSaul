#getwd()
setwd("/home/soul/Desktop/SSPIAI/SSPIAII.24A.MercadoPedrozaSaul")
source("P1.3_Polinomial_SVR/LibPreprocess.r")

set.seed(2002)
options(scipen = 999)

#Preprocesamiento
df.Startups <- read.csv("Datasets/50_Startups.csv", header = T, stringsAsFactors = T)

df.Startups$State <- as.numeric(df.Startups$State)
#plot(df.Startups)
#df.Startups

#Buscando la correlación de variables#
ml.Corelation <- cor(df.Startups)
ml.Corelation

#boxplot(df.Startups$R.D.Spend, df.Startups$Profit)
#plot(df.Startups$Profit ~ df.Startups$R.D.Spend)

#Split de datos#
Split <- sample.split(Y = df.Startups$Profit, SplitRatio = 0.8)

df.Startup.Train <- subset(df.Startups, Split == T)
df.Startup.Test <- subset(df.Startups, Split == F)

#Modelos#
#Función para conocer cuál es el modelo más óptimo con AIC#
polyn <- function() {
for (i in c(1:10)) {
    mdl.Poly <- lm(formula = Profit ~ poly(R.D.Spend, i), data = df.Startup.Train)
    print(paste("Modelo ", i))
    aic <- AIC(mdl.Poly)
    print(aic)
}

}
polyn()

##Polinomial##
#Modelo 3, Modelo 6 y Modelo 7#
mdl.Poly3 <- lm(formula = Profit ~ poly(R.D.Spend, 3), data = df.Startup.Train)
mdl.Poly6 <- lm(formula = Profit ~ poly(R.D.Spend, 6), data = df.Startup.Train)
mdl.Poly7 <- lm(formula = Profit ~ poly(R.D.Spend, 7), data = df.Startup.Train)
#Mejor modelo que se adapta a los datos de acuerdo con el MSE#
mdl.summ3 <- summary(mdl.Poly3)
MSE3 <- mean(mdl.summ3$residuals^2)
mdl.summ6 <- summary(mdl.Poly6)
MSE6 <- mean(mdl.summ6$residuals^2)
mdl.summ7 <- summary(mdl.Poly7)
MSE7 <- mean(mdl.summ7$residuals^2)

print(MSE3)
print(MSE6)
print(MSE7)

#Gráfica del modelos#
mdl.plt7.Train <- ggplot() + geom_point(aes(x = df.Startup.Train$R.D.Spend, 
                        y = df.Startup.Train$Profit)) + 
                        xlab("Gasto de investigación y desarrollo") +
                        ylab("Utilidad") +
                        geom_line(aes(x = df.Startup.Train$R.D.Spend,
                        y = predict(mdl.Poly7, newdata = df.Startup.Train), colour = "orange")) 

mdl.plt7.Train

mdl.plt7.Test <- ggplot() + geom_point(aes(x = df.Startup.Test$R.D.Spend, 
                        y = df.Startup.Test$Profit)) + 
                        xlab("Gasto de investigación y desarrollo") +
                        ylab("Utilidad") +
                        geom_line(aes(x = df.Startup.Test$R.D.Spend,
                        y = predict(mdl.Poly7, newdata = df.Startup.Test), colour = "orange")) 

plot(mdl.plt7.Test)
ggsave(filename = "poli_train.jpg", plot = mdl.plt7.Train, units = "in", height = 7, width = 8)
ggsave(filename = "poli_test.jpg", plot = mdl.plt7.Test, units = "in", height = 7, width = 8)
#ELevar la formula determinada^#
mdl.elev <- lm(formula = Profit ~ R.D.Spend + I(R.D.Spend^7), data = df.Startup.Train)
summary(mdl.elev)

##SVR##
svr.lin <- svm(formula = Profit ~ ., data = df.Startup.Train, kernel = "line", scale = F)
summary(svr.lin)

svr.poly <- svm(formula = Profit ~ ., data = df.Startup.Train, kernel = "polynomial", scale = F)
summary(svr.poly)

svr.rad <- svm(formula = Profit ~ ., data = df.Startup.Train, kernel = "radial", scale = F)
summary(svr.rad)

svr.sig <- svm(formula = Profit ~ ., data = df.Startup.Train, kernel = "sigmoid", scale = F)
summary(svr.sig)

#Gráfica#
plt.svr.Train <- ggplot() +
theme_light()+
geom_point(aes(x = df.Startup.Train$R.D.Spend,y = df.Startup.Train$Profit))+
xlab("Gasto de investigación y desarrollo") +
ylab("Utilidad") +
geom_line(aes(x = df.Startup.Train$R.D.Spend, y = predict(svr.lin, newdata = df.Startup.Train)), colour = "darkblue", linewidth = 1.5)

plt.svr.Train

plt.svr.Test <- ggplot() +
theme_light()+
geom_point(aes(x = df.Startup.Test$R.D.Spend,y = df.Startup.Test$Profit))+
xlab("Gasto de investigación y desarrollo") +
ylab("Utilidad") +
geom_line(aes(x = df.Startup.Test$R.D.Spend, y = predict(svr.lin, newdata = df.Startup.Test)), colour = "darkblue", linewidth = 1.5)

plt.svr.Test

plt.grid <- plot_grid(mdl.plt7.Train,mdl.plt7.Test,plt.svr.Train, plt.svr.Test, cnol = 2)

plt.grid


ggsave(filename = "poli_svr.jpg", plot = plt.grid, units = "in", height = 7, width = 14)

#install.packages("rmarkdown")
#library(rmarkdown)

#render('P1.3_Polinomial_SVR/MercadoPedrozaSaul.Rmd', output_format = 'html_document')
