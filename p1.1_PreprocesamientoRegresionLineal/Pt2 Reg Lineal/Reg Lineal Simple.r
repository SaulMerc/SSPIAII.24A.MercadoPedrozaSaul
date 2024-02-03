#getwd()
#setwd("./SSPIAII.24A.MercadoPedrozaSaul/p1.1_PreprocesamientoRegresionLineal/Pt2 Reg Lineal")
    ##Recursos##
source('LibPreprocess.r', chdir = TRUE)
library(ggplot2)

#Semilla de trabajo#
options(scipen=999)
set.seed(21)

    ##Importar datos##
df.Wine = read.csv(file = '../../Datasets/WineQT.csv', header = TRUE)

    ##Preprocesado##
summary(df.Wine)
#View(df.Wine)

#Grafica de los valores a trabajar#
#boxplot(df.Wine$pH, df.Wine$quality) #Datos atípicos limpiar

mean.WinepH = mean(df.Wine$pH) 
mean.WineQ = mean(df.Wine$quality)
#Filtrando mediante IRQ (Rango intercuartil) para el PH#
Q1.ph <- quantile(df.Wine$pH, 0.25)
Q3.ph <- quantile(df.Wine$pH, 0.75)
IQR.ph <- Q3.ph - Q1.ph

LI.ph <- Q1.ph - 1.5*IQR.ph
LS.ph <- Q3.ph + 1.5*IQR.ph

df.Wine$pH <- ifelse(df.Wine$pH < LI.ph | df.Wine$pH > LS.ph, mean.WinepH, df.Wine$pH)
#View(df.Wine$pH)
#boxplot(df.Wine$pH)

#Filtrando mediante IRQ (Rango intercuartil) para quality#
Q1.q <- quantile(df.Wine$quality, 0.25)
Q3.q <- quantile(df.Wine$quality, 0.75)
IQR.q <- Q3.q - Q1.q

LI.q <- Q1.q - 1.5*IQR.q
LS.q <- Q3.q + 1.5*IQR.q

df.Wine$quality <- ifelse(df.Wine$quality < LI.q | df.Wine$quality > LS.q, mean.WineQ, df.Wine$quality)
#View(df.Wine$quality)
#boxplot(df.Wine$quality)

##Dividir##
Split <- sample.split(Y = df.Wine$quality, SplitRatio = 0.8)

#Subconjuntos
df.Wine.Train <- subset(df.Wine, Split == T)
df.Wine.Test <- subset(df.Wine, Split == F)
df.Wine.Train
df.Wine.Test

#Modelo R.Lineal
mdl.Regresión <- lm(formula = quality ~ pH, data = df.Wine.Train)

#Predicción
mdl.Predict <- predict(object = mdl.Regresión, newdata = df.Wine.Test)

#Gráfico
##Base to make a graphic##
plt.Wine <- ggplot() + theme_gray() + 
            ggtitle("Regresión calidad del vino respecto a su PH") + 
            xlab("QUALITY") + 
            ylab("PH")

##Using the data Train to graphic##
plt.Wine.Data.Train <- plt.Wine + 
                    geom_smooth(method = "loess",se = FALSE, color = "mediumorchid4" ,aes(x = df.Wine.Train$quality,
                                    y = df.Wine.Train$pH)) 



plt.Wine.Data.Test <- plt.Wine + 
                    geom_smooth(method = "lm",se = FALSE, color = "firebrick" ,aes(x = df.Wine.Test$quality,
                                    y = df.Wine.Test$pH)) 
plot(plt.Wine.Data.Train)

#ggsave("Regresión entrenamiento.png", plot = plt.Wine.Data.Train, width = 6, height = 6)
ggsave("Regresión prueba.png", plot = plt.Wine.Data.Test, width = 6, height = 6)