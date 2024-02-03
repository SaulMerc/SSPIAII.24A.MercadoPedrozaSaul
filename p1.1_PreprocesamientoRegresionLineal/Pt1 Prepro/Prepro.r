#Saul Mercado Pedroza#
##Actividades
###Importar datos de un csv
###Crear dataset
###Llamar las variables tipo vector mediante source()
#getwd()
#setwd("./SSPIAII.24A.MercadoPedrozaSaul/p1.1_PreprocesamientoRegresionLineal/Pt 1 Prepro")

source("vector.r", chdir = TRUE)

#Crear Dataset#
df.meses <- data.frame(Mes = mes, Var = variacion, Cal = calificacion)

View(df.meses)

#Importar datos de un csv
df.Wine <- read.csv(file = ("../../Datasets/WineQT.csv"), header = TRUE)