#Comprobar e instalar librerias
if (!require(ggplot2)) {
    install.packages("ggplot2", dependencies = TRUE)
    library(ggplot2)
}
if (!require(caTools)) {
    install.packages("caTools", dependencies = TRUE)
    library(caTools)
}

if (!require(data.table)) {
    install.packages("data.table", dependencies = TRUE)
    library(data.table)
}

if (!require(mltools)) {
    install.packages("mltools", dependencies = TRUE)
    library(mltools)

}

if (!require(corrplot)) {
    install.packages("corrplot", dependencies = TRUE)
    library(corrplot)
}

if (!require(viridis)) {
    install.packages("viridis", dependencies = TRUE)
    library(viridis)
}

if (!require(caret)) {
    install.packages("caret", dependencies = TRUE)
    library(caret)
}