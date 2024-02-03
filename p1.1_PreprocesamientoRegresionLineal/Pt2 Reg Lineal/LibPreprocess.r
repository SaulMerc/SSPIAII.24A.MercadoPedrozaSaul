#Comprobar e instalar librerias
if (!require(ggplot2)) {
    install.packages("ggplot2", dependencies = TRUE)
    library(ggplot2)
}
if (!require(caTools)) {
    install.packages("caTools", dependencies = TRUE)
    library(caTools)
}