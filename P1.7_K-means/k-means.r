#Saul Mercado Pedroza#
source("SSPIAII.24A.MercadoPedrozaSaul/Datasets & Lib/LibPreprocess.r")
options(scipen = 999)
set.seed(2002)

df.Credit <- read.csv("SSPIAII.24A.MercadoPedrozaSaul/Datasets & Lib/Credit Card_Kaggle.csv", header = TRUE, stringsAsFactors = TRUE)



df.Credit$CUST_ID <- NULL

corr <- cor(df.Credit)
View(corr)


df.Credit.new <- NULL
df.Credit.new$CHA <- df.Credit$CASH_ADVANCE
df.Credit.new$BF <- df.Credit$BALANCE_FREQUENCY
df.Credit.new$CSHAT <- df.Credit$CASH_ADVANCE_FREQUENCY
df.Credit.new <- as.data.frame(df.Credit.new)



#Elbow#
n.obs <- length(df.Credit.new$CSHAT)
# 
WCSS <- vector()
for(i in 1:15){
    WCSS[i] <- kmeans(df.Credit.new, i)$tot.withinss
}

WCSS <- as.data.frame(WCSS)
WCSS$K <- 1:15
View(WCSS)

ggplot()+ geom_line(aes(x = WCSS$K, y = WCSS$WCSS))+
    geom_point(aes(x = WCSS$K, y = WCSS$WCSS), color = WCSS$K)+
    ggtitle("Método del codo")+
    xlab("Iteración")+
    ylab("WCSS")+
    theme_minimal()

#K5 resulta ser la mejor opcion resultante tanto en WCSS como en el metodo K-means#

#Manera 1 con libreria factoextra#
#Se utiliza principalmente para visualizar los clusters de K-means#
#toma los resultados de los datos original como argumentos #
install.packages("factoextra")
library(factoextra)

mdl.NSup <- kmeans(df.Credit.new, 5, trace = TRUE)

fviz_cluster(mdl.NSup, data = df.Credit.new, geom = "point",
ellipse.type= "convex")

#Adaptando el gráfico del cluster de K-Means con ggpubr#
#Utilizando ggscatter()#
install.packages("ggpubr")
library(ggpubr)

#Reducir las dimenciones utilizando PCA#
#Análisis de componentes principales, técnica estadística para describir#
#un término de conjuntos de datos en tperminos de nuevas variables# 
#no correlacionadas#

#Reduccion de dimensiones#
res.pca <- prcomp(df.Credit.new, scale = TRUE)

#Coordenadas de los individuos#
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)

#Añadir los clusters obtenidos utilizando el alg K-Means#
ind.coord$cluster <- factor(mdl.NSup$cluster)

#Añadir las caracteristicas CASH_ADVANCE_FREQUENCY, BALANCE_FREQUENCY y CASH_ADVANCE#
ind.coord$CHA <- df.Credit$CASH_ADVANCE
ind.coord$BF <- df.Credit$BALANCE_FREQUENCY
ind.coord$CSHAT <- df.Credit$CASH_ADVANCE_FREQUENCY

#Inspección de los datos#
head(ind.coord)

#Porcentaje de la vairanza explicada mediante dimensiones#
#trabaja con Eigenvalor#
eigenvalue <- round(get_eigenvalue(res.pca),1)
variance.percent <- eigenvalue$variance.percent

head(eigenvalue)

#Graficacion#
ggscatter(ind.coord, "Dim.1", "Dim.2",
    color = "cluster", palette = "npg",
    ellipse = TRUE, ellipse.type = "convex",
    size = 1.5, legend = "right",
    ggtheme = theme_minimal(), xlab = paste0("Dim 1", variance.percent[1], "%"),
    ylab = paste0("Dim 2", variance.percent[2], "%") +
    stat_mean(aes(color = cluster), size = 4)
)

#Modo 3 utilizando Plot cluster#
install.packages("cluster")
install.packages("fpc")
library(cluster)
library(fpc)

#KM <- mdl.NSup#
#De manera simple se puede graficar de la siguiente manera#
plotcluster(df.Credit.new, mdl.NSup$cluster)
#O también de la siguiente manera#
clusplot(df.Credit.new, mdl.NSup$cluster, color = TRUE,
        shade = TRUE, labels = 2, lines = 0)
#Al igual que el plot, es una manera diferente de visualizar los datos del cluster#
#conforme a la matriz de correlacion#
with(df.Credit, pairs(df.Credit.new, col=c(1:3)[mdl.NSup$cluster]))
