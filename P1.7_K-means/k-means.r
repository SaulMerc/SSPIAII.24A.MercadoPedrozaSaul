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

#mdl.NSup <- kmeans(df.Credit.new, 5, trace = T)

#Elbow#
n.obs <- length(df.Credit.new$CSHAT)

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

