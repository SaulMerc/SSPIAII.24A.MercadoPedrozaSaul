source("./Datasets & Lib/LibPreprocess.r")

set.seed(2205200)
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
