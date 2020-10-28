# Title     : TODO
# Objective : TODO
# Created by: Adrian Hoyos
# Created on: 15/09/2020

#--------------------------------- 1 -------------------------------------------------

#install.packages('mice')
library(dplyr)
library(mice)#Libreria que ayuda a identificar si hay datos faltantes
library(ggplot2)
library(corrplot)
library(corrgram)
library(GGally)

#Leer el dataset
dataset <- read.csv('data.csv')

#Verificar que no hayan datos nulos
any(is.na(dataset)|is.null(dataset))
lapply(dataset, function(x) all(x == 0))

#Verificar si hay valores negativos
lapply(dataset, function(x) all(x < 0))

# Ver si hay valores perdidos
#md.pattern(dataset)

#Eliminar la columna X ya que no me dice nada al tener valores perdidos
dataset$X <- NULL

#Convertir en variable cualitativa la columna diagnosis
dataset$diagnosis <- factor(dataset$diagnosis)

#Eliminar datos duplicados
cleanDataset <- distinct(dataset)
str(cleanDataset)
class(cleanDataset)

#Análisis de los datos
summary(cleanDataset)
#------------------------------------------------- 2 --------------------------------------------------
str(dataset)

#------------------------------------------------- 3 --------------------------------------------------
grafico <- ggplot(cleanDataset, aes(x=diagnosis)) + geom_bar()
print(grafico)

#Desbalance
desbalance <- as.data.frame(prop.table(table(cleanDataset$diagnosis)))
desbalance <- (desbalance$Freq[1] - desbalance$Freq[2]) * 100
desbalance <- paste(desbalance, "%")
print(desbalance)

#Grafico balance
desbalance <- as.data.frame(prop.table(table(cleanDataset$diagnosis)))
desbalance
grafic <- ggplot(desbalance, aes(x="", y=Freq*100, fill=Var1)) + geom_bar(stat='identity', width = 1) + coord_polar("y", start = 0)
grafic <- grafic + labs(x="Tipo de cáncer", y="Porcentaje")
print(grafic)

#Correlacion de los datos Pearson
columnas.numericas <- sapply(cleanDataset, is.numeric)
datos.correlacion <- cor(cleanDataset[,columnas.numericas], method = 'pearson')

#Gráfico Correlacion lineal
plot(datos.correlacion)
corrplot(datos.correlacion, method = 'color')

#Gráfico regresión lineal y distribucion de variables
toLinearRegression <- cleanDataset[,columnas.numericas==TRUE]
ggpairs(select(cleanDataset, -id, -diagnosis), title="correlogram lineal", lower = list(continuous = wrap("smooth", alpha = 0.1, size=0.1)))

#Grafico de las variables malignas y benignas
grafico7 <- ggplot(cleanDataset, aes(x=diagnosis)) + geom_bar()
print(grafico7)
