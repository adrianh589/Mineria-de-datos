# Title     : Taller Iris
# Objective : TODO
# Created by: Adrian Hoyos
# Created on: 27/08/2020

library(dplyr)
library(ggplot2)

datos <- iris

###################### 1. Desviacion estandar #####################
desvest <- datos %>% group_by(Species) %>% summarise(desv.estandar = sqrt(var(Sepal.Length, Sepal.Width)))
print(desvest)

###################### 2. Sacar la moda de las variables #####################
getMode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

sapply(datos[1:4], getMode)

print(datos[datos$Petal.Length == 5.0,])

#3. Histograma de petal.length
grafico <- ggplot(datos, aes(x=Petal.Length)) + geom_histogram(binwidth = 0.5)
print(grafico)

###################### 4. Boxplots por cada tipo de especie #####################
boxplots <- ggplot(datos, aes(x=Species, y=Petal.Width)) + geom_boxplot()
print(boxplots)

##################### 5. Dataframe Top 5 #####################

datos$Sepal.area <- datos$Sepal.Length * datos$Sepal.Width # Añadir nueva columna para el area del sepalo
datos$Petal.area <- datos$Petal.Length * datos$Petal.Width # Añadir nueva columna para el area del petalo

topFiveSepal <- head(arrange(datos, desc(Sepal.area)), 5) # Top 5 Sepalos
topFivePetal <- head(arrange(datos, desc(Petal.area)), 5) # Top 5 Petalos
topFive <- rbind(topFiveSepal, topFivePetal) # Unir los dos dataframes

# Dado que hay flores tanto con el mayor Sepal.area y Petal.area, el dataset toma datos repetidos,
# entonces se define esta variable para dejar los datos sin repetir
removeRepeated <- distinct(topFive)

print(removeRepeated)





