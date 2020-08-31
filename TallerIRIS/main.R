# Title     : TODO
# Objective : TODO
# Created by: adria
# Created on: 27/08/2020

datos <- iris

# 1. Desviacion estandar (hecho)
datos %>% group_by(Species) %>% summarise(sd.Sepal.Width = sd(Sepal.Width), sd.Sepal.Length = sd(Sepal.Length))

# 2. Sacar la moda de las variables (hecho)
colMeans(datos[sapply(datos, is.numeric)])

#3. Histograma de petal.length (hecho)
grafico <- ggplot(datos, aes(x=Petal.Length)) + geom_histogram(binwidth = 0.5)
print(grafico)

#4. Boxplots por cada tipo de especie (hecho)
boxplots <- ggplot(datos, aes(x=Species, y=Petal.Width)) + geom_boxplot()
print(boxplots)

#5. Dataframe Top 5 (hecho)
nombres <- c('Avengers: I.W.', 'A.X.L.', 'Anabelle', 'U.S.', 'Cars')
rating <- c(9.5, 7, 7.3, 8.3, 8)
visualizaciones <- c('5MM', '950', '2MM', '1.5MM', '2.5MM')

peliculas <- data.frame(nombres, rating, visualizaciones)
print(peliculas)