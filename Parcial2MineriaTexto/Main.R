# Title     : Parcial Mineria de Datos
# Objective : TODO
# Created by: Adrian Hoyos
# Created on: 27/10/2020

library(dplyr)
library(stringr)
library(ggplot2)
library(tm)
library(wordcloud)
library(plotly)

#Leer el dataset
dataset <- read.csv("tweets_10-24-2020.csv", encoding = "UTF-8")

#Leer malas palabras
bad.words <- read.delim("bad-words.txt", sep = "\n")
bad.words <- as.vector(bad.words$bad.words)

#Limpieza de los datos
data <- select(dataset, -date) #Seleccionar las columas char sin la fecha
date <- dataset$date #Fecha
data <- mutate_if(data, is.character, tolower)#Poner a minuscula
data <- mutate_if(data, is.character, function(x){str_replace_all(x, pattern="[[:digit:]]", "")})#Quitar los numeros
data <- mutate_if(data, is.character, function(x){gsub("n?’. ", " ", x)})#Quitar apostrofos
data <- mutate_if(data, is.character, function(x){gsub("n?'. ", " ", x)})#Quitar apostrofos
data <- mutate_if(data, is.character, function(x){gsub("n?´. ", " ", x)})#Quitar apostrofos
data <- mutate_if(data, is.character, function(x){str_replace_all(x, pattern="[[:punct:]]", "")})#Quitar signos de puntuacion
data <- mutate_if(data, is.character, function(x){str_replace(x, pattern="[~@#$%&-_=<>]", "")})#Quitar caracteres especiales
data <- mutate_if(data, is.character, function(x){str_replace(x, pattern="$", "")})#Comodin

cleandata <- cbind(data, date)
head(cleandata)

#Grafico de barras para cuantificar los sistemas operativos de los tweets
graphicOS <- ggplot(cleandata, aes(x=factor(device)))
graphicOS <- graphicOS + geom_bar(color="red")
print(graphicOS)

#Ver los dispositivos
as.data.frame(unique(cleandata$device))

#Normalizacion de las horas y el respectivo gráfico
hours <- format(as.POSIXct(cleandata$date, format = "%Y-%m-%d %H:%M:%S"), format = "%H:00")
hours <- as.data.frame(hours)
hours.graphic <- ggplot(hours, aes(x = hours, fill=cleandata$device))
hours.graphic <- hours.graphic + geom_bar(color="red")
hours.graphic.interactive <- ggplotly(hours.graphic)
print(hours.graphic.interactive)

#Retweets
retweets <- ggplot(cleandata, aes(x = isRetweet)) + geom_bar()
print(retweets)

#Eliminados
deleted <- ggplot(cleandata, aes(x = isDeleted)) + geom_bar(stat = "count") + stat_count(geom = "text", colour = "white", size = 3.5,aes(label = ..count..),position=position_stack(vjust=0.5))
print(deleted)

#Favoritos
summary(cleandata[c("favorites", "retweets")])

#Funcion para medir las frecuencias de las categorias
FreqCategory <- function(value) {
  strCategory <- ifelse(value <=5,   "         5",
                        ifelse(value <=10,     "     10",
                               ifelse(value <=20,     "     20",
                                      ifelse(value <=50,     "     50",
                                             ifelse(value <=100,    "    100",
                                                    ifelse(value <=500,    "    500",
                                                           ifelse(value <=1000,   "  1,000",
                                                                  ">1,000")))))))
  strCategory
}

mining <- function (dataset, dataset.length, os = ""){
  
  tweets <- dataset[grepl(os, cleandata$device),]
  print(length(tweets$text))
  words <- str_split(tweets$text, " ")
  words <- unlist(words, use.names = FALSE)#Convertirlo a un vector
  words <- words[words != ""]#Palabras vacias
  
  #Convertir a dataframe
  dfrUNPrfWords <- data.frame(words)
  colnames(dfrUNPrfWords) <- "Words"
  dfrUNPrfWords$Words <- as.character(dfrUNPrfWords$Words)
  
  #Traerme las 20 primeras palabras
  print("Primeras 20 palabras:")
  print(head(dfrUNPrfWords, 20))
  
  #Frecuencia de las palabras
  dfrUNPrfFreq <- dfrUNPrfWords %>%
    group_by(Words) %>%
    summarise(Freq=n()) %>%
    arrange(desc(Freq))
  
  print("Frecuencia de las palabras")
  print(head(dfrUNPrfFreq, 30))
  
  #Nube de palabras que no nos dice nada
  print(wordcloud(dfrUNPrfFreq$Words[1:100], dfrUNPrfFreq$Freq[1:100], random.order=F, max.words=100, colors=brewer.pal(8, "Dark2")))
  
  #Eliminación de los StopWords
  dfrUNPrfWords <- filter(dfrUNPrfWords, !(Words %in% stopwords()))
  
  #Analisis de Fracuencias sin los stopwords
  dfrUNPrfFreq <- dfrUNPrfWords %>%
    group_by(Words) %>%
    summarise(Freq=n()) %>%
    arrange(desc(Freq))
  
  print("Frecuencia sin stopwords")
  print(head(dfrUNPrfFreq, 30))
  
  #Nube de palabras sin los stopwords
  print(wordcloud(dfrUNPrfFreq$Words[1:100], dfrUNPrfFreq$Freq[1:100], random.order=F, max.words=100, colors=brewer.pal(8, "Dark2")))
  
  #Discretizacion de los datos
  dfrUNPrfFreq <- mutate(dfrUNPrfFreq, Fcat=FreqCategory(dfrUNPrfFreq$Freq))
  dfrUNPrfFocf <- dfrUNPrfFreq %>% group_by(Fcat) %>% summarise(Rfrq=n())
  dfrUNPrfFocf$Fcat <- factor(dfrUNPrfFocf$Fcat, levels=dfrUNPrfFocf$Fcat, ordered=T)
  
  print("Discretizacion")
  print(head(dfrUNPrfFocf, 10))
  
  #Grafico para las palabras finales minadas
  print(ggplot(slice(dfrUNPrfFreq, 1:40), aes(x=reorder(Words, -Freq), y=Freq)) +
          geom_bar(stat="identity", fill=rainbow(40)) +
          ylab("Frequency") +
          xlab("Words") +
          ggtitle("Word Frequency - Top 40 Words") +
          theme(plot.title=element_text(size=rel(1.5), colour="blue")) +
          coord_flip())
  
  #Cantidad de Tweets con malas palabras
  count.bad.words <- unique(grep(paste0('\\<', bad.words, '\\>', collapse = "|"), tweets$text))
  bad.tweets <- length(count.bad.words)#Total de tweets que contiene malas palabras
  total.tweets <- dataset.length#Total de tweets para hacer la div
  percent <- (bad.tweets/total.tweets)*100
  percent <- format(round(percent, 2), nsmall = 2)
  print("Numero de tweets con malas palabras")
  print(bad.tweets)
  print("Porcentaje de malas palabras")
  paste0(percent,"%")
}

#Minería global
mining(cleandata, length(cleandata$text))

##Minería iPhone
mining(cleandata, length(cleandata$text),"iphone")

##Minería Android
mining(cleandata, length(cleandata$text),"android")


