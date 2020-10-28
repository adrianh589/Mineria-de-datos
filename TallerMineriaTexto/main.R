# Title     : TODO
# Objective : TODO
# Created by: adria
# Created on: 7/10/2020

library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(corpus)
library(SnowballC)
library(utf8)
library(tm)

#Leer los txt
fileDir <- "texts"
files <- list.files(fileDir, pattern=".*.txt")
files1 <- paste(fileDir, files, sep="/")
trumpTweets <- unname(sapply(files1, readLines, encoding = "UTF-8"))

#Separar por espacio
lineasPalabras <- str_split(trumpTweets, " ")
ListaPalabras <- unlist(lapply(lineasPalabras, length))
mean(ListaPalabras)

palabrasDiscurso <- unlist(lineasPalabras)
conteoPalabras <- length(palabrasDiscurso)
conteoPalabras

head(palabrasDiscurso,100)

#Limpieza de datos
palabrasDiscurso <- str_to_lower(palabrasDiscurso)#Poner en minusculas
palabrasDiscurso <- str_replace_all(palabrasDiscurso, pattern="[[:digit:]]", "")#quitar numeros
palabrasDiscurso <- gsub("n?’.*", "", palabrasDiscurso) # Eliminar todo lo que esté despues del apostrofe (’ es el apostrofo, ? significa que la n es opcional, . significa cualquier caracter y * cualquier cantidad de veces)
palabrasDiscurso <- gsub("n?'.*", "", palabrasDiscurso) # Eliminar todo lo que esté despues del apostrofe '
palabrasDiscurso <- gsub("n?´.*", "", palabrasDiscurso) # Eliminar todo lo que esté despues del apostrofe '
palabrasDiscurso <- str_replace_all(palabrasDiscurso, pattern="[[:punct:]]", "")#Signos de puntuacion
palabrasDiscurso <- str_replace_all(palabrasDiscurso, pattern="[[:space:]]", "")#Espacios
palabrasDiscurso <- str_replace_all(palabrasDiscurso, pattern="[~@#$%&-_=<>]", "")#Caracteres especiales
palabrasDiscurso <- palabrasDiscurso[palabrasDiscurso != ""]#Palabras vacias
palabrasDiscurso <- str_replace_all(palabrasDiscurso, pattern="$", "")#Comodin
head(palabrasDiscurso, 100)

#Convertirlo a un dataframe
dfrUNPrfWords <- data.frame(palabrasDiscurso)
colnames(dfrUNPrfWords) <- "Words"
dfrUNPrfWords$Words <- as.character(dfrUNPrfWords$Words)

#Traere las 20 primeras palabras
head(dfrUNPrfWords,20)

dfrUNPrfFreq <- dfrUNPrfWords %>%
  group_by(Words) %>%
  summarise(Freq=n()) %>%
  arrange(desc(Freq))

head(dfrUNPrfFreq,30)

#Nube de palabras que no nos dice nada
wordcloud(dfrUNPrfFreq$Words[1:100], dfrUNPrfFreq$Freq[1:100], random.order=F, max.words=100, colors=brewer.pal(8, "Dark2"))

#Quitar stopwords para evitar el ruido
vcsCmnWords <- c("all", "don", "also","and","any","are","but","can","cant","cry","due","etc","few","for","get","had","has","hasnt","have","her","here","hers","herself","him","himself","his","how","inc","into","its","ltd","may","nor","not","now","off","once","one","only","onto","our","ours","out","over","own","part","per","put","see","seem","she","than","that","the","their","them","then","thence","there","these","they","this","those","though","thus","too","top","upon","very","via","was","were","what","when","which","while","who","whoever","whom","whose","why","will","with","within","without","would","yet","you","your","yours","the") #Vector de stopwords pequño
dfrUNPrfWords <- filter(dfrUNPrfWords, !(Words %in% vcsCmnWords))
dfrUNPrfWords <- filter(dfrUNPrfWords, !(Words %in% stopwords()))

#Quitar malas palabras
vcsBadWords <- c("arse","ass","asshole","bastard","bitch","bloody","bollocks","child-fucker","cunt","damn","fuck","goddamn","godsdamn","hell","motherfucker","shit","shitass","whore")
dfrUNPrfWords <- filter(dfrUNPrfWords, !(Words %in% vcsBadWords))
head(dfrUNPrfWords)

head(dfrUNPrfWords, 10)

dfrUNPrfFreq <- dfrUNPrfWords %>%
  group_by(Words) %>%
  summarise(Freq=n()) %>%
  arrange(desc(Freq))

#Las palabras que mas se repiten
head(dfrUNPrfFreq,10)

#Las palabras que menos se repiten
tail(dfrUNPrfFreq)

#Ver la longitud
intWordCountFinal <- length(dfrUNPrfFreq$Words)
intWordCountFinal

#Funcion para medir las frecuencias de las categorias
FreqCategory <- function(value) {
  strCategory <- ifelse(value <=5,   "      5",
                 ifelse(value <=10,     "     10",
                 ifelse(value <=20,     "     20",
                 ifelse(value <=50,     "     50",
                 ifelse(value <=100,    "    100",
                 ifelse(value <=500,    "    500",
                 ifelse(value <=1000,   "  1,000",
                                  ">1,000")))))))
  strCategory
}

#Discretizacion de los datos
dfrUNPrfFreq <- mutate(dfrUNPrfFreq, Fcat=FreqCategory(dfrUNPrfFreq$Freq))
dfrUNPrfFocf <- dfrUNPrfFreq %>% group_by(Fcat) %>% summarise(Rfrq=n())
dfrUNPrfFocf$Fcat <- factor(dfrUNPrfFocf$Fcat, levels=dfrUNPrfFocf$Fcat, ordered=T)
head(dfrUNPrfFocf,10)

#Nube de palabras con las palabras mas relevantes
wordcloud(dfrUNPrfFreq$Words[1:50], dfrUNPrfFreq$Freq[1:50], random.order=F, max.words=100, colors=brewer.pal(8, "Dark2"))

#Grafico para las palabras finales minadas
ggplot(slice(dfrUNPrfFreq,1:40), aes(x=reorder(Words,-Freq),y=Freq)) +
  geom_bar(stat="identity", fill=rainbow(40)) +
  ylab("Frequency") +
  xlab("Words") +
  ggtitle("Word Frequency - Top 40 Words") +
  theme(plot.title=element_text(size=rel(1.5), colour="blue")) +
  coord_flip()

#Stemming
stem_hunspell <- function(term) {
    # look up the term in the dictionary
    stems <- hunspell::hunspell_stem(term)[[1]]

    if (length(stems) == 0) { # if there are no stems, use the original term
        stem <- term
    } else { # if there are multiple stems, use the last one
        stem <- stems[[length(stems)]]
    }
    stem
}

#Stemming de las palabras
final <- text_tokens(dfrUNPrfFreq$Words, stemmer = stem_hunspell)
final

head(final, 5)

#Frecuencia de las palabras con stemming
#Convertirlo a Dataframe
dfrUNPrfWords2 <- data.frame(Words = matrix(unlist(final), nrow=length(final), byrow=T))
head(dfrUNPrfWords2)

#Obtener la frecuencia
dfrUNPrfFreq2 <- dfrUNPrfWords2 %>%
  group_by(Words) %>%
  summarise(Freq=n()) %>%
  arrange(desc(Freq))

dfrUNPrfFreq2$Words[1:30]

#Las palabras que mas se repiten
head(dfrUNPrfFreq2,30)

#Las palabras que menos se repiten
tail(dfrUNPrfFreq2)

#Ver la longitud
intWordCountFinal <- length(dfrUNPrfFreq2$Words)
intWordCountFinal

#Nube de palabras del stemming
wordcloud(dfrUNPrfFreq2$Words[1:100], dfrUNPrfFreq2$Freq[1:100], random.order=F, max.words=100, colors=brewer.pal(8, "Dark2"))

#Grafico con stemming
ggplot(slice(dfrUNPrfFreq2,1:40), aes(x=reorder(Words,-Freq),y=Freq)) +
  geom_bar(stat="identity", fill=rainbow(40)) +
  ylab("Frequency") +
  xlab("Words") +
  ggtitle("Word Frequency - Top 40 Words") +
  theme(plot.title=element_text(size=rel(1.5), colour="blue")) +
  coord_flip()