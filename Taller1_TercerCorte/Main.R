# Title     : TODO
# Objective : TODO
# Created by: adria
# Created on: 13/11/2020

library(dplyr)
library(mice)#Libreria que ayuda a identificar si hay datos faltantes
library(ggplot2)
library(corrplot)
library(corrgram)
library(GGally)
library(Hmisc)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Leer el dataset
dataset <- read.csv("credit (1).csv")

# Limpieza de los datos
#Verificar que no hayan datos nulos
any(is.na(dataset)|is.null(dataset))
lapply(dataset, function(x) all(x == 0))

#Verificar si hay valores negativos
lapply(dataset, function(x) all(x < 0))

# Ver si hay valores perdidos
md.pattern(dataset)

#Eliminar datos duplicados
cleanDataset <- distinct(dataset)
str(cleanDataset)

#Agregar el id
cleanDataset$id <- 1:1000

#Análisis de los datos
summary(cleanDataset)
describe(cleanDataset)

#Correlacion de los datos Pearson
columnas.numericas <- sapply(cleanDataset, is.numeric)
datos.correlacion <- cor(cleanDataset[,columnas.numericas], method = 'pearson')
corrplot(datos.correlacion, method = "color")

#Graficos de distribución
for (title in colnames(cleanDataset)){
  columnData <- cleanDataset[, title]
  if(title == "age" | title == "months_loan_duration"){
    graph <- ggplot(cleanDataset,aes(x=columnData,fill = default)) + geom_histogram() + ggtitle(title) + xlab(title)
  }else if(title == "amount"){
    graph <- ggplot(cleanDataset, aes(x=columnData)) + geom_freqpoly(binwidth = 100) + ggtitle(title) + xlab(title)
  }
  else{
    graph <- ggplot(cleanDataset,aes(x=factor(columnData),fill = default)) + geom_bar() + ggtitle(title) + xlab(title)
  }
  print(graph)
}

#Dependencia de las variables, ver las mas relevantes segun el credito aprobado
for (title in colnames(cleanDataset)){
  columnData <- cleanDataset[, title]
  graph <- ggplot(cleanDataset,aes(x=columnData, fill = default)) + geom_bar(position = "dodge") + ggtitle(title)
  print(graph)
}


#Gráfico de una variable vs todas
posn.j <- position_jitter(0.5,0)
for (col1 in colnames(cleanDataset)) {
  for (col2 in colnames(cleanDataset)) {
    if( ( col1 != col2 ) && ( col1 != "default" && col2 != "default" )){
      columnData1 <- cleanDataset[, col1]
      columnData2 <- cleanDataset[, col2]
      plot_title <- paste0(col1," vs ", col2)
      print(plot_title)
        print(ggplot(cleanDataset,aes(x=columnData1,y=columnData2, col = factor(default)))+
                geom_jitter(size=3,alpha=0.5,position = posn.j) + ggtitle(plot_title) + xlab(col1) + ylab(col2)
        )
      }
      
    }
  }


# Calcular la distribucion por checking_balance y credit_history
ggplot(cleanDataset,aes(x=factor(checking_balance),fill=factor(credit_history)))+
  geom_bar(position = "dodge")+
  facet_grid(". ~ default")

ggplot(cleanDataset, aes(x=factor(checking_balance), y=credit_history), col=factor(purpose)) +
  geom_jitter(size=3,alpha=0.5) +
  facet_grid(". ~ default")

ggplot(cleanDataset, aes(x=factor(credit_history), y=amount)) +
  geom_jitter(size=3,alpha=0.5) +
  facet_grid(". ~ default")

# Calcular y agrupar 
posn.j <- position_jitter(0.5,0)
ggplot(cleanDataset,aes(x=months_loan_duration,y=amount,col=factor(purpose)))+
  geom_jitter(size=3,alpha=0.5,position = posn.j)+
  facet_grid(". ~ default")

ggplot(cleanDataset,aes(y=months_loan_duration,x=amount,col=factor(checking_balance)))+
  geom_jitter(size=3,alpha=0.5,position = posn.j)+
  facet_grid(". ~ default")

ggplot(cleanDataset,aes(y=amount,x=percent_of_income))+
  geom_jitter(size=3,alpha=0.5,position = posn.j)+
  facet_grid(". ~ default")

ggplot(cleanDataset,aes(x=years_at_residence,y=age))+
  geom_jitter(size=3,alpha=0.5,position = posn.j)+
  facet_grid(". ~ default")

ggplot(cleanDataset,aes(x=existing_loans_count,y=age))+
  geom_jitter(size=3,alpha=0.5,position = posn.j)+
  facet_grid(". ~ default")

ggplot(cleanDataset,aes(x=dependents,y=age))+
  geom_jitter(size=3,alpha=0.5,position = posn.j)+
  facet_grid(". ~ default")

ggplot(cleanDataset,aes(x=job,y=age))+
  geom_jitter(size=3,alpha=0.5,position = posn.j)+
  facet_grid(". ~ default")

ggplot(cleanDataset,aes(x=phone,y=age))+
  geom_jitter(size=3,alpha=0.5,position = posn.j)+
  facet_grid(". ~ default")

ggplot(cleanDataset,aes(x=factor(job),fill=factor(purpose)))+
  geom_bar(position = "dodge")+
  facet_grid(". ~ default")

ggplot(cleanDataset,aes(x=factor(job),y=months_loan_duration))+
  geom_bar(stat="identity")+
  facet_grid(". ~ default")

#Creación del árbol de decisión

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row <- nrow(data)
  total_row <- size * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(cleanDataset, 0.8, train = TRUE)
data_test <- create_train_test(cleanDataset, 0.8, train = FALSE)

str(data_train)
str(data_test)

table(data_train$default)

prop.table(table(data_train$credit_history, data_train$default),1)

#Creacion del arbol
my_tree_two <- rpart(default ~ checking_balance + credit_history + savings_balance + percent_of_income + age + other_credit + housing + existing_loans_count + job, data = data_train, method = "class")
plot(my_tree_two)
text(my_tree_two)

fancyRpartPlot(my_tree_two)

# Hacer predicciones basado en el test de prueba
my_prediction <- predict(my_tree_two,data_test,type="class")
my_solution <- data.frame(id = data_test$id, default = my_prediction)

nrow(my_solution)
ncol(my_solution)

my_tree_three <-  rpart(default ~ checking_balance + credit_history + savings_balance + percent_of_income + age + other_credit + housing + existing_loans_count + job, data = data_train, method = "class", control = rpart.control(minsplit = 50, cp =0))
fancyRpartPlot(my_tree_three)
