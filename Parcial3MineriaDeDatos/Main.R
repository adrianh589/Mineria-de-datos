
# Package names
packages <- c("pROC", "dplyr","mice","ggplot2","corrplot","corrgram","GGally","Hmisc","rpart","rattle","rpart.plot","RColorBrewer","tmap","leaflet","mapview","shiny","stringr","ROCR","plotROC","sf","rgdal","sp","stringi","plotly","recipes","caret")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Leer el dataset
dataset <- read.csv(file="osb_enftransm-covid-19- (1).csv", sep = ";", fileEncoding = "latin1")

#--------------- 1 ----------------------------

# Limpieza de los datos
#Verificar que no hayan datos nulos
any(is.na(dataset)|is.null(dataset))
lapply(dataset, function(x) all(x == 0))

#Verificar si hay valores negativos
lapply(dataset, function(x) all(x < 0))

# Ver si hay valores perdidos
md.pattern(dataset)

date <- dataset$Fecha.de.diagnóstico #Fecha
data <- select(dataset, -Fecha.de.diagnóstico)
data <- mutate_if(data, is.character, function(x){str_replace_all(x, pattern="[[:digit:]]", "")})#Quitar los numeros
data <- mutate_if(data, is.character, function(x){str_to_lower(x)})#Convertir a minusculas
data <- mutate_if(data, is.character, function(x){str_replace_all(x, pattern="[[:punct:]]", "")})#Quitar signos de puntuacion
data$Localidad.de.residencia <- str_replace(data$Localidad.de.residencia, pattern=" .", "")

cleandata <- cbind(data, date)

cleandata$date <- as.Date(cleandata$date, "%d/%m/%Y")

str(cleandata)

#Agregar el id
cleandata$id <- 1:2345

#------------------ 02 ----------------------

#Gráfico de sexo vs ciudad
ggplot(cleandata, aes(x=Sexo, fill=Ciudad.de.residencia)) + geom_bar(position = "dodge")

#Grafico de sexo vs localidad
ggplot(cleandata, aes(x=Localidad.de.residencia, fill = factor(Sexo) ) ) + geom_bar(position = "dodge")

#Grafico de sexo vs edad
ggplot(cleandata, aes(x=Edad, fill = factor(Sexo) ) ) + geom_histogram()

#grafico de sexo vs tipo de caso
ggplot(cleandata, aes(x=Tipo.de.caso, fill = factor(Sexo) ) ) + geom_bar(position = "dodge")

#Gráfico sexo vs ubicación
ggplot(cleandata, aes(x=Ubicación, fill = factor(Sexo) ) ) + geom_bar(position = "dodge")

#Grafico de sexo vs estado
ggplot(cleandata, aes(x=Estado, fill = factor(Sexo) ) ) + geom_bar(position = "dodge")

#Grafico de sexo vs fecha
ggplot(cleandata, aes( x = date, fill = factor(Sexo) ) ) + geom_histogram()

#----------------------- 03 ---------------------------------------------------

#Grafico de localidad
ggplot(cleandata, aes(x=Localidad.de.residencia ) ) + geom_bar(position = "dodge")

# --------------------- 04 ----------------------------------------------------

plot(density(cleandata$Edad))

#-------------------------------- 05 -------------------------------------------

graph <- ggplot(cleandata, aes(fill=Ubicación, x=Localidad.de.residencia)) + geom_bar(position = "dodge")

ggplotly(graph)

# --------------------------- 06 -----------------------------------------------

#Origen del caso vs localidad
ggplot(cleandata,aes(x=factor(Ciudad.de.residencia),y=factor(Localidad.de.residencia)))+
  geom_jitter()+
  facet_grid(". ~ Estado")

#Caso vs Edad
ggplot(cleandata,aes(x=factor(Ciudad.de.residencia),y=factor(Edad)))+
  geom_jitter()+
  facet_grid(". ~ Estado")

#Caso vs Sexo
ggplot(cleandata,aes(x=factor(Ciudad.de.residencia),fill=factor(Sexo)))+
  geom_bar(position = "dodge")+
  facet_grid(". ~ Estado")

#Caso vs tipo de caso
ggplot(cleandata,aes(x=factor(Ciudad.de.residencia),fill=factor(Tipo.de.caso)))+
  geom_bar(position = "dodge")+
  facet_grid(". ~ Estado")

#Caso vs ubicacion
ggplot(cleandata,aes(x=factor(Ciudad.de.residencia),fill=factor(Ubicación)))+
  geom_bar(position = "dodge")+
  facet_grid(". ~ Estado")

#Caso vs estado
ggplot(cleandata,aes(x=factor(Ciudad.de.residencia),fill=factor(Estado)))+
  geom_bar(position = "dodge")+
  facet_grid(". ~ Sexo")

# ----------------------------- 7 ----------------------------------------------
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

data_train <- create_train_test(cleandata, 0.8, train = TRUE)
data_test <- create_train_test(cleandata, 0.8, train = FALSE)

str(data_train)
str(data_test)

table(data_train$Estado)

prop.table(table(data_train$Localidad.de.residencia, data_train$Estado),1)

#Creacion del arbol
my_tree_two <- rpart(Estado ~ Ciudad.de.residencia + Localidad.de.residencia + Ubicación + date, data = data_train, method = "class")
plot(my_tree_two)
text(my_tree_two)

fancyRpartPlot(my_tree_two)

# Hacer predicciones basado en el test de prueba
my_prediction <- predict(my_tree_two,data_test,type="class")
my_solution <- data.frame(id = data_test$id, Estado = my_prediction)

nrow(my_solution)
ncol(my_solution)

my_tree_three <-  rpart(Estado ~ Ciudad.de.residencia + Localidad.de.residencia + Ubicación + date, data = data_train, method = "class", control = rpart.control(minsplit = 50, cp =0))
fancyRpartPlot(my_tree_three)
# ----------------------------- 8 ----------------------------------------------

fancyRpartPlot(my_tree_two)     
###############################################################################################

# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)
# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- data.frame(parameter = "none")

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
objeto_recipe <- recipe(formula = Estado ~  Ciudad.de.residencia + Localidad.de.residencia + Ubicación + date,
                        data =  data_train)
objeto_recipe

trained_recipe <- prep(objeto_recipe, training = data_train)
datos_train_prep <- bake(trained_recipe, new_data = data_train)
datos_test_prep  <- bake(trained_recipe, new_data = data_test)
glimpse(datos_train_prep)

modelo_logistic <- train(Estado ~ Ciudad.de.residencia + Localidad.de.residencia + Ubicación + date, data = datos_train_prep,
                         method = "glm", tuneGrid = hiperparametros,
                         trControl = control_train, family = "binomial")

modelo_logistic

# Se obtienen las probabilidades predichas para cada clase
predicciones <- predict(object = modelo_logistic,
                        newdata = datos_test_prep,
                        type = "prob")
# Cálculo de la curva
curva_roc <- roc(response = datos_test_prep$Estado, 
                 predictor = predicciones$recuperado) 

# Gráfico de la curva
plot(curva_roc)

#========== MAPA ================
bogota <- st_read("localidades/poligonos-localidades.shp")
plot(bogota)
View(bogota)

#Normalizar la columna de localidad
cleandata$Localidad.de.residencia <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(cleandata$Localidad.de.residencia))

#Cuantificar los datos del dataset original
datos.mapa <- cleandata %>% count(Localidad.de.residencia)

#Arreglar nombre de LA CANDELARIA y SANTAFE
datos.mapa[15,1] <- "SANTA FE"
datos.mapa[10,1] <- "CANDELARIA"

#Unir las cantidades
datos.mapa.covid <- merge(bogota, datos.mapa, by.x="Nombre_de_l", by.y="Localidad.de.residencia")

#Graficar
ggplot(data=datos.mapa.covid) + geom_sf(aes(fill=n)) + geom_sf_label(aes(label=Nombre_de_l))  + coord_sf(crs = 3338)
