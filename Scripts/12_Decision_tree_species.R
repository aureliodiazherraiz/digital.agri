library(tidyverse)
library(hrbrthemes)#para temas en ggplot
library(ggthemes)#temas para ggtplot
library(readxl)
library(readr)
library(ggplot2)
library(ggpubr)


library(tree)#simple decision tree
library(rpart) #decision tree
library(caret) #machine learning
library(rpart.plot) #graficar decision tree
library(MASS)#trabaja con ramdomforest no categorico
library(ranger) #trabaja con los hiperparámetros de randomforest no categorico
library(ISLR)#random forest categorico
library(caTools)#entrenar modelos predictivos en decision tree


str(dt)
names(dt)
dt$latitud<-as.numeric(dt$latitud)
dt$longitud<-as.numeric(dt$longitud)

dtilex3<-dt %>% filter(Sp.x == "Quercus ilex" & Sp.y == "Quercus ilex")
dtilex3<-dtilex3 %>% filter(AB3_Mgha_perc>=80)

dtilex3<-dtilex3[, -c(42,47,76,97)]
names(dtilex3)
dtilex3<-dtilex3[, -c(3:6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,39,40,42:47,49:51,53,55,57,59,61:65,67,69,71,73,94:96,112)]
names(dtilex3)
dtilex3<-na.omit(dtilex3)
summary(dtilex3)
head(dtilex3)



####Arbol de decision con libreria tree####https://rpubs.com/Joaquin_AR/255596

#separamos los datos en training y test
set.seed(123)
split <- sample.split(ilex3, SplitRatio = 0.7)
train <- subset(ilex3, split==TRUE)
test <- subset(ilex3, split==FALSE)

# Selección de parámetros para el árbol
setup <- tree.control(nobs = nrow(train), 
                      mincut = 5, 
                      minsize = 10,
                      mindev = 0.01)

# Ajuste del árbol de regresión
modelo_arbolR <- tree(AB3_Mgha ~., data = train, 
                      split = "deviance", 
                      control = setup)


summary(modelo_arbolR)

plot(modelo_arbolR, type = "proportional") 
# pretty = 0 incluye los nombres de los niveles para las variables cualitativas, en lugar de mostrar solo una letra
text(modelo_arbolR, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")

# Pruning (const complexity pruning) por validación cruzada
# ==============================================================================

# El árbol se crece al máximo posible para luego aplicar el pruning
modelo_arbolR <- tree(
  formula = AB3_Mgha ~ .,
  data    = train,
  split   = "deviance",
  mincut  = 1,
  minsize = 2,
  mindev  = 0
)

# Búsqueda por validación cruzada
set.seed(123)
cv_arbol <- cv.tree(modelo_arbolR, K = 5)


# Tamaño óptimo encontrado
# ==============================================================================
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
paste("Tamaño óptimo encontrado:", size_optimo)


resultados_cv <- data.frame(
  n_nodos  = cv_arbol$size,
  deviance = cv_arbol$dev,
  alpha    = cv_arbol$k
)

p1 <- ggplot(data = resultados_cv, aes(x = n_nodos, y = deviance)) +
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = size_optimo, color = "red") +
  labs(title = "Error vs tamaño del árbol") +
  theme_bw() 

p2 <- ggplot(data = resultados_cv, aes(x = alpha, y = deviance)) +
  geom_line() + 
  geom_point() +
  labs(title = "Error vs penalización alpha") +
  theme_bw() 

ggarrange(p1, p2)#los graficos indican que cuanto mayor el numero de nodos, mayor la desviance, o sea que peor


# Estructura del árbol creado final
# ==============================================================================
arbol_final <- prune.tree(tree = cv_arbol, best = size_optimo)
par(mar = c(1,1,1,1))
plot(x = arbol_final, type = "proportional")
text(x = arbol_final, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")
##de aqui ya no puedo pasar, no lo reconoce como arbol y por lo tanto no lo grafica##




# Predicciones del modelo sobre los datos de test
predicciones <- predict(modelo_arbolR, newdata = test)

plot(x = predicciones, y = test$AB3_Mgha, 
     main = "Predicciones modelo vs valor real", 
     xlab = "Predicción", 
     ylab = "Biomasa",
     col = "darkgrey", pch = 19)
abline(a = 0, b = 1, col = "blue")

# Error MSE en test
testMSE <- mean((predicciones - test$AB3_Mgha)^2)
testMSE





####arbol de decision con rpart####


arbol_train <- rpart(AB3_Mgha ~., 
                    method = "class",
                    data=train)

print(arbol_train)

rpart.plot(arbol_train, extra = 100, box.palette="Blues")

prp(arbol_train)
par(xpd = NA)
plot(arbol_train)
text(arbol_train)






####RANDOM FOREST####

# División de los datos en train y test
# ==============================================================================
set.seed(123)
train       <- sample(1:nrow(dtilex3), size = nrow(dtilex3)/2)
datos_train <- dtilex3[train,]
datos_test  <- dtilex3[-train,]

# Creación y entrenamiento del modelo
# ==============================================================================
set.seed(123)
modelo  <- ranger(
  formula   = AB3_Mgha ~ .,
  data      = datos_train,
  num.trees = 100,
  seed      = 123
)

print(modelo)

# Error de test del modelo
# ==============================================================================
predicciones <- predict(
  modelo,
  data = datos_test
)

predicciones <- predicciones$predictions
test_rmse    <- sqrt(mean((predicciones - datos_test$AB3_Mgha)^2))
paste("Error de test (rmse) del modelo:", round(test_rmse,2))#"Error de test (rmse) del modelo: 28.37"





