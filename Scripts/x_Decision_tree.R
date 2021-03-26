#Arbol de decisiones
#https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-decision-trees-c24dfd490abb

url <- "https://raw.githubusercontent.com/parulnith/A-guide-to-Machine-Learning-in-R/master/Decision%20Trees/boston.csv"
boston <- read_csv(url)
write.csv(boston, file = "boston.csv")

skim(boston)
str(boston)

plot(boston$LON, boston$LAT)

points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="blue", pch=19)

summary(boston$NOX)

points((boston$LON[boston$NOX>=0.55]),boston$LAT[boston$NOX>=0.55], col="green", pch=20)

summary(boston$MEDV)

points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

#realizamos un análisis de regresion linear en funcion del precio de las viviendas
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch = 20)

#creamos el modelo para poder estudiar su comportamiento
latlonlm <- lm(MEDV~LAT+LON, data = boston)
summary(latlonlm)

#comparamos los puntos de la gráfica con los puntos ajustados del modelo
#por lo que se ve no hay relacion con la latitud pudiendo dibujar
#una linea horizontal, aun asi el modelo lineal no consigue compreender
#muchos puntos del gráfico

latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values>=21.2], 
       boston$LAT[latlonlm$fitted.values>=21.2], col="blue", pch="$")

#Cart model
latlontree <- rpart(MEDV~LAT+LON, data = boston, minbucket=1)
plot(latlontree)
text(latlontree)

#graficar el modelo del decision tree
prp(latlontree)
#el árbol nos da los valores predichos (arbol de regresion)
#donde cada unoes el promedio de la mediana de los precios de las
#vivienda en ese segmento

#asi podemos comparar con los graficos anteriores 
#con los valores ajustados del modelo lineal

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2],
        boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>21.2],
        boston$LAT[fittedvalues>=21.2], col="blue", pch="$")

#El árbol de regresión ha hecho un trabajo mucho mejor y ha superpuesto los puntos rojos. 
#Ha dejado la zona de bajo valor en Boston.
#y ha logrado clasificar correctamente algunos de esos puntos
#en la parte inferior derecha y superior derecha.


#ahora pasaremos a entrenar el modelo para relizar predicciones
#dividimos el conjunto de los datos

set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split==TRUE)
test = subset(boston, split==FALSE)

#creamos el CART model
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + 
               INDUS + CHAS + NOX + RM + AGE + 
               DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)
