#librerias utilizadas

library(tidyverse)
library(ggpubr) #graficas y statr cor en ggplot
library(hrbrthemes)#para temas en ggplot
library(ggthemes)#temas para ggtplot
library(readxl)
library(readr)
library(gtools)# quancut: para realizar categorias de variables como factores
library(zoo)#para realizar interpolaciones


library(corrplot)#correlaciones
library(scales)#escala, normaliza las variables
library(agricolae)#ANOVA
library(nortest)#para los test de normalidad
library(car)#los test para la homocedasticidad
library(PerformanceAnalytics) #para poder graficar las correlaciones entre variables
#como pearson
library(psych) #para graficar correlaciones tb
library(devtools)#para usar la pìecewise (Ecuaciones SEM)


library(factoextra)#para poder aplicar el analisis de PCA
library(clusterSim)
library(GGally)#öpara matriz de correlaciones
library(Hmisc)
#library(ggbiplot) #graficas
library(devtools)

library(skimr) #resumen datos dataframe
library(mgcv) #GAM
library(performance)#metricas de los modelos
library(MuMIn)#compara desempeño de modelos

library(rJava)#hace falta para el glmulti
library(glmulti)#compara desempeño de modelos
library(rsq) #para obtener R2 de glm
library(lmodel2) #para diferentes regresiones lineales
library(lme) #para modelos LME
library(nlme)#para modelos lme (usado en el script)
library(visreg) #grafica resultados


library(flux) #para calcular el area debajo de una curva
library(DescTools) #calcula el area debajo de una curva


library(rpart) #decision tree
library(caret) #machine learning
library(rattle) #para random forest
library(rpart.plot) #graficar decision tree
library(MASS)#trabaja con ramdomforest no categorico
library(ranger) #trabaja con los hiperparámetros de randomforest no categorico
library(ISLR)#random forest categorico
library(caTools)#entrenar modelos predictivos en decision tree

library(tidymodels)#infierno de paquete, reiniciar sesion de r y correrlo de primeras, si hace falta reinstalarlo para RF
library(ranger) #random forest 
library(doParallel)


library(RSAGA)#para analisis de datos espaciales
library(stars)
library(sp) #para poder trabajar con raster
library(raster)#para poder trabajar con imagenes geolocalizadas
library(usdm)# Analisis de incertidumbre para modelos de distribucion de especies (vif)
library(sf)
library(rgdal)
library(tmap)#grafica variables en mapas 


library(downloader)#baja datos de los repositorios directamente de internet
library(rgl) #grafica resultados en 3d
library(magick)# hace girar los gráficos en 3D


#para graficar graficos grandes
X11()
plot()

### Eliminamos algunos objetos y ganamos memoria RAM
rm(var.cor, var.df, var.df2)
gc()

options(max.print=999999) # Dar mas memoria RAM

capture.output(modelo.rf, file="RF_ilex3.doc")#para salvar documentos en word

#### R cuadrado en ggplot ####

