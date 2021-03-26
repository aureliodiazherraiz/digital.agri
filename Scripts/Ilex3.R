library(tidyverse)
library(ggpubr)
library(corrplot)#correlaciones


library(mgcv)
library(glmulti)
library(rsq) 
library(performance)

library(sp)
library(raster)
library(usdm)# Analisis de incertidumbre para modelos de distribucion de especies (vif)

library(rpart)#decision tree
library(rpart.plot)#decision tree
library(caTools)#crea grupo test y training

library(FactoMineR)#PCA

library(pls)

#para radom forest (https://www.cienciadedatos.net/documentos/33_arboles_decision_random_forest_gradient_boosting_c50#Random_Forest)

library(tidymodels)#infierno de paquete, reiniciar sesion de r y correrlo de primeras, si hace falta reinstalarlo
library(ranger)
library(doParallel)

path2csv <- "C:/Users/Diego/Documents/digital.agri/CSV/"
path2grafic <- "C:/Users/Diego/Documents/digital.agri/Graficos/"
getwd()
setwd("C:/Users/Diego/Documents/digital.agri")


#### ILEX ####

str(ilex3)
names(ilex3)

write.csv(ilex3, file = "ilex3.csv")

lm(log(AB3_Mgha) ~ mean_ndvi, data = ilex3) %>% summary()
plot(ilex3$mean_ndvi, log(ilex3$AB3_Mgha))
abline(a=1.96582, b=2.80590, lwd=3, col='blue')

plot(ilex3$mean_ndvi,(ilex3$AB3_Mgha))
points(ilex3$mean_ndvi[ilex3$Provincia_3 == "Sevilla"], 
       ilex3$AB3_Mgha[ilex3$Provincia_3 == "Sevilla"], col="blue", pch=19)

points(ilex3$mean_ndvi[ilex3$Provincia_3 == "Cordoba"], 
       ilex3$AB3_Mgha[ilex3$Provincia_3 == "Cordoba"], col="red", pch=19)

points(ilex3$mean_ndvi[ilex3$Provincia_3 == "Jaen"], 
       ilex3$AB3_Mgha[ilex3$Provincia_3 == "Jaen"], col="yellow", pch=19)

points(ilex3$mean_ndvi[ilex3$Provincia_3 == "Cadiz"], 
       ilex3$AB3_Mgha[ilex3$Provincia_3 == "Cadiz"], col="green", pch=19)

plot(ilex3$AB3_Mgha, ilex3$Tree_dens)


names(ilex3)
cor_ilex3 <- round(cor(ilex3[, -c(1:3,29:32,40:43)], use="complete.obs"),2)

cor_ilex3_plot<-corrplot(cor_ilex3, method = "shade", shade.col = NA, tl.col = "black",
            tl.srt = 55, tl.cex = 0.5, number.cex = 0.5, 
            addCoef.col = "black", type = "lower", diag = F, 
            addshade = "all", order = "FPC")

ilex3.nor<- scale(ilex3[,-c(1:3,29:32,40:43)],center=T,scale=T) %>% as.data.frame()

### Calcular el coeficiente de correlacion de Spearman entre variables
ilex3cor<-cor(ilex3[, -c(1:3,29:32,40:43)], method = c("spearman"))

### Transformar las correlaciones en un dataframe
cor_ilex3<-as.data.frame(ilex3cor)

head(cor_ilex3)

#### CLUSTER Exportar el dataframe (solo la diagonal superior)####
lower<-ilex3cor
lower[lower.tri(ilex3cor, diag=TRUE)]<-""
lower.df<-as.data.frame(lower)
lower.df


### Transformar la matriz de correlacion en un matriz de distancias
ilex3dist <- abs(as.dist(cor_ilex3))

### Calcular el dendrograma basando en distancias (less distance = more correlation)
ilex3.cluster <- hclust(1-ilex3dist)

### Plot del dendrograma
plot(ilex3.cluster)

### Seleccionar variables con una correlacion < 0.8
abline(h=0.2, lty=2, lwd=2, col="red")
abline(h=0.8, lty=2, lwd=2, col="blue")




#### Calcular VIF basandonos en las funciones vifcor y vifstep ####

v1 <- vifcor(ilex3.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(ilex3.nor, th=3)
v2

re1 <- exclude(ilex3.nor,v2)
names(re1)

cor_ilex3v2 <- round(cor(re1, use="complete.obs"),2) %>% corrplot(type = "lower", 
                         method = "shade", shade.col = NA, tl.col = "black",
                         tl.srt = 55, tl.cex = 0.8, number.cex = 0.8, 
                         addCoef.col = "black", diag = F, 
                         addshade = "all", order = "FPC")


X11()
plot(re1)

#### GLM ####
#con las variables obtenidas en el vistep que mayor correlacion tengan con la biomasa montamos un modelo predictivo glm

#dividimos la muestra en dos grupos para poder testar el poder prdictivo del modelo

model_ilex3_vif <- glm(AB3_Mgha ~ aet_mean + mean_ndvi + pet_max +
            Tree_dens, data = ilex3.nor)
model_ilex3_vif$residuals


model_ilex3_vif2 <- glm(AB3_Mgha ~ aet_mean + mean_ndvi + pet_max +
                               Tree_dens, data = ilex3)#sin normalizar las variables para que pueda comparar la biomasa com los predichos

summary(model_ilex3_vif)
rsq(model_ilex3_vif) #r squared = 0.2187

prediccion_model<-predict.glm(model_ilex3_vif, ilex3.nor)
prediccion_model2<-predict.glm(model_ilex3_vif2, ilex3)

model_mse <- mean((prediccion_model - ilex3.nor$AB3_Mgha)^2)
paste("Error (mse) del modelo:", model_mse)#"Error (mse) del modelo: 0.780262171650025"


model_mse2 <- mean((prediccion_model2 - ilex3$AB3_Mgha)^2)#cuando queremos ver el error medio real entre los valores de la biomasa real y los predichos
paste("Error (mse) del modelo2:", model_mse2)#"Error (mse) del modelo: 654.142444466188"


#introducimos dentro del dataframe
ilex3.nor$prediccion<-prediccion_model
ilex3$prediccion<-prediccion_model2


r2model<-lm(prediccion ~ AB3_Mgha, data = ilex3.nor)#Adjusted R-squared:  0.2189 
summary(r2model)

sink("ilex3_vif_glm.doc")
print(summary(glm(AB3_Mgha ~ aet_mean + mean_ndvi + pet_max +
                          Tree_dens, data=ilex3.nor)))
sink()  # returns output to the console


names(ilex3.nor)
ggplot(ilex3.nor[, c(32,49)],aes(x=AB3_Mgha,y=prediccion))+
        geom_point()+
        geom_smooth(method="glm", se=T)+
        labs(subtitle = "GLM", 
             y = "Biomass_prediction", 
             x = "Qilex_biomass", 
             title = "Q. ilex biomass prediction") +
        stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
                 label.x = 6, label.y = -2, size = 5.5) +
        stat_regline_equation(label.x = 6, label.y = -2.5, size = 5.5) 



#ahora graficamos la prediccion de la biomasa en un mapa par poderlo visualizarlo
names(ilex3)
names(ilex3.nor)
ilex3.sp<-cbind(ilex3[, c(4:5)], ilex3.nor[, c(32,49)])
names(ilex3.sp)[4]<-"prediction"

#lo categorizamos en los quantiles
ilex3.sp<-ilex3.sp %>% mutate(AB3_Mgha_ = quantcut(AB3_Mgha, q = 10, na.rm = TRUE))
ilex3.sp<-ilex3.sp %>% mutate(prediction_ = quantcut(prediction, q = 10, na.rm = TRUE))
head(ilex3.sp)

#reformulamos el dataframe como objeto espacial para poderlo graficarlo en tmap
ilex3.sp<-st_as_sf(ilex3.sp, coords = c("X", "Y"), crs = ("+proj=longlat + datum=WGS84"))

st_transform(ilex3.sp, 4326)
extent(ilex3.sp)
crs(ilex3.sp)

#•cargamos el shape de andalucia
anda_shp<-readOGR("~/digital.agri/Andalucia_edafo/Andalucia_contorno.geojson")
anda_shp<-readOGR("~/digital.agri/anda_comunidad.shp")
extent(anda_shp)
crs(anda_shp)


tmap_mode("view") 
tmap_mode("plot")
ilex3.map.biom <- tm_shape(anda_shp) + tm_fill() + tm_borders() + tm_shape(ilex3.sp) + 
        tm_dots(col = "AB3_Mgha_", size = 0.25, n = 5, palette ="RdYlGn") + 
        tm_layout(title = "Q. ilex real biomass", 
                  legend.position = c(0.05,0.003), scale = 1, legend.frame = F)
tmap_save(ilex3.map.biom, paste(path2grafic, filename = "ilex3.map.biom.png"))


tmap_mode("plot")
ilex3.map.biom.predict <- tm_shape(anda_shp) + tm_fill() + tm_borders() + 
        tm_shape(ilex3.sp) + tm_dots(col = "prediction_", size = 0.25, n = 5, palette ="RdYlGn") + 
        tm_layout(title = "Q. ilex biomass prediction", 
                  legend.position = c(0.05,0.003), scale = 1,legend.frame = F)
tmap_save(ilex3.map.biom.predict, paste(path2grafic, filename = "ilex3.map.biom.predict.png"))



#### PODER PREDICTIVO GLM ####
set.seed(1649)
data_train <- ilex3.nor %>% sample_frac(.8)
data_test <- setdiff(ilex3.nor, data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)

ilex3_vifglm <- glm(AB3_Mgha ~ aet_mean + mean_ndvi + pet_max +
                          Tree_dens, data=data_train) 

summary(ilex3_vifglm) 
rsq(ilex3_vifglm) #r cuadrado de 0.2105

# Predicciones de entrenamiento
prediccion_train<-predict.glm(ilex3_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.824358350441326"

# Predicciones de test
prediccion_test<-predict.glm(ilex3_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.696441794846627"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.2318
summary(r2test)


names(data_test)
ggpairs(data_test[, c(32,50)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

ggplot(data_test[, c(32,50)],aes(x=AB3_Mgha,y=prediccion))+
        geom_point()+
        geom_smooth(method="glm", se=T)+
        labs(subtitle = "GLM", 
             y = "Biomass_prediction", 
             x = "Qilex_biomass", 
             title = "Q. ilex biomass prediction") +
        stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
                 label.x = 7, label.y = -4, size = 5.5) +
        stat_regline_equation(label.x = 7, label.y = -4.5, size = 5.5) 



#### GAM ####

ilex3_vifgam <- gam(AB3_Mgha ~ s(aet_mean) + s(mean_ndvi) + 
                   s(cic) + (Tree_dens) + s(n_sup), data=ilex3.nor) 
summary(ilex3_vifgam)#r cuadrado de 0.26

sink("ilex3_vif_gam.doc")
print(summary(gam(AB3_Mgha ~ s(aet_mean) + s(mean_ndvi) + 
                          s(cic) + (Tree_dens) + s(n_sup), data=ilex3.nor)))
sink()  # returns output to the console



#utilizamos la division anterior para estudiar el poder predictivo del modelo GAM
# Predicciones de entrenamiento
prediccion_train<-predict.gam(ilex3_vifgam,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.760626919312458"

# Predicciones de test
prediccion_test<-predict.gam(ilex3_vifgam,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.647680914375801" es un modelo que comete un error inferior al del grupo de entrenamiento, por lo que es válido






#### SELECCION DE VARIABLES STEP ####
model_step_ilex3 <- step(object    = lm(formula = AB3_Mgha ~ ., data = ilex3.nor),
                         direction = "backward", scope = list(upper = ~., lower = ~1),
                         trace     = FALSE)
var_step_ilex3.norm<-summary(model_step_ilex3) #variables seleccionadas con significando elevada: aet_mean, pet_mean, soil_mean, srad_max, mean_ndvi, Tree_dens, cod_hid

ilex3_modelo_step <- glm(AB3_Mgha ~ aet_mean + pet_mean + 
                                 mean_ndvi + Tree_dens + cod_hid, data = ilex3.nor) 
summary(ilex3_modelo_step)
rsq(ilex3_modelo_step) # r 0.2388889

sink("ilex3_modelo_step.doc")
print(summary(glm(AB3_Mgha ~ aet_mean + pet_mean + 
                          mean_ndvi + Tree_dens + cod_hid, data = ilex3.nor)))
sink()  # returns output to the console



#todas las variables trabajadas para el ifn3

ilex_formula <- as.formula(AB3_Mgha ~ aet_max + aet_min + aet_mean + aet_stdv + pet_max + pet_min +
                             pet_mean +   pet_stdv + soil_max + soil_min  + soil_mean + soil_stdv +
                             srad_max  + srad_min +  srad_mean + max_evi + min_evi + mean_evi +
                             st_d_evi  + max_ndvi  + min_ndvi +  mean_ndvi + st_d_ndvi + 
                             longitud  + latitud + slope + aspect  + elevation + hillshade + 
                             Tree_dens + tf + mo + mo_sup + cod_hid + textura + psb + ps + arc + 
                             are + lim + ca  + cic + crad  + n_sup + ph)   



ilex3_modelo <- glm(AB3_Mgha ~ aet_max + aet_min + aet_mean + aet_stdv + pet_max + pet_min +
                                   pet_mean +   pet_stdv + soil_max + soil_min  + soil_mean + soil_stdv +
                                   srad_max  + srad_min +  srad_mean + max_evi + min_evi + mean_evi +
                                   st_d_evi  + max_ndvi  + min_ndvi +  mean_ndvi + st_d_ndvi + 
                                   longitud  + latitud + slope + aspect  + elevation + hillshade + 
                                   Tree_dens + tf + mo + mo_sup + cod_hid + textura + psb + ps + arc + 
                                   are + lim + ca  + cic + crad  + n_sup + ph, data = ilex3) 
summary(ilex3_modelo)



#### VARIABLES CLIMATICAS EN ILEX MULTIMODEL ####


hist(ilex3.nor$AB3_Mgha)

ilex_clim <- as.formula(AB3_Mgha ~ aet_max + aet_min + aet_mean + aet_stdv + pet_max + pet_min +
                             pet_mean + pet_stdv + soil_max + soil_min + soil_mean + soil_stdv +
                             srad_max + srad_min + srad_mean + Arid_Dm) 


mumodel_ilex3_clim <- glmulti(ilex_clim, data=ilex3.nor, fitfunction=glm,
                              level=1, confsetsize=100, method= "h", crit="aicc")

weightable(mumodel_ilex3_clim)
cumsum(weightable(mumodel_ilex3_clim)$weights) ## suma acumulada a partir del mejor modelo (primero [1])
sum(weightable(mumodel_ilex3_clim)$weights[1:10]) ## suma de weights de los diez mejores modelos
plot(mumodel_ilex3_clim, type="w") ## para visualizar la evoluci?n de los weights de los modelos (l?nea roja =cumsum95%)

jpeg(filename = "mumodel_ilex3_clim.jpeg", width = 600, height = 600)
plot(mumodel_ilex3_clim, type="s") ## para visualizar el peso de las variables  
dev.off()


##probamos los modelos ##
glm.ilex3.clim<- glm(AB3_Mgha ~ aet_mean + soil_mean + srad_mean, data = ilex3.nor)

summary(glm.ilex3.clim)

rsq(glm.ilex3.clim) #r squared 0.068

gam.ilex3.clim<- gam(AB3_Mgha ~ s(aet_mean) + s(soil_mean) + s(srad_mean), data = ilex3.nor)
summary(gam.ilex3) #r squared 0.145





#### VARIABLES TOPOGRAFICAS EN ILEX MULTIMODEL ####

ilex_topoedafo <- as.formula(AB3_Mgha ~ X + Y + slope + aspect + elevation + hillshade + 
                             tf + mo + mo_sup + cod_hid + textura + psb + ps + arc + 
                             are + lim + ca  + cic + crad  + n_sup + ph)           


mumodel_ilex3_topoedafo <- glmulti(ilex_topoedafo, data=ilex3.nor, fitfunction=glm,
                              level=1, confsetsize=100, method= "h", crit="aicc")

weightable(mumodel_ilex3_topoedafo)
cumsum(weightable(mumodel_ilex3_topoedafo)$weights) ## suma acumulada a partir del mejor modelo (primero [1])
sum(weightable(mumodel_ilex3_topoedafo)$weights[1:10]) ## suma de weights de los diez mejores modelos
plot(mumodel_ilex3_topoedafo, type="w") ## para visualizar la evoluci?n de los weights de los modelos (l?nea roja =cumsum95%)

jpeg("mumodel_ilex3_topoedafo.jpeg", width = 600, height = 600)
plot(mumodel_ilex3_topoedafo, type="s") ## para visualizar el peso de las variables  
dev.off()

##probamos los modelos ##
glm.ilex3.topedaf<- glm(AB3_Mgha ~ n_sup + cic + cod_hid + mo + elevation + X + ca, data = ilex3.nor)

summary(glm.ilex3.topedaf)

rsq(glm.ilex3.topedaf) #r squared 0.103




#### VARIABLES ESPECTRALES EN ILEX MULTIMODEL ####

ilex_spect <- as.formula(AB3_Mgha ~ max_evi + min_evi + mean_evi +
                           st_d_evi  + max_ndvi  + min_ndvi +  mean_ndvi + st_d_ndvi)           


mumodel_ilex3_spect <- glmulti(ilex_spect, data=ilex3.nor, fitfunction=glm,
                                   level=1, confsetsize=100, method= "h", crit="aicc")

weightable(mumodel_ilex3_spect)
cumsum(weightable(mumodel_ilex3_spect)$weights) ## suma acumulada a partir del mejor modelo (primero [1])
sum(weightable(mumodel_ilex3_spect)$weights[1:10]) ## suma de weights de los diez mejores modelos
plot(mumodel_ilex3_spect, type="w") ## para visualizar la evoluci?n de los weights de los modelos (l?nea roja =cumsum95%)
jpeg("mumodel_ilex3_spect", width = 600, height = 600)
plot(mumodel_ilex3_spect, type="s") ## para visualizar el peso de las variables 
dev.off()


##probamos los modelos ##
glm.ilex3.spect<- glm(AB3_Mgha ~ mean_ndvi + st_d_ndvi, data = ilex3.nor)

summary(glm.ilex3.spect)

rsq(glm.ilex3.spect) #r squared 0.108




#### ARBOL DE DECISIONES ####
#de alguna forma sirve para ver cuales variables con mas relevantes
set.seed(1649)
data_train <- ilex3[, -c(1:3,29,31:32,39:42)] %>% sample_frac(.8)
data_test <- setdiff(ilex3[, -c(1:3,29,31:32,39:42)], data_train)

head(data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


#creamos el CART model
ilex.tree <- rpart(AB3_Mgha ~ .,data=data_train)

rpart.plot(ilex.tree, extra = 100)

prp(ilex.tree)
par(xpd = NA)

jpeg("ilex.tree.jpg", width=1200, height=820, units="px",
     pointsize=5, bg="white",res=300)
plot(ilex.tree)
text(ilex.tree)
dev.off()

printcp(ilex.tree)
plotcp(ilex.tree)





#### CALCULO DE COMPONENTES PRINCIPALES. Se excluye la columna con la variable respuesta *AB_Mgha* ####

names(ilex3)

apply(X = ilex3[, -c(1:5,29:30,39:42)], MARGIN = 2, FUN = var)#varianza de las variables

pca <- prcomp(ilex3[, -c(1:5,29:30,39:42)], scale. = TRUE)#a función prcomp() centra las variables para que tengan media de 0. Con el argumento scale = TRUE indicamos que queremos escalar las variables para que tengan desviación estándar igual a 1.

# Se muestra la proporci?n de varianza explicada y acumulada de las 9 primeras
# componentes
summary(pca)$importance[, 1:9]#el primer componente apenas consigue explicar el 21%, ya el segundo apenas el 18%
plot(pca)


head(pca$x[, 1:9], 5)

pca$sdev#la desviación estándar de cada componente principal

pca$sdev^2#La varianza explicada por cada componente principal (correspondiente a los eigenvalores) la obtenemos elevando al cuadrado la desviación estándar

#sacamos el primer componente y los relacionamos con la variable dependiente (AB3_Mgha) parar ver las posibles relaciones
pc1<-pca$x[,1:6] %>% as.data.frame()
head(pc1)
names(pc1)[1:6]<-c("pc1","pc2","pc3","pc4","pc5","pc6")
pc1$AB3_Mgha<-ilex3.nor$AB3_Mgha

pc1_AB3_lm<-lm(AB3_Mgha~ pc1 + pc2 + pc3 + pc4 + pc5 + pc6, data = pc1)
summary(pc1_AB3_lm)#la correlación es bajísima

plot(AB3_Mgha ~ pc1, pc1)#la transformaciojn logaritmica arregla algo el gráfico
abline(a=--8.343e-17, b=1.242e-02, lwd=3, col='blue')

##FRACASO, incluso habiendo inserido las 4 componentes principales dentro, la relacion es baja, nada mejor que el modelo glm inicial.




fviz_pca_ind(pca, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5) 

biplot(pca, scale = 0, cex = 0.5, col = c("dodgerblue3", "deeppink3"))

fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 20))
