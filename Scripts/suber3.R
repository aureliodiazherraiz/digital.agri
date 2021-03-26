library(tidyverse)
library(corrplot)#correlaciones

library(scales)
library(mgcv)
library(glmulti)
library(rsq) 
library(performance)
library(sp)
library(raster)
library(usdm)# Analisis de incertidumbre para modelos de distribucion de especies

library(rpart)#decision tree
library(rpart.plot)#decision tree
library(caTools)#crea grupo test y training

library(pls)

library(FactoMineR)#PCA
library(factoextra)#grafica PCA

#### SUBER ####
names(suber3)

cor_suber3<-round(cor(suber3[, -c(1:3,29:32,39:42)], use="complete.obs"),2)
corrplot(cor_suber3, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 55, tl.cex = 0.5, number.cex = 0.5, 
         addCoef.col = "black", type = "lower", diag = F, 
         addshade = "all", order = "FPC")


#las vriables que tienen una mayor correlacion con AB3_Mgha son soil_mean & stdv, aet_mean&stdv, mean_ndvi y tree_dens. Con ellas formulamos el modelo

lm(log(AB3_Mgha) ~ mean_ndvi, data = suber3) %>% summary()
plot(suber3$mean_ndvi, log(suber3$AB3_Mgha))
abline(a=1.2079, b=4.0416, lwd=3, col='blue')

lm(AB3_Mgha ~ Tree_dens, data = suber3) %>%  summary()
plot(suber3$Tree_dens, suber3$AB3_Mgha)
abline(a=44.7596, b=0.0742, lwd=3, col='blue')

names(suber3)
suber3.nor<- scale(suber3[,-c(1:3,29:32,39:42)],center=T,scale=T) %>% as.data.frame()

### Calcular VIF basandonos en las funciones vifcor y vifstep
#############################################################

v1 <- vifcor(suber3.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(suber3.nor, th=3)
v2

re1 <- exclude(suber3.nor,v2)
re1

X11()
plot(re1)

cor_suber3vif <- round(cor(re1, use="complete.obs"),2) %>% corrplot(method = "shade", 
                                                                    shade.col = NA, tl.col = "black", 
                                                                    tl.srt = 55, tl.cex = 0.8, 
                                                                    number.cex = 0.8, 
                                                                    addCoef.col = "black", 
                                                                    type = "lower", diag = F, 
                                                                    addshade = "all", order = "FPC")


suber3_vif <- glm(AB3_Mgha ~ Tree_dens + max_ndvi + cic +
                    ca + psb + textura, data = suber3.nor) 

summary(suber3_vif)
performance(suber3_vif)
rsq(suber3_vif) #r cuadrado de 0.37

#guardamos los datos del modelo
sink("suber3_vif_glm.doc")
print(summary(glm(AB3_Mgha ~ Tree_dens + max_ndvi + cic +
                    ca + psb + textura, data = suber3.nor)))
sink()  # returns output to the console

#podemos comparar los valores reales con los predichos del modelo
prediccion_model<-predict.glm(suber3_vif, suber3.nor)
suber3.nor$prediction<-prediccion_model


model_mse <- mean((prediccion_model - suber3.nor$AB3_Mgha)^2)
paste("Error (mse) del modelo:", model_mse)#"Error (mse) del modelo: 0.628269728562584"

r2model<-lm(prediction ~ AB3_Mgha, data = suber3.nor)#Adjusted R-squared:  0.3701 
summary(r2model)

names(suber3.nor)
ggplot(suber3.nor[, c(31,49)],aes(x=AB3_Mgha,y=prediction))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_prediction", 
       x = "Qsuber_biomass", 
       title = "Q. suber biomass prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 6, label.y = -2, size = 5.5) +
  stat_regline_equation(label.x = 6, label.y = -2.5, size = 5.5) 


#ahora vamos a graficar la prediccion de la biomasa en un mapa par poderlo visualizarlo
names(suber3)
names(suber3.nor)
suber3.sp<-cbind(suber3[, c(4:5)], suber3.nor[, c(31,49)])
head(suber3.sp)

suber3.sp<-st_as_sf(suber3.sp, coords = c("X", "Y"), crs = ("+proj=longlat + datum=WGS84"))

anda_shp<-readOGR("~/digital.agri/Andalucia_edafo/Andalucia_contorno.geojson")

tmap_mode("plot")
suber3.map.biom.predict <- tm_shape(anda_shp) + tm_fill() + tm_borders() + 
  tm_shape(suber3.sp) + tmap_style("white") +
  tm_dots(col = "prediction", size = 0.15, n = 6) + 
  tm_layout(title = "Q. suber biomass prediction", 
            legend.position = c(0.05,0.003), scale = 0.9,legend.frame = F)
tmap_save(suber3.map.biom.predict, filename = "suber3.map.biom.predict.png")




#### PODER PREDICTIVO GLM ####
set.seed(1649)
data_train <- suber3.nor %>% sample_frac(.8)
data_test <- setdiff(suber3.nor, data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


# Predicciones de entrenamiento
prediccion_train<-predict.glm(suber3_vif,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.633172013816434"

# Predicciones de test
prediccion_test<-predict.glm(suber3_vif,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.608598138053504"


data_test$prediccion<-prediccion_test
r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)
summary(r2test)#Adjusted R-squared:  0.4144

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


ggplot(data_test[, c(31,49)],aes(x=AB3_Mgha,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_prediction", 
       x = "Suber_biomass", 
       title = "Suber biomass prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
            label.x = 2, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 2, label.y = -4.5, size = 5.5) 




#### GAM ####
suber3_vifgam <- gam(AB3_Mgha ~ s(Tree_dens) + s(max_ndvi) + s(arc) + 
                       s(cic) + s(slope) + s(ca) + (psb), 
                     data = re1) 

summary(suber3_vifgam)#R-sq.(adj) = 0.45


sink("suber3_vif_gam.doc")
print(summary(gam(AB3_Mgha ~ s(Tree_dens) + s(max_ndvi) + s(arc) + 
                        s(cic) + s(slope) + s(ca) + (psb), 
                      data = re1)))
sink()  # returns output to the console




#### ARBOL DE DECISIONES ####
#de alguna forma sirve para ver cuales variables con mas relevantes
set.seed(1649)
data_train <- suber3[, -c(1:3,29:32,39:42)] %>% sample_frac(.8)
data_test <- setdiff(suber3[, -c(1:3,29:32,39:42)], data_train)

head(data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


#creamos el CART model
suber3.tree <- rpart(AB3_Mgha ~ .,data=data_train)

rpart.plot(suber3.tree, extra = 100)

prp(suber3.tree)
par(xpd = NA)

jpeg("quercus.tree.jpg", width=1200, height=820, units="px",
     pointsize=5, bg="white",res=300)
plot(suber3.tree)
text(suber3.tree)
dev.off()

printcp(suber.tree)
plotcp(suber.tree)




#### CALCULO DE COMPONENTES PRINCIPALES. Se excluye la columna con la variable 
# respuesta *AB_Mgha* ####

names(suber3)

pca <- prcomp(suber3[, -c(1:3,29:32,38:42)], scale. = TRUE)

# Se muestra la proporci?n de varianza explicada y acumulada de las 9 primeras
# componentes
summary(pca)$importance[, 1:9]


pca2 <- PCA(X = suber3[, -c(1:3,29:32,38:42)], scale.unit = TRUE, ncp = 64, graph = FALSE)

head(pca2$eig)

fviz_pca_ind(pca, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5) 

biplot(pca, scale = 0, cex = 0.5, col = c("dodgerblue3", "deeppink3"))





