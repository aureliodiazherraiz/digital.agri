#vamos a modelar la respuesta del crecimiento en ilex entre ifns

names(ilex3_2)
cor_ilex3_2 <- round(cor(ilex3_2[,-(7:8)], use = "complete.obs"), 2) %>% 
  corrplot(method = "shade", 
           shade.col = NA, tl.col = "black",
           tl.srt = 55, tl.cex = 0.5, 
           number.cex = 0.5, addCoef.col = "black", 
           type = "lower", diag = F, addshade = "all", 
           order = "FPC")

ilex3_2.nor<- scale(ilex3_2[,-c(7:8)],center=T,scale=T) %>% as.data.frame()


#### Calcular VIF basandonos en las funciones vifcor y vifstep ####

v1 <- vifcor(ilex3_2.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(ilex3_2.nor, th=3)
v2

re1 <- exclude(ilex3_2.nor,v2)
names(re1)

cor_ilex3_2vif <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot(type = "lower", 
           method = "shade", shade.col = NA, 
           tl.col = "black", tl.srt = 55, tl.cex = 0.8, 
           number.cex = 0.8, addCoef.col = "black", diag = F, 
           addshade = "all", order = "FPC")

#### GLM ####
#con las variables obtenidas en el vistep que mayor correlacion tengan con la biomasa montamos un modelo predictivo glm


model_ilex3_2_vif <- glm(BP ~ Dens + n_sup + Arid_Dm + X, data = ilex3_2.nor)
summary(model_ilex3_2_vif)
rsq(model_ilex3_2_vif) #r squared = 0.3694


sink("ilex3_2_vif_glm.doc")
print(summary(glm(BP ~ Dens + n_sup + Arid_Dm + X, data = ilex3_2.nor)))
sink()  # returns output to the console

#estudiamos los valores predichos
prediccion_model<-predict.glm(model_ilex3_2_vif, ilex3_2.nor)
model_mse <- mean((prediccion_model - ilex3_2.nor$BP)^2)
paste("Error (mse) del modelo:", model_mse)#"Error (mse) del modelo: 0.630242826093649"

#introducimos dentro del dataframe
ilex3_2.nor$prediction<-prediccion_model

#ahora graficamos la prediccion de la biomasa en un mapa par poderlo visualizarlo
names(ilex3_2.nor)
names(ilex3_2.nor)
ilex3_2.sp<-cbind(ilex3_2[, c(1:2)], ilex3_2.nor[, c(25,36)])
getwd()
path2csv<-"./CSV/"

write.csv(ilex3_2.sp, paste(path2csv,"ilex3_2_latlon_BP_prediction.csv"))

#lo categorizamos en los quantiles
ilex3_2.sp<-ilex3_2.sp %>% mutate(BP_ = quantcut(BP, q = 10, na.rm = TRUE))
ilex3_2.sp<-ilex3_2.sp %>% mutate(prediction_ = quantcut(prediction, q = 10, na.rm = TRUE))


#reformulamos el dataframe como objeto espacial para poderlo graficarlo en tmap
ilex3_2.sp<-st_as_sf(ilex3_2.sp, coords = c("X", "Y"), crs = ("+proj=longlat + datum=WGS84"))

st_transform(ilex3_2.sp, 4326)
extent(ilex3_2.sp)
crs(ilex3_2.sp)

#•cargamos el shape de andalucia
anda_shp<-readOGR("~/digital.agri/Andalucia_edafo/Andalucia_contorno.geojson")
anda_shp<-readOGR("~/digital.agri/anda_comunidad.shp")
extent(anda_shp)
crs(anda_shp)


path2grafic<-"./Graficos/"
tmap_mode("view") 
tmap_mode("plot")
ilex3_2.map.biom <- tm_basemap("Stamen.Watercolor") + 
  tm_shape(anda_shp) + tm_fill() + tm_borders() + tm_shape(ilex3_2.sp) + 
  tm_dots(col = "BP_", size = 0.25, n = 5, palette ="RdYlGn") + 
  tm_layout(title = "Q. ilex real biomass", 
            legend.position = c(0.05,0.003), scale = 0.8, legend.frame = F)
tmap_save(ilex3_2.map.biom.predict, paste(path2grafic, filename = "ilex3_2.map.biom.png"))


tmap_mode("plot")
ilex3_2.map.biom.predict <- tm_shape(anda_shp) + tm_fill() + tm_borders() + 
  tm_shape(ilex3_2.sp) + tmap_style("white") +
  tm_dots(col = "prediction_", size = 0.25, n = 5, palette ="RdYlGn") + 
  tm_layout(title = "Q. ilex biomass prediction", 
            legend.position = c(0.05,0.003), scale = 0.8,legend.frame = F)
tmap_save(ilex3_2.map.biom.predict, paste(path2grafic,filename = "ilex3_2.map.biom.predict.png"))



#### PODER PREDICTIVO GLM ####
#dividimos la muestra en dos grupos para poder testar el poder prdictivo del modelo
set.seed(1649)
data_train <- ilex3_2.nor %>% sample_frac(.8)
data_test <- setdiff(ilex3_2.nor, data_train)

mean(data_train$BP)
mean(data_test$BP)

hist(data_train$BP)
hist(data_test$BP)

ilex3_2_vifglm <- glm(BP ~ Dens + n_sup + Arid_Dm + X, data=data_train) 

summary(ilex3_2_vifglm) 
performance(ilex3_2_vifglm)
rsq(ilex3_2_vifglm) #r cuadrado de 0.3917

# Predicciones de entrenamiento
prediccion_train<-predict.glm(ilex3_2_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$BP)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.608174510077047"

# Predicciones de test
prediccion_test<-predict.glm(ilex3_2_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$BP)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.719133039516558"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ BP, data = data_test)#Adjusted R-squared:  0.2792
summary(r2test)

names(data_test)
ggpairs(data_test[, c(25,36)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

ggplot(data_test[, c(25,36)],aes(x=BP,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_growth_prediction", 
       x = "Qilex_biomass_growth", 
       title = "Q. ilex biomass growth prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 5.5, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 5.5, label.y = -4.5, size = 5.5) 
