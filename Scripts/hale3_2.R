#vamos a modelar la respuesta del crecimiento en hale entre ifns

names(hale3_2)
cor_hale3_2 <- round(cor(hale3_2[,-(7:8)], use = "complete.obs"), 2) %>% 
  corrplot(method = "shade", 
           shade.col = NA, tl.col = "black",
           tl.srt = 55, tl.cex = 0.5, 
           number.cex = 0.5, addCoef.col = "black", 
           type = "lower", diag = F, addshade = "all", 
           order = "FPC")

hale3_2.nor<- scale(hale3_2[,-c(7:8)],center=T,scale=T) %>% as.data.frame()


#### Calcular VIF basandonos en las funciones vifcor y vifstep ####

v1 <- vifcor(hale3_2.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(hale3_2.nor, th=3)
v2

re1 <- exclude(hale3_2.nor,v2)
names(re1)

cor_hale3_2vif <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot(type = "lower", 
           method = "shade", shade.col = NA, 
           tl.col = "black", tl.srt = 55, tl.cex = 0.8, 
           number.cex = 0.8, addCoef.col = "black", diag = F, 
           addshade = "all", order = "FPC")

#### GLM ####
#con las variables obtenidas en el vistep que mayor correlacion tengan con la biomasa montamos un modelo predictivo glm

#dividimos la muestra en dos grupos para poder testar el poder prdictivo del modelo

model_hale3_2_vif <- glm(BP ~ Dens + Arid_Dm + ph + tf + lim + ps, data = hale3_2.nor)
summary(model_hale3_2_vif)
rsq(model_hale3_2_vif) #r squared = 0.3158


sink("hale3_2_vif_glm.doc")
print(summary(glm(BP ~ Dens + Arid_Dm + ph + tf + lim + ps, data = hale3_2.nor)))
sink()  # returns output to the console


#estudiamos los valores predichos
prediccion_model<-predict.glm(model_hale3_2_vif, hale3_2.nor)
model_mse <- mean((prediccion_model - hale3_2.nor$BP)^2)
paste("Error (mse) del modelo:", model_mse)#"Error (mse) del modelo: 0.683460287702909"

#introducimos dentro del dataframe
hale3_2.nor$prediction<-prediccion_model

#ahora graficamos la prediccion de la biomasa en un mapa par poderlo visualizarlo
names(hale3_2.nor)
names(hale3_2.nor)
hale3_2.sp<-cbind(hale3_2[, c(1:2)], hale3_2.nor[, c(25,36)])
getwd()
path2csv<-"./CSV/"

write.csv(hale3_2.sp, paste(path2csv,"hale3_2_latlon_BP_prediction.csv"))

#lo categorizamos en los quantiles
hale3_2.sp<-hale3_2.sp %>% mutate(BP_ = quantcut(BP, q = 10, na.rm = TRUE))
hale3_2.sp<-hale3_2.sp %>% mutate(prediction_ = quantcut(prediction, q = 10, na.rm = TRUE))


#reformulamos el dataframe como objeto espacial para poderlo graficarlo en tmap
hale3_2.sp<-st_as_sf(hale3_2.sp, coords = c("X", "Y"), crs = ("+proj=longlat + datum=WGS84"))

st_transform(hale3_2.sp, 4326)
extent(hale3_2.sp)
crs(hale3_2.sp)

#•cargamos el shape de andalucia
anda_shp<-readOGR("~/digital.agri/Andalucia_edafo/Andalucia_contorno.geojson")
anda_shp<-readOGR("~/digital.agri/anda_comunidad.shp")
extent(anda_shp)
crs(anda_shp)


path2grafic<-"./Graficos/"
tmap_mode("view") 
tmap_mode("plot")
hale3_2.map.biom <- tm_shape(anda_shp) + tm_fill() + tm_borders() + 
  tm_shape(hale3_2.sp) + tm_dots(col = "BP_", size = 0.25, n = 5, palette ="RdYlGn") + 
  tm_layout(title = "P. hale real biomass", 
            legend.position = c(0.05,0.003), scale = 0.8, legend.frame = F)
tmap_save(hale3_2.map.biom, paste(path2grafic, filename = "hale3_2.map.biom.png"))


tmap_mode("plot")
hale3_2.map.biom.predict <- tm_shape(anda_shp) + tm_fill() + tm_borders() + 
  tm_shape(hale3_2.sp) + tmap_style("white") +
  tm_dots(col = "prediction_", size = 0.25, n = 5, palette ="RdYlGn") + 
  tm_layout(title = "P. hale biomass prediction", 
            legend.position = c(0.05,0.003), scale = 0.8,legend.frame = F)
tmap_save(hale3_2.map.biom.predict, paste(path2grafic,filename = "hale3_2.map.biom.predict.png"))



#### PODER PREDICTIVO GLM ####
set.seed(136)
data_train <- hale3_2.nor %>% sample_frac(.8)
data_test <- setdiff(hale3_2.nor, data_train)

mean(data_train$BP)
mean(data_test$BP)

hist(data_train$BP)
hist(data_test$BP)

hale3_2_vifglm <- glm(BP ~ Dens + Arid_Dm + tf + lim + ps, data=data_train) 

summary(hale3_2_vifglm) 
performance(hale3_2_vifglm)
rsq(hale3_2_vifglm) #r cuadrado de 0.3110

# Predicciones de entrenamiento
prediccion_train<-predict.glm(hale3_2_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$BP)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.681373578792371"

# Predicciones de test
prediccion_test<-predict.glm(hale3_2_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$BP)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.711547172779216"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ BP, data = data_test)#Adjusted R-squared:  0.3176
summary(r2test)

names(data_test)
ggpairs(data_test[, c(25,36)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

ggplot(data_test[, c(25,36)],aes(x=BP,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_growth_prediction", 
       x = "Qhale_biomass_variation", 
       title = "Q. hale biomass variation prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 2.5, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 2.5, label.y = -4.5, size = 5.5) 
