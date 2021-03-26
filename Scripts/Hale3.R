#### P HALEPENSIS ####
names(hale3)

cor_hale3<-round(cor(hale3[, -c(1:3,29:32,39:42)], use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
                     tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", diag = F,
                     addshade = "all", order = "FPC")

jpeg(filename = "corr_pinaster.jpeg", width = 1000, height = 680)
corrplot::corrplot(cor_pinaster3, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
                   tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", 
                   diag = F, addshade = "all", order = "FPC") ## para visualizar el peso de las variables 
dev.off()


#las vriables que tienen una mayor correlacion con AB3_Mgha son soil_mean & stdv, aet_mean&stdv, mean_ndvi y tree_dens. Con ellas formulamos el modelo
plot(log(hale3$mean_ndvi),log(hale3$AB3_Mgha))

lm(log(AB3_Mgha) ~ log(mean_ndvi), data = hale3) %>% summary()
plot(log(hale3$mean_ndvi),log(hale3$AB3_Mgha))
abline(a=4.81003, b=1.72341, lwd=3, col='blue')


hale3.nor<- scale(hale3[,-c(1:3,29:32,39:42)],center=T,scale=T) %>% as.data.frame()


### Calcular VIF basandonos en las funciones vifcor y vifstep
#############################################################

v1 <- vifcor(hale3.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(hale3.nor, th=3)
v2

re1 <- exclude(hale3.nor,v2)
re1

X11()
plot(re1)

cor_hale3v2 <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black",
                     tl.srt = 55, tl.cex = 0.8, number.cex = 0.8, 
                     addCoef.col = "black", type = "lower", diag = F, 
                     addshade = "all", order = "FPC")



#### GLM ####
hale3_vif <- glm(AB3_Mgha ~ Tree_dens + mean_ndvi + hillshade + ca + soil_mean +
                   mo_sup + lim + ph, data = hale3.nor) 

summary(hale3_vif)
performance(hale3_vif)
rsq(hale3_vif) #r cuadrado de 0.550


sink("hale3_vif_glm.doc")
print(summary(glm(AB3_Mgha ~ Tree_dens + mean_ndvi + hillshade + ca + soil_mean +
                    mo_sup + lim + ph, data = hale3.nor)))
sink()  # returns output to the console



#podemos comparar los valores reales con los predichos del modelo
prediccion_model<-predict.glm(hale3_vif, hale3.nor)
hale3.nor$prediction<-prediccion_model


model_mse <- mean((prediccion_model - hale3.nor$AB3_Mgha)^2)
paste("Error (mse) del modelo:", model_mse)#"Error (mse) del modelo: 0.446515438173759"

r2model<-lm(prediction ~ AB3_Mgha, data = hale3.nor)#Adjusted R-squared:  0.5527 
summary(r2model)

names(hale3.nor)
ggplot(hale3.nor[, c(31,49)],aes(x=AB3_Mgha,y=prediction))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_prediction", 
       x = "P hale_biomass", 
       title = "P. halepensis biomass prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 3, label.y = -2, size = 5.5) +
  stat_regline_equation(label.x = 3, label.y = -2.5, size = 5.5) 



#ahora graficamos la prediccion de la biomasa en un mapa par poderlo visualizarlo
names(hale3)
names(hale3.nor)
hale3.sp<-cbind(hale3[, c(4:5)], hale3.nor[, c(31,49)])


#lo categorizamos en los quantiles
hale3.sp<-hale3.sp %>% mutate(AB3_Mgha_ = quantcut(AB3_Mgha, q = 10, na.rm = TRUE))
hale3.sp<-hale3.sp %>% mutate(prediction_ = quantcut(prediction, q = 10, na.rm = TRUE))
head(hale3.sp)

#reformulamos el dataframe como objeto espacial para poderlo graficarlo en tmap
hale3.sp<-st_as_sf(hale3.sp, coords = c("X", "Y"), crs = ("+proj=longlat + datum=WGS84"))

st_transform(hale3.sp, 4326)
extent(hale3.sp)
crs(hale3.sp)

#•cargamos el shape de andalucia
anda_shp<-readOGR("~/digital.agri/Andalucia_edafo/Andalucia_contorno.geojson")
anda_shp<-readOGR("~/digital.agri/anda_comunidad.shp")
extent(anda_shp)
crs(anda_shp)


tmap_mode("view") 
tmap_mode("plot")
hale3.map.biom<-tm_basemap("Stamen.Watercolor") + 
  tm_shape(anda_shp) + tm_fill() + tm_borders() + tm_shape(hale3.sp) + 
  tm_dots(col = "AB3_Mgha_", size = 0.25, n = 5, palette ="RdYlGn") + 
  tm_layout(title = "Q. hale real biomass", 
            legend.position = c(0.05,0.003), scale = 1, legend.frame = F)
tmap_save(hale3.map.biom, paste(path2grafic, filename = "hale3.map.biom.png"))


hale3.map.biom.predict <- tm_shape(anda_shp) + tm_fill() + tm_borders() + 
  tm_shape(hale3.sp) + tmap_style("white") +
  tm_dots(col = "prediction_", size = 0.25, n = 5, palette ="RdYlGn") + 
  tm_layout(title = "Q. hale biomass prediction", 
            legend.position = c(0.05,0.003), scale = 1,legend.frame = F)
tmap_save(hale3.map.biom.predict, paste(path2grafic, filename = "hale3.map.biom.predict.png"))



#### PODER PREDICTIVO GLM ####
set.seed(123)
data_train <- hale3.nor %>% sample_frac(.8)
data_test <- setdiff(hale3.nor, data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


hale3_vif <- glm(AB3_Mgha ~ Tree_dens + mean_ndvi + hillshade + 
                   mo_sup, data=data_train) 

summary(hale3_vif) 
performance(hale3_vif)
rsq(hale3_vif) #r cuadrado de 0.54

# Predicciones de entrenamiento
prediccion_train<-predict.glm(hale3_vif,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.462366012129676"

# Predicciones de test
prediccion_test<-predict.glm(hale3_vif,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.43479993551309"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.557
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


ggplot(data_test[, c(31,49)],aes(x=AB3_Mgha,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_prediction", 
       x = "Phalepensis_biomass", 
       title = "P halepensis biomass prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 2, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 2, label.y = -4.5, size = 5.5) 


#### GAM ####

hale3_vifgam <- gam(AB3_Mgha ~ s(Tree_dens) + s(mean_ndvi) + (hillshade) + s(ca) + 
                      (mo_sup) + s(lim) + s(ph), data = hale3.nor) 

summary(hale3_vifgam)#R-sq.(adj) = 0.59

sink("hale3_vif_gam.doc")
print(summary(gam(AB3_Mgha ~ s(Tree_dens) + s(mean_ndvi) + (hillshade) + s(ca) + 
                       (mo_sup) + s(lim) + s(ph), data = hale3.nor)))
sink()  # returns output to the console


# Predicciones de entrenamiento
prediccion_train<-predict.glm(hale3_vifgam,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.462366012129676"

# Predicciones de test
prediccion_test<-predict.glm(hale3_vifgam,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.43479993551309"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.5569
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")




#### ARBOL DE DECISIONES ####
#de alguna forma sirve para ver cuales variables con mas relevantes
set.seed(1649)
data_train <- hale3[, -c(1:3,29:32,39:42)] %>% sample_frac(.8)
data_test <- setdiff(hale3[, -c(1:3,29:32,39:42)], data_train)

head(data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


#creamos el CART model
hale3.tree <- rpart(AB3_Mgha ~ .,data=data_train)

rpart.plot(hale3.tree, extra = 100)

prp(hale3.tree)
par(xpd = NA)

jpeg("hale3.tree.jpg", width=1000, height=820, units="px",
     pointsize=5, bg="white",res=300)
plot(hale3.tree)
text(hale3.tree)
dev.off()

printcp(hale3.tree)
plotcp(hale3.tree)
