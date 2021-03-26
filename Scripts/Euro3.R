#### ACEBUCHE ####

names(euro3)

cor_euro3<-round(cor(euro3[, -c(1:3,29:32,39:42)], use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
                     tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", diag = F,
                     addshade = "all", order = "FPC")

#las vriables que tienen una mayor correlacion con AB3_Mgha son soil_mean & stdv, aet_mean&stdv, mean_ndvi y tree_dens. Con ellas formulamos el modelo

plot((euro3$mean_ndvi),(euro3$AB3_Mgha))
plot(log(euro3$mean_ndvi),log(euro3$AB3_Mgha))

lm(log(AB3_Mgha) ~ log(mean_ndvi), data = euro3) %>% summary()#la r squared es bajisima

plot(log(euro3$mean_ndvi),log(euro3$AB3_Mgha))
abline(a=3.4317, b=0.7009, lwd=3, col='blue')


euro3.nor<- scale(euro3[,-c(1:3,29:32,39:42)],center=T,scale=T) %>% as.data.frame()


### Calcular VIF basandonos en las funciones vifcor y vifstep
#############################################################

v1 <- vifcor(euro3.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(euro3.nor, th=3)
v2

re1 <- exclude(euro3.nor,v2)
names(re1)

X11()
plot(re1)

cor_euro3v2 <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black",
                     tl.srt = 55, tl.cex = 0.8, number.cex = 0.8, 
                     addCoef.col = "black", type = "lower", diag = F, 
                     addshade = "all", order = "FPC")


euro3_vif <- glm(AB3_Mgha ~ Tree_dens + aspect + ph, data = euro3.nor)

summary(euro3_vif)
performance(euro3_vif)
rsq(euro3_vif) #r cuadrado de 0.249

sink("euro3_vif_glm.doc")
print(summary(glm(AB3_Mgha ~ Tree_dens + aspect + ph, data = euro3.nor)))
sink()  # returns output to the console


#podemos comparar los valores reales con los predichos del modelo
prediccion_model<-predict.glm(euro3_vif, euro3.nor)
euro3.nor$prediction<-prediccion_model


model_mse <- mean((prediccion_model - euro3.nor$AB3_Mgha)^2)
paste("Error (mse) del modelo:", model_mse)#"Error (mse) del modelo: 0.747063952967151"

r2model<-lm(prediction ~ AB3_Mgha, data = euro3.nor)#Adjusted R-squared:  0.2464 
summary(r2model)

names(euro3.nor)
ggplot(euro3.nor[, c(31,49)],aes(x=AB3_Mgha,y=prediction))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_prediction", 
       x = "O euro_biomass", 
       title = "O. euro biomass prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 3, label.y = -2, size = 5.5) +
  stat_regline_equation(label.x = 3, label.y = -2.5, size = 5.5) 



#ahora vamos a graficar la prediccion de la biomasa en un mapa par poderlo visualizarlo
names(euro3)
names(euro3.nor)
euro3.sp<-cbind(euro3[, c(4:5)], euro3.nor[, c(31,49)])
head(euro3.sp)

euro3.sp<-st_as_sf(euro3.sp, coords = c("X", "Y"), crs = ("+proj=longlat + datum=WGS84"))

anda_shp<-readOGR("~/digital.agri/Andalucia_edafo/Andalucia_contorno.geojson")

tmap_mode("plot")
euro3.map.biom.predict <- tm_shape(anda_shp) + tm_fill() + tm_borders() + 
  tm_shape(euro3.sp) + tmap_style("white") +
  tm_dots(col = "prediction", size = 0.15, n = 3) +
  tm_layout(title = "O. europaea biomass prediction", 
            legend.position = c(0.05,0.003), scale = 1,legend.frame = F)
tmap_save(euro3.map.biom.predict, filename = "euro3.map.biom.predict.png")


tm_basemap("Stamen.Watercolor") + 
  tm_shape(anda_shp) + tm_fill() + tm_borders() + tm_shape(euro3.sp) + 
  tm_dots(col = "AB3_Mgha", size = 0.25, n = 5) + 
  tm_layout(title = "O. euro real biomass", 
            legend.position = c(0.05,0.003), scale = 1, legend.frame = F)



#### PODER PREDICTIVO GLM ####
set.seed(1369)
data_train <- euro3.nor %>% sample_frac(.8)
data_test <- setdiff(euro3.nor, data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


euro3_vifglm <- glm(AB3_Mgha ~ Tree_dens + aspect + ph, data=data_train) 

summary(euro3_vifglm) 
performance(euro3_vifglm)
rsq(euro3_vifglm) #r cuadrado de 0.3381

# Predicciones de entrenamiento
prediccion_train<-predict.glm(euro3_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.59112515686758"

# Predicciones de test
prediccion_test<-predict.glm(euro3_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 1.38254108580981"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.0186
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


ggplot(data_test[, c(31,49)],aes(x=AB3_Mgha,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_prediction", 
       x = "Oeuropaea_biomass", 
       title = "O europaea biomass prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 3, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 3, label.y = -4.5, size = 5.5) 


#### GAM ####

euro3_vifgam <- gam(AB3_Mgha ~ s(Tree_dens) + (max_ndvi) + (aspect) + s(ph), data = euro3.nor) 

summary(euro3_vifgam)#R-sq.(adj) = 0.354

sink("euro3_vif_gam.doc")
print(summary(gam(AB3_Mgha ~ s(Tree_dens) + (max_ndvi) + (aspect) + s(ph), data = euro3.nor)))
sink()  # returns output to the console


# Predicciones de entrenamiento
prediccion_train<-predict.gam(euro3_vifgam,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.480804459730057"

# Predicciones de test
prediccion_test<-predict.gam(euro3_vifgam,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 1.08566171573654"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.2239
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")



#### ARBOL DE DECISIONES ####
#de alguna forma sirve para ver cuales variables con mas relevantes
set.seed(1649)
data_train <- euro3[, -c(1:3,29:32,39:42)] %>% sample_frac(.8)
data_test <- setdiff(euro3[, -c(1:3,29:32,39:42)], data_train)

head(data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


#creamos el CART model
euro3.tree <- rpart(AB3_Mgha ~ .,data=data_train)

rpart.plot(euro3.tree, extra = 100)

prp(euro3.tree)
par(xpd = NA)

jpeg("euro3.tree.jpg", width=1000, height=820, units="px",
     pointsize=5, bg="white",res=300)
plot(euro3.tree)
text(euro3.tree)
dev.off()

printcp(euro3.tree)
plotcp(euro3.tree)
