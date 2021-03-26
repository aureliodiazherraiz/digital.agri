#### P NIGRA ####

names(nigra3)

cor_nigra3<-round(cor(nigra3[, -c(1:3,29:32,39:42)], use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
                     tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", diag = F,
                     addshade = "all", order = "FPC")

jpeg(filename = "corr_nigra3.jpeg", width = 1000, height = 690)
corrplot::corrplot(cor_nigra3, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
                   tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", 
                   diag = F, addshade = "all", order = "FPC") ## para visualizar el peso de las variables 
dev.off()


#las vriables que tienen una mayor correlacion con AB3_Mgha son soil_mean & stdv, aet_mean&stdv, mean_ndvi y tree_dens. Con ellas formulamos el modelo

plot((nigra3$mean_ndvi),(nigra3$AB3_Mgha))
plot(log(nigra3$mean_ndvi),log(nigra3$AB3_Mgha))

lm(log(AB3_Mgha) ~ log(mean_ndvi), data = nigra3) %>% summary()#log en lo0s dos terminos mejora r squared

plot(log(nigra3$mean_ndvi),log(nigra3$AB3_Mgha))
abline(a=5.4929, b=2.0021, lwd=3, col='blue')


nigra3.nor<- scale(nigra3[,-c(1:3,29:32,39:42)],center=T,scale=T) %>% as.data.frame()


### Calcular VIF basandonos en las funciones vifcor y vifstep
#############################################################

v1 <- vifcor(nigra3.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(nigra3.nor, th=3)
v2

re1 <- exclude(nigra3.nor,v2)
names(re1)

X11()
plot(re1)

cor_nigra3v2 <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black",
                     tl.srt = 55, tl.cex = 0.8, number.cex = 0.8, 
                     addCoef.col = "black", type = "lower", diag = F, 
                     addshade = "all", order = "FPC")


nigra3_vif <- glm(AB3_Mgha ~ ca + mean_ndvi + Tree_dens + slope + 
                    srad_mean + pet_mean, data = nigra3.nor)

summary(nigra3_vif)
performance(nigra3_vif)
rsq(nigra3_vif) #r cuadrado de 0.434

sink("nigra3_vif_glm.doc")
print(summary(glm(AB3_Mgha ~ ca + mean_ndvi + Tree_dens + slope + 
                    srad_mean + pet_mean, data = nigra3.nor)))
sink()  # returns output to the console


#### PODER PREDICTIVO GLM ####
set.seed(135)
data_train <- nigra3.nor %>% sample_frac(.8)
data_test <- setdiff(nigra3.nor, data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


nigra3_vifglm <- glm(AB3_Mgha ~ ca + mean_ndvi + Tree_dens + slope + 
                      srad_mean + pet_mean, data=data_train) 

summary(nigra3_vifglm) 
performance(nigra3_vifglm)
rsq(nigra3_vifglm) #r cuadrado de 0.39

# Predicciones de entrenamiento
prediccion_train<-predict.glm(nigra3_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.584208922076207"

# Predicciones de test
prediccion_test<-predict.glm(nigra3_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.495240761132259"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.5829
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

ggplot(data_test[, c(31,49)],aes(x=AB3_Mgha,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_prediction", 
       x = "Pnigra_biomass", 
       title = "P nigra biomass prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 2, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 2, label.y = -4.5, size = 5.5) 



#### GAM ####

nigra3_vifgam <- gam(AB3_Mgha ~ (ca) + s(mean_ndvi) + s(Tree_dens) + s(slope) + 
                       s(srad_mean) + s(pet_mean), data = nigra3.nor) 

summary(nigra3_vifgam)#R-sq.(adj) = 0.506

sink("nigra3_vifgam.doc")
print(summary(gam(AB3_Mgha ~ (ca) + s(mean_ndvi) + s(Tree_dens) + s(slope) + 
                    s(srad_mean) + s(pet_mean), data = nigra3.nor)))
sink()  # returns output to the console


# Predicciones de entrenamiento
prediccion_train<-predict.gam(nigra3_vifgam,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.497584124586263"

# Predicciones de test
prediccion_test<-predict.gam(nigra3_vifgam,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.365411876463693"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.696
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")






#### ARBOL DE DECISIONES ####
#de alguna forma sirve para ver cuales variables con mas relevantes
set.seed(1649)
data_train <- nigra3[, -c(1:3,29:32,39:42)] %>% sample_frac(.8)
data_test <- setdiff(nigra3[, -c(1:3,29:32,39:42)], data_train)

head(data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


#creamos el CART model
nigra3.tree <- rpart(AB3_Mgha ~ .,data=data_train)

rpart.plot(nigra3.tree, extra = 100)

prp(nigra3.tree)
par(xpd = NA)

jpeg("nigra3.tree.jpg", width=1000, height=820, units="px",
     pointsize=5, bg="white",res=300)
plot(nigra3.tree)
text(nigra3.tree)
dev.off()

printcp(nigra3.tree)
plotcp(nigra3.tree)
