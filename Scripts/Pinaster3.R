#### PINASTER ####
names(pinaster3)

cor_pinaster3<-round(cor(pinaster3[, -c(1:3,29:32,39:42)], use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
                     tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", diag = F,
                     addshade = "all", order = "FPC")

jpeg(filename = "corr_pinaster.jpeg", width = 1000, height = 1000)
corrplot::corrplot(cor_pinaster3, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
                   tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", 
                   diag = F, addshade = "all", order = "FPC") ## para visualizar el peso de las variables 
dev.off()


#las vriables que tienen una mayor correlacion con AB3_Mgha son soil_mean & stdv, aet_mean&stdv, mean_ndvi y tree_dens. Con ellas formulamos el modelo
plot(pinaster3$mean_ndvi, pinaster3$AB3_Mgha)

lm(log(AB3_Mgha) ~ mean_ndvi, data = pinaster3) %>% summary()
plot(pinaster3$mean_ndvi, log(pinaster3$AB3_Mgha))
abline(a=1.5618, b=4.0437, lwd=3, col='blue')



pinaster3.nor<- scale(pinaster3[,-c(1:3,29:32,39:42)],center=T,scale=T) %>% as.data.frame()


### Calcular VIF basandonos en las funciones vifcor y vifstep
#############################################################

v1 <- vifcor(pinaster3.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(pinaster3.nor, th=3)
v2

re1 <- exclude(pinaster3.nor,v2)
re1

X11()
plot(re1)

cor_pinaster3v2 <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black",
                     tl.srt = 55, tl.cex = 0.8, number.cex = 0.8, 
                     addCoef.col = "black", type = "lower", diag = F, 
                     addshade = "all", order = "FPC")


#### GLM ####

pinaster3_vif <- glm(AB3_Mgha ~ Tree_dens + mean_ndvi + mo_sup + textura, data = pinaster3.nor) 
summary(pinaster3_vif)
performance(pinaster3_vif)
rsq(pinaster3_vif) #r cuadrado de 0.4732783


sink("pinaster3_vif_glm.doc")
print(summary(glm(AB3_Mgha ~ Tree_dens + mean_ndvi + mo_sup + textura, data = pinaster3.nor)))
sink()  # returns output to the console



#### PODER PREDICTIVO GLM ####
set.seed(1369)
data_train <- pinaster3.nor %>% sample_frac(.8)
data_test <- setdiff(pinaster3.nor, data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)

pinaster3_vifglm <- glm(AB3_Mgha ~ Tree_dens + mean_ndvi + 
                          mo_sup + textura, data=data_train) 

summary(pinaster3_vifglm) 
performance(pinaster3_vifglm)
rsq(pinaster3_vifglm) #r cuadrado de 0.5078

# Predicciones de entrenamiento
prediccion_train<-predict.glm(pinaster3_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.480804459730057"

# Predicciones de test
prediccion_test<-predict.glm(pinaster3_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.671095447411617"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.359
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


ggplot(data_test[, c(31,49)],aes(x=AB3_Mgha,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_prediction", 
       x = "Ppinaster_biomass", 
       title = "P pinaster biomass prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 2, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 2, label.y = -4.5, size = 5.5) 


#### GAM ####

pinaster3_vifgam <- gam(AB3_Mgha ~ s(Tree_dens) + s(mean_ndvi) + s(mo_sup) + s(n_sup), 
                     data = pinaster3.nor) 

summary(pinaster3_vifgam)#R-sq.(adj) = 0.558

sink("pinaster3_vif_gam.doc")
print(summary(gam(AB3_Mgha ~ s(Tree_dens) + s(mean_ndvi) + s(mo_sup) + s(n_sup), 
                  data = pinaster3.nor)))
sink()  # returns output to the console



# Predicciones de entrenamiento
prediccion_train<-predict.gam(pinaster3_vifgam,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.415009070180227"

# Predicciones de test
prediccion_test<-predict.gam(pinaster3_vifgam,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.477256704261409"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.5349
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")



#### ARBOL DE DECISIONES ####
#de alguna forma sirve para ver cuales variables con mas relevantes
set.seed(1649)
data_train <- pinaster3[, -c(1:3,29:32,39:42)] %>% sample_frac(.8)
data_test <- setdiff(pinaster3[, -c(1:3,29:32,39:42)], data_train)

head(data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


#creamos el CART model
pinaster3.tree <- rpart(AB3_Mgha ~ .,data=data_train)

rpart.plot(pinaster3.tree, extra = 100)

prp(pinaster3.tree)
par(xpd = NA)

jpeg("pinaster3.tree.jpg", width=1000, height=820, units="px",
     pointsize=5, bg="white",res=300)
plot(pinaster3.tree)
text(pinaster3.tree)
dev.off()

printcp(suber.tree)
plotcp(suber.tree)




