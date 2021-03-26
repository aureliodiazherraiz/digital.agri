
#### PINEA ####
names(pinea3)

cor_pinea3<-round(cor(pinea3[, -c(1:3,29:32,39:42)], use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
           tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", diag = F,
           addshade = "all", order = "FPC")

jpeg(filename = "corr_pinea3_vif.jpeg", width = 1000, height = 1000)
corrplot::corrplot(cor_pinea3, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
                              tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", 
                              diag = F, addshade = "all", order = "FPC") ## para visualizar el peso de las variables  
dev.off()


#las vriables que tienen una mayor correlacion con AB3_Mgha son soil_mean & stdv, aet_mean&stdv, mean_ndvi y tree_dens. Con ellas formulamos el modelo

lm(log(AB3_Mgha) ~ mean_ndvi, data = pinea3) %>% summary()
plot(pinea3$mean_ndvi, log(pinea3$AB3_Mgha))
abline(a=1.7838, b=3.8575, lwd=3, col='blue')

lm(AB3_Mgha ~ Tree_dens, data = pinea3) %>%  summary()
plot(log(pinea3$Tree_dens), pinea3$AB3_Mgha)
abline(a=44.7596, b=0.0742, lwd=3, col='blue')


pinea3.nor<- scale(pinea3[,-c(1:3,29:32,39:42)],center=T,scale=T) %>% as.data.frame()


### Calcular VIF basandonos en las funciones vifcor y vifstep
#############################################################

v1 <- vifcor(pinea3.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(pinea3.nor, th=3)
v2

re1 <- exclude(pinea3.nor,v2)
re1

X11()
plot(re1)

cor_pinea3v2 <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black",
           tl.srt = 55, tl.cex = 0.8, number.cex = 0.8, 
           addCoef.col = "black", type = "lower", diag = F, 
           addshade = "all", order = "FPC")


### GLM ###
pinea3_vif <- glm(AB3_Mgha ~ Tree_dens + mean_ndvi + aet_min , data = pinea3.nor) 
summary(pinea3_vif)
performance(pinea3_vif)
rsq(pinea3_vif) #r cuadrado de 0.383


sink("pinea3_vif_glm.doc")
print(summary(glm(AB3_Mgha ~ Tree_dens + mean_ndvi + aet_min, data = pinea3.nor)))
sink()  # returns output to the console

#### PODER PREDICTIVO GLM ####
set.seed(135)
data_train <- pinea3.nor %>% sample_frac(.8)
data_test <- setdiff(pinea3.nor, data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


pinea3_vifglm <- glm(AB3_Mgha ~ aet_mean + mean_ndvi +  
                      Tree_dens, data=data_train) 

summary(pinea3_vifglm) 
performance(pinea3_vifglm)
rsq(pinea3_vifglm) #r cuadrado de 0.41

# Predicciones de entrenamiento
prediccion_train<-predict.glm(pinea3_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.587799071511313"

# Predicciones de test
prediccion_test<-predict.glm(pinea3_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.70763916987584"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.2964
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


ggplot(data_test[, c(31,49)],aes(x=AB3_Mgha,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_prediction", 
       x = "Ppinea_biomass", 
       title = "P pinea biomass prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 1.8, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 1.8, label.y = -4.5, size = 5.5) 


#### GAM ####
pinea3_vifgam <- gam(AB3_Mgha ~ s(Tree_dens) + s(max_ndvi) + s(aet_min), 
                     data = re1) 

summary(pinea3_vifgam)#R-sq.(adj) = 0.437

sink("pinea3_vif_gam.doc")
print(summary(gam(AB3_Mgha ~ s(Tree_dens) + s(max_ndvi) + s(aet_min), data = pinea3.nor)))
sink()  # returns output to the console



# Predicciones de entrenamiento
prediccion_train<-predict.gam(pinea3_vifgam,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.53284883314669"

# Predicciones de test
prediccion_test<-predict.gam(pinea3_vifgam,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.609156355551546"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.391
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")





#### ARBOL DE DECISIONES ####
#de alguna forma sirve para ver cuales variables con mas relevantes
set.seed(1649)
data_train <- pinea3[, -c(1:3,29:32,39:42)] %>% sample_frac(.8)
data_test <- setdiff(pinea3[, -c(1:3,29:32,39:42)], data_train)

head(data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


#creamos el CART model
pinea3.tree <- rpart(AB3_Mgha ~ .,data=data_train)

rpart.plot(pinea3.tree, extra = 100)

prp(pinea3.tree)
par(xpd = NA)

jpeg("pinea3.tree.jpg", width=1200, height=820, units="px",
     pointsize=5, bg="white",res=300)
plot(pinea3.tree)
text(pinea3.tree)
dev.off()

printcp(pinea3.tree)
plotcp(pinea3.tree)



