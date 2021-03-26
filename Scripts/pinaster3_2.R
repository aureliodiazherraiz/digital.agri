#vamos a modelar la respuesta del crecimiento en pinaster entre ifns

names(pinaster3_2)
cor_pinaster3_2 <- round(cor(pinaster3_2[,-(7:8)], use = "complete.obs"), 2) %>% 
  corrplot(method = "shade", 
           shade.col = NA, tl.col = "black",
           tl.srt = 55, tl.cex = 0.5, 
           number.cex = 0.5, addCoef.col = "black", 
           type = "lower", diag = F, addshade = "all", 
           order = "FPC")

pinaster3_2.nor<- scale(pinaster3_2[,-c(7:8)],center=T,scale=T) %>% as.data.frame()


#### Calcular VIF basandonos en las funciones vifcor y vifstep ####

v1 <- vifcor(pinaster3_2.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(pinaster3_2.nor, th=3)
v2

re1 <- exclude(pinaster3_2.nor,v2)
names(re1)

cor_pinaster3_2vif <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot(type = "lower", 
           method = "shade", shade.col = NA, 
           tl.col = "black", tl.srt = 55, tl.cex = 0.8, 
           number.cex = 0.8, addCoef.col = "black", diag = F, 
           addshade = "all", order = "FPC")

#### GLM ####
#con las variables obtenidas en el vistep que mayor correlacion tengan con la biomasa montamos un modelo predictivo glm

#dividimos la muestra en dos grupos para poder testar el poder prdictivo del modelo

model_pinaster3_2_vif <- glm(BP ~ Dens + Arid_Dm + psb, data = pinaster3_2.nor)
summary(model_pinaster3_2_vif)
rsq(model_pinaster3_2_vif) #r squared = 0.1296


sink("pinaster3_2_vif_glm.doc")
print(summary(glm(BP ~ Dens + Arid_Dm + psb, data = pinaster3_2.nor)))
sink()  # returns output to the console



#### PODER PREDICTIVO GLM ####
set.seed(1369)
data_train <- pinaster3_2.nor %>% sample_frac(.8)
data_test <- setdiff(pinaster3_2.nor, data_train)

mean(data_train$BP)
mean(data_test$BP)

hist(data_train$BP)
hist(data_test$BP)

pinaster3_2_vifglm <- glm(BP ~ Dens + Arid_Dm + psb, data=data_train) 

summary(pinaster3_2_vifglm) 
performance(pinaster3_2_vifglm)
rsq(pinaster3_2_vifglm) #r cuadrado de 0.1168

# Predicciones de entrenamiento
prediccion_train<-predict.glm(pinaster3_2_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$BP)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.837336453288623"

# Predicciones de test
prediccion_test<-predict.glm(pinaster3_2_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$BP)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 1.00562635830985"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ BP, data = data_test)#Adjusted R-squared:  0.1948
summary(r2test)

names(data_test)
ggpairs(data_test[, c(25,36)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

ggplot(data_test[, c(25,36)],aes(x=BP,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_growth_prediction", 
       x = "Qpinaster_biomass_variation", 
       title = "Q. pinaster biomass variation prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 2.5, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 2.5, label.y = -4.5, size = 5.5) 
