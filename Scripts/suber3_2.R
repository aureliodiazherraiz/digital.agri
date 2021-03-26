#vamos a modelar la respuesta del crecimiento en suber entre ifns

names(suber3_2)
cor_suber3_2 <- round(cor(suber3_2[,-(7:8)], use = "complete.obs"), 2) %>% 
  corrplot(method = "shade", 
           shade.col = NA, tl.col = "black",
           tl.srt = 55, tl.cex = 0.5, 
           number.cex = 0.5, addCoef.col = "black", 
           type = "lower", diag = F, addshade = "all", 
           order = "FPC")

suber3_2.nor<- scale(suber3_2[,-c(7:8)],center=T,scale=T) %>% as.data.frame()


#### Calcular VIF basandonos en las funciones vifcor y vifstep ####

v1 <- vifcor(suber3_2.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(suber3_2.nor, th=3)
v2

re1 <- exclude(suber3_2.nor,v2)
names(re1)

cor_suber3_2vif <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot(type = "lower", 
           method = "shade", shade.col = NA, 
           tl.col = "black", tl.srt = 55, tl.cex = 0.8, 
           number.cex = 0.8, addCoef.col = "black", diag = F, 
           addshade = "all", order = "FPC")

#### GLM ####
#con las variables obtenidas en el vistep que mayor correlacion tengan con la biomasa montamos un modelo predictivo glm

#dividimos la muestra en dos grupos para poder testar el poder prdictivo del modelo

model_suber3_2_vif <- glm(BP ~ Dens + mo_sup + ca + psb, data = suber3_2.nor)
summary(model_suber3_2_vif)
rsq(model_suber3_2_vif) #r squared = 0.149


sink("suber3_2_vif_glm.doc")
print(summary(glm(BP ~ Dens + mo_sup + ca + psb, data = suber3_2.nor)))
sink()  # returns output to the console



#### PODER PREDICTIVO GLM ####
set.seed(123)
data_train <- suber3_2.nor %>% sample_frac(.8)
data_test <- setdiff(suber3_2.nor, data_train)

mean(data_train$BP)
mean(data_test$BP)

hist(data_train$BP)
hist(data_test$BP)

suber3_2_vifglm <- glm(BP ~ Dens + mo_sup + ca + psb, data=data_train) 

summary(suber3_2_vifglm) 
performance(suber3_2_vifglm)
rsq(suber3_2_vifglm) #r cuadrado de 0.1388

# Predicciones de entrenamiento
prediccion_train<-predict.glm(suber3_2_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$BP)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.845313183998693"

# Predicciones de test
prediccion_test<-predict.glm(suber3_2_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$BP)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.87423493356196"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ BP, data = data_test)#Adjusted R-squared:  0.1857
summary(r2test)

names(data_test)
ggpairs(data_test[, c(25,36)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

ggplot(data_test[, c(25,36)],aes(x=BP,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_growth_prediction", 
       x = "Qsuber_biomass_variation", 
       title = "Q. suber biomass variation prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 2.5, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 2.5, label.y = -4.5, size = 5.5) 
