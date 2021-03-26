#vamos a modelar la respuesta del crecimiento en sylv entre ifns

names(sylv3_2)
str(sylv3_2[,-(7:8)])#la columna 25 da rpoblemas "D_ano"

cor_sylv3_2 <- round(cor(sylv3_2[,-c(7:8,25)], use = "complete.obs"), 2) %>% 
  corrplot(method = "shade", 
           shade.col = NA, tl.col = "black",
           tl.srt = 55, tl.cex = 0.5, 
           number.cex = 0.5, addCoef.col = "black", 
           type = "lower", diag = F, addshade = "all", 
           order = "FPC")

ggcorr(sylv3_2[,-c(7:8,25)], palette = "RdBu", label = TRUE)

sylv3_2.nor<- scale(sylv3_2[,-c(7:8,25)],center=T,scale=T) %>% as.data.frame()

#### Calcular VIF basandonos en las funciones vifcor y vifstep ####

v1 <- vifcor(sylv3_2.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(sylv3_2.nor, th=3)
v2

re1 <- exclude(sylv3_2.nor,v2)
names(re1)

cor_sylv3_2vif <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot(type = "lower", 
           method = "shade", shade.col = NA, 
           tl.col = "black", tl.srt = 55, tl.cex = 0.8, 
           number.cex = 0.8, addCoef.col = "black", diag = F, 
           addshade = "all", order = "FPC")

#### GLM ####
#con las variables obtenidas en el vistep que mayor correlacion tengan con la biomasa montamos un modelo predictivo glm

#dividimos la muestra en dos grupos para poder testar el poder prdictivo del modelo

model_sylv3_2_vif <- glm(BP ~ ndvi_max +
                           Dens + ps + psb, data = sylv3_2.nor)
summary(model_sylv3_2_vif)
rsq(model_sylv3_2_vif) #r squared = 0.2483434


sink("sylv3_2_vif_glm.doc")
print(summary(glm(BP ~ ndvi_max + Dens + ps + psb, data = sylv3_2.nor)))
sink()  # returns output to the console



#### PODER PREDICTIVO GLM ####
set.seed(1369)
data_train <- sylv3_2.nor %>% sample_frac(.8)
data_test <- setdiff(sylv3_2.nor, data_train)

mean(data_train$BP)
mean(data_test$BP)

hist(data_train$BP)
hist(data_test$BP)

sylv3_2_vifglm <- glm(BP ~ ndvi_max + Dens + ps + psb, data=data_train) 

summary(sylv3_2_vifglm) 
rsq(sylv3_2_vifglm) #r cuadrado de ºº

# Predicciones de entrenamiento
prediccion_train<-predict.glm(sylv3_2_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$BP)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.747094316030652"

# Predicciones de test
prediccion_test<-predict.glm(sylv3_2_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$BP)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.756519256692847"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ BP, data = data_test)#Adjusted R-squared:  0.1471
summary(r2test)

names(data_test)
ggpairs(data_test[, c(24,35)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

ggplot(data_test[, c(24,35)],aes(x=BP,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_growth_prediction", 
       x = "Psylv_biomass_variation", 
       title = "P. sylv biomass variation prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 1, label.y = -3, size = 5.5) +
  stat_regline_equation(label.x = 1, label.y = -3.3, size = 5.5) 
