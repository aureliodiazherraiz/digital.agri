#### PINEA ####
names(sylv3)

cor_sylv3<-round(cor(sylv3[, -c(1:3,29:32,39:42)], use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
                     tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", diag = F,
                     addshade = "all", order = "FPC")

jpeg(filename = "corr_pinea3_vif.jpeg", width = 1000, height = 1000)
corrplot::corrplot(cor_pinea3, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 55, 
                   tl.cex = 0.5, number.cex = 0.5, addCoef.col = "black", type = "lower", 
                   diag = F, addshade = "all", order = "FPC") ## para visualizar el peso de las variables  
dev.off()


#las vriables que tienen una mayor correlacion con AB3_Mgha son soil_mean & stdv, aet_mean&stdv, mean_ndvi y tree_dens. Con ellas formulamos el modelo

lm(log(AB3_Mgha) ~ (mean_ndvi), data = sylv3) %>% summary()
plot(sylv3$mean_ndvi, log(sylv3$AB3_Mgha))
abline(a=1.2559, b=5.1321, lwd=3, col='blue')


sylv3.nor<- scale(sylv3[,-c(1:3,29:32,39:42)],center=T,scale=T) %>% as.data.frame()


### Calcular VIF basandonos en las funciones vifcor y vifstep
#############################################################

v1 <- vifcor(sylv3.nor, th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(sylv3.nor, th=3)
v2

re1 <- exclude(sylv3.nor,v2)
re1

X11()
plot(re1)

cor_sylv3v2 <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot::corrplot(method = "shade", shade.col = NA, tl.col = "black",
                     tl.srt = 55, tl.cex = 0.8, number.cex = 0.8, 
                     addCoef.col = "black", type = "lower", diag = F, 
                     addshade = "all", order = "FPC")


#### GLM ####
sylv3_vif <- glm(AB3_Mgha ~ Tree_dens + mean_ndvi + X + psb, data = sylv3.nor) 
summary(sylv3_vif)
performance(sylv3_vif)
rsq(sylv3_vif) #r cuadrado de 0.54

sink("sylv3_vif_glm.doc")
print(summary(glm(AB3_Mgha ~ Tree_dens + mean_ndvi + X + psb, data = sylv3.nor)))
sink()  # returns output to the console


#### PODER PREDICTIVO GLM ####
set.seed(123)
data_train <- sylv3.nor %>% sample_frac(.8)
data_test <- setdiff(sylv3.nor, data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


sylv3_vifglm <- glm(AB3_Mgha ~ Tree_dens + mean_ndvi + psb, data=data_train) 

summary(sylv3_vifglm) 
performance(sylv3_vifglm)
rsq(sylv3_vifglm) #r cuadrado de 0.51

# Predicciones de entrenamiento
prediccion_train<-predict.glm(sylv3_vifglm,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.514270464857322"

# Predicciones de test
prediccion_test<-predict.glm(sylv3_vifglm,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.304466591208818"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.6145
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

ggplot(data_test[, c(31,49)],aes(x=AB3_Mgha,y=prediccion))+
  geom_point()+
  geom_smooth(method="glm", se=T)+
  labs(subtitle = "GLM", 
       y = "Biomass_prediction", 
       x = "Psylvestris_biomass", 
       title = "P sylvestris biomass prediction") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 1.8, label.y = -4, size = 5.5) +
  stat_regline_equation(label.x = 1.8, label.y = -4.5, size = 5.5) 


#### GAM ####

sylv3_vifgam <- gam(AB3_Mgha ~ s(Tree_dens) + s(mean_ndvi) + (X), data = sylv3.nor) 

summary(sylv3_vifgam)#R-sq.(adj) = 0.555

sink("sylv3vif_gam.doc")
print(summary(gam(AB3_Mgha ~ s(Tree_dens) + s(mean_ndvi) + (X), data = sylv3.nor)))
sink()  # returns output to the console


# Predicciones de entrenamiento
prediccion_train<-predict.gam(sylv3_vifgam,newdata = data_train)

# MSE de entrenamiento
training_mse <- mean((prediccion_train - data_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)#"Error (mse) de entrenamiento: 0.475141736509419"

# Predicciones de test
prediccion_test<-predict.gam(sylv3_vifgam,newdata = data_test)

# MSE de entrenamiento
test_mse <- mean((prediccion_test - data_test$AB3_Mgha)^2)
paste("Error (mse) del test:", test_mse)#"[1] "Error (mse) del test: 0.227665860590794"

data_test$prediccion<-prediccion_test

r2test<-lm(prediccion ~ AB3_Mgha, data = data_test)#Adjusted R-squared:  0.6714
summary(r2test)

names(data_test)
ggpairs(data_test[, c(31,49)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")




#### ARBOL DE DECISIONES ####
#de alguna forma sirve para ver cuales variables con mas relevantes
set.seed(123)
data_train <- sylv3[, -c(1:3,29:32,39:42)] %>% sample_frac(.8)
data_test <- setdiff(sylv3[, -c(1:3,29:32,39:42)], data_train)

head(data_train)

mean(data_train$AB3_Mgha)
mean(data_test$AB3_Mgha)

hist(data_train$AB3_Mgha)
hist(data_test$AB3_Mgha)


#creamos el CART model
sylv3.tree <- rpart(AB3_Mgha ~ .,data=data_train)

rpart.plot(sylv3.tree, extra = 100)

prp(sylv3.tree)
par(xpd = NA)

jpeg("pinea3.tree.jpg", width=1200, height=820, units="px",
     pointsize=5, bg="white",res=300)
plot(sylv3.tree)
text(sylv3.tree)
dev.off()

printcp(sylv3.tree)
plotcp(sylv3.tree)

sylv3_treeglm <- glm(AB3_Mgha ~ Tree_dens + mean_evi + aet_mean + crad + hillshade, data = sylv3) 
summary(sylv3_vif)
rsq(sylv3_treeglm)


sylv3_treegam <- gam(AB3_Mgha ~ s(Tree_dens) + s(mean_evi) + s(crad) + (hillshade), data = sylv3) 
summary(sylv3_treegam)
