#Decision tree libraria rpart y rpart.plot
#creamos dos dataframes con los datos del IFN2 y IFN3
#uno de ellos servirá para crear el modelo el otro para validarlo

ifn2<-indbio_2_3[, c(8,9,11,13)] %>% filter(Sp.y %in% c("Quercus ilex",
                                                   "Quercus suber",
                                                   "Pinus pinea",
                                                   "Pinus halepensis",
                                                   "Pinus nigra",
                                                   "Pinus pinaster"))
summarise_all(ifn2, funs(sum(is.na(.))))
ifn2 <- ifn2[!is.na(ifn2$NDVI_IFN2),]
ifn2 <- na.omit(ifn2)

ifn3<-indbio_2_3[, c(4,5,11,12)] %>% filter(Sp.x %in% c("Quercus ilex",
                                                        "Quercus suber",
                                                        "Pinus pinea",
                                                        "Pinus halepensis",
                                                        "Pinus nigra",
                                                        "Pinus pinaster"))
summarise_all(ifn3, funs(sum(is.na(.))))
ifn3 <- ifn3[!is.na(ifn3$NDVI_IFN2),]


set.seed(123)
split <- sample.split(ifn2$Sp.y, SplitRatio = 0.7)
train <- subset(ifn2, split==TRUE)
test <- subset(ifn2, split==FALSE)

#creamos el CART model
ifn2.dtree <- rpart(Sp.y ~ NDVI_IFN2 + AB2_Mgha + Arid_Dm, 
                       method = "class",
                       data=train)
print(ifn2.dtree)

rpart.plot(ifn2.dtree, extra = 100)

prp(ifn2.dtree)
par(xpd = NA)
plot(ifn2.dtree)
text(ifn2.dtree)

printcp(ifn2.dtree)
plotcp(ifn2.dtree)

#Validamos el modelo con os datos del test, 
#los cuales podrian ser substituidos por el ifn3 alterando el nombre de las columnas
prediction<-predict(ifn2.dtree, test, type = 'class')
table_predict<-table(test$Sp.y, prediction)
table_predict
write.table(x = table_predict, file = "predictores_ifn2tree.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)

#medimos la accuracy
accuracy_Test <- sum(diag(table_predict)) / sum(table_predict)
print(paste('Accuracy for test', accuracy_Test))



#podriamos realizar el mismo procedimiento para el IFN3 como test
names(ifn3)[c(1,2,4)]<-c("AB2_Mgha","Sp.y","NDVI_IFN2")
summarise_all(ifn3, funs(sum(is.na(.))))

prediction2<-predict(ifn2.dtree, ifn3, type = 'class')

table_predict2<-table(ifn3$Sp.y, prediction2)
table_predict2
write.table(x = table_predict2, file = "predictores_ifn3tree.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)

#medimos la accuracy
accuracy_Test <- sum(diag(table_predict2)) / sum(table_predict2)
print(paste('Accuracy for test', accuracy_Test))



#random forest

#usando libraria ranger(hiperparamtros) y MASS para variables no categoricas
#ISLR para variables categoricas
# Selección de una submuestra del 70% de los datos

set.seed(123)
train_rdft <- sample(1:nrow(ifn2), size = nrow(ifn2)/2)
dt_train <- ifn2[train_rdft,]
dt_test  <- ifn2[-train_rdft,]

# Grid de hiperparámetros evaluados
param_grid = expand_grid(
  'num_trees' = c(50, 100, 500, 1000, 5000),
  'max_depth' = c(1, 3, 10, 20)
)

# Loop para ajustar un modelo con cada combinación de hiperparámetros

oob_error = rep(NA, nrow(param_grid))

for(i in 1:nrow(param_grid)){
    modelo <- ranger(
    formula   = Sp.y ~ .,
    data      = dt_train, 
    num.trees = param_grid$num_trees[i],
    max.depth = param_grid$max_depth[i],
    seed      = 123
  )
    oob_error[i] <- modelo$prediction.error
}

resultados <- param_grid
resultados$oob_error <- oob_error
resultados <- resultados %>% arrange(oob_error)

head(resultados, 1)

#continuar con el ejemplo de clasificacion:
#https://rpubs.com/Joaquin_AR/255596


