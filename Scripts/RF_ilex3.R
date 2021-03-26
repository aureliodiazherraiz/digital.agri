
#### RANDOM FOREST #### https://www.cienciadedatos.net/documentos/33_arboles_decision_random_forest_gradient_boosting_c50#Predicci%C3%B3n_y_evaluaci%C3%B3n_del_modelo38

# División de los datos en train y test

names(ilex3)
set.seed(123)
train       <- sample(1:nrow(ilex3[, -c(1:5,29,30,39:42)]), size = nrow(ilex3)/2)
datos_train <- ilex3[, -c(1:5,29,30,39:42)][train,]
datos_test  <- ilex3[, -c(1:5,29,30,39:42)][-train,]


# Creación y entrenamiento del modelo

set.seed(123)
modelo.rf  <- ranger(
  formula   = AB3_Mgha ~ .,
  data      = datos_train,
  num.trees = 331,
  seed      = 123
)

print(modelo.rf) 
capture.output(modelo.rf, file="RF_ilex3.doc")

#prediccion y evaluacion del modelo

prediccion.rf <- predict(
  modelo.rf,
  data = datos_test
)

prediccion.rf <- prediccion.rf$predictions
test_rmse    <- sqrt(mean((prediccion.rf - datos_test$AB3_Mgha)^2))
paste("Error de test (rmse) del modelo:", round(test_rmse,2))




#### HIPERPARÁMETROS ####

#Numero de árboles, se puede hacer por dos caminos, con y sin validacion cruzada, pues el proprio random forest hace validacion cruzada

# Validación empleando el Out-of-Bag error (root mean squared error)
# ==============================================================================

# Valores evaluados
num_trees_range <- seq(1, 500, 10)

# Bucle para entrenar un modelo con cada valor de num_trees y extraer su error
# de entrenamiento y de Out-of-Bag.

train_errors <- rep(NA, times = length(num_trees_range))
oob_errors   <- rep(NA, times = length(num_trees_range))

for (i in seq_along(num_trees_range)){
  modelo.rf  <- ranger(
    formula   = AB3_Mgha ~ .,
    data      = datos_train,
    num.trees = num_trees_range[i],
    oob.error = TRUE,
    seed      = 123
  )
  
  predicciones_train <- predict(
    modelo.rf,
    data = datos_train
  )
  predicciones_train <- predicciones_train$predictions
  
  train_error <- mean((predicciones_train - datos_train$AB3_Mgha)^2)
  oob_error   <- modelo.rf$prediction.error
  
  train_errors[i] <- sqrt(train_error)
  oob_errors[i]   <- sqrt(oob_error)
  
}

# Gráfico con la evolución de los errores
df_resulados <- data.frame(n_arboles = num_trees_range, train_errors, oob_errors)
ggplot(data = df_resulados) +
  geom_line(aes(x = num_trees_range, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = num_trees_range, y = oob_errors, color = "oob rmse")) +
  geom_vline(xintercept = num_trees_range[which.min(oob_errors)],
             color = "firebrick",
             linetype = "dashed") +
  labs(
    title = "Evolución del out-of-bag-error vs número árboles",
    x     = "número de árboles",
    y     = "out-of-bag-error (rmse)",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

paste("Valor óptimo de num.trees:", num_trees_range[which.min(oob_errors)]) # "Valor óptimo de num.trees: 311"



# Validación empleando k-cross-validation (root mean squared error). El paquete ranger no tiene ninguna funcionalidad propia para realizar validación cruzada, por lo que se recurre a tidymodels

# Valores evaluados
num_trees_range <- seq(1, 500, 10)

# Bucle para entrenar un modelo con cada valor de num_trees y extraer su error
# de entrenamiento y de Out-of-Bag.

train_errors <- rep(NA, times = length(num_trees_range))
cv_errors    <- rep(NA, times = length(num_trees_range))

for (i in seq_along(num_trees_range)){
  
  # Definición del modelo
  modelo.rf <- rand_forest(
    mode  = "regression",
    trees = num_trees_range[i]
  ) %>%
    set_engine(
      engine = "ranger",
      seed   = 123
    )
  
  # Particiones validación cruzada
  set.seed(1234)
  cv_folds <- vfold_cv(
    data    = datos_train,
    v       = 5,
    repeats = 1
  )
  
  # Ejecución validación cruzada
  validacion_fit <- fit_resamples(
    preprocessor = AB3_Mgha ~ .,
    object       = modelo.rf,
    resamples    = cv_folds,
    metrics      = metric_set(rmse)
  )
  
  # Extraer la métrica de validación 
  cv_error <- collect_metrics(validacion_fit)$mean
  
  # Predicción datos train
  modelo_fit <- modelo.rf %>% fit(AB3_Mgha ~ ., data = datos_train)
  predicciones_train <- predict(
    modelo_fit,
    new_data = datos_train
  )
  predicciones_train <- predicciones_train$.pred
  
  train_error <- sqrt(mean((predicciones_train - datos_train$AB3_Mgha)^2))
  
  # Resultados
  train_errors[i] <- train_error
  cv_errors[i]    <- cv_error
  
}

# Gráfico con la evolución de los errores
df_resulados <- data.frame(n_arboles = num_trees_range, train_errors, cv_errors)
ggplot(data = df_resulados) +
  geom_line(aes(x = num_trees_range, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = num_trees_range, y = cv_errors, color = "cv rmse")) +
  geom_vline(xintercept = num_trees_range[which.min(oob_errors)],
             color = "firebrick",
             linetype = "dashed") +
  labs(
    title = "Evolución del cv-error vs número árboles",
    x     = "número de árboles",
    y     = "cv-error (rmse)",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

paste("Valor óptimo de num.trees:", num_trees_range[which.min(cv_errors)]) #[1] "Valor óptimo de num.trees: 201"
#Ambas métricas indican que, a partir de entre 331 y 201 árboles, el error de validación del modelo se estabiliza. 


#### hiperparámetros mtry (permite controlar cuánto se decorrelacionan los árboles entre sí) #### 
#se puede hacer siguiendo dos caminos, con y sin validacion cruzada.


## Validación empleando el Out-of-Bag error (root mean squared error)

# Valores evaluados
mtry_range <- seq(1, ncol(datos_train)-1)

# Bucle para entrenar un modelo con cada valor de mtry y extraer su error de entrenamiento y de Out-of-Bag.

train_errors <- rep(NA, times = length(mtry_range))
oob_errors   <- rep(NA, times = length(mtry_range))

for (i in seq_along(mtry_range)){
  modelo  <- ranger(
    formula   = AB3_Mgha ~ .,
    data      = datos_train,
    num.trees = 100,
    mtry      = mtry_range[i],
    oob.error = TRUE,
    seed      = 123
  )
  
  predicciones_train <- predict(
    modelo,
    data = datos_train
  )
  predicciones_train <- predicciones_train$predictions
  
  train_error <- mean((predicciones_train - datos_train$medv)^2)
  oob_error   <- modelo$prediction.error
  
  train_errors[i] <- sqrt(train_error)
  oob_errors[i]   <- sqrt(oob_error)
  
}

# Gráfico con la evolución de los errores
df_resulados <- data.frame(mtry = mtry_range, train_errors, oob_errors)
ggplot(data = df_resulados) +
  geom_line(aes(x = mtry_range, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = mtry_range, y = oob_errors, color = "oob rmse")) +
  geom_vline(xintercept =  mtry_range[which.min(oob_errors)],
             color = "firebrick",
             linetype = "dashed") +
  labs(
    title = "Evolución del out-of-bag-error vs mtry",
    x     = "mtry",
    y     = "out-of-bag-error (rmse)",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

paste("Valor óptimo de mtry:", mtry_range[which.min(oob_errors)]) #"Valor óptimo de mtry: 26"



## Validación empleando k-cross-validation (root mean squared error)

# Valores evaluados
mtry_range <- seq(1, ncol(datos_train)-1)

# Bucle para entrenar un modelo con cada valor de mtry y extraer su error
# de entrenamiento y de Out-of-Bag.

train_errors <- rep(NA, times = length(mtry_range))
cv_errors    <- rep(NA, times = length(mtry_range))

for (i in seq_along(mtry_range)){
  
  # Definición del modelo
  modelo.rf <- rand_forest(
    mode  = "regression",
    trees = 100,
    mtry  = mtry_range[i]
  ) %>%
    set_engine(
      engine = "ranger",
      seed   = 123
    )
  
  # Particiones validación cruzada
  set.seed(1234)
  cv_folds <- vfold_cv(
    data    = datos_train,
    v       = 5,
    repeats = 1
  )
  
  # Ejecución validación cruzada
  validacion_fit <- fit_resamples(
    preprocessor = AB3_Mgha ~ .,
    object       = modelo.rf,
    resamples    = cv_folds,
    metrics      = metric_set(rmse)
  )
  
  # Extraer datos de validación
  cv_error <- collect_metrics(validacion_fit)$mean
  
  # Predicción datos train
  modelo_fit <- modelo.rf %>% fit(AB3_Mgha ~ ., data = datos_train)
  predicciones_train <- predict(
    modelo_fit,
    new_data = datos_train
  )
  predicciones_train <- predicciones_train$.pred
  
  train_error <- sqrt(mean((predicciones_train - datos_train$medv)^2))
  
  # Resultados
  train_errors[i] <- train_error
  cv_errors[i]    <- cv_error
  
}

# Gráfico con la evolución de los errores
df_resulados <- data.frame(mtry = mtry_range, train_errors, cv_errors)
ggplot(data = df_resulados) +
  geom_line(aes(x = mtry_range, y = train_errors, color = "train error")) + 
  geom_line(aes(x = mtry_range, y = cv_errors, color = "cv error")) +
  geom_vline(xintercept =  mtry_range[which.min(cv_errors)],
             color = "firebrick",
             linetype = "dashed") +
  labs(
    title = "Evolución del out-of-bag-error vs mtry",
    x     = "mtry",
    y     = "cv-error (mse)",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

paste("Valor óptimo de mtry:", mtry_range[which.min(cv_errors)]) #[1] "Valor óptimo de mtry: 46"


#### GRID SEARCH ####

#Aunque el análisis individual de los hiperparámetros es útil para entender su impacto en el modelo e identificar rangos de interés, la búsqueda final no debe hacerse de forma secuencial, ya que cada hiperparámetro interacciona con los demás. Es preferible recurrir a grid search o random search para analizar varias combinaciones de hiperparámetros.

## Grid de hiperparámetros evaluados empleando el Out-of-Bag error

param_grid = expand_grid(
  'num_trees' = c(50, 100, 500, 1000, 5000),
  'mtry'      = c(3, 5, 7, ncol(datos_train)-1),
  'max_depth' = c(1, 3, 10, 20)
)

# Loop para ajustar un modelo con cada combinación de hiperparámetros
# ==============================================================================

oob_error = rep(NA, nrow(param_grid))

for(i in 1:nrow(param_grid)){
  
  modelo <- ranger(
    formula   = AB3_Mgha ~ .,
    data      = datos_train, 
    num.trees = param_grid$num_trees[i],
    mtry      = param_grid$mtry[i],
    max.depth = param_grid$max_depth[i],
    seed      = 123
  )
  
  oob_error[i] <- sqrt(modelo$prediction.error)
}


# Resultados
resultados <- param_grid
resultados$oob_error <- oob_error
resultados <- resultados %>% arrange(oob_error)

# Mejores hiperparámetros por out-of-bag error
head(resultados, 1)


#Una vez identificados los mejores hiperparámetros, se reentrena el modelo indicando los valores óptimos en sus argumentos.



## Grid Search basado en validación cruzada

# DEFINICIÓN DEL MODELO Y DE LOS HIPERPARÁMETROS A OPTIMIZAR

modelo.rf <- rand_forest(
  mode  = "regression",
  mtry  = tune(),
  trees = tune()) %>% 
  set_engine(
    engine     = "ranger",
    max.depth  = tune(),
    importance = "none",
    seed       = 123)

# DEFINICIÓN DEL PREPROCESADO

# En este caso no hay preprocesado, por lo que el transformer solo contiene
# la definición de la fórmula y los datos de entrenamiento.
transformer <- recipe(
  formula = AB3_Mgha ~ .,
  data    =  datos_train
)

# DEFINICIÓN DE LA ESTRATEGIA DE VALIDACIÓN Y CREACIÓN DE PARTICIONES

set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  strata  = AB3_Mgha
)

# WORKFLOW

workflow_modelado <- workflow() %>%
  add_recipe(transformer) %>%
  add_model(modelo.rf)


# GRID DE HIPERPARÁMETROS

hiperpar_grid <- expand_grid(
  'trees'     = c(50, 100, 500, 1000, 5000),
  'mtry'      = c(3, 5, 7, ncol(datos_train)-1),
  'max.depth' = c(1, 3, 10, 20)
)

# EJECUCIÓN DE LA OPTIMIZACIÓN DE HIPERPARÁMETROS

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

grid_fit <- tune_grid(
  object    = workflow_modelado,
  resamples = cv_folds,
  metrics   = metric_set(rmse),
  grid      = hiperpar_grid
)

stopCluster(cl)

# Mejores hiperparámetros por validación cruzada

show_best(grid_fit, metric = "rmse", n = 1)# mttry 0 47, trees = 1000, max.dep = 20, std_err = 0.786


# ENTRENAMIENTO FINAL

mejores_hiperpar <- select_best(grid_fit, metric = "rmse")

modelo_final_fit <- finalize_workflow(
  x = workflow_modelado,
  parameters = mejores_hiperpar
) %>%
  fit(
    data = datos_train
  ) %>%
  pull_workflow_fit()


# Error de test del modelo final

predicciones <- modelo_final_fit %>%
  predict(
    new_data = datos_test,
    type     = "numeric"
  )

predicciones <- predicciones %>% 
  bind_cols(datos_test %>% dplyr::select(AB3_Mgha))

rmse_test  <- rmse(
  data     = predicciones,
  truth    = AB3_Mgha,
  estimate = .pred,
  na_rm    = TRUE
)
rmse_test #da lo mismo, rmse = 25.6


#### Importancia de predictores ####


# Entrenamiento modelo
modelo <- rand_forest(
  mode  = "regression"
) %>%
  set_engine(
    engine     = "ranger",
    importance = "permutation",#podemos colocar "impurity"
    seed       = 123
  )

modelo <- modelo %>% finalize_model(mejores_hiperpar)
modelo <- modelo %>% fit(AB3_Mgha ~., data = datos_train)

# Importancia
importancia_pred <- modelo$fit$variable.importance %>%
  enframe(name = "predictor", value = "importancia")

# Gráfico
ggplot(
  data = importancia_pred,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores (pureza de nodos)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")


## los predictores mejor situados por impurity son mean_ndvi, Tree_dens, pet_mean, n_sup y slope, descartando predictores correlacionados con el ndvi.
## ya los predictores mejor situados por permutation son soil_max, Tree_dens, mean_ndvi, pet_mean y aet_mean.
