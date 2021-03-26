#vamos a analizar la matriz de todas las variables obtenidas para modelar la biomasa forestal

library(pls)#para calcaular pls
library(glmnet)
library(corrr)#correlaciones
library(psych)#graficar distribuciones de variables
library(GGally)#graficar matriz de correlaciones con significancia



#=====Correlación entre columnas numéricas=========================
str(dt)
names(dt)
dt$latitud<-as.numeric(dt$latitud)
dt$longitud<-as.numeric(dt$longitud)

dtilex3<-dt %>% filter(Sp.x == "Quercus ilex" & Sp.y == "Quercus ilex")
dtilex3<-dtilex3 %>% filter(AB3_Mgha_perc>=80)

dtilex3<-dtilex3[, -c(42,47,76,97)]
names(dtilex3)
dtilex3<-dtilex3[, -c(3:6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,39,42:47,49:51,53,55,57,59,61:65,67,69,71,73,94:96,112)]
names(dtilex3)
dtilex3<-na.omit(dtilex3)
summary(dtilex3)
head(dtilex3)

cor<-round(cor(dtilex3, use="complete.obs", method = "pearson"),2)
cor


multi.hist(x = dtilex3, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

ggpairs(dtilex3, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


# División de los datos en train y test
# ==============================================================================
set.seed(1235)
id_train <- sample(1:nrow(dtilex3), size = 0.7*nrow(dtilex3), replace = FALSE)

dt_train <- dtilex3[id_train, ]
dt_test  <- dtilex3[-id_train, ]


# Creación y entrenamiento del modelo
# ==============================================================================
modelo <- glm(AB3_Mgha ~ ., data = dt_train)
summary(modelo)

step(object = modelo, direction = "both", trace = 1)







df_coeficientes <- modelo$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))


# Predicciones de entrenamiento
# ==============================================================================
predicciones_train <- predict(modelo, newdata = dt_train)

# MSE de entrenamiento
# ==============================================================================
training_mse <- mean((predicciones_train - dt_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)


# Predicciones de test
# ==============================================================================
predicciones_test <- predict(modelo, newdata = dt_test)

# MSE de test
# ==============================================================================
test_mse_ols <- mean((predicciones_test - dt_test$AB3_Mgha)^2)
paste("Error (mse) de test:", test_mse_ols)




# Creación y entrenamiento del modelo
# ==============================================================================
modelo <- step(
  object    = lm(formula = AB3_Mgha ~ ., data = dt_train),
  direction = "backward",
  scope     = list(upper = ~., lower = ~1),
  trace     = FALSE
)
summary(modelo)

paste("Número de predictores incluidos en el modelo:", length(modelo$coefficients))

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train <- predict(modelo, newdata = dt_train)

# MSE de entrenamiento
# ==============================================================================
training_mse <- mean((predicciones_train - dt_train$AB3_Mgha)^2)
paste("Error (mse) de entrenamiento:", training_mse)
