#Ejercicio GAM
#primeramente vamos a ver la relacion de puntos entre las variables de un
# dataframe ejercicio para ver que no son lineales

library(MASS)
mcycle<-mcycle
head(mcycle)
plot(mcycle)

lm_mod <- lm(accel~times, data = mcycle)
summary(lm_mod)

termplot(lm_mod, partial.resid = T, se = T)

#ahora usando el paquete mgcv vamos a ver los posibles modelos aditivos
library(mgcv)

# cuando la variable la presuponemos no lineal la introducimos junto a s entre paréntesis 
gam_mod <- gam(accel ~ s(times), data = mcycle)
summary(gam_mod)

#gam(y ~ s(x), data = dat, method = "REML"),# se recomienda utilizar este método
#a fin de limitar el indice de suavidad o smoothing al máximo de la probabilidad

plot(gam_mod, residuals = T, pch = 1)

#para poder estudiar mas a fondo el modelo estudiamos los coeficientes, aparecen 9 funciones
coef(gam_mod)

#despues empezamos a jugar con lamda, o sea el indice de la funcion de suavidad
#y vamos viendo como se va modificando sin exagerar y cometer overfitting

gam_mod_k1 <- gam(accel ~ s(times, k = 1), data = mcycle)
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)
gam_mod_k5 <- gam(accel ~ s(times, k = 5), data = mcycle)
gam_mod_k7 <- gam(accel ~ s(times, k = 7), data = mcycle)
gam_mod_k15 <- gam(accel ~ s(times, k = 15), data = mcycle)
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data = mcycle)

#ahora los graficamos
par(mfrow = c(1, 2))
plot(gam_mod_k1, residuals = TRUE, pch = 1)
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k5, residuals = TRUE, pch = 1)
plot(gam_mod_k7, residuals = TRUE, pch = 1)
plot(gam_mod_k15, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)

#para evitar overfitting usamos parámetros de suavidad (smoothing) llamado de sp
gam_mod <- gam(accel ~ s(times), data = mcycle, method = "REML")

gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)
gam_mod_s3 <- gam(accel ~ s(times), data = mcycle, sp = 0.0007)

#ahora los graficamos para ver las diferencias
par(mfrow = c(1,1))
plot(gam_mod, residuals = T, pch = 1)
plot(gam_mod_s1, residuals = T, pch = 1)
plot(gam_mod_s2, residuals = T, pch = 1)
plot(gam_mod_s3, residuals = T, pch = 1)

#continuando con el ejercicio del curso y ver como se produce el overfitting
#cuando exageramos en el número de funciones no lineales (k) y el indice de suavidad (sp)
#si aumentamos a lo bestia sp (sp = 1000), la relacioin tiende a ser lineal, perdiendo la curvatura

gam_mod_sk <- gam(accel ~ s(times, k = 50), data = mcycle, sp = 0.0001)
plot(gam_mod_sk, residuals = T, pch = 1)


#cuando aplicamos multivariado
# cuando usamos variables categóricas, hay que transformar a factores pues
#El paquete mgcv no usa variables de caracteres.

library(gamair)
data("mpg", package="gamair")

dt.mpg <- mpg
str(dt.mpg)
head(dt.mpg)

mod_city <- gam(city.mpg ~ s(weight) + s(length) + s(price), data = dt.mpg, method = "REML")
par(mfrow = c(1,3))
plot(mod_city, residuals = T, pch = 1)
summary(mod_city)

mod_city_2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + fuel + drive + style, data = dt.mpg, method = "REML")
par(mfrow = c(1,3))
plot(mod_city_2, residuals = T, pch = 1)
# en el proximo grafico muestra la relacion de la variable ciudad en funcion del resto
#sabiendo que las ultimas tres la toma como lineales, las cuales además son factores
par(mfrow = c(1,3))
plot(mod_city_2, all.terms = TRUE, pages = 3)
summary(mod_city_2)


#ahora vamos a inserir el "by" dentro de cada variable, en este caso la variable drive
mod_city_3 <- gam(city.mpg ~ s(weight, by = drive) + s(length, by = drive) + s(price, by = drive), data = dt.mpg, method = "REML")
par(mfrow = c(1,3))
plot(mod_city_3, residuals = T, pch = 1)
summary(mod_city_3)

#continuamos con el ejercico
mod_city_4 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width),
                 data = mpg, method = "REML")
summary(mod_city_4)

#ahora estudiaremos los gráficos para analizar las posibles interpretaciones
plot(mod_city_4)
?plot.mod

plot(mod_city_4, select = c(2,3))
plot(mod_city_4, pages = 1)#mete todos los gráficos en una unica hoja
plot(mod_city_4, pages = 1, all.terms = T)
plot(mod_city_4, rug = T)
plot(mod_city_4, residuals = T)

plot(mod_city_4, residuals = T, pch = 1, cex = 1)#el mas chulo pues coloca los puntos

#mostrando el error estandar
plot(mod_city_4, se = T)
plot(mod_city_4, shade = T)
plot(mod_city_4, shade = T, shade.col = "lightblue")#el mas bonito con el efecto sombra

#para ajustar el gráfico a la escala partiendo del 0 en cada una de las variables
plot(mod_city_4, seWithMean = TRUE, shift = coef(mod_city_4)[1])


#continuamos con el curso
mod <- gam(accel~s(times), data = mcycle, method = "REML")
plot(mod, pages = 1, all.terms = T,
     seWithMean = TRUE, shift = coef(mod)[1], 
     shade = T, shade.col = "lightblue" )

plot(mod, pages = 1, all.terms = T, residuals = T, pch = 1, cex = 1)

mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")
plot(mod, select = 3)
plot(mod, pages = 2, all.terms = T, residuals = T, 
     seWithMean = TRUE, shift = coef(mod)[1], 
     pch = 1, cex = 1)

mod <- gam(hw.mpg ~ s(weight) +s(rpm) + s(price), 
           data = mpg, method = "REML")
plot(mod, pages = 2, all.terms = T, residuals = T, 
     seWithMean = TRUE, shift = coef(mod)[1], 
     shade = T, shade.col = "hotpink",
     pch = 1, cex = 1)

#para verificar los modelos GAM se deve utilizar la funcion gam.check

set.seed(0)
dat <- gamSim(1, n = 200)

mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")
gam.check(mod, pages = 1)

set.seed(0)
dat <- gamSim(1, n = 600, scale = 0.6, verbose = F)
mod1 <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + 
                    s(x2, k = 3) + s(x3, k = 3), 
            data = dat, method = "REML")
gam.check(mod1)

mod2 <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + 
                    s(x2, k = 10) + s(x3, k = 3), 
            data = dat, method = "REML")
gam.check(mod2)
