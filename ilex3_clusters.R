#vamos a analizar por cluster de variables cuales son mas significativas frente a la biomasa. para eso primero analizaremos la relacion de la biomasa frente al resto de las variables en el ifn3

names(ilex3.nor)

#### ilex ####
#### VARIABLES CLIMÁTICAS (terraclimate) ####
## Calcular VIF basandonos en las funciones vifcor y vifstep ##

cor_ilex3v2 <- round(cor(re1, use="complete.obs"),2) %>% 
  corrplot(type = "lower", method = "shade", 
           shade.col = NA, tl.col = "black",
           tl.srt = 55, tl.cex = 0.8, number.cex = 0.8, 
           addCoef.col = "black", diag = F, 
           addshade = "all", order = "FPC")

v1 <- vifcor(ilex3.nor[, c(6:18,33)], th=0.8)#0.8 es la significancia del 80%#
v1
v2 <- vifstep(ilex3.nor[, c(6:18,33)], th=3)
v2

re1 <- exclude(ilex3.nor,v2)
names(re1)


###aplicamos el modelo ###
ilex3_clim<-as.formula(AB3_Mgha ~ aet_stdv + pet_max + pet_min + soil_min + srad_max)

multmodel_ilex_clim <- glmulti(ilex3_clim, data=ilex3.nor, fitfunction=glm, level=1, confsetsize=100, method="h", crit="aicc")
plot(multmodel_ilex_clim, type="s") ## para visualizar el peso de las variables

multmodel_ilex_clim@formulas[1]

weightable(multmodel_ilex_clim)

#Best model: AB3_Mgha~1+aet_stdv+pet_max+pet_min+soil_min
#Crit= 5194.34723529062
#Mean crit= 5227.68535937383
      
