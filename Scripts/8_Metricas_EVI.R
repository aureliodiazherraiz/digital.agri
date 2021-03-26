#introducidmos los valores del indice EVI asi como los valores del NDVi 
#y de la topografia para poder comparar

#los valores de relacionados con la topografia fueron obtenidos del qgis
srtm_anda<-read_csv("SRTM/Metricas_topograficas_Andalucia_IFN.csv")

EVI_95 <- read_excel("indices_nd_savi/LANDSAT_ANUAL_95_EVI.xlsx")
EVI_94 <- read_excel("indices_nd_savi/LANDSAT_ANUAL_94_EVI.xlsx")
EVI_06 <- read_excel("indices_nd_savi/LANDSAT_ANUAL_06_EVI.xlsx")
EVI_07 <- read_excel("indices_nd_savi/LANDSAT_ANUAL_07_EVI.xlsx")
Te_cli_95 <- read_excel("indices_nd_savi/TERRACLIMATE_95.xlsx")
Te_cli_94 <- read_excel("indices_nd_savi/TERRACLIMATE_94.xlsx")
Te_cli_06 <- read_excel("indices_nd_savi/TERRACLIMATE_06.xlsx")
Te_cli_07 <- read_excel("indices_nd_savi/TERRACLIMATE_07.xlsx")
fpar_lai_06 <- read_excel("indices_nd_savi/FPAR_LAI_06.xlsx")
fpar_lai_07 <- read_excel("indices_nd_savi/FPAR_LAI_07.xlsx")


EVI_94$year<-1994
EVI_95$year<-1995
EVI_06$year<-2006
EVI_07$year<-2007
Te_cli_06$year<-2006
Te_cli_07$year<-2007
Te_cli_95$year<-1995
Te_cli_94$year<-1994
fpar_lai_06$year<-2006
fpar_lai_07$year<-2007


evi<-rbind(EVI_06,EVI_07,EVI_94,EVI_95)
te_clim<-rbind(Te_cli_06,Te_cli_07,Te_cli_94,Te_cli_95)
fpar_lai<-rbind(fpar_lai_06,fpar_lai_07)

#introducimos el código de cada parcela para poder filtrar los años del ifn
ifn_cod <- read_csv("ifn_cod.csv")

evi<-merge(evi, ifn_cod, by= c("latitud", "longitud"))
te_clim<-merge(te_clim, ifn_cod, by= c("latitud", "longitud"))
fpar_lai<-merge(fpar_lai, ifn_cod, by= c("latitud", "longitud"))
#data.new<-Reduce(merge, list(evi, ifn_cod, te_clim, fpar_lai))

#afinamos para cada provincia dejando apenas el año de cada IFN
evi[c(evi$cod %in% c(4,11,14,18,21,29,41) & evi$year %in% c(1994,1996)),] <- NA
evi[c(evi$cod %in% c(23) & evi$year %in% c(1995,1996)),] <- NA

evi[c(evi$cod %in% c(4,11,18,21,29,41) & evi$year == 2006),] <- NA
evi[c(evi$cod %in% c(14,23) & evi$year == 2007),] <- NA
evi <- na.omit(evi)
summary(evi)

te_clim[c(te_clim$cod %in% c(4,11,14,18,21,29,41) & te_clim$year %in% c(1994,1996)),] <- NA
te_clim[c(te_clim$cod %in% c(23) & te_clim$year %in% c(1995,1996)),] <- NA

te_clim[c(te_clim$cod %in% c(4,11,18,21,29,41) & te_clim$year == 2006),] <- NA
te_clim[c(te_clim$cod %in% c(14,23) & te_clim$year == 2007),] <- NA
te_clim <- na.omit(te_clim)

fpar_lai[c(fpar_lai$cod %in% c(4,11,18,21,29,41) & fpar_lai$year == 2006),] <- NA
fpar_lai[c(fpar_lai$cod %in% c(14,23) & fpar_lai$year == 2007),] <- NA
fpar_lai <- na.omit(fpar_lai)


#hemos encontrado valores de maximo y minimo fuera del rango -1/1 por lo que seran eliminados
summary(evi)
evi<-evi %>% filter(max_evi<=1)
evi<-evi %>% filter(min_evi>=-1)
summary(evi)

cor_evi<-round(cor(evi, use="complete.obs"),2)
corrplot(cor_evi, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black", 
         type = "lower", diag = F, addshade = "all", order = "AOE")

#debido a la colinealidad se retiran las variables max, min y mediana para el evi

evi_total<-evi#para utilizarlo en otro script
write.csv(evi_total, file = "evidata.csv")
evi<-evi[, -c(3:5,7,10)]


cor_teclim<-round(cor(te_clim, use="complete.obs"),2)
corrplot(cor_teclim, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black", 
         type = "lower", diag = F, addshade = "all", order = "AOE")

#volvemos a hacer lo mismo con los parametros climaticos, eliminando 
te_clim_total<-te_clim
write.csv(te_clim_total, file = "climadata.csv")
te_clim<-te_clim[, -c(3:5,7,11:13,15,17,21)]

cor_fparlai<-round(cor(fpar_lai, use="complete.obs"),2)
corrplot(cor_fparlai, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black", 
         type = "lower", diag = F, addshade = "all", order = "AOE")

#debido a existir elevadas correlaciones entre media con máx y con mediana eliminamos la mediana
#es posible que en lugar de eliminar la mediana pudiese ser elimnada la media 
#para mantener la mediana y el max
fpar_lai_total<-fpar_lai
write.csv(fpar_lai_total, file = "fpar_laidata.csv")
fpar_lai<-fpar_lai[, -c(3:5,7,8,11,13)]

#comenzamos a juntar los dataframes obteniendo las variables para cada punto

evi$year<-as.character(evi$year)
evi$year<-str_replace_all(evi$year, "1995", "ifn2")
evi$year<-str_replace_all(evi$year, "1994", "ifn2")
evi$year<-str_replace_all(evi$year, "2006", "ifn3")
evi$year<-str_replace_all(evi$year, "2007", "ifn3")

te_clim$year<-as.character(te_clim$year)
te_clim$year<-str_replace_all(te_clim$year, "1995", "ifn2")
te_clim$year<-str_replace_all(te_clim$year, "1994", "ifn2")
te_clim$year<-str_replace_all(te_clim$year, "2006", "ifn3")
te_clim$year<-str_replace_all(te_clim$year, "2007", "ifn3")

fpar_lai$year<-str_replace_all(fpar_lai$year, "2006", "ifn3")
fpar_lai$year<-str_replace_all(fpar_lai$year, "2007", "ifn3")

#para trabajar despues con mas facilidad pivoteamos aumentando las variables
names(evi)
evi_pw<-pivot_wider(evi, names_from = year, values_from = c("mean_evi", "st_d_evi"))
names(te_clim)
te_clim_pw<-pivot_wider(te_clim, names_from = year, values_from = c("aet_mean", "pet_max",
                                                                    "pet_min", "pet_mean",
                                                                    "soil_mean", "srad_max", 
                                                                    "srad_mean", "srad_stdv"))

#ahora podemos integrar EVI, NDVI y variables topograficas, archivo que ya lo teniamos creado anteriormente
#parece haber problemas con la longitud de los caracteres en las variables longitud y latitud
srtm_AUC <- read_csv("indices_srtm_AUC.csv")

clim_lai<-Reduce(merge, list(te_clim_pw, fpar_lai))

str_length(srtm_AUC$latitud)
str_length(clim_lai$latitud)

#igualamos la longitud de las entradas de las variables
srtm_AUC [,2:3] <- lapply(srtm_AUC[,2:3], str_sub, 1, 6)
clim_lai [,1:2] <- lapply(clim_lai[,1:2], str_sub, 1, 6)

#unimos los dataframes de topografia con los de los indices espectrales
final <- merge(clim_lai, srtm_AUC, by = c("latitud", "longitud"))
str(final)

final$latitud<-as.numeric(final$latitud)
final$longitud<-as.numeric(final$longitud)

final %>% filter(Sp.y %in% c("Quercus ilex", "Quercus suber", 
                                        "Pinus pinea","Pinus pinaster", "Pinus nigra",
                                        "Pinus halepensis", "Olea europaea", "Pinus sylvestris")) %>% 
  ggplot(aes(Sp.y, BP_2_3)) + 
  geom_point(shape = 1) + 
  geom_smooth(method=lm)

bigot<-final %>% 
  filter(Sp.y %in% c("Quercus ilex", "Quercus suber", 
                                        "Pinus pinea","Pinus pinaster", 
                                        "Pinus nigra","Pinus halepensis", 
                                        "Olea europaea", "Pinus sylvestris")) %>% 
  ggplot(aes(x = RGR, y = Sp.y, color = Sp.y)) +
  geom_boxplot() +
  theme_bw()

names(final)
final<-final[, -c(3,23:25,51,52,61,62,64:68)]

#creamos nuevas variables con las variaciones entre inventarios forestales
final<-final %>% mutate(dens =Tree_dens-Tree_dens2)
final<-final %>% mutate(max_ndvi = max_ndvi_ifn3-max_ndvi_ifn2)
final<-final %>% mutate(mean_ndvi = mean_ndvi_ifn3-mean_ndvi_ifn2)
final<-final %>% mutate(st_dv_ndvi = standard_deviation_ndvi_ifn3- standard_deviation_ndvi_ifn2)
final<-final %>% mutate(min_ndvi = abs(min_ndvi_ifn3)-abs(min_ndvi_ifn2))

final<-final %>% mutate(max_evi = max_evi_ifn3-max_evi_ifn2)
final<-final %>% mutate(mean_evi = mean_evi_ifn3-mean_evi_ifn2)
final<-final %>% mutate(st_dv_evi = st_d_evi_ifn3-st_d_evi_ifn2)
final<-final %>% mutate(min_evi = abs(min_evi_ifn3)-abs(min_evi_ifn2))

final<-final %>% mutate(aet_mean = aet_mean_ifn3-aet_mean_ifn2)
final<-final %>% mutate(pet_max = pet_max_ifn3-pet_max_ifn2)
final<-final %>% mutate(pet_min = pet_min_ifn3-pet_min_ifn2)
final<-final %>% mutate(pet_mean = pet_mean_ifn3-pet_mean_ifn2)
final<-final %>% mutate(srad_max = srad_max_ifn3-srad_max_ifn2)
final<-final %>% mutate(srad_mean = srad_mean_ifn3-srad_mean_ifn2)
final<-final %>% mutate(srad_stdv = srad_stdv_ifn3-srad_stdv_ifn2)

final<-final[, order(c(names(final)))]

#por un acaso si existe algun duplicado entre las observaciones
final %>% count(Sp.x)
final[duplicated(c("latitud","longitud")),]
head(final)

spdt<-read_csv("edafo_anda.csv")
names(spdt)[17]<-"longitud"
names(spdt)[18]<-"latitud"

#introducimos las variables edáficas

str_length(spdt$latitud)
str_length(final$latitud)

#igualamos la longitud de las entradas de las variables
spdt [,17:18] <- lapply(spdt[,17:18], str_sub, 1, 6)

finaldt<-merge(final, spdt, by = c("latitud","longitud"))

write_csv(finaldt, file = "total_variables.csv")

names(finaldt)

#ahora podemos seguir dos caminos, crear un modelo con los incrementos de las variables frente a la variable
#dependiente que es la variacion de biomasa, o podemos sumar las variables del IFN2 a las del IFN3
#teniendo asi el dobre de entradas y relacionando la variable biomasa con el resto

#vamos a estudiar la variacion o incremento de las variables en cada especie

qilex_var<-finaldt %>% filter(Sp.x == "Quercus ilex" & Sp.y == "Quercus ilex")
qsuber_var<-final %>% filter(Sp.x == "Quercus suber" & Sp.y == "Quercus suber")
ppinea_var<-final %>% filter(Sp.x == "Pinus pinea" & Sp.y == "Pinus pinea")
ppinaster_var<-final %>% filter(Sp.x == "Pinus pinaster" & Sp.y == "Pinus pinaster")
pnigra_var<-final %>% filter(Sp.x == "Pinus nigra" & Sp.y == "Pinus nigra")
phale_var<-final %>% filter(Sp.x == "Pinus halepensis" & Sp.y == "Pinus halepensis")
oeuro_var<-final %>% filter(Sp.x == "Olea europaea" & Sp.y == "Olea europaea")
psylv_var<-final %>% filter(Sp.x == "Pinus sylvestris" & Sp.y == "Pinus sylvestris")


##############
#quercus ilex#
##############

###empezamos por Q ilex estudiando el crecimiento de la especie y la variacion de variables entre ifn----
names(qilex_var)

qilex_ifn<-qilex_var[, c(5,7,10:13,15,16:18,22:24,27,28,31,34,39,42,45,48,51,54,55,60,63,66,71,72)]
qilex_ifn<-qilex_ifn %>% filter(AB3_Mgha_perc>=80)
names(qilex_ifn)

cor_qilex_ifn<-round(cor(qilex_ifn, use="complete.obs"),2)
corrplot(cor_qilex_ifn, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 55, tl.cex = 0.5, number.cex = 0.75, 
         addCoef.col = "black", type = "lower", diag = F, 
         addshade = "all")

#aunque la densidad explique gran parte de la variabilidad del crecimiento, al no ser una variable remota,
#es descartada como variable explicativa
gam.ilex<-gam(BP_2_3 ~ s(slope_grd) + s(AUC_ndvi) +
                s(eleva_m) + s(longitud) + s(Arid_Dm) +
                s(st_dv_ndvi) + s(srad_stdv) +
                s(pet_max) + s(st_dv_evi) +
                s(mean_ndvi), 
              data = qilex_ifn, method="REML")
summary(gam.ilex)

#algunas variables se muestran lineales por lo que las sacamos de los terminos no lineales o s

gam.ilex<-gam(BP_2_3 ~ s(Arid_Dm) + s(AUC_ndvi) + s(st_dv_evi) + (eleva_m) +
                (longitud) + (mean_ndvi) + (slope_grd) + (pet_max) + 
                (st_dv_ndvi) + (srad_stdv), 
              data = qilex_ifn, method="REML")
summary(gam.ilex)

#descartamos las variables menos significativas para simplificar el modelo sin alterar mucho la variabilidad explicada

gam.ilex<-gam(BP_2_3 ~ s(Arid_Dm) + s(AUC_ndvi) + s(st_dv_evi) + (eleva_m) +
                (st_dv_ndvi) + (srad_stdv), 
              data = qilex_ifn, method="REML")
summary(gam.ilex)
gam.check(gam.ilex)
plot(gam.ilex)


glm.ilex<-gam(BP_2_3 ~ (slope_grd) + (AUC_ndvi) +
                (eleva_m) + (longitud) + (Arid_Dm) +
                (st_dv_ndvi) + (srad_stdv) +
                (pet_max) + (st_dv_evi) +
                (mean_ndvi), 
              data = qilex_ifn, method="REML")
summary(glm.ilex)
performance(glm.ilex)

#vamos a estudiar multimodelos (glm) todas las posibles combinaciones de variables en los modelos para escoger el mejor
#primero se crea el modelo
ilex_23<-as.formula(BP_2_3 ~ (slope_grd) + (AUC_ndvi) +
                    (eleva_m) + (longitud) + (Arid_Dm) +
                    (st_dv_ndvi) + (srad_stdv) +
                    (pet_max) + (st_dv_evi) +
                    (mean_ndvi))

multimodelo_ilex_23 <- glmulti(ilex_23, data=qilex_ifn, fitfunction=glm, level=1, confsetsize=100, method="h", crit="aicc")
weightable(multimodelo_ilex_23)
cumsum(weightable(multimodelo_ilex_23)$weights) ## suma acumulada a partir del mejor modelo (primero [1])
sum(weightable(multimodelo_ilex_23)$weights[1:10]) ## suma de weights de los diez mejores modelos
plot(multimodelo_ilex_23, type="w") ## para visualizar la evoluci?n de los weights de los modelos (l?nea roja =cumsum95%)
plot(multimodelo_ilex_23, type="s") ## para visualizar el peso de las variables

model_23<-glm(BP_2_3 ~ AUC_ndvi + eleva_m + st_dv_ndvi + srad_stdv + pet_max + 
  st_dv_evi, data = qilex_ifn)
summary(model_23)
rsq(model_23)
performance(model_23)




###de la otra forma, trabajaremos con la relacion entre biomasa y los datos del IFN3----
names(qilex_var)
qilex_ifn3<-qilex_var[, c(1,2,6,7,11:13,17,18,20:23,26,30,33,36,38,41,44,47,50,53,55,57,62,65,68,70,74, 75,78:92)]
qilex_ifn3<-qilex_ifn3 %>% filter(AB3_Mgha_perc>=80)
names(qilex_ifn3)
str(qilex_ifn3)

write.csv(qilex_ifn3, file = "qilex_ifn3.csv")

qilex_ifn3$latitud<-as.numeric(qilex_ifn3$latitud)
qilex_ifn3$longitud<-as.numeric(qilex_ifn3$longitud)

cor_qilex_ifn3<-round(cor(qilex_ifn3, use="complete.obs"),2)

corrplot(cor_qilex_ifn3, method = "shade", shade.col = NA, tl.col = "black", tl.cex = 0.5,
         number.cex = 0.75, tl.srt = 45, addCoef.col = "black", 
         type = "lower", diag = F, addshade = "all", order = "AOE")

gam.ilex3<-gam(AB3_Mgha ~ s(Arid_Dm) + s(aet_mean_ifn3) + s(eleva_m) + s(FPAR_500m_mean) + 
                 s(LAI_500m_mean) + s(max_ndvi_ifn3) + s(max_evi_ifn3) +
                 s(mean_evi_ifn3) + s(mean_ndvi_ifn3) + 
                 s(pet_mean_ifn3)  + s(min_evi_ifn3) + s(pet_min_ifn3) +
                 s(soil_mean_ifn3) + s(srad_mean_ifn3) + s(standard_deviation_ndvi_ifn3),
              data = qilex_ifn3, method="REML")
summary(gam.ilex3)

#descartamos las variables menos significativas para simplificar el modelo y sacamos las variables no lineales

gam.ilex3<-gam(AB3_Mgha ~ (Arid_Dm) + s(aet_mean_ifn3) + s(eleva_m) + (FPAR_500m_mean) +
                 s(LAI_500m_mean) + (mean_evi_ifn3) + s(mean_ndvi_ifn3) + 
                 s(pet_mean_ifn3)  + s(min_evi_ifn3) + s(pet_min_ifn3) +
                 s(soil_mean_ifn3) + (srad_mean_ifn3) + (standard_deviation_ndvi_ifn3),
               data = qilex_ifn3, method="REML")
summary(gam.ilex3)
performance(gam.ilex3)

hist(qilex_ifn3$AB3_Mgha)#distribucion gamma continua
glm.ilex3<-glm(AB3_Mgha ~ (Arid_Dm) + (aet_mean_ifn3) + (eleva_m) + 
                 (LAI_500m_mean) + (mean_evi_ifn3) + (mean_ndvi_ifn3) + 
                 (pet_mean_ifn3)  + (min_evi_ifn3) + (pet_min_ifn3) +
                 (soil_mean_ifn3) + (srad_mean_ifn3) + (standard_deviation_ndvi_ifn3),
               data = qilex_ifn3, family = Gamma)

summary(glm.ilex3)
rsq(glm.ilex3)
performance(glm.ilex3)




###vamos a estudiar multimodelos (glm) todas las posibles combinaciones de variables en los modelos para escoger el mejor----
#primero se crea el modelo
ilex3<-as.formula(AB3_Mgha ~ (Arid_Dm) + (aet_mean_ifn3) + (eleva_m) + (FPAR_500m_mean) + 
                 (LAI_500m_mean) + (max_ndvi_ifn3) + (max_evi_ifn3) +
                 (mean_evi_ifn3) + (mean_ndvi_ifn3) + 
                 (pet_mean_ifn3)  + (min_evi_ifn3) + (pet_min_ifn3) +
                 (soil_mean_ifn3) + (srad_mean_ifn3) + (standard_deviation_ndvi_ifn3))


multimodelo <- glmulti(ilex3, data=qilex_ifn3, fitfunction=glm, level=1, confsetsize=100, method="h", crit="aicc")
weightable(multimodelo)
cumsum(weightable(multimodelo)$weights) ## suma acumulada a partir del mejor modelo (primero [1])
sum(weightable(multimodelo)$weights[1:10]) ## suma de weights de los diez mejores modelos
plot(multimodelo, type="w") ## para visualizar la evoluci?n de los weights de los modelos (l?nea roja =cumsum95%)
plot(multimodelo, type="s") ## para visualizar el peso de las variables


#ahora ya pòdemos saber cual es el modelo que tiene una r cuadrada mejor, en el glm se trabaja con pseudo r2
multimodelo@formulas
glm2.ilex3<-glm(AB3_Mgha ~ Arid_Dm + aet_mean_ifn3 + eleva_m + LAI_500m_mean + 
                  max_evi_ifn3 + mean_evi_ifn3 + pet_mean_ifn3 + pet_min_ifn3 + 
                  standard_deviation_ndvi_ifn3, data = qilex_ifn3)

summary(glm2.ilex3)
rsq(glm2.ilex3)
performance(glm2.ilex3)


#simplificamos el modelo
formula<- ~ (Arid_Dm) + (aet_mean_ifn3) + (eleva_m) + (FPAR_500m_mean) + 
                    (LAI_500m_mean) + (max_ndvi_ifn3) + (max_evi_ifn3) +
                    (mean_evi_ifn3) + (mean_ndvi_ifn3) + 
                    (pet_mean_ifn3)  + (min_evi_ifn3) + (pet_min_ifn3) +
                    (soil_mean_ifn3) + (srad_mean_ifn3) + (standard_deviation_ndvi_ifn3)

library(MASS)
modelo.reducido <- stepAIC(glm.ilex3, scope = list(upper = formula, lower = ~1), direction="both", k=2, trace=TRUE)
summary(modelo.reducido)

nullmodel<-~1
modelo.nulo <- stepAIC(glm.ilex3, scope = list(upper = nullmodel, lower = ~1), direction="both", k=2, trace=TRUE)
summary(modelo.nulo)
performance(modelo.nulo)









