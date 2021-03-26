
#vamos a modelar la biomasa en cada especie por grupos de variables, para eso las agruparemos en dataframes y luego las modelaremos. Atención, varios dataframes provienen del script 8_metricas_EVI!!!
srtm_AUC

#ahora preparamos el resto de las variables
evi_total
evi_total$year<-as.character(evi_total$year)
evi_total$year<-str_replace_all(evi_total$year, "1995", "ifn2")
evi_total$year<-str_replace_all(evi_total$year, "1994", "ifn2")
evi_total$year<-str_replace_all(evi_total$year, "2006", "ifn3")
evi_total$year<-str_replace_all(evi_total$year, "2007", "ifn3")

te_clim_total
te_clim_total$year<-as.character(te_clim_total$year)
te_clim_total$year<-str_replace_all(te_clim_total$year, "1995", "ifn2")
te_clim_total$year<-str_replace_all(te_clim_total$year, "1994", "ifn2")
te_clim_total$year<-str_replace_all(te_clim_total$year, "2006", "ifn3")
te_clim_total$year<-str_replace_all(te_clim_total$year, "2007", "ifn3")

fpar_lai_total
fpar_lai_total$year<-str_replace_all(fpar_lai_total$year, "2006", "ifn3")
fpar_lai_total$year<-str_replace_all(fpar_lai_total$year, "2007", "ifn3")


#evi
names(evi_total)
evi_pwtl<-pivot_wider(evi_total, names_from = year, values_from = c("max_evi", "min_evi",
                                                                    "mean_evi", "median_evi",
                                                                    "st_d_evi"))
str_length(evi_pwtl$latitud)
str_length(srtm_AUC$latitud)
evi_pwtl [,1:2] <- lapply(evi_pwtl[,1:2], str_sub, 1, 6)
dt<-merge(evi_pwtl, srtm_AUC, by = c("latitud","longitud"))


str_length(fpar_lai_total$latitud)
str_length(dt$latitud)
fpar_lai_total [,1:2] <- lapply(fpar_lai_total[,1:2], str_sub, 1, 6)

dt<-merge(dt, fpar_lai_total, by = c("latitud","longitud"))




#climaticos
names(te_clim_total)
te_clim_pwtl<-pivot_wider(te_clim_total, names_from = year, values_from = c("aet_max","aet_min",
                                                                            "aet_mean", "aet_stdv",
                                                                            "pet_max","pet_min", 
                                                                            "pet_mean",
                                                                            "pet_stdv", "soil_max",
                                                                            "soil_min", "soil_mean", 
                                                                            "soil_stdv", "srad_max", 
                                                                            "srad_min","srad_mean",
                                                                            "srad_stdv"))
str_length(te_clim_pwtl$latitud)
str_length(dt$latitud)
te_clim_pwtl [,1:2] <- lapply(te_clim_pwtl[,1:2], str_sub, 1, 6)
dt<-merge(te_clim_pwtl, dt, by = c("latitud","longitud"))


#edafologicas
str_length(spdt$latitud)
str_length(dt$latitud)
spdt [,17:18] <- lapply(spdt[,17:18], str_sub, 1, 6)
dt<-merge(dt, spdt,by = c("latitud","longitud"))


names(dt)
dt<-dt[, -c(38:50)]
write.csv(dt, file = "dt_total.csv")



####MODELOS####
#ahora una vez tenemos todas las variables juntas podemos crear los modelos con cada grupo de variables para ver cuales son mas significativas, usando apenas para el ifn3 
#decir que no me ha servido para nada pues los modelos proporcionan una r cuadrada muy baja

#### para ilex ####
dt_ilex <- dt %>% filter(Sp.x == "Quercus ilex" & Sp.y == "Quercus ilex")

options(max.print=999999) # Dar mas memoria RAM

####climáticas####

names(dt_ilex)

ilex3_clim<-as.formula(AB3_Mgha ~ aet_max_ifn3 + aet_min_ifn3 + aet_mean_ifn3 +
                         aet_stdv_ifn3 + pet_max_ifn3 + pet_min_ifn3 + pet_mean_ifn3 + 
                         pet_stdv_ifn3 + soil_max_ifn3 + soil_min_ifn3 + soil_mean_ifn3 + 
                         soil_stdv_ifn3 + srad_max_ifn3 + srad_min_ifn3 + srad_mean_ifn3 +
                         srad_stdv_ifn3 + Arid_Dm)
                       

multmodel_ilex_clim <- glmulti(ilex3_clim, data=dt_ilex, fitfunction=glm, level=1, confsetsize=100, method="h", crit="aicc")

weightable(multmodel_ilex_clim)
cumsum(weightable(multmodel_ilex_clim)$weights) ## suma acumulada a partir del mejor modelo (primero [1])
sum(weightable(multmodel_ilex_clim)$weights[1:10]) ## suma de weights de los diez mejores modelos
plot(multmodel_ilex_clim, type="w") ## para visualizar la evoluci?n de los weights de los modelos (l?nea roja =cumsum95%)
plot(multmodel_ilex_clim, type="s") ## para visualizar el peso de las variables                       
                       
#siendo el mejor modelo:
model.clim_ilex3<-glm(AB3_Mgha ~ aet_max_ifn3 + aet_min_ifn3 + aet_mean_ifn3 + 
                        aet_stdv_ifn3 + pet_min_ifn3 + pet_mean_ifn3 + soil_max_ifn3 + 
                        soil_min_ifn3 + soil_mean_ifn3 + soil_stdv_ifn3 + srad_max_ifn3 + 
                        srad_mean_ifn3 + Arid_Dm, data = dt_ilex)
summary(model.clim_ilex3)
plot(model.clim_ilex3) 
rsq(model.clim_ilex3)#r squared: 0.143


model.clim_ilex3<-glm(AB3_Mgha ~ aet_min_ifn3 + 
                        aet_stdv_ifn3 + pet_mean_ifn3 + 
                        soil_min_ifn3 + soil_mean_ifn3 + soil_stdv_ifn3 + srad_max_ifn3 + 
                        srad_mean_ifn3, data = qilex_te_clim_bioma)
summary(model.clim_ilex3)
plot(model.clim_ilex3) 

rsq(model.clim_ilex3)#r squared: 0.134


####topo####

ilex3_topo<-as.formula(AB3_Mgha ~ latitud + longitud + slope_grd + NS_adim + EW_adim +
                         dyy_m + dxy_m + dxx_m + curv_tg_m + curv_per_m + aspect_grd + eleva_m)       

multmodel_ilex_topo <- glmulti(ilex3_topo, data=dt_ilex, fitfunction=glm, level=1, confsetsize=100, method="h", crit="aicc")

weightable(multmodel_ilex_topo)
cumsum(weightable(multmodel_ilex_topo)$weights) ## suma acumulada a partir del mejor modelo (primero [1])
sum(weightable(multmodel_ilex_topo)$weights[1:10]) ## suma de weights de los diez mejores modelos
plot(multmodel_ilex_topo, type="w") ## para visualizar la evoluci?n de los weights de los modelos (l?nea roja =cumsum95%)
plot(multmodel_ilex_topo, type="s") ## para visualizar el peso de las variables  

#el mejor modelo
modelo.topo.ilex3<-glm(AB3_Mgha ~ latitud + longitud + NS_adim + dxx_m + curv_per_m + eleva_m, data = dt_ilex, family = Gamma)
summary(modelo.topo.ilex3)
rsq(modelo.topo.ilex3)


####evi####
names(dt_ilex)
ilex3_evi<-as.formula(AB3_Mgha ~ FPAR_500m_mean + LAI_500m_mean + FPAR_500m_max + LAI_500m_max +
                        FPARstdDev_500m + LAIStdDev_500m + LAI_500m_min + FPAR_500m_min + 
                        max_evi_ifn3.y + mean_evi_ifn3.y + min_evi_ifn3.y +
                        st_d_evi_ifn3.y)  

multmodel_ilex_evi <- glmulti(ilex3_evi, data=dt_ilex, fitfunction=glm, level=1, confsetsize=100, method="h", crit="aicc")

weightable(multmodel_ilex_evi)
cumsum(weightable(multmodel_ilex_evi)$weights) ## suma acumulada a partir del mejor modelo (primero [1])
sum(weightable(multmodel_ilex_evi)$weights[1:10]) ## suma de weights de los diez mejores modelos
plot(multmodel_ilex_evi, type="w") ## para visualizar la evoluci?n de los weights de los modelos (l?nea roja =cumsum95%)
plot(multmodel_ilex_evi, type="s") ## para visualizar el peso de las variables


modelo.evi.ilex3<-glm(AB3_Mgha ~ FPAR_500m_mean + LAI_500m_mean + FPAR_500m_max + LAIStdDev_500m +
                        LAI_500m_min + FPAR_500m_min + mean_evi_ifn3.y + min_evi_ifn3.y + 
                        st_d_evi_ifn3.y, data = dt_ilex)
summary(modelo.evi.ilex3)
rsq(modelo.topo.ilex3) #r squared 0.059


####edafo####
ilex3_edafo<-as.formula(AB3_Mgha ~ tf + mo + mo_sup + cod_hid +
                    textura + psb + ps + arc + are + lim + ca + cic + crad + n_sup + ph) 

multmodel_ilex_edafo <- glmulti(ilex3_edafo, data=dt_ilex, fitfunction=glm, level=1, confsetsize=100, method="h", crit="aicc")

weightable(multmodel_ilex_edafo)
cumsum(weightable(multmodel_ilex_edafo)$weights) ## suma acumulada a partir del mejor modelo (primero [1])
sum(weightable(multmodel_ilex_edafo)$weights[1:10]) ## suma de weights de los diez mejores modelos
plot(multmodel_ilex_edafo, type="w") ## para visualizar la evoluci?n de los weights de los modelos (l?nea roja =cumsum95%)
plot(multmodel_ilex_edafo, type="s") ## para visualizar el peso de las variables


modelo.edafo.ilex3<-glm(AB3_Mgha ~ cod_hid + ps + ca + cic + crad + n_sup + ph, data = dt_ilex)
summary(modelo.edafo.ilex3)
rsq(modelo.edafo.ilex3) #r squared 0.056
performance(modelo.edafo.ilex3)



####ndvi####
ilex3_ndvi<-as.formula(AB3_Mgha ~ max_ndvi_ifn3 + 
                         mean_ndvi_ifn3 + median_ndvi_ifn3 + min_ndvi_ifn3 + 
                         standard_deviation_ndvi_ifn3)  

multmodel_ilex_ndvi <- glmulti(ilex3_ndvi, data=dt_ilex, fitfunction=glm, level=1, confsetsize=100, method="h", crit="aicc")

weightable(multmodel_ilex_ndvi)
cumsum(weightable(multmodel_ilex_ndvi)$weights) ## suma acumulada a partir del mejor modelo (primero [1])
sum(weightable(multmodel_ilex_ndvi)$weights[1:10]) ## suma de weights de los diez mejores modelos
plot(multmodel_ilex_ndvi, type="w") ## para visualizar la evoluci?n de los weights de los modelos (l?nea roja =cumsum95%)
plot(multmodel_ilex_ndvi, type="s") ## para visualizar el peso de las variables

modelo.ndvi.ilex3<-glm(AB3_Mgha ~  max_ndvi_ifn3 + mean_ndvi_ifn3 + median_ndvi_ifn3 + 
                         min_ndvi_ifn3 + standard_deviation_ndvi_ifn3, data = dt_ilex)
summary(modelo.ndvi.ilex3)
rsq(modelo.ndvi.ilex3) #r squared 0.07
performance(modelo.ndvi.ilex3)



