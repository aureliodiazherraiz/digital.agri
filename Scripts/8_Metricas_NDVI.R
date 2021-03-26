#volvemos a intentarlo con otras métricas del NDVI como la media, minimo, máximo, mediana

land5_anual00 <- read_excel("indices_nd_savi/LANDSAT5_ANUAL_00.xlsx")
land5_anual00$year<-2000
land5_anual00$max_ndvi <- str_sub(land5_anual00$max_ndvi, 2,7)
land5_anual00$min_ndvi <- str_sub(land5_anual00$min_ndvi, 2,7)
land5_anual00$mean_ndvi <- str_sub(land5_anual00$mean_ndvi, 2,7)
land5_anual00$median_ndvi <- str_sub(land5_anual00$median_ndvi, 2,7)
land5_anual00$standard_deviation_ndvi <- str_sub(land5_anual00$standard_deviation_ndvi, 2,7)

land5_anual94 <- read_excel("indices_nd_savi/LANDSAT5_ANUAL_94.xlsx")
land5_anual94$year<-1994
land5_anual94 [,4:8] <- lapply(land5_anual94[,4:8], str_sub, 2, 7) #para quitar los corchetes

land5_anual95 <- read_excel("indices_nd_savi/LANDSAT5_ANUAL_95.xlsx")
land5_anual95$year<-1995
land5_anual95 [,4:8] <- lapply(land5_anual95[,4:8], str_sub, 2, 7)

land5_anual96 <- read_excel("indices_nd_savi/LANDSAT5_ANUAL_96.xlsx")
land5_anual96$year<-1996
land5_anual96 [,4:8] <- lapply(land5_anual96[,4:8], str_sub, 2, 7)

land5_anual97 <- read_excel("indices_nd_savi/LANDSAT5_ANUAL_97.xlsx")
land5_anual97$year<-1997
land5_anual97 [,4:8] <- lapply(land5_anual97[,4:8], str_sub, 2, 7)

land5_anual98 <- read_excel("indices_nd_savi/LANDSAT5_ANUAL_98.xlsx")
land5_anual98$year<-1998
land5_anual98 [,4:8] <- lapply(land5_anual98[,4:8], str_sub, 2, 7)

land5_anual99 <- read_excel("indices_nd_savi/LANDSAT5_ANUAL_99.xlsx")
land5_anual99$year<-1999
land5_anual99 [,4:8] <- lapply(land5_anual99[,4:8], str_sub, 2, 7)

modis_anual00_1 <- read_excel("indices_nd_savi/MODIS_ANUAL_00_1.xlsx")
modis_anual00_2 <- read_excel("indices_nd_savi/MODIS_ANUAL_00_2.xlsx")
modis_anual00 <- rbind(modis_anual00_1,modis_anual00_2)
modis_anual00$year<-2000
modis_anual00 [,4:8] <- lapply(modis_anual00[,4:8], str_sub, 2, 7)
modis_anual00$...1<-0:7177

modis_anual01_1 <- read_excel("indices_nd_savi/MODIS_ANUAL_01_1.xlsx")
modis_anual01_2 <- read_excel("indices_nd_savi/MODIS_ANUAL_01_2.xlsx")
modis_anual01 <- rbind(modis_anual01_1,modis_anual01_2)
modis_anual01$year <-2001
modis_anual01 [,4:8] <- lapply(modis_anual01[,4:8], str_sub, 2, 7)
modis_anual01$...1<-0:7177

modis_anual02_1 <- read_excel("indices_nd_savi/MODIS_ANUAL_02_1.xlsx")
modis_anual02_2 <- read_excel("indices_nd_savi/MODIS_ANUAL_02_2.xlsx")
modis_anual02 <- rbind(modis_anual02_1,modis_anual02_2)
modis_anual02$year <-2002
modis_anual02$...1<-0:7177
modis_anual02 [,4:8] <- lapply(modis_anual02[,4:8], str_sub, 2, 7)

modis_anual03_1 <- read_excel("indices_nd_savi/MODIS_ANUAL_03_1.xlsx")
modis_anual03_2 <- read_excel("indices_nd_savi/MODIS_ANUAL_03_2.xlsx")
modis_anual03 <- rbind(modis_anual03_1,modis_anual03_2)
modis_anual03$year <-2003
modis_anual03 [,4:8] <- lapply(modis_anual03[,4:8], str_sub, 2, 7)
modis_anual03$...1<-0:7177

modis_anual04_1 <- read_excel("indices_nd_savi/MODIS_ANUAL_04_1.xlsx")
modis_anual04_2 <- read_excel("indices_nd_savi/MODIS_ANUAL_04_2.xlsx")
modis_anual04_2 [,4:8] <- lapply(modis_anual04_2[,4:8], str_sub, 2, 7)
modis_anual04_1 [,4:8] <- lapply(modis_anual04_1[,4:8], str_sub, 1, 7)
modis_anual04 <- rbind(modis_anual04_1,modis_anual04_2)
modis_anual04$year <-2004
modis_anual04$...1<-0:7177

modis_anual05_1 <- read_excel("indices_nd_savi/MODIS_ANUAL_05_1.xlsx")
modis_anual05_2 <- read_excel("indices_nd_savi/MODIS_ANUAL_05_2.xlsx")
modis_anual05 <- rbind(modis_anual05_1,modis_anual05_2)
modis_anual05$year <-2005
modis_anual05 [,4:8] <- lapply(modis_anual05[,4:8], str_sub, 2, 7)
modis_anual05$...1<-0:7177

modis_anual06_1 <- read_excel("indices_nd_savi/MODIS_ANUAL_06_1.xlsx")
modis_anual06_2 <- read_excel("indices_nd_savi/MODIS_ANUAL_06_2.xlsx")
modis_anual06 <- rbind(modis_anual01_1,modis_anual01_2)
modis_anual06$year <-2006
modis_anual06 [,4:8] <- lapply(modis_anual06[,4:8], str_sub, 2, 7)
modis_anual06$...1<-0:7177

modis_anual07_1 <- read_excel("indices_nd_savi/MODIS_ANUAL_07_1.xlsx")
modis_anual07_2 <- read_excel("indices_nd_savi/MODIS_ANUAL_07_2.xlsx")
modis_anual07 <- rbind(modis_anual07_1,modis_anual07_2)
modis_anual07$year <-2007
modis_anual07 [,4:8] <- lapply(modis_anual07[,4:8], str_sub, 2, 7)
modis_anual07$...1<-0:7177

metricas <- rbind(modis_anual00, modis_anual01,
     modis_anual02, modis_anual03, 
     modis_anual04, modis_anual05,
     modis_anual06, modis_anual07,
     land5_anual99, land5_anual98,
     land5_anual97, land5_anual96,
     land5_anual95, land5_anual94) 
   

#teniendo los códigos de cada punto en base a su provincia podemos seleccionar los puntos de cada año
metricas<-merge(metricas,ifn_cod, by = c("latitud","longitud"))
#exploramos las posibles correlaciones entre las nuevas metricas
str(metricas)
metricas$max_ndvi<-as.numeric(metricas$max_ndvi)
metricas$min_ndvi<-as.numeric(metricas$min_ndvi)
metricas$mean_ndvi<-as.numeric(metricas$mean_ndvi)
metricas$median_ndvi<-as.numeric(metricas$median_ndvi)
metricas$standard_deviation_ndvi<-as.numeric(metricas$standard_deviation_ndvi)


#hemos encontrado dos valores de mínimo que son superiores a 1 por lo que seran eliminados
summary(metricas)
metricas<-metricas %>% filter(min_ndvi<=1)

cor_metricas<-round(cor(metricas, use="complete.obs"),2)
corrplot(cor_metricas, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black", addcolorlabel = "no",
         type = "lower", diag = F, addshade = "all")


write.csv(metricas, file = "metricas_NDVI_94_07.csv")  

#ahora le atribuimos a cada provincia los años de IFN eliminando el resto

ndvi_mes[c(ndvi_mes$cod %in% c(4,18,11,14,21,29,41) & ndvi_mes$year == 1994),] <- NA


metricas_ifn <- metricas %>% filter(year %in% c(1994,1995,1996,2006,2007))

#afinamos para cada provincia dejando apenas el año de cada IFN

metricas_ifn[c(metricas_ifn$cod %in% c(4,11,14,18,21,29,41) & metricas_ifn$year %in% c(1994,1996)),] <- NA
metricas_ifn[c(metricas_ifn$cod %in% c(23) & metricas_ifn$year %in% c(1995,1996)),] <- NA

metricas_ifn[c(metricas_ifn$cod %in% c(4,11,18,21,29,41) & metricas_ifn$year == 2006),] <- NA
metricas_ifn[c(metricas_ifn$cod %in% c(14,23) & metricas_ifn$year == 2007),] <- NA
summary(metricas_ifn)

#eliminamos los NAs
metricas_ifn <- na.omit(metricas_ifn)

metricas_ifn$year<-str_replace_all(metricas_ifn$year, "1995", "ifn2")
metricas_ifn$year<-str_replace_all(metricas_ifn$year, "1994", "ifn2")
metricas_ifn$year<-str_replace_all(metricas_ifn$year, "2006", "ifn3")
metricas_ifn$year<-str_replace_all(metricas_ifn$year, "2007", "ifn3")

#lo pivoteamos para poder operar entre años y variables
metricas_ifn<-metricas_ifn %>% pivot_wider(names_from = year, 
                                 names_sep = "_",
                                 values_from = c(max_ndvi, min_ndvi, median_ndvi, mean_ndvi, standard_deviation_ndvi),
                                 )

#apesar de haber calculado, no parece tener sentido pues es dificil que una medida como el NDVI consiga relacionarse
#con la biomasa
#otra cosa, cuidad con la resta de los minimos pues habria que hacerlo en valor absoluto
varia_metricas_ifn <- mutate(metricas_ifn, max_ndvi_ifn= abs(max_ndvi_ifn3)-abs(max_ndvi_ifn2))


#mezclamos los datos de NDVI con los de Biomasa
ifn_2_3 <- read_csv("Indices_Biomasa_2_3.csv")
ifn_2_3<-ifn_2_3[, -c(1, 7:28, 47:62,64)]
names(ifn_2_3)[3]<-"latitud"
names(ifn_2_3)[4]<-"longitud"

ndvi_ifn <- merge(ifn_2_3, varia_metricas_ifn, by = c("latitud","longitud"))

#eliminamos variables como la mediana y otras columnas que no tienen utilidad
ndvi_ifn[, c(3:10,12,27:28)]<-NULL


