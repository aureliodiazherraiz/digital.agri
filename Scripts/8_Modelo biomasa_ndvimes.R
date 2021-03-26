###otra alternativa pùede ser modelar la biomasa con los valores mensuales del NDVI a lo largo de un año----

LAND5_94_1 <- read_excel("indices_nd_savi/LANDSAT5_1994_1.xlsx")
LAND5_94_2 <- read_excel("indices_nd_savi/LANDSAT5_1994_2.xlsx")
Land5_94<-rbind(LAND5_94_1,LAND5_94_2)
Land5_94$year<-1994
Land5_94$...1<-0:7177
Land5_94<-merge(Land5_94,ifn_cod, by = c("latitud","longitud"))

LAND5_95_1 <- read_excel("indices_nd_savi/LANDSAT5_1995_1.xlsx")
LAND5_95_2 <- read_excel("indices_nd_savi/LANDSAT5_1995_2.xlsx")
Land5_95<-rbind(LAND5_95_1,LAND5_95_2)
Land5_95$year<-1995
Land5_95$...1<-0:7177
Land5_95<-merge(Land5_95,ifn_cod, by = c("latitud","longitud"))

LAND5_96_1 <- read_excel("indices_nd_savi/LANDSAT5_1996_1.xlsx")
LAND5_96_2 <- read_excel("indices_nd_savi/LANDSAT5_1996_2.xlsx")
Land5_96<-rbind(LAND5_96_1,LAND5_96_2)
Land5_96$year<-1996
Land5_96$...1<-0:7177
Land5_96<-merge(Land5_96,ifn_cod, by = c("latitud","longitud"))

Land_94_96<-cbind(Land5_94,Land5_95,Land5_96)
Land_94_96_it<-na.spline(Land_94_96)
Land_94_96_it<-as.data.frame(Land_94_96_it)
Land_95_it<-Land_94_96_it[, c(18:34)]

#ahora cruzamos los valores de NDVI con los de biomasa, 
#para lo cual debemos trabajar con caracteres de la misma longitud
srtm_AUC <- read_csv("indices_srtm_AUC.csv")

str_length(srtm_AUC$latitud)
str_length(Land_95_it$latitud)

#igualamos la longitud de las entradas de las variables
srtm_AUC [,2:3] <- lapply(srtm_AUC[,2:3], str_sub, 1, 6)
Land_95_it [,2:3] <- lapply(Land_95_it[,2:3], str_sub, 1, 6)

Land_95_it<-merge(Land_95_it,srtm_AUC, by = c("latitud", "longitud"))

Land_95_it<-Land_95_it[, c(5:16,26:28,30)]

qilex_95_ifn2<-Land_95_it %>% filter(Sp.y == "Quercus ilex") 

qilex_95_ifn2 <- qilex_95_ifn2[, -14]

qilex_95_ifn2<-na.omit(qilex_95_ifn2)

cor_qilex_95_ifn2<- round(cor(qilex_95_ifn2, use="complete.obs"),2)

corrplot(cor_qilex_95_ifn2, method = "shade", shade.col = NA, tl.col = "black", tl.cex = 0.5,
         number.cex = 0.75, tl.srt = 45, addCoef.col = "black", 
         type = "lower", diag = F, addshade = "all")

gam_ifn2<-gam(AB2_Mgha ~ (`01`) + s(`02`) + s(`03`) + (`04`) + s(`05`) +
              (`06`) + s(`07`) + s(`08`) + s(`09`) + s(`10`) + s(`11`) + (`12`),
              data = qilex_95_ifn2, method="REML")

summary(gam_ifn2)







#calculo del AUC apenas con los datos del NDVI en Landsat mediante interpolaciones para el calculo de los NAs
Land_94_00<-cbind(Land5_94,Land5_95, Land5_96, Land5_97, Land5_98, Land5_99, Land5_00)

#una vez que los tenemos todos juntos interpolamos para sacar los valores que son NA


land_94vp <- Land5_94 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                       names_to = "Mes", values_to = "NDVI")

land_95vp <- Land5_95 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                       names_to = "Mes", values_to = "NDVI")

