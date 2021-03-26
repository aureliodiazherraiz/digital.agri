
#### RECOMENZANDO #### datos 250121 nuevas coordenadas reproyectadas
library(pls)

topo_250121<-read_csv("IFN/Topo_ifn_250121.csv")
topo_250121_gee<-read_excel("IFN/srtm_GEE_250121.xlsx")

head(topo_250121_gee)
str(topo_250121_gee)
names(topo_250121_gee)[1]<-"ID"

#debido a no existir datos coincidentes entre GEE, R y Qgis, he optado por mantener los datos del GEE como datos de referencia para el analisis

cor_topo<-round(cor(topo_250121_gee, use="complete.obs"),2)
corrplot(cor_topo, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 55, tl.cex = 0.5, number.cex = 0.55, 
         addCoef.col = "black", type = "lower", diag = F, 
         addshade = "all")# de entre ellas no existen correlaciones a destacar, apenas entre la elvacion y la longitud y el aspecto y el sombreamiento (hillshade)


biomas250121<-read_csv("IFN/Andalucia_TotalBiomass_250121_2.csv")
head(biomas250121)
str(biomas250121)
names(biomas250121)[1]<-"ID"

ifn_cod<-biomas250121[, c(1:3,6,17,18)]

edafo_250121<-read_csv("edafo_anda_250121.csv")
names(edafo_250121)
edafo_250121<-edafo_250121[,-c(18:22,25)]

cor_edafo<-edafo_250121[, -c(1:2)]
cor_edafo<-round(cor(cor_edafo, use="complete.obs"),2)
corrplot(cor_edafo, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 55, tl.cex = 0.5, number.cex = 0.55, 
         addCoef.col = "black", type = "lower", diag = F, 
         addshade = "all", order = "AOE")#no existen correlaciones entre variables apenas entre mo y mo_sup y entre textura y are

edtobi<-merge(topo_250121_gee,biomas250121, by = "ID")

edtobi_dt<-merge(edtobi, edafo_250121, by = "ID")#unimos los tres dataframes con datos edaficos, topograficos e biologicos
names(edtobi_dt)

edtobi_dt<-edtobi_dt[, -c(23:26,42:43)]

write.csv(edtobi_dt, file = "edtobi_250121.csv")




#### INTRODUCIMOS LOS DATOS ESPECTRALES Y CLIMÁTICOS ####

#### IFN 3 ####

Z07_EVI_NDVI <- read_excel("IFN/LANDSAT_ANUAL_Z07_EVI_NDVI.xlsx")
Z06_EVI_NDVI <- read_excel("IFN/LANDSAT_ANUAL_Z06_EVI_NDVI.xlsx")
Z08_EVI_NDVI <- read_excel("IFN/LANDSAT_ANUAL_Z08_EVI_NDVI.xlsx")

names(Z07_EVI_NDVI)[1]<-"ID"
Z07_EVI_NDVI$ID<-1:7297
names(Z06_EVI_NDVI)[1]<-"ID"
Z06_EVI_NDVI$ID<-1:7297
names(Z08_EVI_NDVI)[1]<-"ID"
Z08_EVI_NDVI$ID<-1:7297


Z08_EVI_NDVI$ifn<-2008
Z07_EVI_NDVI$ifn<-2007
Z06_EVI_NDVI$ifn<-2006

Z07_EVI_NDVI<-merge(Z07_EVI_NDVI, ifn_cod, by = "ID")
Z06_EVI_NDVI<-merge(Z06_EVI_NDVI, ifn_cod, by = "ID")
Z08_EVI_NDVI<-merge(Z08_EVI_NDVI, ifn_cod, by = "ID")

evi_ndvi<-rbind(Z08_EVI_NDVI, Z07_EVI_NDVI, Z06_EVI_NDVI)

evi_ndvi<-evi_ndvi %>% filter((ifn == year3))

#evi_ndvi[c(evi_ndvi$Provincia_3 %in% c("Almeria","Cadiz","Granada","Huelva","Malaga","Sevilla") & evi_ndvi$ifn == 2006),] <- NA
#evi_ndvi[c(evi_ndvi$Provincia_3 %in% c("Cordoba","Jaen") & evi_ndvi$ifn == 2007),] <- NA

evi_ndvi <- na.omit(evi_ndvi)
summary(evi_ndvi)
names(evi_ndvi)

cor_spect<-round(cor(evi_ndvi[, -c(1,12:15)], use="complete.obs"),2) %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 55, tl.cex = 0.5, number.cex = 0.55, 
         addCoef.col = "black", type = "lower", diag = F, 
         addshade = "all")# de entre todas ellas nos debemos quedar apenas con mean y st_d de ndvi 


TERCLIM_Z06 <- read_excel("IFN/TERRACLIMATE_Z06.xlsx")
TERCLIM_Z07 <- read_excel("IFN/TERRACLIMATE_Z07.xlsx")

names(TERCLIM_Z07)[1]<-"ID"
TERCLIM_Z07$ID<-1:7297
names(TERCLIM_Z06)[1]<-"ID"
TERCLIM_Z06$ID<-1:7297

TERCLIM_Z07$ifn<-2007
TERCLIM_Z06$ifn<-2006

TERCLIM_Z07<-merge(TERCLIM_Z07, ifn_cod, by = "ID")
TERCLIM_Z06<-merge(TERCLIM_Z06, ifn_cod, by = "ID")

terclim<-rbind(TERCLIM_Z07, TERCLIM_Z06)

terclim[c(terclim$Provincia_3 %in% c("Almeria","Cadiz","Granada","Huelva","Malaga","Sevilla") & terclim$ifn == 2006),] <- NA

terclim[c(terclim$Provincia_3 %in% c("Cordoba","Jaen") & terclim$ifn == 2007),] <- NA
terclim <- na.omit(terclim)
summary(terclim)
names(terclim)
terclim<-terclim[, -c(1:3,20:22)]

cor_terclim<-round(cor(terclim, use="complete.obs"),2)
corrplot(cor_terclim, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 55, tl.cex = 0.5, number.cex = 0.55, 
         addCoef.col = "black", type = "lower", diag = F, 
         addshade = "all", order = "AOE")# de entre todas ellas nos debemos quedar apenas con las medias y las desviaciones de aet, pet y srad


te.cli.spt<-merge(terclim,evi_ndvi, by = c("X","Y"))
write.csv(te.cli.spt, file = "te_clim_spec_ifn3.csv")

names(te.cli.spt)

dt.raw<-merge(te.cli.spt[, -c(19:21)], edtobi_dt, by =c("Provincia_3","Estadillo_3"))
names(dt.raw)

write.csv(dt.raw, file = "datos_brutos_250121.csv")


#analizamos los datos de cada especie para el IFN3

dt.raw <- read_csv("datos_brutos_250121.csv")

names(dt.raw)


dt.raw3<-dt.raw[, -c(43:48)]
dt.raw3 %>% group_by(year3) %>% count()

dt.raw3[dt.raw3$year3 %in% c(1995,2002,2008),] <- NA
dt.raw3 <- na.omit(dt.raw3)

#### SEPARAMOS LAS ESPECIES ####
ilex3<-dt.raw3 %>% filter(Sp.x == "Quercus ilex")
suber3<-dt.raw3 %>% filter(Sp.x == "Quercus suber")
pinea3<-dt.raw3 %>% filter(Sp.x == "Pinus pinea")
pinaster3<-dt.raw3 %>% filter(Sp.x == "Pinus pinaster")
nigra3<-dt.raw3 %>% filter(Sp.x == "Pinus nigra")
hale3<-dt.raw3 %>% filter(Sp.x == "Pinus halepensis")
euro3<-dt.raw3 %>% filter(Sp.x == "Olea europaea")
sylv3<-dt.raw3 %>% filter(Sp.x == "Pinus sylvestris")


dt.raw3 %>% 
  filter(Sp.x %in% c("Quercus ilex", "Quercus suber", 
                       "Pinus pinea","Pinus pinaster", "Pinus nigra",
                       "Pinus halepensis", "Olea europaea", "Pinus sylvestris")) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x= Sp.x, fill = Sp.x), width = 1) +
  coord_polar() 

dt.raw3 %>% na.omit() %>% 
  filter(Sp.x %in% c("Quercus ilex", "Quercus suber", 
                     "Pinus pinea","Pinus pinaster", "Pinus nigra",
                     "Pinus halepensis", "Olea europaea", "Pinus sylvestris")) %>% 
  ggplot(aes(x= Sp.x)) + geom_bar() 



#### IFN2 ####

biomas250121<-read_csv("IFN/Andalucia_TotalBiomass_250121_2.csv")

names(biomas250121)

ifn_cod2<-biomas250121[, c(1:3,12,17:18)]

biomas250121 %>% filter(year2==1994) %>% group_by(Provincia_3) %>% count() 
biomas250121 %>% group_by(Provincia_3) %>% count()
biomas250121 %>% group_by(year2) %>% count()

TERCLIM_Z96 <- read_excel("IFN/TERRACLIMATE_Z96.xlsx")
TERCLIM_Z96$year_<-1996
names(TERCLIM_Z96)[1]<-"ID"
TERCLIM_Z96<-merge(TERCLIM_Z96, ifn_cod2, by.x = "ID", by.y = "X1")


TERCLIM_Z95 <- read_excel("IFN/TERRACLIMATE_Z95.xlsx")
TERCLIM_Z95$year_<-1995
names(TERCLIM_Z95)[1]<-"ID"
TERCLIM_Z95<-merge(TERCLIM_Z95, ifn_cod2, by.x = "ID", by.y = "X1")


TERCLIM_Z94 <- read_excel("IFN/TERRACLIMATE_Z94.xlsx")
TERCLIM_Z94$year_<-1994
names(TERCLIM_Z94)[1]<-"ID"
TERCLIM_Z94<-merge(TERCLIM_Z94, ifn_cod2, by.x = "ID", by.y = "X1")


terclim<-rbind(TERCLIM_Z96, TERCLIM_Z94, TERCLIM_Z95)
terclim<-terclim %>% filter(year2==year_)



ND_EVI_94<-read_excel("IFN/LANDSAT_ANUAL_Z94_EVI_NDVI.xlsx")
ND_EVI_94$year_<-1994
names(ND_EVI_94)[1]<-"ID"
ND_EVI_94<-merge(ND_EVI_94, ifn_cod2, by.x = "ID", by.y = "X1")

ND_EVI_95<-read_excel("IFN/LANDSAT_ANUAL_Z95_EVI_NDVI.xlsx")
ND_EVI_95$year_<-1995
names(ND_EVI_95)[1]<-"ID"
ND_EVI_95<-merge(ND_EVI_95, ifn_cod2, by.x = "ID", by.y = "X1")

ND_EVI_96<-read_excel("IFN/LANDSAT_ANUAL_Z96_EVI_NDVI.xlsx")
ND_EVI_96$year_<-1996
names(ND_EVI_96)[1]<-"ID"
ND_EVI_96<-merge(ND_EVI_96, ifn_cod2, by.x = "ID", by.y = "X1")


nd_evi<-rbind(ND_EVI_96, ND_EVI_95, ND_EVI_94)
nd_evi<-nd_evi %>% filter(year2==year_)


#vamos a anexar las variables del ifn2 a las del ifn2 para conseguir obtener las variaciones y posteriormente modelar
# de todas las variables, apenas podemos intentar relacionar las variables espectrales con el crecimiento, pues las variables climáticas aunque cambien con el tiempo, no tienen un efecto acumulativo, habria que hacer un AUC de cada una de ellas
#prefixamos las variables del ifn3


dt.raw<-read.csv("datos_brutos_250121.csv")
head(dt.raw)
dt.raw<-dt.raw[,-1]
names(dt.raw)
colnames(dt.raw)[5:29] <- paste("ifn3", colnames(dt.raw[, 5:29]), sep = "_")


#una vez obtenidas las varibles del ifn2 las prefixamos
names(terclim)
var_ifn<-merge(terclim[c(4:20,24:25)], nd_evi[, c(2:11,16:17)], by = c("X","Y"))
names(var_ifn)
colnames(var_ifn)[c(3:19, 22:29)] <- paste("ifn2", colnames(var_ifn[c(3:19, 22:29)]), sep = "_")

names(dt.raw)
dt.raw.ifn <- merge(dt.raw[c(3:29,33:64)], var_ifn, by = c("X","Y"))
dt.raw.ifn<-na.omit(dt.raw.ifn)

names(dt.raw.ifn)

dt.raw.ifn<-dt.raw.ifn %>% mutate(D_ano = year3 - year2) 
dt.raw.ifn %>% count(D_ano)
dt.raw.ifn<-dt.raw.ifn %>% filter(D_ano >= 11)

dt.raw.ifn<-dt.raw.ifn %>% mutate(Dens = Tree_dens - Tree_dens2)
dt.raw.ifn<-dt.raw.ifn %>% mutate(BP = AB3_Mgha - AB2_Mgha)
dt.raw.ifn<-dt.raw.ifn %>% mutate(BP_ano = (AB3_Mgha - AB2_Mgha)/(year3 - year2))

dt.raw.ifn<-dt.raw.ifn %>% mutate(ndvi_mean = ifn3_mean_ndvi - ifn2_mean_ndvi)
dt.raw.ifn<-dt.raw.ifn %>% mutate(ndvi_mean_ano = (ifn3_mean_ndvi - ifn2_mean_ndvi)/(year3 - year2))
dt.raw.ifn<-dt.raw.ifn %>% mutate(ndvi_min = ifn3_min_ndvi - ifn2_min_ndvi)
dt.raw.ifn<-dt.raw.ifn %>% mutate(ndvi_max = ifn3_max_ndvi - ifn2_max_ndvi)
dt.raw.ifn<-dt.raw.ifn %>% mutate(ndvi_st_d = ifn3_st_d_ndvi - ifn2_st_d_ndvi)


dt.raw.ifn<-dt.raw.ifn %>% mutate(evi_mean = ifn3_mean_evi - ifn2_mean_evi)
dt.raw.ifn<-dt.raw.ifn %>% mutate(evi_mean_ano = (ifn3_mean_evi - ifn2_mean_evi)/(year3 - year2))
dt.raw.ifn<-dt.raw.ifn %>% mutate(evi_min = ifn3_min_evi - ifn2_min_evi)
dt.raw.ifn<-dt.raw.ifn %>% mutate(evi_max = ifn3_max_evi - ifn2_max_evi)
dt.raw.ifn<-dt.raw.ifn %>% mutate(evi_st_d = ifn3_st_d_evi - ifn2_st_d_evi)

write.csv(dt.raw.ifn, file = "datos_brutos_ifn3_2.csv")
names(dt.raw.ifn)

dt.ifn3_2<-dt.raw.ifn[c(1:2,28:31,35,41,44,46:60,88:100)]
names(dt.ifn3_2)

write.csv(dt.ifn3_2, file = "variacion_ifn3_2.csv")

#ahora separamos el dataframe por especies
#hay mayor numero de individuos en los nuevos dataframes pues no hemos retirado los individuos años fuera de los ifn

ilex3_2<-dt.ifn3_2 %>% filter(Sp.x == Sp.y) %>% filter(Sp.x == "Quercus ilex") %>% na.omit()
suber3_2<-dt.ifn3_2 %>% filter(Sp.x == Sp.y) %>% filter(Sp.x == "Quercus suber") %>% na.omit()
pinea3_2<-dt.ifn3_2 %>% filter(Sp.x == Sp.y) %>% filter(Sp.x == "Pinus pinea") %>% na.omit()
pinaster3_2<-dt.ifn3_2 %>% filter(Sp.x == Sp.y) %>% filter(Sp.x == "Pinus pinaster") %>% na.omit()
nigra3_2<-dt.ifn3_2 %>% filter(Sp.x == Sp.y) %>% filter(Sp.x == "Pinus nigra") %>% na.omit()
hale3_2<-dt.ifn3_2 %>% filter(Sp.x == Sp.y) %>% filter(Sp.x == "Pinus halepensis") %>% na.omit()
euro3_2<-dt.ifn3_2 %>% filter(Sp.x == Sp.y) %>% filter(Sp.x == "Olea europaea") %>% na.omit()
sylv3_2<-dt.ifn3_2 %>% filter(Sp.x == Sp.y) %>% filter(Sp.x == "Pinus sylvestris")%>% na.omit()









