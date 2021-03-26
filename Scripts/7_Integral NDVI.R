#bajamos los datos del ifn para tener los codigos de cada provincia y asi asignarles
#el año del IFN correspondiente
ifn_2_3 <- read_csv("Indices_Biomasa_2_3.csv")
ifn_2_3<-ifn_2_3[, -c(1, 7:28, 47:57, 60:61, 64)]
names(ifn_2_3)[3]<-"latitud"
names(ifn_2_3)[4]<-"longitud"
ifn_cod<-ifn_2_3[, c(3,4,6)]
names(ifn_cod)[3]<-"cod"
write.csv(ifn_cod, file = "ifn_cod.csv")


#vamos unir todos los dataframes de valores mensuales de NDVI por año
MOD_07_1 <- read_excel("indices_nd_savi/MODIS_2007_part1.xlsx")
MOD_07_2 <- read_excel("indices_nd_savi/MODIS_2007_part2.xlsx")
Mod_07<-rbind(MOD_07_1,MOD_07_2)
Mod_07$year<-2007
Mod_07$...1<-0:7177
Mod_07<-merge(Mod_07,ifn_cod, by = c("latitud","longitud"))
#hacemos la traspuesta para ordenar todos los puntos por año y mes
mod_07vp <- Mod_07 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                    names_to = "Mes", values_to = "NDVI")



MOD_06_1 <- read_excel("indices_nd_savi/MODIS_2006_part1.xlsx")
MOD_06_2 <- read_excel("indices_nd_savi/MODIS_2006_part2.xlsx")
Mod_06<-rbind(MOD_06_1,MOD_06_2)
Mod_06$year<-2006
Mod_06$...1<-0:7177
Mod_06<-merge(Mod_06,ifn_cod, by = c("latitud","longitud"))

mod_06vp <- Mod_06 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                    names_to = "Mes", values_to = "NDVI")


MOD_05_1 <- read_excel("indices_nd_savi/MODIS_2005_part1.xlsx")
MOD_05_2 <- read_excel("indices_nd_savi/MODIS_2005_part2.xlsx")
Mod_05<-rbind(MOD_05_1,MOD_05_2)
Mod_05$year<-2005
Mod_05$...1<-0:7177
Mod_05<-merge(Mod_05,ifn_cod, by = c("latitud","longitud"))

mod_05vp <- Mod_05 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                    names_to = "Mes", values_to = "NDVI")

MOD_04_1 <- read_excel("indices_nd_savi/MODIS_2004_part1.xlsx")
MOD_04_2 <- read_excel("indices_nd_savi/MODIS_2004_part2.xlsx")
Mod_04<-rbind(MOD_04_1,MOD_04_2)
Mod_04$year<-2004
Mod_04$...1<-0:7177
Mod_04<-merge(Mod_04,ifn_cod, by = c("latitud","longitud"))

mod_04vp <- Mod_04 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                    names_to = "Mes", values_to = "NDVI")

MOD_03_1 <- read_excel("indices_nd_savi/MODIS_2003_part1.xlsx")
MOD_03_2 <- read_excel("indices_nd_savi/MODIS_2003_part2.xlsx")
Mod_03<-rbind(MOD_03_1,MOD_03_2)
Mod_03$year<-2003
Mod_03$...1<-0:7177
Mod_03<-merge(Mod_03,ifn_cod, by = c("latitud","longitud"))

mod_03vp <- Mod_03 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                    names_to = "Mes", values_to = "NDVI")

MOD_02_1 <- read_excel("indices_nd_savi/MODIS_2002_part1.xlsx")
MOD_02_2 <- read_excel("indices_nd_savi/MODIS_2002_part2.xlsx")
Mod_02<-rbind(MOD_02_1,MOD_02_2)
Mod_02$year<-2002
Mod_02$...1<-0:7177
Mod_02<-merge(Mod_02,ifn_cod, by = c("latitud","longitud"))

mod_02vp <- Mod_02 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                    names_to = "Mes", values_to = "NDVI")

MOD_01_1 <- read_excel("indices_nd_savi/MODIS_2001_part1.xlsx")
MOD_01_2 <- read_excel("indices_nd_savi/MODIS_2001_part2.xlsx")
Mod_01<-rbind(MOD_01_1,MOD_01_2)
Mod_01$year<-2001
Mod_01$...1<-0:7177
Mod_01<-merge(Mod_01,ifn_cod, by = c("latitud","longitud"))

mod_01vp <- Mod_01 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                    names_to = "Mes", values_to = "NDVI")

Mod_00 <- read_excel("indices_nd_savi/MODIS_2000.xlsx")
Mod_00$year<-2000
Mod_00$...1<-0:7177
Mod_00<-merge(Mod_00,ifn_cod, by = c("latitud","longitud"))

mod_00vp <- Mod_00 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                    names_to = "Mes", values_to = "NDVI")

LAND5_94_1 <- read_excel("indices_nd_savi/LANDSAT5_1994_1.xlsx")
LAND5_94_2 <- read_excel("indices_nd_savi/LANDSAT5_1994_2.xlsx")
Land5_94<-rbind(LAND5_94_1,LAND5_94_2)
Land5_94$year<-1994
Land5_94$...1<-0:7177
Land5_94<-merge(Land5_94,ifn_cod, by = c("latitud","longitud"))

land_94vp <- Land5_94 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                    names_to = "Mes", values_to = "NDVI")

LAND5_95_1 <- read_excel("indices_nd_savi/LANDSAT5_1995_1.xlsx")
LAND5_95_2 <- read_excel("indices_nd_savi/LANDSAT5_1995_2.xlsx")
Land5_95<-rbind(LAND5_95_1,LAND5_95_2)
Land5_95$year<-1995
Land5_95$...1<-0:7177
Land5_95<-merge(Land5_95,ifn_cod, by = c("latitud","longitud"))

land_95vp <- Land5_95 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                       names_to = "Mes", values_to = "NDVI")

LAND5_96_1 <- read_excel("indices_nd_savi/LANDSAT5_1996_1.xlsx")
LAND5_96_2 <- read_excel("indices_nd_savi/LANDSAT5_1996_2.xlsx")
Land5_96<-rbind(LAND5_96_1,LAND5_96_2)
Land5_96$year<-1996
Land5_96$...1<-0:7177
Land5_96<-merge(Land5_96,ifn_cod, by = c("latitud","longitud"))

land_96vp <- Land5_96 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                       names_to = "Mes", values_to = "NDVI")

LAND5_97_1 <- read_excel("indices_nd_savi/LANDSAT5_1997_1.xlsx")
LAND5_97_2 <- read_excel("indices_nd_savi/LANDSAT5_1997_2.xlsx")
Land5_97<-rbind(LAND5_97_1,LAND5_97_2)
Land5_97$year<-1997
Land5_97$...1<-0:7177
Land5_97<-merge(Land5_97,ifn_cod, by = c("latitud","longitud"))

land_97vp <- Land5_97 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                       names_to = "Mes", values_to = "NDVI")

LAND5_98_1 <- read_excel("indices_nd_savi/LANDSAT5_1998_1.xlsx")
LAND5_98_2 <- read_excel("indices_nd_savi/LANDSAT5_1998_2.xlsx")
Land5_98<-rbind(LAND5_98_1,LAND5_98_2)
Land5_98$year<-1998
Land5_98$...1<-0:7177
Land5_98<-merge(Land5_98,ifn_cod, by = c("latitud","longitud"))

land_98vp <- Land5_98 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                       names_to = "Mes", values_to = "NDVI")

LAND5_99_1 <- read_excel("indices_nd_savi/LANDSAT5_1999_1.xlsx")
LAND5_99_2 <- read_excel("indices_nd_savi/LANDSAT5_1999_2.xlsx")
Land5_99<-rbind(LAND5_99_1,LAND5_99_2)
Land5_99$year<-1999
Land5_99$...1<-0:7177
Land5_99<-merge(Land5_99,ifn_cod, by = c("latitud","longitud"))

land_99vp <- Land5_99 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                       names_to = "Mes", values_to = "NDVI")

LAND5_00_1 <- read_excel("indices_nd_savi/LANDSAT5_2000_1.xlsx")
LAND5_00_2 <- read_excel("indices_nd_savi/LANDSAT5_2000_2.xlsx")
Land5_00<-rbind(LAND5_00_1,LAND5_00_2)
Land5_00$year<-2000
Land5_00$...1<-0:7177
Land5_00<-merge(Land5_00,ifn_cod, by = c("latitud","longitud"))

land_00vp <- Land5_00 %>% pivot_longer(cols = starts_with(c("0", "1")), 
                                       names_to = "Mes", values_to = "NDVI")

#se decidió trabajar con Landsat5_00 pues el Modis_00 no tiene valores 
#completos para el año 2000

ndvi_mes<-do.call("rbind", list(land_94vp, land_95vp, land_96vp, 
                                land_97vp, land_98vp,land_99vp, 
                                land_00vp, mod_01vp, mod_02vp, 
                                mod_03vp, mod_04vp, mod_05vp,
                                mod_06vp, mod_07vp))
ndvi_mes$Mes<-as.numeric(ndvi_mes$Mes)
names(ndvi_mes)[3]<-"point"
ndvi_mes$point<-as.numeric(ndvi_mes$point)

#a cada punto (y provincia) debemos asignarles el periodo correcto para despues calcular el AUC

ndvi_mes[c(ndvi_mes$cod %in% c(4,18,11,14,21,29,41) & ndvi_mes$year == 1994),] <- NA
ndvi_mes[c(ndvi_mes$cod %in% c(14,23) & ndvi_mes$year == 2007),] <- NA
ndvi_mes[c(ndvi_mes$cod %in% c(11,21,41) & ndvi_mes$year == 1995),] <- NA

ndvi_mes <- na.omit(ndvi_mes)

summary(ndvi_mes)

write.csv(ndvi_mes, file = "ndvi_mes_pivot.csv")


#otro método dentro del pivot seria mediante los atributos sin embargo
#no consegui separar los años
#dt_mes<- do.call("cbind", list(Land5_94, Land5_95, Land5_96, Land5_97, Land5_98,
              #Land5_99, Land5_00, Mod_01, Mod_02, 
              #Mod_03, Mod_04, Mod_05, Mod_06, Mod_07))

#dt<-dt_mes %>% pivot_longer(cols = starts_with(c("0", "1")), 
                        #names_to = "Parcela", 
                        #names_repair = "unique", 
                        #values_to = "NDVI") " 

#limpiamos el entorno para poder tener mas memoria y conseguir procesar
ndvi_mes <- read_csv("ndvi_mes_pivot.csv")

ndvi_mes[, 1] <- NULL

str(ndvi_mes)

ndvi_mes %>% group_by(Mes) %>% 
  summarise(mean = mean(NDVI, na.rm = TRUE), 
            sum = sum(NDVI, na.rm = TRUE))

ggplot(ndvi_mes, aes(NDVI)) + 
  geom_histogram(bins = 10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("NDVI") +
  facet_wrap(vars(Mes))

#intentamos para el primer punto ordenando y seleccionando
ndvi_mes_0<-ndvi_mes %>% filter(point == 0)
ndvi_mes_0$nmes<-1:152
plot(ndvi_mes_0$nmes, ndvi_mes_0$NDVI, 
     pch=20, cex=1, col="red", type="o")
ggplot(ndvi_mes_0, aes(x=nmes, y=NDVI)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)
lm_0<-lm(NDVI~nmes, ndvi_mes_0)
summary(lm_0)
names(lm_0)
lm_0$coefficients[2]

#Modelamos un punto mediante GAM para encontrar la funcion (aunque no sirve de nada)
#que sigue cada punto y asi despues poder clacular el área interna (mediante integral?)
gam_0 <- gam(NDVI ~ s(Mes) + s(year) + ti(Mes, year), #variables anidadas?
    data = ndvi_mes_0, method = "REML", 
    na.action = "na.omit" ) #Missing Data Filter for GAMs
#las variables anidadas apenas mejoran la R2

summary(gam_0)
coefficients(gam_0)

gam.check(gam_0)
plot(gam_0, seWithMean = T, shift = coef(gam_0)[1],
     shade = T, shade.col = "lightblue", 
     all.terms = TRUE, pages = 1)

#en realidad no hace falta modelar, apenas calcular el área total
#para calcular el área de debajo de la gráfica

AUC <- auc(ndvi_mes_0$nmes,ndvi_mes_0$NDVI, thresh = NULL, 
           dens = 100, sort.x = TRUE)

#otro método libraria cattools
AUC(ndvi_mes_0$nmes,ndvi_mes_0$NDVI,
    method = "trapezoid", absolutearea = FALSE, 
    subdivisions = 100, na.rm = T)


#ahora lo replicamos para todas los puntos creando la variable continua nmes
#creamos un vector para inserirlo depues en en dataframe ordenado por punto y mes
nmes<-1:168

ndvi_mes <- ndvi_mes %>% arrange(point) %>% cbind(nmes)

#dedicamos y limpiamos la memoria RAM para el proceso
memory.limit(size = 6135)
gc()

#aqui conseguimos introducir una función dentro de dplyr, anidar procesos de dos funciones
AUC_ndvi <- ndvi_mes %>% 
  group_by(point) %>% 
  summarise(AUC = auc(nmes,NDVI, thresh = NULL, 
                      dens = 100, sort.x = TRUE))

#una vez que lo tenemos, cambiamos el nombre y le volvemos a asignar por cada geopunto el valor del AUC
names(Mod_07)[3]<-"point"
AUC_ndvi<-merge(AUC_ndvi, Mod_07, by = "point")

AUC_ndvi_f<-AUC_ndvi[, c(1:4)]

write.csv(AUC_ndvi_f, file = "AUC_ndvi_final.csv")


#ahora que tenemos los datos del área de NDVI devemos relacionarlos con 
#el resto de parámetros del IFN para buscar modelarlos

AUC_ndvi_bio <- merge(AUC_ndvi_f, ifn_2_3, by = c("latitud","longitud"))

ggplot(AUC_ndvi_bio, aes(x=AUC, y=RGR)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)


