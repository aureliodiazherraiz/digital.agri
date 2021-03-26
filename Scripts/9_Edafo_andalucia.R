library(raster)#cargar imagenes
library(sf)#trabaja como sp pero da otro formato mas fcail de usar a los archivos
library(sp)#carga shapes
library(rgdal)
library(spatstat) # Analisis de patrones de puntos espaciales, ajuste de modelos, simulacion, tests, 
library(spatialEco) # Analisis espacial y utilidades de modelado
library(maptools)
library(mapview)
library(ggplot2)
library(ggspatial)#visualizacion de raster

library(spThin)
library(biomod2)#para aplicar un gran paquete de técnica de analisis de datos (algoritmos)
library(rgbif) 

#cargamos el shape con los puntos de andalucia

plots_shp <- readOGR("~/digital.agri/IFN/Geo_anda_ifn_250121.shp")
plots_shp@coords
#tb podemos usar 
plots_shp2<-shapefile("~/digital.agri/Andalucia_puntos_shape/Geo_andalucia_IF_220121.shp")
plot(plots_shp)
plot(plots_shp2)

anda_shp<-readOGR("~/digital.agri/Andalucia_contorno.geojson")
plot(anda_shp)
plot(plots_shp, add = TRUE)

#vemos el sistema de referencia
crs(plots_shp2)
crs(plots_shp)

#podemos transformar dataframe y explorar
plots_shp <- as(plots_shp, "data.frame")
plots_shp
str_length(plots_shp$coords.x1)

### Convertir los datos en un objeto espacial
#caso no tuviese sistema de coordenadas podriamos introducirlo mediante coordinates
names(plots_shp)[names(plots_shp) == "coords.x1"] <- "x"
names(plots_shp)[names(plots_shp) == "coords.x2"] <- "y"
coordinates(plots_shp)=~x+y
plot(plots_shp)

plots_shp <- SpatialPoints(plots_shp[,c("x","y")], 
                             proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
qplot(x=x,y=y,data=plots_shp)

#tb lo podemos hacer con el paquete sf
topo_shp<-st_read("~/digital.agri/Andalucia_puntos_shape/Metricas_topograficas_Andalucia_IFN_220121.shp")
topo_shp
class(topo_shp)

# Asignamos el sistema de referencia caso tengamos sistemas de coordenadas distintos 
#se pueden bajar mas sistemas de referencia en la pagina spatial reference

proj4string(plots_shp)<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
proj4string(anda_shp)<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#tb se puede
st_transform(topo_shp, 4326)


####trabajamos con los raster####
#no funciona porque textura tiene dimensiones diferentes
rastlist<-stack(list.files(path = "C:/Users/Diego/Documents/Var_Edaficas", pattern='.TIF$', all.files=TRUE, full.names=FALSE))
allrasters <- stack(rastlist)

#por eso las vamos cargando una a una

arc<-raster("C:/Users/Diego/Documents/Var_Edaficas/arc.tif")
arc
extent(arc)

hid<-raster("C:/Users/Diego/Documents/Var_Edaficas/cod_hid.tif")
hid
extent(hid)

are<-raster("C:/Users/Diego/Documents/Var_Edaficas/are.tif")
are
extent(are)

lim<-raster("C:/Users/Diego/Documents/Var_Edaficas/lim.tif")
lim
extent(lim)

ca<-raster("C:/Users/Diego/Documents/Var_Edaficas/ca.tif")
ca
extent(ca)

cic<-raster("C:/Users/Diego/Documents/Var_Edaficas/cic.tif")
cic
extent(cic)

crad<-raster("C:/Users/Diego/Documents/Var_Edaficas/crad.tif")
crad
extent(crad)

mo<-raster("C:/Users/Diego/Documents/Var_Edaficas/mo.tif")
mo
extent(mo)

mo_sup<-raster("C:/Users/Diego/Documents/Var_Edaficas/mo_sup.tif")
mo_sup
extent(mo_sup)

n_sup<-raster("C:/Users/Diego/Documents/Var_Edaficas/n_sup.tif")
n_sup
extent(n_sup)

ph<-raster("C:/Users/Diego/Documents/Var_Edaficas/ph.tif")
ph
extent(ph)

ps<-raster("C:/Users/Diego/Documents/Var_Edaficas/ps.tif")
ps
extent(ps)

psb<-raster("C:/Users/Diego/Documents/Var_Edaficas/psb.tif")
psb
extent(psb)

tf<-raster("C:/Users/Diego/Documents/Var_Edaficas/tf.tif")
tf
extent(tf)

textura<-raster("C:/Users/Diego/Documents/Var_Edaficas/textura.tif")
textura
extent(textura)

#la textura tiene diferentes dimensiones por lo que hay que ajustarla al resto
text<-resample(textura,tf)
extent(text)

#juntamos todos los raster
edafo<-stack(tf, mo, mo_sup, hid, text, psb, ps, arc, are, lim, ca, cic, crad, n_sup, ph)
plot(edafo)
edafo@crs
print(edafo)
edafo$tf

#parece haber problemas con las coordenadas a la hora de hacer el extract por lo que hacemos una recodificacion de la latitud y longuitud y datum en los raster
#proj4string(arc)<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")!!!!!no funciona con raster

srs <- "+proj=longlat +datum=WGS84 +no_defs"

text<-projectRaster(text,
                    crs = srs)
text

edafo<-projectRaster(edafo,
                     crs = srs)

#tb 
crs(textura) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
textura<-st_transform(textura, 4326)#siendo 4326 el sistema de coordenadas
projectRaster(edafo,crs = 4326)

plot(edafo)
plot(plots_shp, add= T)

#lo podemos transformar en un dataframe y posteriormente graficarlo
text_df <- raster::as.data.frame(text, xy = TRUE)
text_df

ggplot() +
  geom_raster(data = text_df, aes(x = x, y = y, fill = text)) +
  geom_spatial_point(data = plots_shp, aes(x = x, y = y)) +
  coord_equal() +
  scale_fill_gradientn(colours = rev(terrain.colors(10)))

#ahora sacaremos los valores para cada punto del ifn, primero transformamos el shape para spatial points
spdt<-raster::extract(edafo, SpatialPoints(plots_shp), sp = T)
summary(spdt)
spdt_df <- raster::as.data.frame(spdt, xy = TRUE)

#o tb
spdt_df2<-cbind(extract(edafo, plots_shp, df =T), plots_shp)

mapview(edafo)

write.csv(spdt_df2, file = "edafo_anda_250121.csv")

head(spdt_df2)






