library(rgdal)
library(sp)
library(sf)
library(raster)


bioshp_250121 <- read_csv("IFN/Andalucia_TotalBiomass_250121.csv")

### Convertir los datos en un objeto espacial
#caso no tuviese sistema de coordenadas podriamos introducirlo mediante coordinates


coordinates(bioshp_250121)=~X+Y
plot(bioshp_250121)
bioshp_250121
crs(bioshp_250121)
bioshp_250121_1 <- SpatialPoints(bioshp_250121, 
                           proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

elevation <-get_elev_raster(cali, z = 8)






plots_shp <- readOGR("~/digital.agri/IFN/Geo_anda_ifn_250121.shp")
plots_shp@coords