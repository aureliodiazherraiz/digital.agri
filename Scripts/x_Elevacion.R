#para calcular indices geográficos como el perfil de curvatura de un punto
#There are currently two endpoints that elevatr accesses. For point elevation data 
#it uses USGS Elevation Point Query Service
#and to access raster elevation data (e.g. a DEM) 
#it uses the Amazon Web Services Terrain Tiles.

library(elevatr)
dt_latlon<-indbio[, c(1,4,5)]

