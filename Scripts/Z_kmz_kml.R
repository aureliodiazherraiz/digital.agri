library(rgdal)
library(sf)
library(fs)
library(magrittr)
library(leaflet)
library(raster)


#transformar kmz en kml
# a workaround
input_file <- ('C:/Users/Diego/Downloads/parques/parques/Cardena/cardena.kmz')
read_sf(input_file)

# workaround
target_file <- ('C:/Users/Diego/Downloads/parques/parques/Cardena/.temp.kml.zip')
fs::file_copy(input_file, target_file)
unzip(target_file, )

# read as kml now
(routes_sf_3 <- read_sf('doc.kml'))

# check for equality between routes
assertthat::are_equal(routes_sf_1, routes_sf_3)

# cleanup the temp files
fs::file_delete(target_file)
fs::file_delete('doc.kml')