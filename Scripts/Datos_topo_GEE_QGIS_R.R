#### DATOS DE TOPOGRAFIA OBTENIDOS DE DIFERENTES FORMAS ####

topo_250121_3857 <- read_csv("IFN/topo_250121_3857.csv")
names(topo_250121_3857)[1] <- "ID"

srtm_GEE_250121 <- read_excel("IFN/srtm_GEE_250121.xlsx")
names(srtm_GEE_250121)[1] <- "ID"

X30_plots_terrain <- read_excel("IFN/30_plots_terrain.xlsx")

a <- merge(topo_250121_3857, srtm_GEE_250121, by = "ID")
names(a)[2] <- "Provincia"
names(a)[3] <- "Estadillo"

b <- merge(a,X30_plots_terrain, by = c("Provincia", "Estadillo"))

c <- b %>% group_by(Estadillo) %>% summarise(slope_pbl = mean(slope2), slope_GEE = mean(slope), aspect_pbl = mean(aspect2), aspect_qgis = mean(aspecto_25), slope_qgis = mean(pendiente_), elev_GEE = mean(elevation), aspect_GEE = mean(aspect), elev_qgis = mean(Andalucia_))

plot(c$slope_pbl, c$slope_GEE)
plot(c$slope_pbl, c$slope_qgis)
plot(c$slope_GEE, c$slope_qgis)

plot(c$aspect_pbl, c$aspect_qgis)
plot(c$aspect_pbl, c$aspect_GEE)
plot(c$aspect_GEE, c$aspect_qgis)

plot(c$elev_GEE, c$elev_qgis)

