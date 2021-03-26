#importamos el fichero con todos los datos de los dos INF con sus indices y biomasas

indbio <- read_csv("Indices_Biomasa_2_3.csv")
head(indbio_2_3)
indbio_2_3<-indbio[,c(4,5,36:39,41:44,46,58:66)]
head(indbio_2_3)
skim(indbio_2_3)

#aplicando regresiones paramétricas como RMA (reduced major axis)
#dentro del paquete lmodel2
#hay que introducirle mas variables independientes

model.new <- lmodel2(AB3_Mgha~NDVI_IFN3, data = indbio_2_3, 
                     "relative", "relative", nperm=99)
model.new

plot(model.new)
