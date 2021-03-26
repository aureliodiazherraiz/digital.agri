#leemos los dos archivos, uno con las coordenadas de cada plot, donde estan los codigos
#de los estadillos
#el otro donde estan los códigos de los plots, ambos tienen entradas diferentes
#por eso hay que prepararlos para realizar un merge
A_plots <- read_delim("070920_Andalucia_plots_cod.csv", ";", escape_double = FALSE, col_types = cols(lat = col_character(), 
                                                                   long = col_character()), trim_ws = TRUE)

A_plots_cod_ano <- read_delim("070920_Andalucia_plots_cod_ano.csv", 
                                              ";", escape_double = FALSE, trim_ws = TRUE)

#unimos los dataframes mediante loas variables estadillo y provincia
And_plots<-merge(A_plots, A_plots_cod_ano, by = c("Provincia","Estadillo"))


#subimos todos los dataframe con los indices NDVI e SAVI
dt_94_1<-read_excel("94_NDVI_SAVI.xlsx")
dt_94_2<-read_excel("94_2_NDVI_SAVI.xlsx")
dt_95_1<-read_excel("95_NDVI_SAVI.xlsx")
dt_95_2<-read_excel("95_2_NDVI_SAVI.xlsx")
dt_96_1<-read_excel("96_NDVI_SAVI.xlsx")
dt_96_2<-read_excel("96_2_NDVI_SAVI.xlsx")

dt_06_1<-read_excel("06_NDVI_SAVI.xlsx")
dt_06_2<-read_excel("06_2_NDVI_SAVI.xlsx")
dt_07_1<-read_excel("07_NDVI_SAVI.xlsx")
dt_07_2<-read_excel("07_2_NDVI_SAVI.xlsx")


names(dt_94_1)[c(4,5)]=c("NDVI_94_1","SAVI_94_1")
names(dt_94_2)[c(4,5)]=c("NDVI_94_2","SAVI_94_2")
names(dt_95_1)[c(4,5)]=c("NDVI_95_1","SAVI_95_1")
names(dt_95_2)[c(4,5)]=c("NDVI_95_2","SAVI_95_2")
names(dt_96_1)[c(4,5)]=c("NDVI_96_1","SAVI_96_1")
names(dt_96_2)[c(4,5)]=c("NDVI_96_2","SAVI_96_2")
names(dt_06_1)[c(4,5)]=c("NDVI_06_1","SAVI_06_1")
names(dt_06_2)[c(4,5)]=c("NDVI_06_2","SAVI_06_2")
names(dt_07_1)[c(4,5)]=c("NDVI_07_1","SAVI_07_1")
names(dt_07_2)[c(4,5)]=c("NDVI_07_2","SAVI_07_2")


#creo un dataframe con todos los indices de todos los anos, 
#reduce me dio problemas la no respetar el orden de los datos
#ndvi_savi_total1<-Reduce(merge, list(dt_06_1,dt_06_2,dt_07_1,dt_07_2,dt_94_1,dt_94_2,dt_95_1,dt_95_2,dt_96_1,dt_96_2))

ndvi_savi_total<-cbind(dt_06_1, dt_06_2, dt_07_1, dt_07_2, dt_94_1, dt_94_2, dt_95_1, dt_95_2, dt_96_1, dt_96_2)

ndvi_savi_total<-ndvi_savi_total[,-c(6:8,11:13,16:18,21:23,26:28,31:33,36:38,41:43,46:48)]

#para saber el numero de nulos en los indices
sapply(ndvi_savi_total, function(x) sum(is.na(x)))

#para poder juntarlos con un merge, debemos transformar la longitud o latitud en caracter, cosa que hace str_subs pues los transforma en string
#ademas tenemos que iguaar los digitos de las variables para poder cruzarlos, en este caso usamos lat&long
#notar que cuando reduzimos el numero de digitos conseguirmos mayor numero de puntos entre los dos dt frame
ndvi_savi_total$longitud<-str_sub(ndvi_savi_total$longitud, 1, 9)
ndvi_savi_total$latitud<-str_sub(ndvi_savi_total$latitud, 1, 9)

And_plots$long<-str_sub(And_plots$long, 1, 9)
And_plots$lat<-str_sub(And_plots$lat, 1, 9)

head(And_plots$long)
head(ndvi_savi_total$longitud)
head(And_plots$lat)
head(ndvi_savi_total$latitud)

names(ndvi_savi_total)[2]<-"long"
names(ndvi_savi_total)[3]<-"lat"

dt_final_plots<-merge(ndvi_savi_total, And_plots, by = c("lat","long"))


#analizaremos las posibles correlaciones entre las variables

#importamos la tabla con los datos referentes al INF3
Anda_Total_Biomass <- read_csv("Andalucia_TotalBiomass.csv")

names(Anda_Total_Biomass)[2]<-"Provincia"
names(Anda_Total_Biomass)[3]<-"Estadillo"

summary(Anda_Total_Biomass)
summary(dt_final_plots)

Anda_Total_Biomass$Estadillo<-as.numeric(Anda_Total_Biomass$Estadillo)

dt_plt_ind_biomas<-merge(dt_final_plots, Anda_Total_Biomass, by = c("Provincia","Estadillo"))
write.csv(x=dt_plt_ind_biomas, file="Indices_puntos_biomasa_Andalucia.csv")

#seleccionamos le valor maximo de cada ano para cada indice en cada INF
dt_plt_ind_biomas$NDVI94<- 0 
for(i in 1:nrow(dt_plt_ind_biomas)){
  if(dt_plt_ind_biomas[i,16] >= dt_plt_ind_biomas[i,14]) {
    dt_plt_ind_biomas$NDVI94[i]= dt_plt_ind_biomas[i,16]
  } else {dt_plt_ind_biomas$NDVI94[i]= dt_plt_ind_biomas[i,14]}
}

dt_plt_ind_biomas$NDVI95<- 0 
for(i in 1:nrow(dt_plt_ind_biomas)){
  if(dt_plt_ind_biomas[i,20] >= dt_plt_ind_biomas[i,18]) {
    dt_plt_ind_biomas$NDVI95[i]= dt_plt_ind_biomas[i,20]
  } else {dt_plt_ind_biomas$NDVI95[i]= dt_plt_ind_biomas[i,18]}
}

dt_plt_ind_biomas$NDVI96<- 0 
for(i in 1:nrow(dt_plt_ind_biomas)){
  if(!is.na(dt_plt_ind_biomas[i,22]) & !is.na(dt_plt_ind_biomas[i,24])){
    if(dt_plt_ind_biomas[i,22] >= dt_plt_ind_biomas[i,24]) {
      dt_plt_ind_biomas$NDVI96[i]= dt_plt_ind_biomas[i,22]
    } else {dt_plt_ind_biomas$NDVI96[i]= dt_plt_ind_biomas[i,24]}
  } else {dt_plt_ind_biomas$NDVI96[i] <-NA}
}

dt_plt_ind_biomas$NDVI06<- 0 
for(i in 1:nrow(dt_plt_ind_biomas)){
  if(!is.na(dt_plt_ind_biomas[i,06]) & !is.na(dt_plt_ind_biomas[i,08])){
    if(dt_plt_ind_biomas[i,06] >= dt_plt_ind_biomas[i,08]) {
      dt_plt_ind_biomas$NDVI06[i]= dt_plt_ind_biomas[i,06]
    } else {dt_plt_ind_biomas$NDVI06[i]= dt_plt_ind_biomas[i,08]}
  } else {dt_plt_ind_biomas$NDVI06[i] <- NA}
}


dt_plt_ind_biomas$NDVI07<- 0 
for(i in 1:nrow(dt_plt_ind_biomas)){
  if(!is.na(dt_plt_ind_biomas$NDVI_07_1[i])&!is.na(dt_plt_ind_biomas$NDVI_07_2[i])){
    if(dt_plt_ind_biomas[i,10] >= dt_plt_ind_biomas[i,12]) {
      dt_plt_ind_biomas$NDVI07[i]= dt_plt_ind_biomas[i,10]
    } else {dt_plt_ind_biomas$NDVI07[i]= dt_plt_ind_biomas[i,12]}
  } else {dt_plt_ind_biomas$NDVI07[i] <- NA}
} 





dt_plt_ind_biomas$SAVI07<- 0 
for(i in 1:nrow(dt_plt_ind_biomas)){
  if(!is.na(dt_plt_ind_biomas$SAVI_07_1[i])&!is.na(dt_plt_ind_biomas$SAVI_07_2[i])){
    if(dt_plt_ind_biomas[i,11] >= dt_plt_ind_biomas[i,13]) {
      dt_plt_ind_biomas$SAVI07[i]= dt_plt_ind_biomas[i,11]
    } else {dt_plt_ind_biomas$SAVI07[i]= dt_plt_ind_biomas[i,13]}
  } else {dt_plt_ind_biomas$SAVI07[i] <- NA}
} 

dt_plt_ind_biomas$SAVI06<- 0 
for(i in 1:nrow(dt_plt_ind_biomas)){
  if(!is.na(dt_plt_ind_biomas$SAVI_06_1[i])&!is.na(dt_plt_ind_biomas$SAVI_06_2[i])){
    if(dt_plt_ind_biomas[i,07] >= dt_plt_ind_biomas[i,09]) {
      dt_plt_ind_biomas$SAVI06[i]= dt_plt_ind_biomas[i,07]
    } else {dt_plt_ind_biomas$SAVI06[i]= dt_plt_ind_biomas[i,09]}
  } else {dt_plt_ind_biomas$SAVI06[i] <- NA}
} 

dt_plt_ind_biomas$SAVI96<- 0 
for(i in 1:nrow(dt_plt_ind_biomas)){
  if(!is.na(dt_plt_ind_biomas$SAVI_96_1[i])&!is.na(dt_plt_ind_biomas$SAVI_96_2[i])){
    if(dt_plt_ind_biomas[i,23] >= dt_plt_ind_biomas[i,25]) {
      dt_plt_ind_biomas$SAVI96[i]= dt_plt_ind_biomas[i,23]
    } else {dt_plt_ind_biomas$SAVI96[i]= dt_plt_ind_biomas[i,25]}
  } else {dt_plt_ind_biomas$SAVI96[i] <- NA}
} 


dt_plt_ind_biomas$SAVI95<- 0 
for(i in 1:nrow(dt_plt_ind_biomas)){
  if(!is.na(dt_plt_ind_biomas$SAVI_95_1[i])&!is.na(dt_plt_ind_biomas$SAVI_95_2[i])){
    if(dt_plt_ind_biomas[i,19] >= dt_plt_ind_biomas[i,21]) {
      dt_plt_ind_biomas$SAVI95[i]= dt_plt_ind_biomas[i,19]
    } else {dt_plt_ind_biomas$SAVI95[i]= dt_plt_ind_biomas[i,21]}
  } else {dt_plt_ind_biomas$SAVI95[i] <- NA}
} 


dt_plt_ind_biomas$SAVI94<- 0 
for(i in 1:nrow(dt_plt_ind_biomas)){
  if(!is.na(dt_plt_ind_biomas$SAVI_94_1[i])&!is.na(dt_plt_ind_biomas$SAVI_94_2[i])){
    if(dt_plt_ind_biomas[i,15] >= dt_plt_ind_biomas[i,17]) {
      dt_plt_ind_biomas$SAVI94[i]= dt_plt_ind_biomas[i,15]
    } else {dt_plt_ind_biomas$SAVI94[i]= dt_plt_ind_biomas[i,17]}
  } else {dt_plt_ind_biomas$SAVI94[i] <- NA}
} 

dt_plt_ind_biomas$year2 %>% as.factor() %>% summary()
dt_plt_ind_biomas$year3 %>% as.factor() %>% summary()
dt_plt_ind_biomas$Sp.x %>% as.factor() %>% summary()

#asignamos a cada provincia sus valores reales de NDVI y SAVI
dt_plt_ind_biomas$NDVI_IFN3 <- 0
for (i in 1:nrow(dt_plt_ind_biomas)) {
  if(dt_plt_ind_biomas[i,30] <= 2006) {
    dt_plt_ind_biomas$NDVI_IFN3[i] = dt_plt_ind_biomas[i,50]
  } else {
      dt_plt_ind_biomas$NDVI_IFN3[i] = dt_plt_ind_biomas[i,51]
    } 
}


dt_plt_ind_biomas$NDVI_IFN2 <- 0
for(i in 1: nrow(dt_plt_ind_biomas)) {
  if(dt_plt_ind_biomas[i,29] < 1995) {
    dt_plt_ind_biomas$NDVI_IFN2[i] = dt_plt_ind_biomas[i,47]
  }else {
    if(dt_plt_ind_biomas[i,29] <- 1995) {
      dt_plt_ind_biomas$NDVI_IFN2[i] = dt_plt_ind_biomas[i,48]
      }else {dt_plt_ind_biomas$NDVI_IFN2[i] = dt_plt_ind_biomas[i,49]
      }
  }
}

# analogamente para el SAVI
dt_plt_ind_biomas$SAVI_IFN3 <- 0
for (i in 1:nrow(dt_plt_ind_biomas)) {
  if(dt_plt_ind_biomas[i,30] <= 2006) {
    dt_plt_ind_biomas$SAVI_IFN3[i] = dt_plt_ind_biomas[i,53]
  } else {
    dt_plt_ind_biomas$SAVI_IFN3[i] = dt_plt_ind_biomas[i,52]
  } 
}


dt_plt_ind_biomas$SAVI_IFN2 <- 0
for(i in 1: nrow(dt_plt_ind_biomas)) {
  if(dt_plt_ind_biomas[i,29] < 1995) {
    dt_plt_ind_biomas$SAVI_IFN2[i] = dt_plt_ind_biomas[i,56]
  }else {
    if(dt_plt_ind_biomas[i,29] <- 1995) {
      dt_plt_ind_biomas$SAVI_IFN2[i] = dt_plt_ind_biomas[i,55]
    }else {dt_plt_ind_biomas$SAVI_IFN2[i] = dt_plt_ind_biomas[i,54]
    }
  }
}


dt_plt_ind_biomas$NDVI_2_3 <- dt_plt_ind_biomas[,57]-dt_plt_ind_biomas[,58]
dt_plt_ind_biomas$BP_2_3 <- (dt_plt_ind_biomas$AB3_Mgha - dt_plt_ind_biomas$AB2_Mgha)
dt_plt_ind_biomas$SAVI_2_3 <-dt_plt_ind_biomas[,59]-dt_plt_ind_biomas[,60]
dt_plt_ind_biomas$BP_2_3_ano <-(dt_plt_ind_biomas$AB3_Mgha - dt_plt_ind_biomas$AB2_Mgha)/dt_plt_ind_biomas$year32
dt_plt_ind_biomas$RGR<-(log(dt_plt_ind_biomas$AB3_Mgha) - log(dt_plt_ind_biomas$AB2_Mgha))/dt_plt_ind_biomas$year32

write.csv(x=dt_plt_ind_biomas, file = "Indices_Biomasa_2_3.csv")



###----
#separamos los archivos en provincia para despues unirlos utilizando el codigo estadillo
Alm_04<-A_plots[A_plots$Provincia == "Almeria",]
Cad_11<-A_plots[A_plots$Provincia == "Cadiz",]
Cor_14<-A_plots[A_plots$Provincia == "Cordoba",]
Gra_18<-A_plots[A_plots$Provincia == "Granada",]
Hue_12<-A_plots[A_plots$Provincia == "Huelva",]
Jae_23<-A_plots[A_plots$Provincia == "Jaen",]
Mal_29<-A_plots[A_plots$Provincia == "Malaga",]
Sev_41<-A_plots[A_plots$Provincia == "Sevilla",]

Alm_cod<-A_plots_cod_ano[A_plots_cod_ano$Provincia == "Almeria",]
Cad_cod<-A_plots_cod_ano[A_plots_cod_ano$Provincia == "Cadiz",]
Cor_cod<-A_plots_cod_ano[A_plots_cod_ano$Provincia == "Cordoba",]
Gra_cod<-A_plots_cod_ano[A_plots_cod_ano$Provincia == "Granada",]
Jae_cod<-A_plots_cod_ano[A_plots_cod_ano$Provincia == "Jaen",]
Hue_cod<-A_plots_cod_ano[A_plots_cod_ano$Provincia == "Huelva",]
Mal_cod<-A_plots_cod_ano[A_plots_cod_ano$Provincia == "Malaga",]
Sev_cod<-A_plots_cod_ano[A_plots_cod_ano$Provincia == "Sevilla",]


#unimos los archivos de cada provincia descartando aquellos que no tienen estadillo
Almeria<-merge(Alm_04,Alm_cod, by = "Estadillo")
Cadiz<-merge(Cad_11,Cad_cod, by= "Estadillo")
Cordoba<-merge(Cor_14,Cor_cod, by = "Estadillo")
Granada<-merge(Gra_18,Gra_cod, by = "Estadillo")
Huelva<-merge(Hue_12,Hue_cod, by = "Estadillo")
Jaen<-merge(Jae_23,Jae_cod, by = "Estadillo")
Malaga<-merge(Mal_29,Mal_cod, by = "Estadillo")
Sevilla<-merge(Sev_41,Sev_cod, by = "Estadillo")

#ahora unimos todos los puntos en un unico dataframe
Andalucia_final<-rbind(Almeria, Cadiz, Cordoba, Granada, Huelva, Jaen, Malaga, Sevilla)

Andalucia_final<-Andalucia_final[, -7]#elimina una columna repetida
###----













