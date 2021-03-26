#importamos el fichero con todos los datos de los dos INF con sus indices y biomasas

indbio <- read_csv("Indices_Biomasa_2_3.csv")
head(indbio_2_3)
indbio_2_3<-indbio[,c(4,5,36:39,41:44,46,58:66)]
head(indbio_2_3)
skim(indbio_2_3)

plot(indbio_2_3$long,indbio_2_3$lat)
points(indbio_2_3$long[indbio_2_3$Sp.x=="Quercus ilex"],indbio_2_3$lat[indbio_2_3$Sp.x=="Quercus ilex"], col="blue", pch="#")
points(indbio_2_3$long[indbio_2_3$Sp.x=="Quercus suber"],indbio_2_3$lat[indbio_2_3$Sp.x=="Quercus suber"], col="red", pch="$")
points(indbio_2_3$long[indbio_2_3$Sp.x=="Pinus halepensis"],indbio_2_3$lat[indbio_2_3$Sp.x=="Pinus halepensis"], col="green", pch="*")
points(indbio_2_3$long[indbio_2_3$Sp.x=="Pinus pinea"],indbio_2_3$lat[indbio_2_3$Sp.x=="Pinus pinea"], col="orange", pch="+")
points(indbio_2_3$long[indbio_2_3$Sp.x=="Pinus nigra"],indbio_2_3$lat[indbio_2_3$Sp.x=="Pinus nigra"], col="yellow", pch="º")
points(indbio_2_3$long[indbio_2_3$Sp.x=="Pinus pinaster"],indbio_2_3$lat[indbio_2_3$Sp.x=="Pinus pinaster"], col="black", pch="x")
legend(-8,37,legend = c("Quercus ilex", "Quercus suber","Pinus halepensis",
                                "Pinus pinea", "Pinus nigra","Pinus pinaster"),
       col = c("blue", "red", "green", "orange", "yellow", "black"), 
       pch = c("#", "$", "*", "+","º","x"), 
       bty = "n", 
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F)

#separamos los dos INF e retiramos las variables no numericas filtrando por valores superiores a 0,3

dt2_3<-indbio_2_3[, -c(1,2,5,9)]
head(dt2_3)

#analizamos las posibles correlaciones entre las varibles por cada IFN
cor2_3<-round(cor(dt2_3, use="complete.obs"),2)
cor2_3
corrplot(cor2_3, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "lower", diag = F, addshade = "all")


dt2<-dt2_3[,c(4:7,9,11)]
head(dt2)
dt3<-dt2_3[,c(1:3,7,8,10)]
head(dt3)
dt32<-dt2_3[,c(7,12:16)] 
head(dt32)


th<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  ## delete background
           panel.background = element_blank(), axis.line = element_line(colour = "black"),
           axis.text=element_text(size=16),
           legend.text=element_text(size=25),
           strip.text = element_text(size=25),#aumenta tamaño de la fuente
           legend.direction = 'horizontal',
           legend.position="top",
           axis.title.x=element_text(size=16),
           axis.title.y=element_text(size=16))

# estudiamos las correlaciones entre las variables del IFN2
cor2<-round(cor(dt2, use="complete.obs"),2)
cor2
corrplot(cor2, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "lower", diag = F, addshade = "all")
chart.Correlation(dt2)
pairs.panels(dt2,
              smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
              scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
              density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
              ellipses = TRUE,    # Si TRUE, dibuja elipses
              method = "pearson", # Método de correlación (también "spearman" o "kendall")
              pch = 21,           # Símbolo pch
              cex = 2, 
              cex.labels = 2,
              lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
              cor = TRUE,         # Si TRUE, agrega correlaciones
              jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
              factor = 2,         # Nivel de ruido añadido a los datos
              hist.col = 4,       # Color de los histogramas
              stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
              ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes)

ggplot(dt2, aes(x=NDVI_IFN2, y=AB2_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm) 

#aplicamos un modelo lineal
lm2<-lm(formula = AB2_Mgha ~ NDVI_IFN2 + 
          Arid_Dm, data = dt2)
summary(lm2)
plot(lm2)

glm2<-glm(formula = AB2_Mgha ~ NDVI_IFN2 + 
            Arid_Dm, data = dt2, trace   = FALSE)
rsq(glm2,adj=T)

gam2 <- gam(AB2_Mgha ~ s(NDVI_IFN2) + (Arid_Dm), 
            data = dt2, method = "REML" )

#gls2<-gls(formula = AB2_Mgha ~ NDVI_IFN2 + Arid_Dm, data = dt2)

gam.check(gam2)
summary(gam2)
summary(glm2)

plot(gam2, seWithMean = TRUE, shift = coef(gam2)[1],
     residuals = T, pch = 1, cex = 1, 
     all.terms = T, pages = 1, 
     shade = T, shade.col = "lightblue")
plot(gam2, seWithMean = TRUE, shift = coef(gam2)[1], 
     all.terms = T, pages = 1)

capture.output(summary(lm2),file="modelo lineal_2.doc")
capture.output(summary(gam2),file="gam2.doc")

# estudiamos las correlaciones entre las variables del IFN3
cor3<-round(cor(dt3, use="complete.obs"),2)
cor3
corrplot(cor3, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "lower", diag = F, addshade = "all")
chart.Correlation(dt3, pch = 19)
pairs.panels(dt3,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             cex = 2,
             cex.labels = 2,     # tamaño de la fuente de la diagonal
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 0.5,       # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes)

ggplot(dt3, aes(x=NDVI_IFN3, y=AB3_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm) 

lm3<-lm(formula = AB3_Mgha ~ NDVI_IFN3 + Arid_Dm, data = dt3)
summary(lm3)

gam3 <- gam(AB3_Mgha ~ s(NDVI_IFN3, k = 5) + (Arid_Dm), 
            data = dt3, method = "REML" )
gam.check(gam3)

summary(gam3)
plot(gam3, seWithMean = TRUE, shift = coef(gam3)[1],
     residuals = T, cex = 1, 
     all.terms = T, pages = 1, 
     shade = T, shade.col = "lightblue")
plot(gam3, seWithMean = TRUE, shift = coef(gam3)[1], 
     all.terms = T, pages = 1)


# estudiamos las correlaciones entre las variaciones de las variables del IFN
cor32<-round(cor(dt32, use="complete.obs"),2)
cor32
corrplot(cor32, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "lower", diag = F, addshade = "all")
chart.Correlation(dt32)
pairs.panels(dt32,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             cex = 2,
             cex.labels = 2,
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes)


ggplot(dt32, aes(x=NDVI_2_3, y=BP_2_3)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm) 


lm32<-lm(formula = NDVI_2_3 ~ RGR + Arid_Dm, data = dt32)
summary(lm32)

capture.output(summary(lm3),file="modelo lineal_3.doc")
capture.output(summary(gam3),file="gam3.doc")

capture.output(summary(lm32),file="modelo lineal_32.doc")

# si decidimos realizar un análisis por especie para intentar identificar cual de ellas tiene una mayor correlacion entre variables
# aclarar que el análisis está relacionando el NDVI con una especie por plot
#lo cual no es cierto pues relaciona con la especie mayoritaria, caso fuese hecho con una unica especie
#tendria que aplicar un filtro para la variable AB2_Mgha_sp despues del %>% 
#alteracion que no ayuda en la correlación

#Quercus ilex

indbio_2_3 %>% 
  filter(Sp.y == "Quercus ilex") %>% 
  ggplot(aes(x=NDVI_IFN2, y=AB2_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm) +
  ggtitle("AB2~NDVI en Q ilex")

dt_qilex_2 <- indbio_2_3 %>% 
  filter(Sp.y == "Quercus ilex")
head(dt_qilex_2)
cor(dt_qilex_2$NDVI_IFN2, dt_qilex_2$AB2_Mgha, 
    use="pairwise.complete.obs")

indbio_2_3 %>% 
  filter(Sp.x == "Quercus ilex")%>% 
  ggplot(aes(x=NDVI_IFN3, y=AB3_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_qilex_3 <- indbio_2_3 %>% 
  filter(Sp.x == "Quercus ilex")
cor(dt_qilex_3$NDVI_IFN3, dt_qilex_3$AB3_Mgha, 
    use="pairwise.complete.obs")

dt_qilex_2[, -c(3:6,9, 12, 14:20)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Qilex_2", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           order = "AOE", type = "lower", diag = F, addshade = "all")

dt_qilex_3[, c(1:4,11,12)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Qilex_3", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           type = "lower", diag = F, addshade = "all")

#Quercus suber

indbio_2_3 %>% 
  filter(Sp.y == "Quercus suber") %>% 
  ggplot(aes(x=NDVI_IFN2, y=AB2_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_qsuber_2 <- indbio_2_3 %>% 
  filter(Sp.y == "Quercus suber") 
cor(dt_qsuber_2$NDVI_IFN2, dt_qsuber_2$AB2_Mgha, 
    use="pairwise.complete.obs")


indbio_2_3 %>% 
  filter(Sp.x == "Quercus suber") %>% 
  ggplot(aes(x=NDVI_IFN3, y=AB3_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_qsuber_3 <- indbio_2_3 %>% 
  filter(Sp.x == "Quercus suber") 
cor(dt_qsuber_3$NDVI_IFN3, dt_qsuber_3$AB3_Mgha, 
    use="pairwise.complete.obs")

dt_qsuber_2[, -c(3:6,12, 14:20)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Qsuber_2", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           order = "AOE", type = "lower", diag = F, addshade = "all")

dt_qsuber_3[, c(1:4,11,12)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Qsuber_3", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           type = "lower", diag = F, addshade = "all")
dt_qsuber_3[, c(3,4,6,11,12)] %>% chart.Correlation()

#Pinus halepensis

indbio_2_3 %>% 
  filter(Sp.y == "Pinus halepensis") %>% 
  ggplot(aes(x=NDVI_IFN2, y=AB2_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_phale_2 <- indbio_2_3 %>% 
  filter(Sp.y == "Pinus halepensis") 
cor(dt_phale_2$NDVI_IFN2, dt_phale_2$AB2_Mgha, 
    use="pairwise.complete.obs")

indbio_2_3 %>% 
  filter(Sp.x == "Pinus halepensis") %>% 
  ggplot(aes(x=NDVI_IFN3, y=AB3_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_phale_3 <- indbio_2_3 %>% 
  filter(Sp.x == "Pinus halepensis") 
cor(dt_phale_3$NDVI_IFN3, dt_phale_3$AB3_Mgha, 
    use="pairwise.complete.obs")


dt_phale_2[, -c(3:6,9, 12, 14:20)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Phale_2", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           order = "AOE", type = "lower", diag = F, addshade = "all")

dt_phale_3[, c(1:4,11,12)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Phale_3", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           type = "lower", diag = F, addshade = "all")


# Pinus nigra

indbio_2_3 %>% 
  filter(Sp.y == "Pinus nigra") %>% 
  ggplot(aes(x=NDVI_IFN2, y=AB2_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_pnigra_2 <- indbio_2_3 %>% 
  filter(Sp.y == "Pinus nigra") 
cor(dt_pnigra_2$NDVI_IFN2, dt_pnigra_2$AB2_Mgha, 
    use="pairwise.complete.obs")

indbio_2_3 %>% 
  filter(Sp.x == "Pinus nigra") %>% 
  ggplot(aes(x=NDVI_IFN2, y=AB2_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_pnigra_3 <- indbio_2_3 %>% 
  filter(Sp.x == "Pinus nigra") 
cor(dt_pnigra_3$NDVI_IFN3, dt_pnigra_3$AB3_Mgha, 
    use="pairwise.complete.obs")


dt_pnigra_2[, -c(3:6,9, 12, 14:20)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Pnigra_2", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           order = "AOE", type = "lower", diag = F, addshade = "all")

dt_pnigra_3[, c(1:4,11,12)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Pnigra_3", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           type = "lower", diag = F, addshade = "all")

#Pinus pinaster

indbio_2_3 %>% 
  filter(Sp.y == "Pinus pinaster") %>% 
  ggplot(aes(x=NDVI_IFN2, y=AB2_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_pnaster_2 <- indbio_2_3 %>% 
  filter(Sp.y == "Pinus pinaster") 
cor(dt_pnaster_2$NDVI_IFN2, dt_pnaster_2$AB2_Mgha, 
    use="pairwise.complete.obs")

indbio_2_3 %>% 
  filter(Sp.x == "Pinus pinaster") %>% 
  ggplot(aes(x=NDVI_IFN2, y=AB2_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_pnaster_3 <- indbio_2_3 %>% 
  filter(Sp.x == "Pinus pinaster") 
cor(dt_pnaster_3$NDVI_IFN3, dt_pnaster_3$AB3_Mgha, 
    use="pairwise.complete.obs")

dt_pnaster_2[, -c(3:6,9, 12, 14:20)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Pnaster_2", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           order = "AOE", type = "lower", diag = F, addshade = "all")

dt_pnaster_3[, c(1:4,11,12)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Ppinaster_3", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           type = "lower", diag = F, addshade = "all")


# Pinus pinea
indbio_2_3 %>% 
  filter(Sp.y == "Pinus pinea") %>% 
  ggplot(aes(x=SAVI_IFN2, y=AB2_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_pinea_2 <- indbio_2_3 %>% 
  filter(Sp.y == "Pinus pinea") 
cor(dt_pinea_2$NDVI_IFN2, dt_pinea_2$AB2_Mgha, 
    use="pairwise.complete.obs")

indbio_2_3 %>% 
  filter(Sp.x == "Pinus pinea") %>% 
  ggplot(aes(x=SAVI_IFN2, y=AB2_Mgha)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

dt_pinea_3 <- indbio_2_3 %>% 
  filter(Sp.x == "Pinus pinea") 
cor(dt_pinea_3$NDVI_IFN3, dt_pinea_3$AB3_Mgha, 
    use="pairwise.complete.obs")
head(dt_pinea_3)

dt_pinea_2[, -c(3:6,9, 12, 14:20)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Ppinea_2", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           order = "AOE", type = "lower", diag = F, addshade = "all")

dt_pinea_3[, c(1:4,11,12)] %>% 
  cor(use="pairwise.complete.obs") %>% 
  corrplot(method = "shade", shade.col = NA, tl.col = "black", 
           title = "Ppinea_3", mar=c(0,0,5,0),tl.offset = 1, 
           tl.srt = 45, addCoef.col = "black", addcolorlabel = "no", 
           type = "lower", diag = F, addshade = "all")




#haciendo un anova entre especies para ver las posibles diferencias entre ellas para eso trabajamos con las especies del INF3
dt_sp3<-indbio_2_3 %>% 
  filter(Sp.x %in% c("Quercus ilex",
                     "Quercus suber",
                     "Pinus pinea",
                     "Pinus halepensis",
                     "Pinus nigra",
                     "Pinus pinaster"))

th<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  ## delete background
           panel.background = element_blank(), axis.line = element_line(colour = "black"),
           axis.text=element_text(size=16),
           legend.text=element_text(size=20),
           strip.text = element_text(size=20),
           legend.direction = 'horizontal',
           legend.position="top",
           axis.title.x=element_text(size=16),
           axis.title.y=element_text(size=16))

Figura_sp3_NDVI3_AB3 <- ggplot(dt_sp3, aes(NDVI_IFN3, AB3_Mgha)) + 
  geom_point(shape = 1) + 
  geom_smooth(method=lm) + 
  facet_wrap(vars(Sp.x)) +
  pbl_th
Figura_sp3_NDVI3_AB3

#le metemos un tema creado por pablo apra mejorar el histograma
pbl_th<-theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),  ## delete background
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    axis.text=element_text(size=20),
                    strip.text = element_text(size=20),#aumenta e tamaño de la fuente
                    legend.text=element_text(size=20),
                    legend.direction = 'horizontal',
                    legend.position="top",
                    axis.title.x=element_text(size=16),
                    axis.title.y=element_text(size=16))

Fig_sp3_NDVI3_HIST <- ggplot(dt_sp3, aes(NDVI_IFN3)) + 
  geom_histogram(bins = 10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("NDVI_INF3") + 
  pbl_th +
  facet_wrap(vars(Sp.x))
Fig_sp3_NDVI3_HIST


Figura_sp3_Arid_AB3 <- ggplot(dt_sp3, aes(Arid_Dm, AB3_Mgha)) + 
  geom_point(shape = 1) + 
  geom_smooth(method=lm) + 
  facet_wrap(vars(Sp.x)) +
  th
Figura_sp3_Arid_AB3

Fig_sp3_Arid_Dm_HIST <- ggplot(dt_sp3, aes(Arid_Dm)) + 
  geom_histogram(bins = 10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Arid_Dm") +
  pbl_th +
  facet_wrap(vars(Sp.x))
Fig_sp3_Arid_Dm_HIST

Figura_sp3_Arid_NDVI3 <- ggplot(dt_sp3, aes(Arid_Dm, NDVI_IFN3)) + 
  geom_point(shape = 1) + 
  geom_smooth(method=lm) + 
  facet_wrap(vars(Sp.x)) +
  th
Figura_sp3_Arid_NDVI3

Fig_sp3_AB3_Mgha_HIST <- ggplot(dt_sp3, aes(AB3_Mgha)) + 
  geom_histogram(bins = 10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("AB3_Mgha") +
  pbl_th +
  facet_wrap(vars(Sp.x))
Fig_sp3_AB3_Mgha_HIST

table(dt_sp3$Sp.x)

#para poder ver como las diferentes especies responden a los NDVI y a los indices de aridez:
#grafico de bigotes
ggplot(data = dt_sp3, aes(x = NDVI_IFN3, y = Sp.x, color = Sp.x)) +
  geom_boxplot() +
  theme_bw()

head(dt_sp3)

#los graficos anteriores tb pueden representarse de la siguiente forma para ver la diferencia
#entre las especies para los dos parámetros NDVI y Arid
ggline(dt_sp3, x = names(dt_sp3)[5],
       y = names(dt_sp3)[12],
       add=c("mean_ci", "jitter"), palette = "jco") + ggtitle ("NDVI_3")
ggline(dt_sp3, x = names(dt_sp3)[5],
       y = names(dt_sp3)[11],
       add=c("mean_ci", "jitter"), palette = "jco") + ggtitle ("Arid_Dm")

ggline(dt_sp3, x = names(dt_sp3)[5],
       y = names(dt_sp3)[4],
       add=c("mean_ci", "jitter"), palette = "jco") + ggtitle ("AB3_Dm")


#GAM

str(dt_sp3)
dt_sp3$Sp.x <- as.factor(dt_sp3$Sp.x)
dt_sp3$Sp.y <- as.factor(dt_sp3$Sp.y)
str(dt_sp3)

#creamos los modelos aditivos entre todos los puntos
#algunos ya estan repetidos al inicio del scripr para cada IFN

gam_sp3_1 <- gam(AB3_Mgha ~ s(NDVI_IFN3), data = dt_sp3, method = "REML")
plot(gam_sp3_1, seWithMean = TRUE, shift = coef(gam_sp3_1)[1], 
     all.terms = TRUE, pages = 1, 
     shade = T, shade.col = "lightblue")
summary(gam_sp3_1)
capture.output(summary(gam_sp3_1),file="gam_sp3_1.doc")

#introduciendo la variable aridez como variable no lineal
gam_sp3_2 <- gam(AB3_Mgha ~ s(NDVI_IFN3) + s(Arid_Dm), data = dt_sp3, method = "REML")
gam.check(gam_sp3_2)
plot(gam_sp3_2, seWithMean = TRUE, shift = coef(gam_sp3_1)[1], 
     shade = T, shade.col = "lightblue", 
     all.terms = TRUE, pages = 1)
summary(gam_sp3_2)
capture.output(summary(gam_sp3_1),file="gam_sp3_2.doc")

#podemos comparar con los gráficos de ggplot
ggplot(dt_sp3, aes(AB3_Mgha, NDVI_IFN3)) + 
  geom_point(shape = 1) + 
  geom_smooth(method=gam) + 
  facet_wrap(vars(Sp.x))

#este grafico me devuelve una relación inversa donde la variable respuesta es el NDVI
ggplot(dt_sp3, aes(NDVI_IFN3, AB3_Mgha)) + 
  geom_point(shape = 1) + 
  geom_smooth(method=gam) + 
  facet_wrap(vars(Sp.x))

#el modelo no explica la causalidad
gam_sp3_3 <- gam(AB3_Mgha ~ s(NDVI_IFN3, by = Sp.x) + Sp.x, data = dt_sp3, method = "REML")
plot(gam_sp3_3, all.terms = TRUE, pages = 4)
summary(gam_sp3_3)
capture.output(summary(gam_sp3_1),file="gam_sp3_3.doc")

#el modelo no explica la causalidad
gam_sp3_4 <- gam(AB3_Mgha ~ s(NDVI_IFN3, by = Sp.x) + s(Arid_Dm, by = Sp.x) + Sp.x, data = dt_sp3, method = "REML")
plot(gam_sp3_4, seWithMean = TRUE, shift = coef(gam_sp3_4)[1],
     all.terms = TRUE, pages = 10)
summary(gam_sp3_4)
capture.output(summary(gam_sp3_1),file="gam_sp3_4.doc")


#hacemos un summary para ver el resumen estadístico, al o tener terminos lineales 
#no aparecen relaciones significativas en la primera parte de la tabla
#el termino edf muestra el grado de los coeficientes de las curvas, si edf fuese 2 la 
#curva seria cuadrática y asi sucesivamente


#analizando la R podemos comprobar que esta no es muy elevada incluso cuando la comparamos con
#un modelo lineal, existen diferencias entre las especies, a excepcion del P pinaster el cual no
#muestra diferencias significativas.
lm_sp3 <-lm(AB3_Mgha ~ NDVI_IFN3 + Arid_Dm, data = dt_sp3)
summary(lm_sp3)
par(mfrow = c(1,1))
plot(lm_sp3, residuals= TRUE, pch =1, pages = 1)

lm_sp2 <-lm(AB2_Mgha ~ NDVI_IFN2 + Arid_Dm, data = dt_sp3)
summary(lm_sp3)
par(mfrow = c(1,1))
plot(lm_sp2, all.terms = TRUE, pch =1, pages = 1)

capture.output(summary(lm_sp3),file="lm_AB3_NDVI_Arid.doc")
capture.output(summary(lm_sp2),file="lm_AB2_NDVI_Arid.doc")

#podemos ver el modelo general lineal
glm1<-glm(AB3_Mgha ~ NDVI_IFN3 + Arid_Dm, data = dt_sp3)
summary(glm1)
plot(glm1)

#para exportar los resultados de los modelos
texreg::htmlreg(list(gam_sp3, gam_sp3_1,gam_sp3_2),file='models.doc')


#caso estudiasemos las especies aplicando un GLM/GAM
#para q ilex, el termino Aridez acerca el edf a 1, 
# por lo que se queda como lineal,
gamqilex3<-gam(AB3_Mgha ~ s(NDVI_IFN3) + (Arid_Dm), 
               data = dt_qilex_3, method = "REML")
plot(gamqilex3, seWithMean = TRUE, shift = coef(gamqilex3)[1], 
     shade = T, shade.col = "lightblue",
     all.terms = TRUE, pages = 1)
summary(gamqilex3)

glmqilex3<-glm(AB3_Mgha ~ NDVI_IFN3 + Arid_Dm, 
            data = dt_qilex_3)
summary(lmqilex3)
capture.output(summary(gamqilex3),file="gamqilex3.doc")
capture.output(summary(glmqilex3),file="glmqilex3.doc")

#en suber el termino Aridez mejora la  explicacion de la
#desviacion explicada por lo que inicialmente fue introducido
#en el modelo como parametro no lineal 
#pero debido al gráfico ser raro decidi dejar la relacion lineal 
gamqsuber3<-gam(AB3_Mgha ~ s(NDVI_IFN3) + 
                  (Arid_Dm), data = dt_qsuber_3, method = "REML")
plot(gamqsuber3, seWithMean = T, shift = coef(gamqsuber3)[1],
     shade = T, shade.col = "lightblue",
     all.terms = TRUE, pages = 1)
summary(gamqsuber3)
capture.output(summary(gamqsuber3),file="gamqsuber3.doc")


#pinea
#de nuevo la aridez entra como lineal, la cual no le afecta estadisticamente
gamppinea3<-gam(AB3_Mgha ~ s(NDVI_IFN3) + 
                  (Arid_Dm), data = dt_pinea_3, method = "REML")
plot(gamppinea3, seWithMean = T, shift = coef(gamppinea3)[1],
     shade = T, shade.col = "lightblue", 
     all.terms = TRUE, pages = 1)
summary(gamppinea3)
capture.output(summary(gamppinea3),file="gamppinea3.doc")


lmppinea3<-lm(AB3_Mgha ~ NDVI_IFN3 + 
                Arid_Dm, data = dt_pinea_3)
summary(lmppinea3)
ggplot(dt_pinea_3, aes(NDVI_IFN3, AB3_Mgha)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm)


#en halepensis aunque el edf no es 0, mantenemos la aridez
#como factor lineal
gamphale3<-gam(AB3_Mgha ~ s(NDVI_IFN3) + 
                 (Arid_Dm), data = dt_phale_3, method = "REML")
plot(gamphale3, 
     seWithMean = T, shift = coef(gamphale3)[1],
     shade = T, shade.col = "lightblue",
     all.terms = TRUE, pages = 1)
summary(gamphale3)
capture.output(summary(gamphale3),file="gamphale3.doc")


#pinaster, ambas variables muestran su relacion linear por lo que podriamos
# aplicar modelo lineal pero la varianza explicada no es mejor
gampnaster3<-gam(AB3_Mgha ~ s(NDVI_IFN3) + 
                   (Arid_Dm), data = dt_pnaster_3, method = "REML")
summary(gampnaster3)
capture.output(summary(gampnaster3),file="gampnaster3.doc")

lmpnaster3 <- lm(AB3_Mgha ~ NDVI_IFN3 + Arid_Dm, data = dt_pnaster_3)
summary(lmpnaster3)

plot(gampnaster3,
     seWithMean = T, shift = coef(gampnaster3)[1],
     shade = T, shade.col = "lightblue", 
     all.terms = TRUE, pages = 1)
ggplot(dt_pnaster_3, aes(NDVI_IFN3, AB3_Mgha)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm)


#nigra
gampnigra3<-gam(AB3_Mgha ~ s(NDVI_IFN3) + 
                  (Arid_Dm), data = dt_pnigra_3, method = "REML")
plot(gampnigra3, 
     seWithMean = T, shift = coef(gampnigra3)[1],
     shade = T, shade.col = "lightblue", 
     all.terms = TRUE, pages = 1)
summary(gampnigra3)
capture.output(summary(gampnigra3),file="gampnigra3.doc")

#aqui aunque el nigra tenga edf algo bajos y los gráficos sean lineales
#el modelo lineal no se ajusta dando correlaciones incluso negativas
#no entiendo el porque

ggplot(dt_pnigra_3, aes(NDVI_IFN3, AB3_Mgha)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x))


#graficos en 3d

visreg2d(gampnigra3, x = "NDVI_IFN3", y = "Arid_Dm", 
         scale = "response",
         data = dt_sp3, plot.type = "persp")

visreg2d(gamphale3, x = "NDVI_IFN3", y = "Arid_Dm", 
         scale = "response",
         data = dt_sp3, plot.type = "persp")

visreg2d(gamppinea33, x = "NDVI_IFN3", y = "Arid_Dm", 
         scale = "response",
         data = dt_sp3, plot.type = "persp")

visreg2d(gamqilex3, x = "NDVI_IFN3", y = "Arid_Dm", 
         scale = "response",
         data = dt_sp3, plot.type = "persp")

visreg2d(gamqsuber33, x = "NDVI_IFN3", y = "Arid_Dm", 
         scale = "response",
         data = dt_sp3, plot.type = "persp")


