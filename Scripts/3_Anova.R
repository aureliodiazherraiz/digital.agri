indbio <- read_csv("Indices_Biomasa_2_3.csv")

indbio_2_3<-indbio[,c(4,5,36:39,41:44,46,58:66)]
head(indbio_2_3)

#haciendo un anova entre especies para ver las posibles diferencias entre ellas para eso trabajamos con las especies del INF3
dt_sp3<-indbio_2_3 %>% 
  filter(Sp.x %in% c("Quercus ilex",
                     "Quercus suber",
                     "Pinus pinea",
                     "Pinus halepensis",
                     "Pinus nigra",
                     "Pinus pinaster"))

#aplicamos anova entre especies para el IFN3 respecto al NDVI
anova_spx_NDVI3<-aov(NDVI_IFN3 ~ Sp.x, data=dt_sp3)
summary(anova_spx_NDVI3)
pairwise.t.test(x = dt_sp3$NDVI_IFN3, g = dt_sp3$Sp.x, p.adjust.method = "holm",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

TukeyHSD(anova_spx_NDVI3)
plot(TukeyHSD(anova_spx_NDVI3))
Tukey<-HSD.test(anova_spx_NDVI3, "Sp.x", group=TRUE)
Tukey

par(mfrow = c(1,2))
plot(anova_spx_NDVI3, which = 1:4)

capture.output(summary(anova_spx_NDVI3),file="Anova_NDVI_Especies.doc")
capture.output(Tukey,file="Tukey_sp_NDVI3.doc")

#aplicamos anova entre especies para el IFN3 respecto al Indice de Aridez
aov_spx_Arid_Dm<-aov(Arid_Dm ~ Sp.x, data=dt_sp3)
summary(aov_spx_Arid_Dm)
pairwise.t.test(x = dt_sp3$Arid_Dm, g = dt_sp3$Sp.x, p.adjust.method = "holm",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

TukeyHSD(aov_spx_Arid_Dm)
plot(TukeyHSD(aov_spx_Arid_Dm))
Tukey_Arid<-HSD.test(aov_spx_Arid_Dm, "Sp.x", group=TRUE)
Tukey_Arid

par(mfrow = c(1,2))
plot(anova_spx_NDVI3, which = 1:4)

capture.output(summary(aov_spx_Arid_Dm),file="Anova_Arid_Especies.doc")
capture.output(Tukey_Arid,file="Tukey_sp_Arid.doc")

#de la misma forma podemos verlo con la biomasa forestal AB3_Mgha
anova_spx_AB3<-aov(AB3_Mgha ~ Sp.x, data=dt_sp3)
summary(anova_spx_AB3)
pairwise.t.test(x = dt_sp3$AB3_Mgha, g = dt_sp3$Sp.x, p.adjust.method = "holm",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

TukeyHSD(anova_spx_AB3)
plot(TukeyHSD(anova_spx_AB3))
Tukey<-HSD.test(anova_spx_AB3, "Sp.x", group=TRUE)
Tukey

par(mfrow = c(1,2))
plot(anova_spx_AB3, which = 1:4)

capture.output(summary(anova_spx_AB3),file="Anova_AB3_Especies.doc")
capture.output(Tukey,file="Tukey_sp_AB3.doc")





#verificamos la nomralidad (lillie.test) y homocedasticidad (levenetest) 4 de los grupos no siguen una distribucion normal y no son homocedasticos
by(data = dt_anova_sp2,INDICES = dt_anova_sp2$Sp.x,FUN = function(x){ lillie.test(x$NDVI_2)})
leveneTest(NDVI_IFN2 ~ Sp.x,dt_anova_sp2,center = "median")

#aun asi corremos Kruskal-wallis

kruskal.test(NDVI_IFN2 ~ Sp.x, data = dt_anova_sp2)
pairwise.wilcox.test(x = dt_anova_sp2$NDVI_IFN2, g = dt_anova_sp2$Sp.x, p.adjust.method = "holm" )
