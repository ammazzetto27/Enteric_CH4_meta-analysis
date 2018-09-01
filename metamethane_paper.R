methane <- read.csv("database_CH4.csv")

##for later random effects model
methane$experiment <- as.factor(1:nrow(methane))

##calculate SDs
methane$DMI_grass_sd <- methane$DMI_grass_sem * sqrt(methane$N)
methane$DMI_conc_sd <- methane$DMI_conc_sem * sqrt(methane$N)
methane$CH4_sd <- methane$CH4_sem * sqrt(methane$N)
methane$Ym_sd <- methane$Ym_sem * sqrt(methane$N)
methane$Total_DMI_sd <- methane$Total_DMI_sem * sqrt(methane$N)

##total DMI
methane$DMI_total <- methane$DMI_grass_mean + methane$DMI_conc_mean
methane$CH4_mean_DMI <- methane$CH4_mean/methane$DMI_total
methane$CH4_sd_DMI <- methane$CH4_sd/methane$DMI_total


require(metafor)
require(multcomp)

##calculate effect sizes
methane <- escalc("MN", mi=DMI_grass_mean, sdi=DMI_grass_sd, ni=N, data=methane,
                  var.names=c("DMI_grass_yi", "DMI_grass_vi"))
methane <- escalc("MN", mi=DMI_conc_mean, sdi=DMI_conc_sd, ni=N, data=methane,
                  var.names=c("DMI_conc_yi", "DMI_conc_vi"))
methane <- escalc("MN", mi=CH4_mean, sdi=CH4_sd, ni=N, data=methane,
                  var.names=c("CH4_yi", "CH4_vi"))
methane <- escalc("MN", mi=CH4_mean_DMI, sdi=CH4_sd_DMI, ni=N, data=methane,
                  var.names=c("CH4_DMI_yi", "CH4_DMI_vi"))
methane <- escalc("MN", mi=Ym_mean, sdi=Ym_sd, ni=N, data=methane,
                  var.names=c("Ym_yi", "Ym_vi"))
methane <- escalc("MN", mi=Total_DMI, sdi=Total_DMI_sd, ni=N, data=methane,
                  var.names=c("Total_DMI_yi", "Total_DMI_vi"))

str(methane)

##data now like this
summary(methane)

##metaanalysis
##takes into account Experiments nested in each paper

##CH4 emission
smd.rma.reml<-rma(CH4_yi, CH4_vi, method="REML",data=methane)
summary(smd.rma.reml)

CH4_fit <- rma.mv(yi=CH4_yi, V=CH4_vi, mods=~Stage_IPCC-1,
                 random=~1|Author/experiment,  data=methane)
summary(CH4_fit)
predict(CH4_fit)
robust(CH4_fit, cluster=methane$Author)

methane.linfct <- c("Stage_IPCCOther = 0",
                    "Stage_IPCCDairy = 0")

summary(glht(CH4_fit, linfct=methane.linfct[-5]))

CH4_fit <- rma.mv(yi=CH4_yi, V=CH4_vi, mods=~Climate+Stage_IPCC-1,
                  random=~1|Author/experiment,  data=methane)
summary(CH4_fit)
predict(CH4_fit)
robust(CH4_fit, cluster=methane$Author)

methane.linfct <- c("ClimateTropical + Stage_IPCCDairy = 0",
                    "ClimateSubtropical + Stage_IPCCDairy = 0",
                    "ClimateTropical = 0",
                    "ClimateSubtropical = 0")


summary(glht(CH4_fit, linfct=methane.linfct[-5]))

CH4_fit <- rma.mv(yi=CH4_yi, V=CH4_vi, mods=~Breed+Stage_IPCC-1,
                  random=~1|Author/experiment,  data=methane)
summary(CH4_fit)
predict(CH4_fit)
robust(CH4_fit, cluster=methane$Author)

methane.linfct <- c("BreedBrahman + Stage_IPCCOther = 0",
                    "BreedCharolais + Stage_IPCCOther = 0",
                    "BreedNellore  + Stage_IPCCOther = 0",
                    "BreedHereford + Stage_IPCCOther = 0",
                    "BreedCrossbreed + Stage_IPCCOther = 0",
                    "BreedCrossbreed = 0",
                    "BreedHolstein = 0",
                    "BreedHolstein + Stage_IPCCOther= 0")

summary(glht(CH4_fit, linfct=methane.linfct[-5]))

CH4_fit <- rma.mv(yi=CH4_yi, V=CH4_vi, mods=~Diet_class+Stage_IPCC-1,
                  random=~1|Author/experiment,  data=methane)
summary(CH4_fit)
predict(CH4_fit)
robust(CH4_fit, cluster=methane$Author)

methane.linfct <- c("Diet_classForage = 0",
                    "Diet_classConcentrate = 0",
                    "Diet_classForage + Stage_IPCCDairy = 0",
                    "Diet_classConcentrate + Stage_IPCCDairy= 0")

summary(glht(CH4_fit, linfct=methane.linfct[-5]))

##Ym
smd.rma.reml<-rma(Ym_yi, Ym_vi, method="REML",data=methane)
summary(smd.rma.reml)

Ym_fit <- rma.mv(yi=Ym_yi, V=Ym_vi, mods=~Stage_IPCC-1,
                 random=~1|Author/experiment,  data=methane)
summary(Ym_fit)
predict(Ym_fit)
robust(Ym_fit, cluster=methane$Author)

methane.linfct <- c("Stage_IPCCOther = 0",
                    "Stage_IPCCDairy = 0")

summary(glht(Ym_fit, linfct=methane.linfct[-5]))

Ym_fit <- rma.mv(yi=Ym_yi, V=Ym_vi, mods=~Climate+Stage_IPCC-1,
                  random=~1|Author/experiment,  data=methane)
summary(Ym_fit)
predict(Ym_fit)
robust(Ym_fit, cluster=methane$Author)

methane.linfct <- c("ClimateTropical + Stage_IPCCDairy = 0",
                    "ClimateSubtropical + Stage_IPCCDairy = 0",
                    "ClimateTropical = 0",
                    "ClimateSubtropical = 0")

summary(glht(Ym_fit, linfct=methane.linfct[-5]))

Ym_fit <- rma.mv(yi=Ym_yi, V=Ym_vi, mods=~Breed+Stage_IPCC-1,
                  random=~1|Author/experiment,  data=methane)
summary(Ym_fit)
predict(Ym_fit)
robust(Ym_fit, cluster=methane$Author)

methane.linfct <- c("BreedBrahman + Stage_IPCCOther = 0",
                    "BreedCharolais + Stage_IPCCOther = 0",
                    "BreedNellore  + Stage_IPCCOther = 0",
                    "BreedHereford + Stage_IPCCOther = 0",
                    "BreedCrossbreed + Stage_IPCCOther = 0",
                    "BreedCrossbreed = 0",
                    "BreedHolstein = 0",
                    "BreedHolstein + Stage_IPCCOther= 0")

summary(glht(Ym_fit, linfct=methane.linfct[-5]))

Ym_fit <- rma.mv(yi=Ym_yi, V=Ym_vi, mods=~Diet_class+Stage_IPCC-1,
                  random=~1|Author/experiment,  data=methane)
summary(Ym_fit)
predict(Ym_fit)
robust(Ym_fit, cluster=methane$Author)

methane.linfct <- c("Diet_classForage = 0",
                    "Diet_classConcentrate = 0",
                    "Diet_classForage + Stage_IPCCDairy = 0",
                    "Diet_classConcentrate + Stage_IPCCDairy= 0")

summary(glht(Ym_fit, linfct=methane.linfct[-5]))

##CH4/DMI
smd.rma.reml<-rma(CH4_DMI_yi, CH4_DMI_vi, method="REML",data=methane)
summary(smd.rma.reml)

CH4_DMI_fit <- rma.mv(yi=CH4_DMI_yi, V=CH4_DMI_vi, mods=~Stage_IPCC-1,
                 random=~1|Author/experiment,  data=methane)
summary(CH4_DMI_fit)
predict(CH4_DMI_fit)
robust(CH4_DMI_fit, cluster=methane$Author)

methane.linfct <- c("Stage_IPCCOther = 0",
                    "Stage_IPCCDairy = 0")

summary(glht(CH4_DMI_fit, linfct=methane.linfct[-5]))

CH4_DMI_fit <- rma.mv(yi=CH4_DMI_yi, V=CH4_DMI_vi, mods=~Climate+Stage_IPCC-1,
                 random=~1|Author/experiment,  data=methane)
summary(CH4_DMI_fit)
predict(CH4_DMI_fit)
robust(CH4_DMI_fit, cluster=methane$Author)

methane.linfct <- c("ClimateTropical + Stage_IPCCDairy = 0",
                    "ClimateSubtropical + Stage_IPCCDairy = 0",
                    "ClimateTropical = 0",
                    "ClimateSubtropical = 0")

summary(glht(CH4_DMI_fit, linfct=methane.linfct[-5]))

CH4_DMI_fit <- rma.mv(yi=CH4_DMI_yi, V=CH4_DMI_vi, mods=~Breed+Stage_IPCC-1,
                 random=~1|Author/experiment,  data=methane)
summary(CH4_DMI_fit)
predict(CH4_DMI_fit)
robust(CH4_DMI_fit, cluster=methane$Author)

methane.linfct <- c("BreedBrahman + Stage_IPCCOther = 0",
                    "BreedCharolais + Stage_IPCCOther = 0",
                    "BreedNellore  + Stage_IPCCOther = 0",
                    "BreedHereford + Stage_IPCCOther = 0",
                    "BreedCrossbreed + Stage_IPCCOther = 0",
                    "BreedCrossbreed = 0",
                    "BreedHolstein = 0",
                    "BreedHolstein + Stage_IPCCOther= 0")

summary(glht(CH4_DMI_fit, linfct=methane.linfct[-5]))

CH4_DMI_fit <- rma.mv(yi=CH4_DMI_yi, V=CH4_DMI_vi, mods=~Diet_class+Stage_IPCC-1,
                      random=~1|Author/experiment,  data=methane)
summary(CH4_DMI_fit)
predict(CH4_DMI_fit)
robust(CH4_DMI_fit, cluster=methane$Author)

methane.linfct <- c("Diet_classForage = 0",
                    "Diet_classConcentrate = 0",
                    "Diet_classForage + Stage_IPCCDairy = 0",
                    "Diet_classConcentrate + Stage_IPCCDairy= 0")

summary(glht(CH4_DMI_fit, linfct=methane.linfct[-5]))