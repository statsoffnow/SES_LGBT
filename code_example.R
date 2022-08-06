#WD
setwd("~/Documents/Samoocena LGBT")

library(readxl)
baza = read_excel("baza_samoocena.xlsx")





options(scipen = 999)




# 518 przypadków: "baza.zdrowie"
baza.zdrowie = subset(baza, Art_zdr == 1)


#### płeć: 0-cis, 1 & 2 - trans ####
summary(as.factor(baza.zdrowie$bin_nonbin))

baza.zdrowie$plec[baza.zdrowie$bin_nonbin == 0] <- 0
baza.zdrowie$plec[baza.zdrowie$bin_nonbin == 1] <- 1
baza.zdrowie$plec[baza.zdrowie$bin_nonbin == 2] <- 1

summary(as.factor(baza.zdrowie$plec))
#420-cis
#98-trans

#### plec metrykalna to factor ####
baza.zdrowie$Plec_metrykalna = factor(baza.zdrowie$Plec_metrykalna,
                                      levels = c(1,2,3),
                                      labels = c("M", "F", NA))

#### cisM, cisF, T ####
baza.zdrowie$plec[baza.zdrowie$plec == 0 & baza.zdrowie$Plec_metrykalna == "M"] <- "M"
baza.zdrowie$plec[baza.zdrowie$plec == 0 & baza.zdrowie$Plec_metrykalna == "F"] <- "F"
baza.zdrowie$plec[baza.zdrowie$plec == 1] <- "T"

baza.zdrowie$plec = factor(baza.zdrowie$plec,
                           levels = c("M","F","T"),
                           labels = c("M","F","T"))
summary(baza.zdrowie$plec)

##F-245   M-175   T-98

#### wiek - centracja ####
summary(baza.zdrowie$wiek)

baza.zdrowie$wiek_c = (baza.zdrowie$wiek)-26.92
summary(baza.zdrowie$wiek_c)

#### DHEQ średnia ####

baza.zdrowie$DHEQsr = (baza.zdrowie$DHEQ1 + baza.zdrowie$DHEQ2 + baza.zdrowie$DHEQ3 +
                         baza.zdrowie$DHEQ4 + baza.zdrowie$DHEQ5 + baza.zdrowie$DHEQ6 +
                         baza.zdrowie$DHEQ7 + baza.zdrowie$DHEQ8 + baza.zdrowie$DHEQ9 +
                         baza.zdrowie$DHEQ10 + baza.zdrowie$DHEQ11 + baza.zdrowie$DHEQ12 +
                         baza.zdrowie$DHEQ13 + baza.zdrowie$DHEQ14 + baza.zdrowie$DHEQ15 +
                         baza.zdrowie$DHEQ16 + baza.zdrowie$DHEQ17 + baza.zdrowie$DHEQ18 +
                         baza.zdrowie$DHEQ19 + baza.zdrowie$DHEQ20 + baza.zdrowie$DHEQ21 +
                         baza.zdrowie$DHEQ22 + baza.zdrowie$DHEQ23 + baza.zdrowie$DHEQ24 +
                         baza.zdrowie$DHEQ25 + baza.zdrowie$DHEQ26 + baza.zdrowie$DHEQ27 +
                         baza.zdrowie$DHEQ28 + baza.zdrowie$DHEQ29 + baza.zdrowie$DHEQ30 +
                         baza.zdrowie$DHEQ31 + baza.zdrowie$DHEQ32)/32

summary(baza.zdrowie$DHEQsr)

##### anova z kontrolą wieku #####


library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(dplyr)
library(emmeans)

baza.zdrowie$Gender.name[baza.zdrowie$Gender == "SDCM"] <- "SDCM"
baza.zdrowie$Gender.name[baza.zdrowie$Gender == "SDCW"] <- "SDCW"
baza.zdrowie$Gender.name[baza.zdrowie$Gender == "TGD"] <- "TG&GDP"

baza.zdrowie$Gender.name = as.factor(baza.zdrowie$Gender.name)
baza.zdrowie$Gender.name

#### ses ####
baza.zdrowie %>%
  anova_test(SESsum ~ Gender.name*wiek_c)

pwc.ses = baza.zdrowie %>% 
  emmeans_test(SESsum ~ Gender.name, covariate = wiek_c,
               p.adjust.method = "bonferroni")

pwc.ses
get_emmeans(pwc.ses)

pwc.ses = pwc.ses %>% add_xy_position(x="Gender.name", fun="mean_se")

ems.rses = ggline(get_emmeans(pwc.ses), x = "Gender.name", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc.ses, hide.ns = TRUE, tip.length = FALSE)+
  ylab(label = "Self-esteem")+
  xlab(label = "")+
  scale_x_discrete(label=c("SDCM", "SDCW", "TG&GDP"))

ems.rses

#### DHEQ ####

baza.zdrowie %>%
  anova_test(DHEQsuma ~ Gender.name*wiek_c)

emmeans_test(data = baza.zdrowie,
             DHEQsr ~ Gender.name, covariate = wiek_c,
             p.adjust.method = "bonferroni")

pwc.dheq = baza.zdrowie %>%
  emmeans_test(DHEQsuma ~ Gender.name, covariate = wiek_c,
               p.adjust.method = "bonferroni")
pwc.dheq
get_emmeans(pwc.dheq)

pwc.dheq = pwc.dheq %>% add_xy_position(x="Gender.name", fun="mean_se")

ems.dheq = ggline(get_emmeans(pwc.dheq), x = "Gender.name", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc.dheq, hide.ns = TRUE, tip.length = FALSE)+
  ylab(label = "Stigma exposure")+
  xlab(label = "")+
  scale_x_discrete(label=c("SDCM", "SDCW", "TG&GDP"))

ems.dheq

#### SPP ####
baza.zdrowie %>%
  anova_test(SPPsr ~ Gender.name*wiek_c)

pwc.spp = baza.zdrowie %>%
  emmeans_test(SPPsr ~ Gender.name, covariate = wiek_c,
               p.adjust.method = "bonferroni")
pwc.spp
get_emmeans(pwc.spp)

pwc.spp = pwc.spp %>% add_xy_position(x="Gender.name", fun="mean_se")

ems.spp = ggline(get_emmeans(pwc.spp), x = "Gender.name", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc.spp, hide.ns = TRUE, tip.length = FALSE)+
  ylab(label = "Resilience")+
  xlab(label = "")+
  scale_x_discrete(label=c("SDCM", "SDCW", "TG&GDP"))
ems.spp

#### CESD ####
baza.zdrowie %>%
  anova_test(CESDsum ~ Gender.name*wiek_c)

pwc.cesd = baza.zdrowie %>%
  emmeans_test(CESDsum ~ Gender.name, covariate = wiek_c,
               p.adjust.method = "bonferroni")
pwc.cesd
get_emmeans(pwc.cesd)

pwc.cesd = pwc.cesd %>% add_xy_position(x="Gender.name", fun="mean_se")

ems.cesd = ggline(get_emmeans(pwc.cesd), x = "Gender.name", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc.cesd, hide.ns = TRUE, tip.length = FALSE)+
  ylab(label = "Depression")+
  xlab(label = "")+
  scale_x_discrete(label=c("SDCM", "SDCW", "TG&GDP"))

ems.cesd

#### EMS PLOTS ####
ggarrange(ems.rses, ems.spp, ems.dheq, ems.cesd)



#### SES ~ wiek + spp + dheq + tpim + cesd ####

#spp
summary(baza.zdrowie$SPPsr)
baza.zdrowie$SPP_c = (baza.zdrowie$SPPsr)-3.506

#dheq
summary(baza.zdrowie$DHEQsuma)
baza.zdrowie$DHEQ_c = (baza.zdrowie$DHEQsuma)-76.83

#tpim
summary(baza.zdrowie$TPIMsuma)
baza.zdrowie$TPIM_c = (baza.zdrowie$TPIMsuma)-122.7

#cesd
summary(baza.zdrowie$CESDsum)
baza.zdrowie$CESD_c = (baza.zdrowie$CESDsum)-47.49



summary(aov(data = baza.zdrowie,
    SESsum ~ wiek_c + plec + SPP_c + DHEQ_c + TPIM_c + CESD_c))

##MLM null model
null.mod = lmer(data = baza.zdrowie,
                SESsum ~ 1 + (1|plec),
                REML = FALSE)
summary(null.mod)

#ICC=0.92
#ICC nie przekracza 10% - nie wymaga MLM


#### women: SES ~ wiek + spp + dheq + tpim + cesd ####
model.fem = subset(baza.zdrowie, plec == "F")

summary(lm(data = model.fem,
            SESsum ~ wiek_c + SPP_c + DHEQ_c + TPIM_c + CESD_c))


#### men: SES ~ wiek + spp + dheq + tpim + cesd ####
model.hom = subset(baza.zdrowie, plec == "M")

summary(lm(data = model.hom,
            SESsum ~ wiek_c + SPP_c + DHEQ_c + TPIM_c + CESD_c))


#### trans: SES ~ wiek + spp + dheq + tpim + cesd ####
model.trans = subset(baza.zdrowie, plec == "T")

summary(lm(data = model.trans,
            SESsum ~ wiek_c + SPP_c + DHEQ_c + TPIM_c + CESD_c))


########## 20.01.2021 - do artykułu #############

#SPP, DHEQa, CESD-R, SES 

#porównanie: 

#edukacji (podstawowe + zawodowe; średnie, niepełne wyższe i wyżej), 

#1 - podstawowe
#2 - zawodowe
#3 - średnie
#4 - niepełne wyższe
#5 - wyższe

as.factor(baza.zdrowie$Edu)

baza.zdrowie$edukacja[baza.zdrowie$Edu == 1] <- 0
baza.zdrowie$edukacja[baza.zdrowie$Edu == 2] <- 0
baza.zdrowie$edukacja[baza.zdrowie$Edu == 3] <- 1
baza.zdrowie$edukacja[baza.zdrowie$Edu == 4] <- 2
baza.zdrowie$edukacja[baza.zdrowie$Edu == 5] <- 2

summary(as.factor(baza.zdrowie$edukacja))


baza.zdrowie$edukacja = factor(baza.zdrowie$edukacja,
                               levels = c(0,1,2),
                               labels = c("podstawowe", "średnie", "wyższe"))


#wielkość miejscowości zamieszkania 
#do 10.000; 
#10.000 do 500.000 
#powyżej 500.000 mieszkańców

summary(as.factor(baza.zdrowie$Miejsce1))

#1 - wieś
#2 - do 10 tys
#3 - 10-100 tys
#4 - 100 - 500 tys
#5 - 500 - 1 mln.
#6 - > 1 mln.

baza.zdrowie$pochodzenie[baza.zdrowie$Miejsce1 == 1] <- 0
baza.zdrowie$pochodzenie[baza.zdrowie$Miejsce1 == 2] <- 0
baza.zdrowie$pochodzenie[baza.zdrowie$Miejsce1 == 3] <- 1
baza.zdrowie$pochodzenie[baza.zdrowie$Miejsce1 == 4] <- 1
baza.zdrowie$pochodzenie[baza.zdrowie$Miejsce1 == 5] <- 2
baza.zdrowie$pochodzenie[baza.zdrowie$Miejsce1 == 6] <- 2

baza.zdrowie$pochodzenie = factor(baza.zdrowie$pochodzenie,
                                  levels = c(0,1,2),
                                  labels = c("do 10 tys","10-500tys",
                                  "powyżej 500 tys"))


#wielkość miejscowości pochodzenia 
#do 10.000; 
#10.000 do 500.000 
#powyżej 500.000 mieszkańców

summary(as.factor(baza.zdrowie$Miejsce2))

baza.zdrowie$zamieszkanie[baza.zdrowie$Miejsce2 == 1] <- 0
baza.zdrowie$zamieszkanie[baza.zdrowie$Miejsce2 == 2] <- 0
baza.zdrowie$zamieszkanie[baza.zdrowie$Miejsce2 == 3] <- 1
baza.zdrowie$zamieszkanie[baza.zdrowie$Miejsce2 == 4] <- 1
baza.zdrowie$zamieszkanie[baza.zdrowie$Miejsce2 == 5] <- 2
baza.zdrowie$zamieszkanie[baza.zdrowie$Miejsce2 == 6] <- 2

baza.zdrowie$zamieszkanie = factor(baza.zdrowie$zamieszkanie,
                                  levels = c(0,1,2),
                                  labels = c("do 10 tys","10-500tys",
                                             "powyżej 500 tys"))



#dochód (wystarcza - nie wystarcza)
#tak, z łatwością
#tak, pewną z trudnością
#tak, z dużą trudnością
#nie
#nie chcę odpowiadać na to pytanie

summary(as.factor(baza.zdrowie$Dochód))

baza.zdrowie$income[baza.zdrowie$Dochód == 1] <- 1
baza.zdrowie$income[baza.zdrowie$Dochód == 2] <- 1
baza.zdrowie$income[baza.zdrowie$Dochód == 3] <- 1
baza.zdrowie$income[baza.zdrowie$Dochód == 4] <- 0
baza.zdrowie$income[baza.zdrowie$Dochód == 5] <- NA

summary(as.factor(baza.zdrowie$income))

baza.zdrowie$income = factor(baza.zdrowie$income,
                             levels = c(0,1),
                             labels = c("nie starcza", "wystarcza"))


summary(baza.zdrowie$income)



#### podsumowanie modeli liniowych: pakiet jtools ####
library(jtools)

fit = lm(data = baza.zdrowie,
         CESDsum ~ plec + wiek_c)

summ(fit)


#### wykresy sjPlot dla modelu liniowego z interakcją ####
##wykres dla modelu z interakcją
library(sjPlot)
plot_model(fit)
plot_model(fit, type = "pred")


#### 23.01.2021 - DV: SES, plec x edukacja ####
ses1 = lm(data = baza.zdrowie,
          SESsum ~ wiek_c + plec*edukacja)
summ(ses1)

plot_model(ses1, type = "int", terms = c("plec", "edukacja"))


#### 23.01.2021 - DV: SES, plec x pochodzenie ####
ses2 = lm(data = baza.zdrowie,
          SESsum ~ wiek_c + plec*pochodzenie)
summ(ses2)

plot_model(ses2, type = "int", terms = c("plec", "pochodzenie"))

#### 23.01.2021 - DV: SES, plec x zamieszkanie ####
ses3 = lm(data = baza.zdrowie,
          SESsum ~ wiek_c + plec*zamieszkanie)
summ(ses3, digits = 3)

plot_model(ses3, type = "int", 
           terms = c("plec", "zamieszkanie"))


#### 23.01.2021 - DV: SES, plec x income ####
ses4 = lm(data = baza.zdrowie,
          SESsum ~ wiek_c + plec*income)
summ(ses4, digits = 3)

plot_model(ses4, type = "int", 
           terms = c("plec", "income"))



#### 23.01.2021 - DV: SPP, plec x edukacja ####
spp1 = lm(data = baza.zdrowie,
          SPPsr ~ wiek_c + plec*edukacja)
summ(spp1, digits = 3)

plot_model(spp1, type = "int", 
           terms = c("plec", "edukacja"))


#### 23.01.2021 - DV: SPP, plec x pochodzenie ####
spp2 = lm(data = baza.zdrowie,
          SPPsr ~ wiek_c + plec*pochodzenie)
summ(spp2, digits = 3)

plot_model(spp2, type = "int", 
           terms = c("plec", "pochodzenie"))


#### 23.01.2021 - DV: SPP, plec x zamieszkanie ####
spp3 = lm(data = baza.zdrowie,
          SPPsr ~ wiek_c + plec*zamieszkanie)
summ(spp3, digits = 3)

plot_model(spp3, type = "int", 
           terms = c("plec", "zamieszkanie"))


#### 23.01.2021 - DV: SPP, plec x income ####
spp4 = lm(data = baza.zdrowie,
          SPPsr ~ wiek_c + plec*income)
summ(spp4, digits = 3)

plot_model(spp4, type = "int", 
           terms = c("plec", "income"))


#### 23.01.2021 - DV: DHEQ, plec x edukacja ####
dheq1 = lm(data = baza.zdrowie,
          DHEQsr ~ wiek_c + plec*edukacja)
summ(dheq1, digits = 3)

plot_model(dheq1, type = "int", 
           terms = c("plec", "edukacja"))


#### 23.01.2021 - DV: DHEQ, plec x pochodzenie ####
dheq2 = lm(data = baza.zdrowie,
          DHEQsuma ~ wiek_c + plec*pochodzenie)
summ(dheq2, digits = 3)

plot_model(dheq2, type = "int", 
           terms = c("plec", "pochodzenie"))


#### 23.01.2021 - DV: DHEQ, plec x zamieszkanie ####
dheq3 = lm(data = baza.zdrowie,
          DHEQsuma ~ wiek_c + plec*zamieszkanie)
summ(dheq3, digits = 3)

plot_model(dheq3, type = "int", 
           terms = c("plec", "zamieszkanie"))


#### 23.01.2021 - DV: DHEQ, plec x income ####
dheq4 = lm(data = baza.zdrowie,
          DHEQsuma ~ wiek_c + plec*income)
summ(dheq4, digits = 3)

plot_model(dheq4, type = "int", 
           terms = c("plec", "income"))






#### 23.01.2021 - DV: CESD, plec x edukacja ####
cesd1 = lm(data = baza.zdrowie,
           CESDsum ~ wiek_c + plec*edukacja)
summ(cesd1, digits = 3)

plot_model(cesd1, type = "int", 
           terms = c("plec", "edukacja"))


#### 23.01.2021 - DV: CESD, plec x pochodzenie ####
cesd2 = lm(data = baza.zdrowie,
           CESDsum ~ wiek_c + plec*pochodzenie)
summ(cesd2, digits = 3)

plot_model(cesd2, type = "int", 
           terms = c("plec", "pochodzenie"))


#### 23.01.2021 - DV: CESD, plec x zamieszkanie ####
cesd3 = lm(data = baza.zdrowie,
           CESDsum ~ wiek_c + plec*zamieszkanie)
summ(cesd3, digits = 3)

plot_model(cesd3, type = "int", 
           terms = c("plec", "zamieszkanie"))


#### 23.01.2021 - DV: DHEQ, plec x income ####
cesd4 = lm(data = baza.zdrowie,
           CESDsum ~ wiek_c + plec*income)
summ(cesd4, digits = 3)

plot_model(cesd4, type = "int", 
           terms = c("plec", "income"))



######## dochód surowy ##########

baza.zdrowie$dochod = as.factor(baza.zdrowie$Dochód)

dochod1 = lm(data = baza.zdrowie,
           CESDsum ~ wiek_c + plec*dochod)
summ(dochod1, digits = 3)

plot_model(dochod1)
plot_model(dochod1, type = "pred")
plot_model(dochod1, type = "int", 
           terms = c("plec", "dochod"))


dochod2 = lm(data = baza.zdrowie,
             SPPsr ~ wiek_c + plec*dochod)
summ(dochod2, digits = 3)

plot_model(dochod2)
plot_model(dochod2, type = "pred")
plot_model(dochod2, type = "int", 
           terms = c("plec", "dochod"))


dochod3 = lm(data = baza.zdrowie,
             SESsum ~ wiek_c + plec*dochod)
summ(dochod3, digits = 3)

plot_model(dochod3, sort.est = "sort.all")
plot_model(dochod3, type = "pred")
plot_model(dochod3, type = "int", 
           terms = c("plec", "dochod"))



dochod4 = lm(data = baza.zdrowie,
             DHEQsuma ~ wiek_c + plec*dochod)
summ(dochod4, digits = 3)

plot_model(dochod4, sort.est = "sort.all")
plot_model(dochod4, type = "pred")
plot_model(dochod4, type = "int", 
           terms = c("plec", "dochod"))





###### korelacje by group #######
#ses x dheq
ggplot(data = baza.zdrowie, aes(x=SESsum, y=DHEQsuma, col=plec, shape=plec))+
  geom_point()+
  geom_smooth(method = "lm")

#ses x cesd
ggplot(data = baza.zdrowie, aes(x=SESsum, y=CESDsum, col=plec, shape=plec))+
  geom_point()+
  geom_smooth(method = "lm")

#ses x tpim
ggplot(data = baza.zdrowie, aes(x=SESsum, y=TPIMsuma, col=plec, shape=plec))+
  geom_point()+
  geom_smooth(method = "lm")

#ses x spp
ggplot(data = baza.zdrowie, aes(x=SESsum, y=SPPsr, col=plec, shape=plec))+
  geom_point()+
  geom_smooth(method = "lm")


#tpim x cesd
ggplot(data = baza.zdrowie, aes(x=TPIMsuma, y=CESDsum, col=plec, shape=plec))+
  geom_point()+
  geom_smooth(method = "lm")


#cesd x dheq
ggplot(data = baza.zdrowie, aes(x=DHEQsuma, y=CESDsum, col=plec, shape=plec))+
  geom_point()+
  geom_smooth(method = "lm")


#tpim x dheq
ggplot(data = baza.zdrowie, aes(x=TPIMsuma, y=DHEQsuma, col=plec, shape=plec))+
  geom_point()+
  geom_smooth(method = "lm")


##### 25.01.2021 - samoocena metody ####

library(psych)

library(car)
library(effects)
library(emmeans)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS2)

##SES
model = aov(data = baza.zdrowie,
            SESsum ~ plec + wiek_c)
Anova(model, type = "II")

posth = glht(model, plec='Tukey')
summary(posth)

TukeyHSD(model)


emmeans(model, ~ plec:wiek_c)


lm(data = baza.zdrowie,
   SESsum ~ plec + wiek_c)
TukeyHSD(lm(data = baza.zdrowie,
            SESsum ~ plec + wiek_c))

ses.aov = lm(baza.zdrowie$SESsum ~ baza.zdrowie$plec + baza.zdrowie$wiek_c)
anova = aov(ses.aov)
summary(ses.aov)
tukey = TukeyHSD(x=anova, 'baza.zdrowie$plec')
tukey
plot(tukey, las=1, col="red")



TUKEY <- TukeyHSD(x=ANOVA, 'data$treatment', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")

##SPP
describe(baza.zdrowie$SPPsr)

model.spp = aov(data = baza.zdrowie,
            SPPsr ~ plec + wiek_c)
Anova(model.spp, type = "II")
emmeans(model.spp, ~ plec:wiek_c)

spp.aov = lm(baza.zdrowie$SPPsr ~ baza.zdrowie$plec + baza.zdrowie$wiek_c)
anova = aov(spp.aov)
summary(spp.aov)
tukey = TukeyHSD(x=anova, 'baza.zdrowie$plec')
tukey


##DHEQ
describe(baza.zdrowie$DHEQsr)

model.dheq = aov(data = baza.zdrowie,
                DHEQsr ~ Gender + wiek_c)
Anova(model.dheq, type = "II")
emmeans(model.dheq, ~ Gender:wiek_c)

dheq.aov = lm(baza.zdrowie$DHEQsr ~ baza.zdrowie$Gender + baza.zdrowie$wiek_c)
anova = aov(dheq.aov)
summary(dheq.aov)
tukey = TukeyHSD(x=anova, 'baza.zdrowie$Gender')
tukey


##CESD
describe(baza.zdrowie$CESDsum)

model.cesd = aov(data = baza.zdrowie,
                 CESDsum ~ plec + wiek_c)
Anova(model.cesd, type = "II")
emmeans(model.cesd, ~ plec:wiek_c)

cesd.aov = lm(baza.zdrowie$CESDsum ~ baza.zdrowie$plec + baza.zdrowie$wiek_c)
anova = aov(cesd.aov)
summary(cesd.aov)
tukey = TukeyHSD(x=anova, 'baza.zdrowie$plec')
tukey






############ model z efektem wieku w grupach (05.02.2021) ########
clean = theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(color = "black"))


baza.zdrowie$Gender = factor(baza.zdrowie$plec,
                           levels = c("M","F","T"),
                           labels = c("SDCM","SDCW","TGD"))

baza.zdrowie$age = baza.zdrowie$wiek_c


spp.int = lm(data = baza.zdrowie,
             SPPsr ~ Gender.name*age)
summ(spp.int, digits = 3, confint = TRUE)

plot.spp = plot_model(spp.int, type = "pred", 
           terms = c("age","Gender.name"),
           show.legend = FALSE,
           title = "")+
  ylab("Resilience")+
  clean


ses.int = lm(data = baza.zdrowie,
             SESsum ~ Gender.name*age)
summ(ses.int, digits = 3, confint = TRUE)


plot.ses = plot_model(ses.int, type = "pred", 
           terms = c("age","Gender.name"),
           show.legend = TRUE,
           title = "",
           legend.title = "")+
  ylab("Self-esteem")+
  clean


dheq.int = lm(data = baza.zdrowie,
              DHEQsr ~ Gender.name*age)
summ(dheq.int, digits = 3, confint = TRUE)

plot.dheq = plot_model(dheq.int, type = "pred", 
           terms = c("age","Gender.name"),
           show.legend = FALSE,
           title = "")+
  ylab("Stigma exposure")+
  clean
plot.dheq


cesd.int = lm(data = baza.zdrowie,
              CESDsum ~ Gender.name*age)
summ(cesd.int, confint = TRUE)

plot.cesd = plot_model(cesd.int, type = "pred", 
           terms = c("age", "Gender.name"),
           show.legend = FALSE,
           title = "")+
  ylab("Depression")+
  clean

#### 4 plots in 1 fig ####

ggarrange(plot.ses, plot.spp,
          legend = "left")


ggarrange(plot.dheq, plot.cesd, 
          legend = "top")


ggarrange(plot.ses, plot.spp, plot.dheq, plot.cesd,
          legend = "top")


#### logistic regression - demographic ####
#gender assigned
levels(baza.zdrowie$Plec_metrykalna)

log1 = glm(data = baza.zdrowie,
    Plec_metrykalna ~ Gender, family = "binomial")

summary(log1)
confint(log1) #log-likelihood 95%CI
exp(coef(log1)) #OR
exp(cbind(OR=coef(log1), confint(log1)))

#place of residence
levels(baza.zdrowie$zamieszkanie)

baza.zdrowie$residence[baza.zdrowie$zamieszkanie == "do 10 tys"] <- 0
baza.zdrowie$residence[baza.zdrowie$zamieszkanie == "10-500tys"] <- 0
baza.zdrowie$residence[baza.zdrowie$zamieszkanie == "powyżej 500 tys"] <- 1

baza.zdrowie$residence = factor(baza.zdrowie$residence,
                                levels = c(0,1),
                                labels = c("poniżej 500", "powyżej 500"))


summary(baza.zdrowie$zamieszkanie)
summary(baza.zdrowie$residence)

table(baza.zdrowie$Gender, baza.zdrowie$residence)



log2 = glm(data = baza.zdrowie,
           residence ~ Gender, family = "binomial")

summary(log2)
confint(log2) #log-likelihood 95%CI
exp(coef(log2)) #OR
exp(cbind(OR=coef(log2), confint(log2))) #OR 95%CI

#income

#tak, z łatwością
#tak, pewną z trudnością
#tak, z dużą trudnością
#nie
#nie chcę odpowiadać na to pytanie

#wage_wystarcza
baza.zdrowie$wage[baza.zdrowie$Dochód == 1] <- 1
baza.zdrowie$wage[baza.zdrowie$Dochód == 2] <- 1
baza.zdrowie$wage[baza.zdrowie$Dochód == 3] <- 1
baza.zdrowie$wage[baza.zdrowie$Dochód == 4] <- 0
baza.zdrowie$wage[baza.zdrowie$Dochód == 5] <- 2


baza.zdrowie$wage = factor(baza.zdrowie$wage,
                           levels = c(0,1,2),
                           labels = c("nie starczy", "wystarczy", "odmowa"))
levels(baza.zdrowie$wage)

summary(baza.zdrowie$wage)

table(baza.zdrowie$Gender, baza.zdrowie$wage)


####### log-reg kto ma większą szansę być trans #########

baza.zdrowie$Plec_metrykalna
baza.zdrowie$plec_fem = if_else(baza.zdrowie$Plec_metrykalna == "F", 1, 0)
baza.zdrowie$plec_fem = as.factor(baza.zdrowie$plec_fem)

levels(baza.zdrowie$Gender)
baza.zdrowie$identity = if_else(baza.zdrowie$Gender == "TGD", 1, 0)
baza.zdrowie$identity = as.factor(baza.zdrowie$identity)


log1 = glm(data = baza.zdrowie,
           identity ~ plec_fem, family = "binomial")
summary(log1)
confint(log1) #log-likelihood 95%CI
exp(coef(log1)) #OR
exp(cbind(OR=coef(log1), confint(log1)))
  


######3 która płeć mieszka w dużym mieście #######

baza.zdrowie$miasto = if_else(baza.zdrowie$residence == "powyżej 500", 1, 0)
baza.zdrowie$miasto

baza.zdrowie$miasto = as.factor(baza.zdrowie$miasto)

log2 = glm(data = baza.zdrowie,
           miasto ~ Gender, family = "binomial")
summary(log2)
confint(log2) #log-likelihood 95%CI
exp(coef(log2)) #OR
exp(cbind(OR=coef(log2), confint(log2)))


##### log-reg: dochód wystarczy ~ gender #########

baza.zdrowie$wage

baza.zdrowie$wystarczy_tak = if_else(baza.zdrowie$wage == "wystarczy", 1, 0)
baza.zdrowie$wystarczy_tak = as.factor(baza.zdrowie$wystarczy_tak)

summary(baza.zdrowie$wage)
summary(baza.zdrowie$wystarczy_tak)


log3 = glm(data = baza.zdrowie,
           wystarczy_tak ~ Gender, family = "binomial")
summary(log3)
confint(log3) #log-likelihood 95%CI
exp(coef(log3)) #OR
exp(cbind(OR=coef(log3), confint(log3)))




######### odmowa odpowiedzi ~ gender  ########

baza.zdrowie$odmowa_tak = if_else(baza.zdrowie$wage == "odmowa", 1, 0)

baza.zdrowie$odmowa_tak = as.factor(baza.zdrowie$odmowa_tak)
baza.zdrowie$odmowa_tak

log4 = glm(data = baza.zdrowie,
           odmowa_tak ~ Gender, family = "binomial")
summary(log4)
confint(log4) #log-likelihood 95%CI
exp(coef(log4)) #OR
exp(cbind(OR=coef(log4), confint(log4)))


######### 4.04.2021 - OR DLA PŁCI ##########

####zamieszkanie 500k ####
baza.zdrowie$zamieszkanie01[baza.zdrowie$zamieszkanie == "do 10 tys"] <- 0
baza.zdrowie$zamieszkanie01[baza.zdrowie$zamieszkanie == "10-500tys"] <- 0
baza.zdrowie$zamieszkanie01[baza.zdrowie$zamieszkanie == "powyżej 500 tys"] <- 1

baza.zdrowie$zamieszkanie01 = factor(baza.zdrowie$zamieszkanie01,
                                     levels = c(0,1),
                                     labels = c("<500k", ">500k"))

table(baza.zdrowie$zamieszkanie01)
prop.table(table(baza.zdrowie$zamieszkanie01))

#### OR dla gender identity ####

baza.zdrowie$Gender
baza.zdrowie$Plec_metrykalna


baza.zdrowie$assigned[baza.zdrowie$Plec_metrykalna == "M"] <- 0
baza.zdrowie$assigned[baza.zdrowie$Plec_metrykalna == "F"] <- 1

baza.zdrowie$assigned = factor(baza.zdrowie$assigned,
                               levels = c(0,1),
                               labels = c("0", "1"))

or1 = glm(data = baza.zdrowie, family = "binomial",
          assigned ~ Gender)
summary(or1)
exp(or1$coefficients)



#### OR dla zamieszkania ####
baza.zdrowie$zamieszkanie01

or2 = glm(data = baza.zdrowie, family = "binomial",
          zamieszkanie01 ~ Gender)
summary(or2)
exp(or2$coefficients)


#### OR dla income ####
baza.zdrowie$income

or3 = glm(data = baza.zdrowie, family = "binomial",
           income ~ Gender)
summary(or3)
exp(or3$coefficients)


#### OR dla 
baza.zdrowie$edukacja

######### 5.04.2021 - DEMOGRAPHICS ########
describeBy(baza.zdrowie$wiek, group = baza.zdrowie$Gender)

summary(lm(data = baza.zdrowie,
   wiek ~ Gender))


baza.zdrowie$Gender.tg = relevel(baza.zdrowie$Gender, ref = "TGD")

summary(lm(data = baza.zdrowie,
           wiek ~ Gender.tg))

#### demopgraphics - place of residence ####
library(gmodels)
CrossTable(baza.zdrowie$Gender, baza.zdrowie$zamieszkanie01, 
           prop.r = TRUE, format = "SPSS")


or2 = glm(data = baza.zdrowie, family = "binomial",
          zamieszkanie01 ~ Gender)
summary(or2)
exp(or2$coefficients)
exp(confint(or2))

or2.a = glm(data = baza.zdrowie, family = "binomial",
          zamieszkanie01 ~ Gender.tg)
summary(or2.a)
exp(or2.a$coefficients)
exp(confint(or2.a))


#### demographics - income (sufficient) ####
CrossTable(baza.zdrowie$Gender, baza.zdrowie$income, 
           prop.r = TRUE, format = "SPSS")

or3 = glm(data = baza.zdrowie, family = "binomial",
          income ~ Gender)
summary(or3)
exp(or3$coefficients)
exp(confint(or3))


or3.a = glm(data = baza.zdrowie, family = "binomial",
          income ~ Gender.tg)
summary(or3.a)
exp(or3.a$coefficients)
exp(confint(or3.a))

#### demographics - income NO ANSWER ####
summary(as.factor(baza.zdrowie$Dochód))

#dochód (wystarcza - nie wystarcza)
# 1- tak, z łatwością
# 2- tak, pewną z trudnością
# 3- tak, z dużą trudnością
# 4- nie
# 5- nie chcę odpowiadać na to pytanie

# 0 - answered, 1 - no answer
baza.zdrowie$income.no[baza.zdrowie$Dochód == 1] <- 0
baza.zdrowie$income.no[baza.zdrowie$Dochód == 2] <- 0
baza.zdrowie$income.no[baza.zdrowie$Dochód == 3] <- 0
baza.zdrowie$income.no[baza.zdrowie$Dochód == 4] <- 0
baza.zdrowie$income.no[baza.zdrowie$Dochód == 5] <- 1

baza.zdrowie$income.no = factor(baza.zdrowie$income.no,
                                levels = c(0,1),
                                labels = c("0", "1"))


CrossTable(baza.zdrowie$Gender, baza.zdrowie$income.no,
           prop.r = TRUE, format = "SPSS")

or4 = glm(data = baza.zdrowie, family = "binomial",
          income.no ~ Gender)
summary(or4)
exp(or4$coefficients)
exp(confint(or4))

or4.a = glm(data = baza.zdrowie, family = "binomial",
          income.no ~ Gender.tg)
summary(or4.a)
exp(or4.a$coefficients)
exp(confint(or4.a))




#### demographics - education (university) ####
baza.zdrowie$edukacja

baza.zdrowie$edukacja01[baza.zdrowie$edukacja == "podstawowe"] <- 0
baza.zdrowie$edukacja01[baza.zdrowie$edukacja == "średnie"] <- 0
baza.zdrowie$edukacja01[baza.zdrowie$edukacja == "wyższe"] <-1

baza.zdrowie$edukacja01 = factor(baza.zdrowie$edukacja01,
                                 levels = c(0,1),
                                 labels = c("0", "1"))

CrossTable(baza.zdrowie$Gender, baza.zdrowie$edukacja01, 
           prop.r = TRUE, format = "SPSS")

or5 = glm(data = baza.zdrowie, family = "binomial",
          edukacja01 ~ Gender)
summary(or5)
exp(or5$coefficients)
exp(confint(or5))


or5.a = glm(data = baza.zdrowie, family = "binomial",
          edukacja01 ~ Gender.tg)
summary(or5.a)
exp(or5.a$coefficients)
exp(confint(or5.a))

baza.zdrowie$SESsum



#### SESsum jitterplot ~ gender #####
library(ggplot2)
clean = theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(color = "black"))


describe(baza.zdrowie$SESsum)
hist(baza.zdrowie$SESsum)
describeBy(baza.zdrowie$SESsum, group = baza.zdrowie$Gender)

jit1 = ggplot(data = baza.zdrowie,
       aes(SESsum, Gender))+
  geom_jitter(size = .7, alpha = .7, color = "#3C3838")+
  clean+
  scale_y_discrete(" ", 
                   labels = c("SDCM", 
                              "SDCW", 
                              "TG&GDP"))+
  scale_x_continuous("RSES",
                     breaks = c(10,20,30,40))+
  geom_segment(aes(x = 29.13, y = 0.55, xend = 29.13, yend = 1.45), 
             color = "red")+
  geom_segment(aes(x = 25.79, y = 1.55, xend = 25.79, yend = 2.45), 
               color = "red")+
  geom_segment(aes(x = 24.33, y = 2.6, xend = 24.33, yend = 3.45), 
               color = "red")

  
  

##### DHEQ jitterplot ~ gender #####
hist(baza.zdrowie$DHEQsuma)
describe(baza.zdrowie$DHEQsuma)
describeBy(baza.zdrowie$DHEQsuma, group = baza.zdrowie$Gender)

jit2 = ggplot(data = baza.zdrowie,
         aes(DHEQsuma, Gender))+
    geom_jitter(size = .7, alpha = .7, color = "#3C3838")+
    clean+
    scale_y_discrete(" ", 
                     labels = c("SDCM", 
                                "SDCW", 
                                "TG&GDP"))+
  scale_x_continuous("DHEQ",
                     breaks = c(50, 75, 100, 125, 150))+
  geom_segment(aes(x = 75.79, y = 0.55, xend = 75.79, yend = 1.45), 
              color = "red")+
  geom_segment(aes(x = 75.48, y = 1.55, xend = 75.48, yend = 2.45), 
              color = "red")+
  geom_segment(aes(x = 81.85, y = 2.6, xend = 81.85, yend = 3.45), 
              color = "red")



##### CESD jitterplot ~ gender #####

ggplot(data = baza.zdrowie,
       aes(x=wiek_c, y=CESDsum))+
  geom_point()+
  facet_wrap(Gender~.)+
  geom_smooth(method = "loess")




describeBy(baza.zdrowie$CESDsum, group = baza.zdrowie$Gender)


jit3 = ggplot(data = baza.zdrowie,
         aes(CESDsum, Gender))+
    geom_jitter(size = .7, alpha = .7, color = "#3C3838")+
    clean+
    scale_y_discrete(" ", 
                     labels = c("SDCM", 
                                "SDCW", 
                                "TG&GDP"))+
    scale_x_continuous("CESD-R",
                       breaks = c(20, 40, 60, 80),
                       labels = c("20","40" ,"60", "80"))+
  geom_segment(aes(x = 34, y = 0.55, xend = 34, yend = 1.45), 
               color = "red")+
  geom_segment(aes(x = 47, y = 1.55, xend = 47, yend = 2.45), 
               color = "red")+
  geom_segment(aes(x = 55, y = 2.6, xend = 55, yend = 3.45), 
               color = "red")
  
  
##### SPP-25 jitterplot ~ gender #####
hist(baza.zdrowie$SPPsr)
describeBy(baza.zdrowie$SPPsr, group = baza.zdrowie$Gender)
  
jit4 = ggplot(data = baza.zdrowie,
         aes(SPPsr, Gender))+
    geom_jitter(size = .7, alpha = .7, color = "#3C3838")+
    clean+
    scale_y_discrete(" ",
      labels = c("SDCM", 
                 "SDCW", 
                 "TG&GDP"))+
    scale_x_continuous("SPP-25")+
    geom_segment(aes(x = 3.73, y = 0.55, xend = 3.73, yend = 1.45), 
                 color = "red")+
    geom_segment(aes(x = 3.41, y = 1.55, xend = 3.41, yend = 2.45), 
                 color = "red")+
    geom_segment(aes(x = 3.31, y = 2.6, xend = 3.31, yend = 3.45), 
                 color = "red")



#### jitter plots ####
ggarrange(jit1, jit2, jit3, jit4)


library(jmv)

descriptives(baza.zdrowie, vars = vars(SPPsr, CESDsum, age),
             sw = TRUE, variance = TRUE)



# Cronbach's alpha ------------
library(ltm)
cronbach.alpha()
names(baza.zdrowie)


#### Cronbach's alpha for CESD-R ####
CESD <- data.frame(baza.zdrowie[352:371])
CESD = na.omit(CESD)

cronbach.alpha(CESD)


#### Cronbach's slpha for TPIM ####
TPIM <- data.frame(baza.zdrowie[127:150])
TPIM = na.omit(TPIM)

cronbach.alpha(TPIM)


#### Cronbach's slpha for SPP ####
SPP <- data.frame(baza.zdrowie[172:196])
SPP = na.omit(SPP)

cronbach.alpha(SPP)

#### Cronbach's slpha for DHEQ ####
dheq <- data.frame(baza.zdrowie[93:124])
dheq = na.omit(dheq)

cronbach.alpha(dheq)

#### Cronbach's slpha for SES ####
ses <- data.frame(baza.zdrowie[200:213])
ses <- (ses[c(1,3,4,6,7,9,11,12,13,14)])
ses <- na.omit(ses)

cronbach.alpha(ses)

# moderation analysis for review ----------

moderation1 <- lm(SPPsr~wiek_c+Gender+wiek_c*Gender, data = baza.zdrowie)
summary(moderation1)



library(rockchalk)
plotSlopes(moderation1, plotx = "wiek_c", modx = "Gender")

a
moderation2 <- lm(DHEQsr~wiek_c+Gender+wiek_c*Gender, data = baza.zdrowie)
summary(moderation2)
plotSlopes(moderation2, plotx = "wiek_c", modx = "Gender")


moderation3 <- lm(CESDsum~wiek_c+Gender+wiek_c*Gender, data = baza.zdrowie)
summary(moderation3)
plotSlopes(moderation3, plotx = "wiek_c", modx = "Gender")


moderation4 <- lm(SESsum~wiek_c+Gender+wiek_c*Gender, data = baza.zdrowie)
summary(moderation4)
plotSlopes(moderation4, plotx = "wiek_c", modx = "Gender")



cor.test(baza.zdrowie$wiek_c, baza.zdrowie$SPPsr)
cor.test(baza.zdrowie$wiek_c, baza.zdrowie$DHEQsuma)
cor.test(baza.zdrowie$wiek_c, baza.zdrowie$SESsum)
cor.test(baza.zdrowie$wiek_c, baza.zdrowie$CESDsum)


cor.test(baza.zdrowie$SPPsr, baza.zdrowie$DHEQsuma)
cor.test(baza.zdrowie$SPPsr, baza.zdrowie$SESsum)
cor.test(baza.zdrowie$SPPsr, baza.zdrowie$CESDsum)
cor.test(baza.zdrowie$SESsum, baza.zdrowie$DHEQsuma)






describe(baza.zdrowie$wiek)






