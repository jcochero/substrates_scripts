#### SCRIPTS USED IN THE SUBSTRATES ARTICLE, WORK IN PROGRESS! ###

setwd("D:/Users/Juaco/Google Drive/Papers en produccion/Sustratos/Analisis")
library(ggplot2)
library(apaTables)
library(xtable)


####ANALISIS FQ####
matrizFQ <- read.csv(file.choose())
matrizFQ$TIME <- as.factor(matrizFQ$TIME)

#transformaci?n de datos
matrizFQ.log <- matrizFQ
matrizFQ.log[,3:5] <- log(matrizFQ.log[,3:5]+1)
matrizFQ.log[,7:12] <- log(matrizFQ.log[,7:12]+1)
matrizFQ.cero  <- matrizFQ.log[which(matrizFQ.log$TIME != 0),] #remove the data from T0 from the initial data for graphs
matrizFQ.cero  <- matrizFQ.cero[which(matrizFQ.cero$Canal == "C"),] #remove the data from A and B

aov <- lm(matrizFQ.cero$Conductivity ~ matrizFQ.cero$TIME, data=matrizFQ.cero)
apa.aov.table(aov, filename= paste("Conductivity", ".doc", sep=""),type=2)
aov <- lm(matrizFQ.cero$Temperature ~ matrizFQ.cero$TIME, data=matrizFQ.cero)
apa.aov.table(aov, filename= paste("Temperature", ".doc", sep=""),type=2)
aov <- lm(matrizFQ.cero$DO ~ matrizFQ.cero$TIME, data=matrizFQ.cero)
apa.aov.table(aov, filename= paste("DO", ".doc", sep=""),type=2)
aov <- lm(matrizFQ.cero$pH ~ matrizFQ.cero$TIME, data=matrizFQ.cero)
apa.aov.table(aov, filename= paste("pH", ".doc", sep=""),type=2)
aov <- lm(matrizFQ.cero$PRS ~ matrizFQ.cero$TIME, data=matrizFQ.cero)
apa.aov.table(aov, filename= paste("PRS", ".doc", sep=""),type=2)
aov <- lm(matrizFQ.cero$NO3 ~ matrizFQ.cero$TIME, data=matrizFQ.cero)
apa.aov.table(aov, filename= paste("NO3", ".doc", sep=""),type=2)
aov <- lm(matrizFQ.cero$NO2 ~ matrizFQ.cero$TIME, data=matrizFQ.cero)
apa.aov.table(aov, filename= paste("NO2", ".doc", sep=""),type=2)
aov <- lm(matrizFQ.cero$NH4 ~ matrizFQ.cero$TIME, data=matrizFQ.cero)
apa.aov.table(aov, filename= paste("NH4", ".doc", sep=""),type=2)
aov <- lm(matrizFQ.cero$BOD5 ~ matrizFQ.cero$TIME, data=matrizFQ.cero)
apa.aov.table(aov, filename= paste("BOD5", ".doc", sep=""),type=2)
aov <- lm(matrizFQ.cero$COD ~ matrizFQ.cero$TIME, data=matrizFQ.cero)
apa.aov.table(aov, filename= paste("COD", ".doc", sep=""),type=2)

#tablas resumen max y min
apa.1way.table(TIME, pH, matrizFQ[which(matrizFQ$TIME != 0 & matrizFQ$Canal == "C"),] )
apa.1way.table(TIME, PRS, matrizFQ[which(matrizFQ$TIME != 1 & matrizFQ$TIME != 3 ),])


####ANALISIS BIOLOGICAS####
matriz <- read.csv(file.choose())
matriz$TIME <- as.factor(matriz$TIME)
matriz$SUBSTRATE <- as.factor(matriz$SUBSTRATE)

#transformaci?n de datos
matriz.log <- matriz
matriz.log[,3:6] <- log(matriz.log[,3:6]+1)


# Homogeneidad de varianza con test de Levene
library(car)

leveneTest(matriz.log$CLA ~ matriz.log$SUBSTRATE * matriz.log$TIME, data = matriz.log)
leveneTest(matriz.log$O2CONS ~ matriz.log$SUBSTRATE * matriz.log$TIME, data = matriz.log)
leveneTest(matriz.log$BACT ~ matriz.log$SUBSTRATE * matriz.log$TIME, data = matriz.log)
leveneTest(matriz.log$CARB ~ matriz.log$SUBSTRATE * matriz.log$TIME, data = matriz.log)


# First of all, do the three types of substrates develop similar fluvial biofilms 
# in nutrient rich streams?
library(ggsci)
matriz.inicial.log <- matriz.log[which(matriz.log$TIME == 0),]
matriz.inicial <- matriz[which(matriz$TIME == 0),]
aov <- lm(matriz.inicial.log$CLA ~ matriz.inicial.log$SUBSTRATE, data=matriz.inicial.log)
anova(aov)
ggplot(data=matriz.inicial, aes(x=SUBSTRATE, y=CLA, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()


aov <- lm(matriz.inicial.log$BACT ~ matriz.inicial.log$SUBSTRATE, data=matriz.inicial.log)
anova(aov)
ggplot(data=matriz.inicial, aes(x=SUBSTRATE, y=BACT, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()

aov <- lm(matriz.inicial.log$O2CONS ~ matriz.inicial.log$SUBSTRATE, data=matriz.inicial.log)
anova(aov)
ggplot(data=matriz.inicial, aes(x=SUBSTRATE, y=O2CONS, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()

aov <- lm(matriz.inicial.log$CARB ~ matriz.inicial.log$SUBSTRATE, data=matriz.inicial.log)
anova(aov)
ggplot(data=matriz.inicial, aes(x=SUBSTRATE, y=CARB, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()


#	Second, the acclimation of the developed fluvial biofilms to the laboratory settings varies 
# depending on the substrate? If so, which substrate acclimates faster and with less variability?
matriz.log <- matriz.log[which(matriz.log$TIME != 0),] #remove the data from T0
matriz.cero  <- matriz[which(matriz$TIME != 0),] #remove the data from T0 from the initial data for graphs
#graficos
library(ggsci)
library(ggpubr)  

#clorofila-a
aov <- lm(matriz.log$CLA ~ matriz.log$SUBSTRATE * matriz.log$TIME, data=matriz.log)
apa.aov.table(aov, filename="clorofila.doc",type=2)
ggplot(data=matriz.cero, aes(x=TIME, y=CLA, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek() + stat_compare_means(method= "anova", label = "p.signif")

#BACT
aov <- aov(matriz.log$BACT ~ matriz.log$SUBSTRATE * matriz.log$TIME, data=matriz.log)
apa.aov.table(aov, filename="bacteria.doc",type=2)
ggplot(data=matriz.cero, aes(x=TIME, y=BACT, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek() + stat_compare_means(label = "p.signif")

#O2CONS
aov <- aov(matriz.log$O2CONS ~ matriz.log$SUBSTRATE * matriz.log$TIME, data=matriz.log)
apa.aov.table(aov, filename="consumo.doc",type=2)
ggplot(data=matriz.cero, aes(x=TIME, y=O2CONS, fill=SUBSTRATE))+ geom_boxplot() + 
  scale_fill_startrek() + stat_compare_means(method= "anova", label = "p.signif")

#CARB
aov <- aov(matriz.log$CARB ~ matriz.log$SUBSTRATE * matriz.log$TIME, data=matriz.log)
apa.aov.table(aov, filename="carbohidratos.doc",type=2)
ggplot(data=matriz.cero, aes(x=TIME, y=CARB, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek() + stat_compare_means(method= "anova", label = "p.signif")

#	When used in laboratory experiments, which substrate provides the lesser variability 
# between replicates? 

# ANOVA using variances, data log for the anova, not log for graphs
matrizCV <- read.csv(file.choose())
matrizCV <- matrizCV[which(matrizCV$TIME != 0),]
matrizCV$TIME <- as.factor(matrizCV$TIME)
matrizCV.log <- matrizCV
matrizCV.log[,3:6] <- log(matrizCV.log[,3:6]+1)

aovCV <- aov(BACT ~ SUBSTRATE, data=matrizCV.log)
summary(aovCV)
apa.aov.table(aovCV, filename="bact_desvest.doc",type=2)

aovCV <- aov(CLA ~ SUBSTRATE, data=matrizCV.log)
summary(aovCV)
apa.aov.table(aovCV, filename="cla_desvest.doc",type=2)

aovCV <- aov(O2CONS ~ SUBSTRATE, data=matrizCV.log)
summary(aovCV)
apa.aov.table(aovCV, filename="o2_desvest.doc",type=2)

aovCV <- aov(CARB ~ SUBSTRATE, data=matrizCV.log)
summary(aovCV)
apa.aov.table(aovCV, filename="carb_desvest.doc",type=2)


#Gr?ficos
ggplot(data=matrizCV, aes(x=SUBSTRATE, y=BACT, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()+ stat_compare_means(method= "anova", label = "p.signif")

ggplot(data=matrizCV, aes(x=SUBSTRATE, y=CLA, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()+ stat_compare_means(method= "anova", label = "p.signif")

ggplot(data=matrizCV, aes(x=SUBSTRATE, y=O2CONS, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()+ stat_compare_means(method= "anova", label = "p.signif")

ggplot(data=matrizCV, aes(x=SUBSTRATE, y=CARB, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()+ stat_compare_means(method= "anova", label = "p.signif")



########### OBSOLETO ################
#Coef de variacion por fechas
matrizCV <- read.csv(file.choose())
matrizCV <- matrizCV[which(matrizCV$TIME != 0),]
matrizCV$TIME <- as.factor(matrizCV$TIME)
aovCV <- aov(BACT ~ SUBSTRATE, data=matrizCV)
summary(aovCV)

aovCV <- aov(CLA ~ SUBSTRATE, data=matrizCV)
summary(aovCV)

aovCV <- aov(O2CONS ~ SUBSTRATE, data=matrizCV)
summary(aovCV)

aovCV <- aov(CARB ~ SUBSTRATE, data=matrizCV)
summary(aovCV)

#Gr?ficos
ggplot(data=matrizCV, aes(x=SUBSTRATE, y=BACT, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()+ stat_compare_means(method= "anova", label = "p.signif")

ggplot(data=matrizCV, aes(x=SUBSTRATE, y=CLA, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()+ stat_compare_means(method= "anova", label = "p.signif")

ggplot(data=matrizCV, aes(x=SUBSTRATE, y=O2CONS, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()+ stat_compare_means(method= "anova", label = "p.signif")

ggplot(data=matrizCV, aes(x=SUBSTRATE, y=CARB, fill=SUBSTRATE)) + geom_boxplot() + 
  scale_fill_startrek()+ stat_compare_means(method= "anova", label = "p.signif")



