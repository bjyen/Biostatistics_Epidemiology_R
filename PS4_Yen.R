getwd()
setwd("...") 
library(foreign)

library(memisc)
PS4data <- as.data.set(spss.system.file('Expedition 4 data set(1).sav'))
PS4data<-data.frame(PS4data)
case<-as.factor(PS4data$case)
sex<-data.frame(PS4data$sex)
race<-data.frame(PS4data$race)
skinpru<-data.frame(PS4data$skinpru)

install.packages("epitools")
install.packages("chisq.test")
install.packages("gmodels")
library(gmodels)
install.packages("fisher.test")
library(epitools)
library(fisher.test)
library(chisq.test)
CrossTable(PS4data$sex, PS4data$case)

chisq.test(PS4data$sex, PS4data$case) #X-squared = 3.8883, df = 1, p-value = 0.04862
chisq.test(PS4data$sex, PS4data$case)$expected  
chisq.test(PS4data$sex, PS4data$case)$stdres     
fisher.test(PS4data$sex, PS4data$case)# OR= 1.878104 
oddsratio.fisher(PS4data$sex, PS4data$case) #1.421994 0.7449896 2.691733
fsex<-fisher.test(PS4data$sex, PS4data$case)$chi.square
oddsratio.fisher(PS4data$sex, PS4data$case)$chi.square
fsex
str(fsex)
fsex$estimate
fsex$conf.int
fsex$p.value
#Race
race<-as.numeric(PS4data$race)
case<-as.numeric(PS4data$case)

chisq.test(PS4data$race, PS4data$case)# X-squared = 2.0892, df = 2, p-value = 0.3518
chisq.test(PS4data$race, PS4data$case)$expected  
chisq.test(PS4data$race, PS4data$case)$stdres  

frace<-fisher.test(PS4data$race, PS4data$case)#p-value = 0.3965 
oddsratio.fisher(PS4data$race, PS4data$case)
frace
str(frace)
frace$estimate
frace$conf.int
frace$p.value

#Diabeties 
diabetes<-as.numeric(PS4data$diabetes)

chisq.test(PS4data$diabetes, PS4data$case)# X-squared = 1.0139, df = 1, p-value = 0.314
chisq.test(PS4data$diabetes, PS4data$case)$expected  
chisq.test(PS4data$diabetes, PS4data$case)$stdres  
fisher.test(PS4data$diabetes, PS4data$case)#p-value = 0.2774, OR 1.421994
oddsratio.fisher(PS4data$diabetes, PS4data$case) #1.421994 0.7449896 2.691733

#Renal insufficiency
renalinsuff<-as.factor(PS4data$renalinsuff)
chisq.test(PS4data$renalinsuff, PS4data$case)# X-squared = 4.582, df = 1, p-value = 0.03231
fisher.test(PS4data$renalinsuff, PS4data$case)#odds ratio 1.978797, p-value = 0.03108
oddsratio.fisher(PS4data$renalinsuff, PS4data$case) #1.978797 1.050717 3.733318
chisq.test(PS4data$renalinsuff, PS4data$case)$expected  
chisq.test(PS4data$renalinsuff, PS4data$case)$stdres 

#Paralysis
paralysis<-as.factor(PS4data$paralysis)
chisq.test(PS4data$paralysis, PS4data$case)# X-squared = 1.675e-30, df = 1, p-value = 1
fisher.test(PS4data$paralysis, PS4data$case)# odds ratio 1.480688 p-value = 1
oddsratio.fisher(PS4data$paralysis, PS4data$case) #1.978797 1.050717 3.733318
chisq.test(PS4data$paralysis, PS4data$case)$expected  
chisq.test(PS4data$paralysis, PS4data$case)$stdres 


#Admist Source
admsource<-as.factor(PS4data$admsource)
chisq.test(PS4data$admsource, PS4data$case)# X-squared = 2.0514, df = 2, p-value = 0.3585
fisher.test(PS4data$admsource, PS4data$case)# p-value = 0.3814 alternative hypothesis: two.sided
oddsratio.fisher(PS4data$admsource, PS4data$case) #
chisq.test(PS4data$admsource, PS4data$case)$expected  
chisq.test(PS4data$admsource, PS4data$case)$stdres 

# Admitted through ER
admed<-as.factor(PS4data$admed)
chisq.test(PS4data$admed, PS4data$case)     #X-squared = 0.016542, df = 1, p-value = 0.8977
fisher.test(PS4data$admed, PS4data$case)    #odds ratio 1.086311 p-value = 0.8811
oddsratio.fisher(PS4data$admed, PS4data$case) #
chisq.test(PS4data$admed, PS4data$case)$expected  
chisq.test(PS4data$admed, PS4data$case)$stdres 
# Low blood pressure
lowbp<-as.factor(PS4data$lowbp)
chisq.test(PS4data$lowbp, PS4data$case)    #X-squared = 13.185, df = 1, p-value = 0.0002822
fisher.test(PS4data$lowbp, PS4data$case)   #odds ratio 18.14271  p-value = 2.614e-05
oddsratio.fisher(PS4data$lowbp, PS4data$case) #
chisq.test(PS4data$lowbp, PS4data$case)$expected  
chisq.test(PS4data$lowbp, PS4data$case)$stdres

# Venilated
vent<-as.factor(PS4data$vent)
chisq.test(PS4data$vent, PS4data$case)     #X-squared = 33.792, df = 1, p-value = 6.133e-09
fisher.test(PS4data$vent, PS4data$case)    #odds ratio 6.33395, p-value = 1.25e-08
oddsratio.fisher(PS4data$vent, PS4data$case) #
chisq.test(PS4data$vent, PS4data$case)$expected  
chisq.test(PS4data$vent, PS4data$case)$stdres

# Other trauma in hospital
traumainhosp<-as.factor(PS4data$traumainhosp)
chisq.test(PS4data$traumainhosp, PS4data$case) # warning, X-squared = 0.00031522, df = 1, p-value = 0.9858
fisher.test(PS4data$traumainhosp, PS4data$case)# odds ratio  0.5844435 p-value = 1
oddsratio.fisher(PS4data$traumainhosp, PS4data$case) #
chisq.test(PS4data$traumainhosp, PS4data$case)$expected  
chisq.test(PS4data$traumainhosp, PS4data$case)$stdres


