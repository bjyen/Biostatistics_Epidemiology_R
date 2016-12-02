# November 30th 2016 Bing-Jie Yen
# This is for Linear and Logistic Regression
# Expedition Objective: Demonstrate knowledge around calculating univariate and multivariate linear and logistic regression.
getwd()
setwd("") 
library(foreign)
install.packages("installr")
library(installr)
library(memisc)
install.packages("epitools")
library(epitools)
library(car)
install.packages("pscl")  # http://artax.karlin.mff.cuni.cz/r-help/library/pscl/html/pR2.html
library(pscl)


PS5data <- as.data.set(spss.system.file('Expedition5data.sav'))
PS5data<-data.frame(PS5data)
names(PS5data)
lengthofinitialstay<-as.numeric(PS5data$lengthofinitialstay)
case<-data.frame(PS5data$case)
race<-data.frame(PS5data$race) # black or white
age<-data.frame(PS5data$age)
hxca<-data.frame(PS5data$hxca)
incontinencepreop<-data.frame(PS5data$incontinencepreop)
preopinfx<-data.frame(PS5data$preopinfx)
asa<-data.frame(PS5data$asa)
woundclass<-data.frame(PS5data$woundclass)
propofol<-data.frame(PS5data$propofol)
otherproc<-data.frame(PS5data$otherproc) # second procedure
bmi<-data.frame(PS5data$bmi)
intraoptransfusion<-data.frame(PS5data$intraoptransfusion)
postoptransfu<-data.frame(PS5data$postoptransfu)
proceduretime<-data.frame(PS5data$proceduretime)
anasthesiatime<-data.frame(PS5data$anasthesiatime)
diabetes<-data.frame(PS5data$diabetes) #0 or 1
smokinghistory<-data.frame(PS5data$smokinghistory) #prior, current, never smoked
library("MASS")

summary(lengthofinitialstay)
#case
summary(case) 
model.1<-lm(lengthofinitialstay~case,data=PS5data) #case: control and case
confint(model.1, level=0.95)
coefficients(model.1)
summary(model.1)

####   race: I use variable "race" in this question, which includes black n hispanic and white non-hispanic
summary(race)
model.2<-lm(lengthofinitialstay~race,data=PS5data)
confint(model.2, level=0.95)
coefficients(model.2)
summary(model.2)
blacknh<-data.frame(PS5data$blacknh)
summary(blacknh)
#model.2_blacknh<-lm(lengthofinitialstay~blacknh,data=PS5data)
#confint(model.2_blacknh, level=0.95)
#coefficients(model.2_blacknh)
#summary(model.2_blacknh)

model.2.1<-glm(case~race,family=binomial("logit"),data=PS5data)
confint(model.2.1, level=0.95)
oddsratio.fisher(PS5data$race,PS5data$case) #1.421994 0.7449896 2.691733
pR2(model.2.1)
coefficients(model.2.1)
summary(model.2.1)

# age
summary(age)
model.3<-lm(lengthofinitialstay~age,data=PS5data)
confint(model.3, level=0.95)
coefficients(model.3)
summary(model.3)

model.3.1<-glm(case~age,family=binomial("logit"),data=PS5data)
summary(model.3.1)
confint(model.3.1, level=0.95)
exp(cbind(OR = coef(model.3.1), confint(model.3.1))) # OR and OR 95 CI
oddsratio.fisher(PS5data$case,PS5data$age) 
pR2(model.3.1)
#ever smoker
eversmoker<-data.frame(PS5data$eversmoker)
currensmoker<-data.frame(PS5data$currensmoker)
model.eversmoke<-lm(lengthofinitialstay~eversmoker,data=PS5data)
confint(model.eversmoke, level=0.95)
coefficients(model.eversmoke)
summary(model.eversmoke)

model.currentsmoker<-lm(lengthofinitialstay~currensmoker,data=PS5data)
model.smoke<-lm(lengthofinitialstay~smokinghistory,data=PS5data)
# Smoking history
summary(smokinghistory)
confint(model.smoke, level=0.95)
coefficients(model.smoke)
summary(model.smoke)
model.3.1<-glm(case~smokinghistory,family=binomial("logit"),data=PS5data)
summary(model.3.1)
oddsratio.fisher(PS5data$smokinghistory,PS5data$case) #1.421994 0.7449896 2.691733
pR2(model.3.1)
pR2(model.3B)
## hxca: does not match the variables "current smoker" and "ever smoker" so I use the hxca directly 
summary(hxca)
model.4<-lm(lengthofinitialstay~hxca,data=PS5data)
confint(model.4, level=0.95)
coefficients(model.4)
summary(model.4)
model.4.1<-glm(case~hxca,family=binomial(link='logit'),data=PS5data)
oddsratio.fisher(PS5data$hxca,PS5data$case) #1.421994 0.7449896 2.691733
confint(model.4.1)
pR2(model.4.1)

# incontinence 
summary(incontinencepreop)
model.5<-lm(lengthofinitialstay~incontinencepreop,data=PS5data)
confint(model.5, level=0.95)
coefficients(model.5)
summary(model.5)


model.5.1<-glm(case~smokinghistory,family=binomial(link='logit'),data=PS5data)
oddsratio.fisher(PS5data$smokinghistory,PS5data$case) 

model.5.2<-glm(case~incontinencepreop,family=binomial(link='logit'),data=PS5data)
oddsratio.fisher(PS5data$incontinencepreop,PS5data$case) 
pR2(model.5.2)


# Preoperation infection 
table(preopinfx)
summary(preopinfx)
model.6<-lm(lengthofinitialstay~preopinfx,data=PS5data)
confint(model.6, level=0.95)
coefficients(model.6)
summary(model.6)
# Post operative 
incontipostop<-data.frame(PS5data$incontipostop)
model.6.1<-glm(case~incontipostop,family=binomial(link='logit'),data=PS5data)
oddsratio.fisher(PS5data$incontipostop,PS5data$case)
pR2(model.6.1)


model.6.2<-glm(case~preopinfx,family=binomial(link='logit'),data=PS5data)
oddsratio.fisher(PS5data$preopinfx,PS5data$case) 
pR2(model.6.2)

##ASA
summary(asa)
model.7<-lm(lengthofinitialstay~asa,data=PS5data)
confint(model.7, level=0.95)
coefficients(model.7)
summary(model.7)

# Wound class
summary(woundclass)
model.8<-lm(lengthofinitialstay~woundclass,data=PS5data)
confint(model.8, level=0.95)
coefficients(model.8)
summary(model.8)


summary(propofol)
model.9<-lm(lengthofinitialstay~propofol,data=PS5data)
confint(model.9, level=0.95)
coefficients(model.9)
summary(model.9)
model.9.1<-glm(case~propofol,family=binomial(link='logit'),data=PS5data)
summary(model.9.1)

# Other procedure
summary(otherproc)
model.10<-lm(lengthofinitialstay~otherproc,data=PS5data)
confint(model.10, level=0.95)
coefficients(model.10)
summary(model.10)
model.10.1<-glm(case~otherproc,family=binomial(link='logit'),data=PS5data)
summary(model.10.1)
oddsratio.fisher(PS5data$otherproc,PS5data$case) 

pR2(model.10.1) 


#BMI
summary(bmi)
model.11<-lm(lengthofinitialstay~bmi,data=PS5data)
confint(model.11, level=0.95)
coefficients(model.11)
summary(model.11)
model.11.1<-glm(case~bmi,family=binomial(link='logit'),data=PS5data)
summary(model.11.1)
exp(cbind(OR = coef(model.11.1), confint(model.11.1))) # OR and OR 95 CI

pR2(model.11.1) 
# Tranfusion
summary(intraoptransfusion)
model.12<-lm(lengthofinitialstay~intraoptransfusion,data=PS5data)
confint(model.12, level=0.95)
coefficients(model.12)
summary(model.12)
model.12.1<-glm(case~intraoptransfusion,family=binomial(link='logit'),data=PS5data)
oddsratio.fisher(PS5data$intraoptransfusion,PS5data$case) 
pR2(model.12.1) 

oddsratio.fisher(PS5data$postoptransfu,PS5data$case) 
model.12.2<-glm(case~postoptransfu,family=binomial(link='logit'),data=PS5data)


##postoptransfu
postoptransfu<-data.frame(PS5data$postoptransfu)
summary(postoptransfu)
model.postoptransfu<-lm(lengthofinitialstay~postoptransfu,data=PS5data)
confint(model.postoptransfu, level=0.95)
coefficients(model.postoptransfu)
summary(model.postoptransfu)

model.postoptransfu_glm<-glm(case~postoptransfu,family=binomial(link='logit'),data=PS5data)
summary(model.postoptransfu_glm)
oddsratio.fisher(PS5data$postoptransfu,PS5data$case) 
pR2(model.postoptransfu_glm) 



# Procedure time
summary(proceduretime)
model.13<-lm(lengthofinitialstay~proceduretime,data=PS5data)
confint(model.13, level=0.95)
coefficients(model.13)
summary(model.13)
model.13.1<-glm(case~proceduretime,family=binomial(link='logit'),data=PS5data)
summary(model.13.1)
exp(cbind(OR = coef(model.13.1), confint(model.13.1))) # OR and OR 95 CI
pR2(model.13.1)
#
summary(anasthesiatime)
model.14<-lm(lengthofinitialstay~anasthesiatime,data=PS5data)
confint(model.14, level=0.95)
coefficients(model.14)
summary(model.14)
model.14.1<-glm(case~anasthesiatime,family=binomial(link='logit'),data=PS5data)
summary(model.14.1)
oddsratio.fisher(PS5data$anasthesiatime,PS5data$case) 


# Deliverable 2A: I include the variables we just analyze and include all the significant variables into explnatory 
#variables, then I compare the different combination, it turns out model 2B has less AIC. 
### Test normaility:
qqnorm(resid(model.1));qqline(resid(model.1))# no
qqnorm(resid(model.2));qqline(resid(model.2))
qqnorm(resid(model.3));qqline(resid(model.3))# yes age
qqnorm(resid(model.4));qqline(resid(model.4))# No
qqnorm(resid(model.5));qqline(resid(model.5))# No
qqnorm(resid(model.6));qqline(resid(model.6))# No
qqnorm(resid(model.7));qqline(resid(model.7))
qqnorm(resid(model.8));qqline(resid(model.8))
qqnorm(resid(model.9));qqline(resid(model.9))
qqnorm(resid(model.10));qqline(resid(model.10))
qqnorm(resid(model.11));qqline(resid(model.11))
qqnorm(resid(model.12));qqline(resid(model.12))
qqnorm(resid(model.13));qqline(resid(model.13)) #Yes procedure time
qqnorm(resid(model.14));qqline(resid(model.14)) # Yes anasthesiatime

qqnorm(resid(model.postoptransfu));qqline(resid(model.postoptransfu)) # No


####
###  Deliverable 3A: Summary questions.
###
model.2A<-lm(lengthofinitialstay~age+hxca+woundclass+otherproc+anasthesiatime,data=PS5data)
model.2B<-lm(lengthofinitialstay~age+woundclass+anasthesiatime,data=PS5data)
model.2c<-lm(lengthofinitialstay~age+proceduretime+anasthesiatime,data=PS5data) # by normality test,
model.2d<-lm(lengthofinitialstay~age+woundclass+proceduretime+anasthesiatime,data=PS5data) # combine B and C
summary(model.2B)

AIC(model.2A,model.2B,model.2c,model.2d)#2B wins


names(PS5data)
library(ggplot2)
resid(model.1)
qqnorm(resid(model.1));qqline(resid(model.1))
glm(lengthofinitialstay~case+race+age+hxca+incontinencepreop+preopinfx+asa+woundclass+proceduretime)

## Odds ratio
oddsratio.fisher(PS5data$race,PS5data$case) 
oddsratio.fisher(PS5data$hxca,PS5data$case) 
oddsratio.fisher(PS5data$smokinghistory,PS5data$case) 
oddsratio.fisher(PS5data$incontinencepreop,PS5data$case) 
oddsratio.fisher(PS5data$otherproc,PS5data$case) 
oddsratio.fisher(PS5data$diabetes,PS5data$case) 
oddsratio.fisher(PS5data$age,PS5data$case) # too many
oddsratio.fisher(PS5data$bmi,PS5data$case) # too many
oddsratio.fisher(PS5data$proceduretime,PS5data$case) # too many
model.diabetes<-glm(case~diabetes,family=binomial(link='logit'),data=PS5data)



## R square

pR2(model.2.1)#race
pR2(model.3.1)#age
pR2(model.4.1)#hxca
pR2(model.5.1)#smoking history
pR2(model.5.2)# preoperative incontinuity 
pR2(model.6.1)#post operative incontinuity
pR2(model.6.2)#preoperative inflection
pR2(model.10.1)#other procedure
pR2(model.11.1)#bmi
pR2(model.12.1)#intra transfusion
pR2(model.12.2)#post tranfusion
pR2(model.diabetes)#diabetes
pR2(model.13.1)# procedure time
model.3A_1<-glm(case~smokinghistory,family=binomial(link='logit'),data=PS5data)
pR2(model.3A_1)
model.3A<-glm(case~race+smokinghistory+diabetes+hxca+incontinencepreop+incontipostop+preopinfx+intraoptransfusion+postoptransfu+
                otherproc,family=binomial(link='logit'),data=PS5data)
summary(model.3A)
exp(cbind(OR = coef(model.3A), confint(model.3A))) # OR and OR 95 CI
pR2(model.3A)

model.3B<-glm(case~hxca+incontinencepreop+incontipostop+preopinfx+intraoptransfusion+postoptransfu+
                 otherproc,family=binomial(link='logit'),data=PS5data)
pR2(model.3A)
pR2(model.3B)
oddsratio.fisher(model.3A) 

summary(model.3B)
summary(model.3A)  # 3A wins 
model.3C<-glm(case~smokinghistory,family=binomial(link='logit'),data=PS5data)
pR2(model.3C)


model.3D<-glm(case~hxca+incontinencepreop+preopinfx+intraoptransfusion+postoptransfu
                ,family=binomial(link='logit'),data=PS5data)
exp(cbind(OR = coef(model.3D), confint(model.3D))) # OR and OR 95 CI
pR2(model.3D)

model.3E<-glm(case~race+smokinghistory+diabetes+incontinencepreop
              +preopinfx+intraoptransfusion+postoptransfu
              ,family=binomial(link='logit'),data=PS5data)

exp(cbind(OR = coef(model.3E), confint(model.3E))) # OR and OR 95 CI
pR2(model.3E)


model.3F<-glm(case~race+smokinghistory+diabetes+hxca+incontinencepreop
              +preopinfx+intraoptransfusion+postoptransfu+
                otherproc,family=binomial(link='logit'),data=PS5data)
exp(cbind(OR = coef(model.3F), confint(model.3F))) # OR and OR 95 CI
pR2(model.3F)
AIC(model.3A,model.3B,model.3C,model.3D,model.3E,model.3F)


qqnorm(model.1);qqline(model.1, col = 2)
qqnorm(woundclass);qqline(woundclass, col = 2)

