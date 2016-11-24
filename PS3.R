# Biostatistics PS3a


getwd()
setwd('C:/Users/JJ/Documents')
library(foreign)
install.packages("memisc")

#Import SPSS data into R
library(memisc)
PS3data <- as.data.set(spss.system.file('Expedition_3_data.sav'))
PS3data_B <- as.data.set(spss.system.file('Expedition_3B_data.sav'))
# Longer Total Hospitalization
#1) What are the mean, median, standard deviation and range for LIH for Uninfected patients?
names(PS3data)
names(PS3data_B)
# Decribe the data labels
install.packages("Hmisc")
library(Hmisc)
PS3data<-data.frame(PS3data)
describe(PS3data)            # case means infection / not infections
hypercol<-data.frame(PS3data$hyperchol)
case<-data.frame.labelled(PS3data$case)
totalhospitalizationdays<-data.frame(PS3data$totalhospitalizationdays)

summary(subset(PS3data, case=="Not infected", select=totalhospitalizationdays))
sd(subset(PS3data, case=="Not infected")$totalhospitalizationdays)

#2) What are the mean, median, standard deviation and range for LIH for Infected patients?
summary(subset(PS3data, case=="Infected", select=totalhospitalizationdays))

sd(subset(PS3data, case=="Infected")$totalhospitalizationdays)

#3) Perform a Shapiro-Wilk test and indicate if the group data meets the normality assumption:

totalhospitalizationdays<-as.numeric(PS3data$totalhospitalizationdays)
shapiro.test(PS3data$totalhospitalizationdays[PS3data$case=='Not infected'])
shapiro.test(PS3data$totalhospitalizationdays[PS3data$case=='Infected'])
# For yes and No
describe(subset(PS3data, case=="Infected", select=totalhospitalizationdays))
describe(subset(PS3data, case=="Not infected", select=totalhospitalizationdays))

# 4) Perform a Levene's test and indicate if the data meets the homogeneity of variance (HoV) assumption:
### http://www.cookbook-r.com/Statistical_analysis/Homogeneity_of_variance/
install.packages("car")
# load leveneTest function
library(car)
# run the levene test centered around the mean
leveneTest(PS3data$totalhospitalizationdays, PS3data$case, center=mean)

#5) Run an indepdenent samples t-test, Mann-Whitney U (Wilcoxon) test and Kolmogrov- Smirnov Z test for this hypothesis and list the results below:

# independent samples t test
t.test(totalhospitalizationdays~case) #t.test(totalhospitalizationdays~infection)
 
# Mann-Whitney U (Wilcoxon) test
wilcox.test(totalhospitalizationdays ~ case, data=PS3data) 
wilcox.test(totalhospitalizationdays ~ infection, data=PS3data) 

# Kolmogrov- Smirnov (x must be numeric)
PS3data$infection <- ifelse(PS3data$case =="Infected", 
                            1, 0) 
infection<-as.numeric(PS3data$infection)
ks.test(totalhospitalizationdays, infection)

# 6) What would be the appropriate comparison test for this hypothesis?
#7) Write a summary statement for which hypothesis to accept (null or alternative) for the test chosen in question 6.

#############################
##### Higher Initial Cost ###
#############################
costinitialstay<-as.numeric(PS3data$costinitialstay)
summary(subset(PS3data, case=="Not infected", select=costinitialstay))
sd(subset(PS3data, case=="Not infected")$costinitialstay)
summary(subset(PS3data, case=="Infected", select=costinitialstay))
sd(subset(PS3data, case=="Infected")$costinitialstay)

#3) Perform a Shapiro-Wilk test and indicate if the group data meets the normality assumption:


shapiro.test(PS3data$costinitialstay[PS3data$case=='Not infected'])
shapiro.test(PS3data$costinitialstay[PS3data$case=='Infected'])
# For yes and No
describe(subset(PS3data, case=="Not infected", select=costinitialstay))
describe(subset(PS3data, case=="Infected", select=costinitialstay))


# 4) Perform a Levene's test and indicate if the data meets the homogeneity of variance (HoV) assumption:
# load leveneTest function
library(car)
# run the levene test centered around the mean
leveneTest(PS3data$costinitialstay, PS3data$case, center=mean)

#5) Run an indepdenent samples t-test, Mann-Whitney U (Wilcoxon) test and Kolmogrov- Smirnov Z test for this hypothesis and list the results below:

# independent samples t test
t.test(costinitialstay~infection) #t.test(totalhospitalizationdays~infection)

# Mann-Whitney U (Wilcoxon) test
wilcox.test(costinitialstay ~ case, data=PS3data) 
wilcox.test(costinitialstay ~ infection, data=PS3data) 

# Kolmogrov- Smirnov (x must be numeric)

ks.test(costinitialstay, infection)

# 6) What would be the appropriate comparison test for this hypothesis?
#7) Write a summary statement for which hypothesis to accept (null or alternative) for the test chosen in question 6.

#############################
##### Higher total Cost ###
#############################
totalcostsinitialandreadmissions<-as.numeric(PS3data$totalcostsinitialandreadmissions)
summary(subset(PS3data, case=="Not infected", select=totalcostsinitialandreadmissions))
sd(subset(PS3data, case=="Not infected")$totalcostsinitialandreadmissions)
summary(subset(PS3data, case=="Infected", select=totalcostsinitialandreadmissions))
sd(subset(PS3data, case=="Infected")$totalcostsinitialandreadmissions)

#3) Perform a Shapiro-Wilk test and indicate if the group data meets the normality assumption:


shapiro.test(PS3data$totalcostsinitialandreadmissions[PS3data$case=='Not infected'])
shapiro.test(PS3data$totalcostsinitialandreadmissions[PS3data$case=='Infected'])
# For yes and No
describe(subset(PS3data, case=="Not infected", select=totalcostsinitialandreadmissions))
describe(subset(PS3data, case=="Infected", select=totalcostsinitialandreadmissions))


# 4) Perform a Levene's test and indicate if the data meets the homogeneity of variance (HoV) assumption:
# load leveneTest function
library(car)
# run the levene test centered around the mean
leveneTest(PS3data$totalcostsinitialandreadmissions, PS3data$case, center=mean)

#5) Run an indepdenent samples t-test, Mann-Whitney U (Wilcoxon) test and Kolmogrov- Smirnov Z test for this hypothesis and list the results below:

# independent samples t test
t.test(totalcostsinitialandreadmissions,infection) #t.test(totalhospitalizationdays~infection)

# Mann-Whitney U (Wilcoxon) test
wilcox.test(totalcostsinitialandreadmissions~ case, data=PS3data) 
wilcox.test(totalcostsinitialandreadmissions ~ infection, data=PS3data) 

# Kolmogrov- Smirnov (x must be numeric)

ks.test(totalcostsinitialandreadmissions, infection)

# 6) What would be the appropriate comparison test for this hypothesis?
#7) Write a summary statement for which hypothesis to accept (null or alternative) for the test chosen in question 6.
