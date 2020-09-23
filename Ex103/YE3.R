install.packages("openxlsx")
install.packages("zoo")
install.packages("lmtest")
install.packages("sampleSelection")
install.packages("visualize")
library("openxlsx")
library('psych')
library(ggplot2)
library(visualize)
library("zoo")
library("lmtest")
library("maxLik")
library("miscTools")
library("sampleSelection")
install.packages("mvtnorm") 
library("mvtnorm")

#  Clear Objects
rm(list=ls())
#removing specific object
#rm( x )
#data$var <- NULL
######__________________________________________________________________________________________

# Change Directory
setwd("/Users/EfiPaka/Desktop/Econometrics/Ex103")

#Import Data
ps3=read.csv("ps3.csv")
# Q1
# create a dummy var for employment
# including absence from work
#ps1$emp = as.numeric(ps1$lf_char <= 3)
#table(ps1$emp, useNA = 'always')

#check "schooling" var
# limit the data for men in ages 25-66
# with reported education only
ps3_new = ps3[ps3$age <= 66 & ps3$age >= 25 & !is.na(ps3$schooling) ,]
describe(ps3_new)

# Q2
# new var for selection if the obs is salaried worker
ps3_new$selection = as.numeric(ps3_new$status== 1|ps3_new$status== 2 )
table(ps3_new$selection, useNA = 'always')


# Q3
ps3_new$agesq = (ps3_new$age)^2
mincerModel<-lm(lwage_2011 ~ age + agesq + schooling + arab, ps3_new)
options(scipen=999, digits=4)
summary (mincerModel)

jewishSchooling<-lm( ps3_new$lwage_2011[ps3_new$arab == 0]~ps3_new$schooling[ps3_new$arab == 0])
summary(jewishSchooling)
arabSchooling<-lm( ps3_new$lwage_2011[ps3_new$arab == 1]~ps3_new$schooling[ps3_new$arab == 1])
summary(arabSchooling)
describe(ps3_new$arab)
plot(ps3_new$lwage_2011[ps3_new$arab == 0]~ps3_new$schooling[ps3_new$arab == 0],main="School Years~Monthly Work Hours" , xlab="School Years", ylab="Monthly Wrok Hours",
     col="blue4", xlim=c(0, 30), ylim=c(0, 8), pch=5, cex=c(0.9,0))
points(ps3_new$lwage_2011[ps3_new$arab == 1]~ps3_new$schooling[ps3_new$arab == 1],main="School Years~Monthly Work Hours" , xlab="School Years", ylab="Monthly Wrok Hours",
     col="firebrick1")#, xlim=c(0, 27), ylim=c(0, 600), pch=5, cex=c(0.9,0))
abline( col="deepskyblue3", lwd=3,coef(jewishSchooling))
abline( col="firebrick1", lwd=3,coef(arabSchooling)) 
#we want to check how much will i work if i've studyied that much years
#add regression line:

#jewish men
#plot(newdata$WPH_ln[newdata$national=="jewish" & newdata$sex=="male"]~newdata$schooly[newdata$national=="jewish" & newdata$sex=="male"] ,main="School Years~Log of Wage per Hour- Jewish Men" , xlab="School Years", ylab="Log of Wage per Hour",
 #    col="aquamarine4", xlim=c(0, 25), ylim=c(0, 7), pch=11, cex=c(0.9,0)) abline( col="aquamarine3", lwd=3,coef(modelT3.JM))

#performs a 2-step Heckman (heckit) estimation that corrects for non-random sample selection.
hecmanModel <- heckit(selection ~ age + agesq + schooling + arab + children_0_4 ,lwage_2011 ~ age + agesq + schooling + arab, ps3_new)

summary(heckman)
