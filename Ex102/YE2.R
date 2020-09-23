install.packages("openxlsx")
install.packages("zoo")
install.packages("lmtest")
library("openxlsx")
library('psych')
library(ggplot2)
install.packages("visualize")
library(visualize)
library("zoo")
library("lmtest")

#  Clear Objects
rm(list=ls())
#removing specific object
#rm( x )
#data$var <- NULL
######__________________________________________________________________________________________

# Change Directory
setwd("/Users/EfiPaka/Desktop/Econometrics/Ex102")

#Import Data
ps1=read.csv("ps2.csv")

# Q1
# create a dummy var for employment
# including absence from work
ps1$emp = as.numeric(ps1$lf_char <= 3)
table(ps1$emp, useNA = 'always')

#check "schooling" var 
summary(ps1$schooling)
# limit the data for men in ages 25-66
# with reported education only
ps1_new = ps1[ps1$age <= 66 & ps1$age >= 25 & !is.na(ps1$schooling) ,]
describe(ps1_new)

# Q2
# new var of the age squared
ps1_new$agesq = (ps1_new$age)^2

# new vars for marital status
ps1_new$married= as.numeric(ps1_new$marital ==1)
ps1_new$divorced= as.numeric(ps1_new$marital ==2)
ps1_new$widowed=as.numeric(ps1_new$marital ==3)
ps1_new$single=as.numeric(ps1_new$marital ==4)
ps1_new$seperated=as.numeric(ps1_new$marital ==5)

# create a Probability Linear Model
LPModel<-lm( emp ~ age + agesq + schooling + married + divorced + widowed +seperated , ps1_new)
options(scipen=999, digits=4)
summary (LPModel)

# Q3
ps1_new$EmpLPM_predict<- LPModel$fitted.values #multiplays x ?? B = any number is possible here 
ps1_new$EmpLPM_res<- LPModel$residuals # 1-p

plot (ps1_new$EmpLPM_predict, ps1_new$EmpLPM_res, main= "Probablitites and Residuals", xlab ="Probability", ylab = "Residuals" ,col= "firebrick1")

# Q4
#Logit model
LogitModel<-glm( emp ~ age + agesq + schooling + married + divorced + widowed +seperated , ps1_new, family="binomial" (link="logit"))
summary (LogitModel)

#Probit model
ProbitModel<-glm( emp ~ age + agesq + schooling + married + divorced + widowed +seperated , ps1_new, family="binomial" (link="probit"))
summary (ProbitModel)

#build coeffs table
coeffs <- data.frame(Lpm = LPModel$coefficients, Logit = LogitModel$coefficients, Probit = ProbitModel$coefficients)
View(coeffs)

# Q5

#calculate predictions logit
ps1_new$EmpLog_predict<- LogitModel$fitted.values #multiplays B(g(xb)) - 0<P<1
ps1_new$EmpLog_res<- LogitModel$residuals # 1-p 

#calculate predictions probit
ps1_new$EmpProb_predict<- ProbitModel$fitted.values #multiplays B(g(xb)) - 0<P<1
ps1_new$EmpProb_res<- ProbitModel$residuals # 1-p 

#aggregates the avgs of all combinations of the vars by the var age 
emp_age<-aggregate(ps1_new[c("emp","EmpLPM_predict","EmpLog_predict", "EmpProb_predict")], by=list(age=ps1_new$age),FUN=mean)

plot(emp_age$age, emp_age$emp, type = 'o', xlab = 'Age', ylab = "Probability", main = "Prob. of employment by age ",col = 'gold',pch=7,lwd=3)
lines(emp_age$ age, emp_age$EmpLPM_predict, type = 'o', col = 'firebrick1',pch=1,lwd=2)
lines(emp_age$ age, emp_age$EmpLog_predict, type = 'o', col = 'forestgreen',pch=2,lwd=2)
lines(emp_age$ age, emp_age$EmpProb_predict, type = 'o', col="darkslateblue",pch=5,lwd=1)
legend('topright', legend = c('Employment', 'LPM model', 'Logit model', 'Probit model'), col = c('gold', 'firebrick1', 'forestgreen', 'darkslateblue'),lwd=4, lty = 1, cex= 0.5)

# Q6 in excel
# Q7
#Derivative of the probability in excel

# Q8
#create an interaction var
ps1_new$arab_schooling <- (ps1_new$schooling * ps1_new$arab)
ps1_new$haredi_schooling <- (ps1_new$schooling * ps1_new$yeshiva)

#logit model inorder to check the hypothesis
LogitBySector_schoolingEffect <- glm( emp ~ age + agesq + schooling + married + divorced + widowed +seperated +arab +yeshiva + arab_schooling + haredi_schooling , ps1_new, family = "binomial"(link="logit"))
summary(LogitBySector_schoolingEffect)
#likliyhood test
lrtest(LogitModel,LogitBySector_schoolingEffect)

#rm( Logit_Schooling_Model )
Logit_Schooling_Model <- glm(emp ~ age + agesq + schooling + arab_schooling + haredi_schooling , ps1_new, family = "binomial"(link="logit"))
summary(Logit_Schooling_Model)






