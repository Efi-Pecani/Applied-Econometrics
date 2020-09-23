install.packages("openxlsx")
install.packages("AER")
install.packages("zoo")
install.packages("lmtest")
install.packages("visualize")
install.packages("orcutt")
install.packages("data.table")
install.packages("dummies")
library("openxlsx")
library('psych')
library(ggplot2)
library(visualize)
library("zoo")
library("lmtest")
library("orcutt")
library("AER") 
library("dummies")
library("data.table")

#  Clear Objects
rm(list=ls())
#removing specific object
#rm( x )
#data$var <- NULL
######__________________________________________________________________________________________

# Change Directory
setwd("/Users/EfiPaka/Desktop/Econometrics/GINI/")


#---------------------------------------------------------------
# ind data
ind=read.delim("IND2017.txt")

######__________________________________________________________________________________________
table(ind$ecrel)
ind=ind[ind$ecrel==1, ]
names(ind)
ind=ind[  , c(2, 8,9,10,11,19,20,21)]
#---------------------------------------------------------------

#---------------------------------------------------------------
# mb data
mb=read.delim("MB2017.txt")
names(mb)
mb=mb[ , c(2, 7, 8, 64, 66, 100)]
#---------------------------------------------------------------

#---------------------------------------------------------------
# merge ind data and MB data hhnum
data=merge(ind, mb, by="hhnum")
names(data)
#---------------------------------------------------------------

#---------------------------------------------------------------
# Check merge is right
ind[ind$hhnum==18  , ]
mb[mb$hhnum==18  , ]
data[data$hhnum==18  , ]
#---------------------------------------------------------------

# Analysis
#---------------------------------------------------------------

#Q3
# Calculate mean Consumption & mean Income by Decile
data_jewish=data[data$national==1,] #create data for 2 sectors
data_arab=data[data$national==2,]


d1<-as.data.table(data) 
d1[ , mean(c3), by="decile"]   
d1[, list(Consumption=mean(c3), Income=mean(incmonet)), by="decile"]
table1=d1[, list(Consumption=mean(c3), Income=mean(incmonet)), by="decile"]
table1[order(decile)]
#---------------------------------------------------------------

#---------------------------------------------------------------
# Cut Decile to Hamishon: 
data$Hamishon=cut(data$decile, breaks=c(0,2,4,6,8,10), labels=c(1,2,3,4,5))  # (a,b)  
data$hamC3=cut(data$c3, breaks=c(0,2,4,6,8,10), labels=c(1,2,3,4,5))  # (a,b)  #tsrija
table(data$Hamishon, data$decile)


d_jewish<-as.data.table(data_jewish)  #tosefet
d_jewish[ , mean(c3), by="decile"]   
d_jewish[, list(Consumption=mean(c3), Income=mean(incmonet)), by="Hamishon"]
table_j=d_jewish[, list(Consumption=mean(c3), Income=mean(incmonet)), by="Hamishon"]
table_j[order(Hamishon)]

d_arab<-as.data.table(data_arab)  #tosefet
d_arab[ , mean(c3), by="decile"]   
d_arab[, list(Consumption=mean(c3), Income=mean(incmonet)), by="Hamishon"]
table_a=d_arab[, list(Consumption=mean(c3), Income=mean(incmonet)), by="Hamishon"]
table_a[order(Hamishon)]


#---------------------------------------------------------------
#statistical description

#  entire households
describe(d1[ ,c("c3", "incmonet")])
# jewish sector
describe(d_jewish[ ,c("c3", "incmonet")])
# arab sector
describe(d_arab[ ,c("c3", "incmonet")])

#---------------------------------------------------------------

# Q 3.2
# Quantile for each
quantile(data$c3, c(0.2, 0.4, 0.6, 0.8, 1))  # entire households
quantile(data$incmonet, c(0.2, 0.4, 0.6, 0.8, 1))
quantile(d_arab$c3, c(0.2, 0.4, 0.6, 0.8, 1))  # arab sector
quantile(d_arab$incmonet, c(0.2, 0.4, 0.6, 0.8, 1))
quantile(d_jewish$c3, c(0.2, 0.4, 0.6, 0.8, 1))  # jewish sector
quantile(d_jewish$incmonet, c(0.2, 0.4, 0.6, 0.8, 1))


describe(data$Hamishon)
table(data$Hamishon[data$national==1],data$decile[data$national==1]) #jewish sector
table(data$Hamishon[data$national==2], data$decile[data$national==2]) #arab sector

#---------------------------------------------------------------
# Create Hamishon by percentiles of Income to nefesh:  Hajnasa
data$inc=data$incmonet/data$hhprsns
quantile(data$inc,c(0.20,0.40,0.60,0.80,1.0))
data$Hamishon_my=cut(data$inc, breaks=c(0,quantile(data$inc, 0.20),quantile(data$inc, 0.40),quantile(data$inc, 0.60),quantile(data$inc, 0.80),quantile(data$inc, 1.0)), labels=c(1,2,3,4,5))
table(data$Hamishon_my)
table(data$Hamishon_my[data$national==1]) #jewish sector
table(data$Hamishon_my[data$national==2]) #arab sector
#---------------------------------------------------------------


#---------------------------------------------------------------
# Calculate mean Consumption & mean Income by Hamishon for each group
summary_all = d1[ , list(Consumption=mean(c3), Income=mean(incmonet)), by="Hamishon"]
summary_arab = d_arab[ , list(Consumption=mean(c3), Income=mean(incmonet)), by="Hamishon"]
summary_jewish = d_jewish[ , list(Consumption=mean(c3), Income=mean(incmonet)), by="Hamishon"]
summary_all[order(Hamishon)]
summary_arab[order(Hamishon)]
summary_jewish[order(Hamishon)]

#---------------------------------------------------------------
# Create dummy variables with values 1 / 0 
data=cbind(data, dummy(data$diploma, sep="_"))
names(data)
colnames(data)[18]="diploma_BA"
colnames(data)[19]="diploma_MA"
#---------------------------------------------------------------

# Q 3.3
#Lorenz Curve and GINI calculation

# inc deciles for all households
d1$inc_decile=cut(d1$inc, breaks=c(0,quantile(d1$inc, 0.10),
                                                 quantile(d1$inc, 0.20),
                                                 quantile(d1$inc, 0.30),
                                                 quantile(d1$inc, 0.40),
                                                 quantile(d1$inc, 0.50),
                                                 quantile(d1$inc, 0.60),
                                                 quantile(d1$inc, 0.70),
                                                 quantile(d1$inc, 0.80),
                                                 quantile(d1$inc, 0.90),
                                                 quantile(d1$inc, 1.0)),
                         labels=c(1,2,3,4,5,6,7,8,9,10))
# Values for Lorenz Curve and Gini Coefficient - entire households
inc_sums=d1[, list(Total_Income=sum(inc)), by="inc_decile"]
inc_sums[order(inc_decile)]  
# inc deciles for arab households
d_arab$inc_decile=cut(d_arab$inc, breaks=c(0,quantile(d_arab$inc, 0.10),
                                                   quantile(d_arab$inc, 0.20),
                                                   quantile(d_arab$inc, 0.30),
                                                   quantile(d_arab$inc, 0.40),
                                                   quantile(d_arab$inc, 0.50),
                                                   quantile(d_arab$inc, 0.60),
                                                   quantile(d_arab$inc, 0.70),
                                                   quantile(d_arab$inc, 0.80),
                                                   quantile(d_arab$inc, 0.90),
                                                   quantile(d_arab$inc, 1.0)),
                          labels=c(1,2,3,4,5,6,7,8,9,10))
# Values for Lorenz Curve and Gini Coefficient - arab households
inc_sums_arab=d_arab[, list(Total_Income=sum(inc)), by="inc_decile"]
inc_sums_arab[order(inc_decile)] 
# inc deciles for jewish households
d_jewish$inc_decile=cut(d_jewish$inc, breaks=c(0,quantile(d_jewish$inc, 0.10),
                                                       quantile(d_jewish$inc, 0.20),
                                                       quantile(d_jewish$inc, 0.30),
                                                       quantile(d_jewish$inc, 0.40),
                                                       quantile(d_jewish$inc, 0.50),
                                                       quantile(d_jewish$inc, 0.60),
                                                       quantile(d_jewish$inc, 0.70),
                                                       quantile(d_jewish$inc, 0.80),
                                                       quantile(d_jewish$inc, 0.90),
                                                       quantile(d_jewish$inc, 1.0)),
                            labels=c(1,2,3,4,5,6,7,8,9,10))
# Values for Lorenz Curve and Gini Coefficient - jewish households
inc_sums_jewish=d_jewish[, list(Total_Income=sum(inc)), by="inc_decile"]
inc_sums_jewish[order(inc_decile)] 
#---------------------------------------------------------------
A1=mean(data$c3[data$national == 2])
J1=mean(data$c3[data$national == 1])
consumption=mean(data$c3)
APC_J=(J1/consumption)
print(APC_J)
APC_A=(A1/consumption)
print(APC_A)

# APC total
avg_inc=mean(data$incmonet)
avg_con=mean(data$c3) 
APC=avg_con/avg_inc
APC


#APC jewish sector
avg_incJ=mean(data$incmonet[data$national==1])
avg_conJ=mean(data$c3[data$national==1]) 
APC_J=avg_conJ/avg_incJ
APC_J

#APC arab sector
avg_incA=mean(data$incmonet[data$national==2])
avg_conA=mean(data$c3[data$national==2]) 
APC_A=avg_conA/avg_incA
APC_A
#_________________________________________________________________________

#Q 3.4
# Regressions 
mean(data$incmonet)
model1=lm(data$c3~incmonet , data)
summary(model1)

arabSector = lm(c3~incmonet, data_arab)
jewSector = lm(c3~incmonet, data_jewish)
summary(arabSector)
summary(jewSector)


model2=lm(c3~incmonet+schooly , data)
summary(model2)
coef(model2)
#_____________________________________________________________

#Q 4
sumModel1=summary(model1)$coefficient[c("incmonet","(Intercept)"),"Estimate"]
m.inc=mean(data$incmonet)
plus1000 = mean(data$incmonet) + 1000
(sum(sumModel1 * c(plus1000,1)))
(sum(sumModel1 * c(m.inc,1)))
#____________________________________________________
# Q.5
colnames(data)

# adding hhprsns schooly & age to the regression model 
# describing the vars
describe(data$hhprsns)
describe(data$age)
describe(data$schooly)
# Marital has dummies therefore "single" will be the basis variable
data$agesq=(data$age)^2
options(scipen=9999)
model2 = lm(c3 ~ incmonet +age+ agesq + hhprsns + schooly , data)
summary(model2)

qf(0.95, df1=5, df2=9011)

#Q 8

model3 = lm(c3 ~ incmonet + hhprsns , data)
summary(model3)

anova(model2,model3)
qf(0.95, df1=3, df2=9011)

# Q 9 
#correalation matrix
cor(subset(data, select = c(incmonet ,age, agesq , hhprsns, schooly)), use="complete.obs")


# Q10

model4 = lm(c3 ~ incmonet +age+ agesq + schooly , data)
summary(model4)

qf(0.95, df1=5, df2=9011)
anova(model4,model2)

#Q 11 - wrriten
#---------------------------------------------------------------
# Q.12 Chow Fisher test
data_arab$agesq=(data_arab$age)^2
data_jewish$agesq=(data_jewish$age)^2


model5_arab = lm(c3 ~ incmonet +age+ agesq + schooly, data_arab)
summary(model5_arab)
model6_jew = lm(c3 ~ incmonet +age+ agesq + schooly, data_jewish)
summary(model6_jew)

# RSS in each
anova(model5_arab)["Residuals", 2]
anova(model6_jew)["Residuals", 2]
anova(model4)["Residuals", 2] #restricted model


qf(0.95, df1=10, df2=8627)
####


plot(c3~incmonet,data,xlim=c(0, 60000) ,pch=6, cex=c(0.5,0),col="dodgerblue")





