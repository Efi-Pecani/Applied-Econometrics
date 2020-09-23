install.packages("openxlsx")
install.packages("AER")
install.packages("zoo")
install.packages("lmtest")
install.packages("visualize")
install.packages("orcutt")
library("openxlsx")
library('psych')
library(ggplot2)
library(visualize)
library("zoo")
library("lmtest")
library("orcutt")
library("AER") 

#  Clear Objects
rm(list=ls())
#removing specific object
#rm( x )
#data$var <- NULL
######__________________________________________________________________________________________

# Change Directory
setwd("/Users/EfiPaka/Desktop/Econometrics/Ex106/")

#Import Data
ps6=read.csv("ps6.csv")

#transform data to quarters
ps6$date_q = as.yearqtr(ps6$date_q)
#restric the data to  2001q1-2018q4
ps6_new = ps6[ps6$date_q >= 2005.75 & ps6$date_q <= 2018.75,]
describe(ps6_new)


#natural log of the product

#ps6_new$log_gdp_capita=log(ps6_new$gdp_capita)

#r=i-pi=real interst rate by fisher's eq
#ps5_new$real_interest = (ps5_new$interest_rate-ps5_new$inflation)

# 1.Q1
# create a model of IS curve
model1<-lm(inflation ~ unemp, ps6_new)
summary(model1)

dwtest(model1)$statistic

# T=53
# K=2
#53.   2.  1.51833  1.59505

#1.Q3
coch <- cochrane.orcutt(model1)
summary(coch)
coch$rho
nrow(coch$model)

# 1.Q5
#lag of inf
ps6_new$l.inf[2:nrow(ps6_new)] <- ps6_new$inflation[1:(nrow(ps6_new)-1)]
#lagged model
model2 <- lm(inflation ~ unemp + l.inf, ps6_new)
summary(model2)


#ps6_new$uhat <- model1$residuals
#1.Q6
#u-hat model
ps6_new$l.uhat[2:nrow(ps6_new)] <- ps6_new$uhat[1:(nrow(ps6_new)-1)]
ps6_new$uhat[2:nrow(ps6_new)] <- model2$residuals
residmodel <- lm(uhat ~ l.uhat + 0, ps6_new)
summary(residmodel)

#Q7
#define 2st lag of inflation
ps6_new$l2.inf[3:nrow(ps6_new)]<-ps6_new$inflation[1:(nrow(ps6_new)-2)]
#new estimation with 2 lags of inf
model3 <- lm(inflation ~ unemp + l.inf+ l2.inf, ps6_new)
summary(model3)
#check for rho
dwtest(model3)$statistic


# T=51
# K=4
#51.   4.  1.42734  1.67538

coch2 <- cochrane.orcutt(model2)
summary(coch2)
coch2$rho
nrow(coch2$model)
describe(ps6)
#________________________________________________________________________
#2.Q1
ps6$date_q = as.yearqtr(ps6$date_q)
ps6 = ps6[ps6$date_q >= 2005.75 & ps6$date_q <= 2018.75,]
#define time
ps6$time <- c(1:nrow(ps6))
#defince Log gdp
ps6$log_gdp_capita=log(ps6$gdp_capita)
#calculate gap
trendmodel = lm(log_gdp_capita ~ time, ps6)
#calculating the gap of y
ps6$outputgap = trendmodel$residuals

#define 1st lag of interest
ps6$l.interest[2:nrow(ps6)]<-ps6$interest_rate[1:(nrow(ps6)-1)]
#define 2st lag of interest
ps6$l2.interest[3:nrow(ps6)]<-ps6$interest_rate[1:(nrow(ps6)-2)]

#esitmate a taylor rule model
Model_Taylor<-lm(interest_rate ~ inflation + outputgap + l.interest+ l2.interest, ps6[ps6$date_q >= 2005.75 & ps6$date_q <= 2012.75,])
summary(Model_Taylor)

#2.Q2

#define 1st lag of inflation
ps6$l.inflation[2:nrow(ps6)]<-ps6$inflation[1:(nrow(ps6)-1)]
#define 1st lag of outputgap
ps6$l.outputgap[2:nrow(ps6)]<-ps6$outputgap[1:(nrow(ps6)-1)]

#ps6$forecast_Model_Taylor[ps6$date_q > 2003.75] <- predict(base, mydata[ps6$date_q > 2003.75, ])

tsls_Taylor <- (ivreg(interest_rate ~ inflation + outputgap + l.interest+ l2.interest ,~ l.inflation+ l.outputgap + l.interest +l2.interest , ps6[ps6$date_q >= 2005.75 & ps6$date_q <= 2012.75,]))
summary(tsls_Taylor)

#2.Q3

tsls_Taylor2 <- (ivreg(interest_rate ~ inflation + outputgap + l.interest ,~ l.inflation+ l.outputgap + l.interest +l2.interest  , ps6[ps6$date_q >= 2005.75 & ps6$date_q <= 2012.75,]))
summary(tsls_Taylor2)

#2.Q4
tsls_Taylor3 <- (ivreg(interest_rate ~ inflation + outputgap + l.interest+ l2.interest+l.inflation ,~ l.outputgap + l.interest +l2.interest , ps6[ps6$date_q >= 2005.75 & ps6$date_q <= 2012.75,]))
summary(tsls_Taylor3)

#2.Q5
#written answer

#2.Q6
#prediction for a known period of an OLS model

ps6$forecast_MT[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75] <- predict(Model_Taylor,ps6[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75,])

#prediciton for TSLS Model

ps6$forecast_tslsMT[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75] <- predict(tsls_Taylor,ps6[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75,])

#prediction for TSLS 2nd (Restricted) Model without l2.interest

ps6$forecast_tslsMT2[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75] <- predict(tsls_Taylor2,ps6[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75,])

#create graphs for a test by "eye"
plot(ps6$date_q[ps6$date_q >= 2010.0], ps6$interest_rate[ps6$date_q >= 2010.0],type = 'o', xlab = 'Time', ylab = "Intrest", main = "Forecasted intrest rate over time",col = 'gold',pch=7,lwd=3)
lines(ps6$date_q, ps6$forecast_MT, type = 'o', col = 'forestgreen',pch=2,lwd=2)
lines(ps6$date_q, ps6$forecast_tslsMT, type = 'o', col = 'firebrick1',pch=1,lwd=2)
lines(ps6$date_q, ps6$forecast_tslsMT2,type = 'o', col = 'blueviolet',pch=5,lwd=2)
#original intrest data

#lines(emp_age$ age, emp_age$EmpProb_predict, type = 'o', col="darkslateblue",pch=5,lwd=1)
legend('topright', legend = c('Original interest Data', 'OLS model', '1st TSLS model', '2nd TSLS model (restricted)'), col = c('gold', 'forestgreen','firebrick1', 'blueviolet'),lwd=4, lty = 1, cex= 0.5)

#2.Q7
#calculating RMSE
#1st -base model
ps6$error_sq_MT[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75] <- (ps6$interest_rate[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75] - ps6$forecast_MT[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75]) ^ 2 
RMSE_base=sqrt(mean(ps6$error_sq_MT, na.rm="true"))
RMSE_base
#2nd model
ps6$error_sq_MT1[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75] <- (ps6$interest_rate[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75] - ps6$forecast_tslsMT[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75]) ^ 2 
RMSE_TSLS1=sqrt(mean(ps6$error_sq_MT1, na.rm="true"))
RMSE_TSLS1
#3rd model
ps6$error_sq_MT2[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75] <- (ps6$interest_rate[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75] - ps6$forecast_tslsMT2[ps6$date_q >= 2013.0 & ps6$date_q <= 2018.75]) ^ 2 
RMSE_TSLS2=sqrt(mean(ps6$error_sq_MT2, na.rm="true"))
RMSE_TSLS2

