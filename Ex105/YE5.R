install.packages("openxlsx")
install.packages("zoo")
install.packages("lmtest")
install.packages("visualize")
library("openxlsx")
library('psych')
library(ggplot2)
library(visualize)
library("zoo")
library("lmtest")
library("orcutt")

#  Clear Objects
rm(list=ls())
#removing specific object
#rm( x )
#data$var <- NULL
######__________________________________________________________________________________________

# Change Directory
setwd("/Users/EfiPaka/Desktop/Econometrics/Ex105/")

#Import Data
ps5=read.csv("ps5.csv")

#transform data to quarters
ps5$date_q = as.yearqtr(ps5$date_q)
#restric the data to  2001q1-2018q4
ps5_new = ps5[ps5$date_q >= 2001.0 & ps5$date_q <= 2018.75,]
describe(ps5_new) 

#natural log of the product

ps5_new$log_gdp_capita=log(ps5_new$gdp_capita)

#r=i-pi=real interst rate by fisher's eq
ps5_new$real_interest = (ps5_new$interest_rate-ps5_new$inflation)

# Q1
# create a model of IS curve
model1<-lm(log_gdp_capita ~ real_interest, ps5_new)
summary(model1)
#plot(log_gdp_capita ~ real_interest, ps5_new)
#abline(coef(model1))

# Q4
#adding a trend with time var
ps5_new$time <- c(1:nrow(ps5_new))
#correalation matrix for 
cor(subset(ps5_new, select = c(log_gdp_capita, real_interest, time)))

#Q5
#adding time var
model2 = lm(log_gdp_capita ~ real_interest + time, ps5_new)
summary(model2)

#Q5
trendmodel = lm(log_gdp_capita ~ time, ps5_new)
#calculating the gap of y
ps5_new$outputgap = trendmodel$residuals
model3 = lm(outputgap ~ real_interest, ps5_new)
summary(model3)

