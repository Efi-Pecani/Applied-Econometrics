install.packages("openxlsx")
library("openxlsx")
library('psych')
library(ggplot2)
install.packages("visualize")
library(visualize)

#  Clear Objects
rm(list=ls())
#removing specific object
#rm( x )
######__________________________________________________________________
# Change Directory
setwd("/Users/EfiPaka/Desktop/Econometrics/Ex101")

#Import Data
ps1=read.csv("ps1.csv")

#Q1
#ps1$emp -creates a dummy var for work
ps1$emp = as.numeric(ps1$lf_char <= 3)
table(ps1$emp, useNA = 'always')
#ps1$empfull -cerates a dummy var if the obs works 35 hs
ps1$empfull = as.numeric(ps1$lf_char <= 3 & ps1$week_hours >= 35 & !is.na(ps1$week_hours))
table(ps1$empfull, useNA = 'always')

#Q2
#defining a categorical var
ps1$cert_group[ps1$highest_cert <= 2 | ps1$highest_cert == 9 | is.na(ps1$highest_cert)] = 1
ps1$cert_group[ps1$highest_cert == 3] = 2
ps1$cert_group[ps1$highest_cert == 4 | ps1$highest_cert == 8] = 3
ps1$cert_group[ps1$highest_cert == 5] = 4
ps1$cert_group[ps1$highest_cert == 6 | ps1$highest_cert == 7] = 5
table(ps1$cert_group, useNA = 'always')

#Q3
#defining a categorical var by: 1-arabs, 2-orthodox jews ,3-evreybody else
ps1$pop_group = ifelse(ps1$arab == 1, 1, ifelse(ps1$yeshiva == 1, 2, 3))
table(ps1$pop_group, useNA = 'always')

#Q4
ps1_new = ps1[ps1$age <= 66 & ps1$age >= 25,]
describe(ps1_new)

#numeric variables
describe(ps1_new)[c("age", "children_0_1", "children_0_17", "children_2_4", "children_5_9", 
"children_10_14", "children_15_17","schooling", "emp", "empfull"), c("mean", "sd")]

# categorical variables
table(ps1_new$marital)
prop.table(table(ps1_new$marital))

table(ps1_new$cert_group)
prop.table(table(ps1_new$cert_group))

table(ps1_new$pop_group)
prop.table(table(ps1_new$pop_group))

#Q5
#add a var of the avg employment by age
ps1_new$avgEmpByAge = ave(ps1_new$emp, ps1_new$age)

#add a var of the avg full employment (>35 hs) rate by age
ps1_new$avgFullEmpByAge = ave(ps1_new$empfull, ps1_new$age)

#order them out
ps1_new = ps1_new[order(ps1_new$age), ]
avgFullEmpByAge = aggregate(ps1_new[c("emp", "empfull")], by=list(age = ps1_new$age), FUN=mean) 

plot(ps1_new$age, ps1_new$avgEmpByAge, pch=18,type="l", ylim=c(0,1), main ="Employment Rate by Age" , xlab="Age", ylab="Employment Rate",lty=2,cex=0.5,col="blue")
lines(ps1_new$age, ps1_new$avgFullEmpByAge, pch=18,type="l", col="red")
# Add a legend

legend(48, 0.2, legend=c("employment rate", "full employment rate (>35 hs)"),
       col=c("blue", "red"), lty=2:1, cex=0.8,text.font=4, bg='lightblue')


#Q6
AvgEmpAgeCert = aggregate(ps1_new["emp"], by=list(age = ps1_new$age, cert_group = ps1_new$cert_group), FUN=mean)
AvgEmpAgeCert = reshape(AvgEmpAgeCert, v.names = "emp", idvar = "age", timevar = "cert_group", direction = "wide")

plot(AvgEmpAgeCert$age, AvgEmpAgeCert$emp.1, type = "l", ylim = c(0.2,1),main ="Employment Rate by Age and by Education", xlab = "Age", ylab = "Rate", col= "darkslateblue", lwd=2)

lines(AvgEmpAgeCert$age, AvgEmpAgeCert$emp.2, type = "l", col= "firebrick1")
lines(AvgEmpAgeCert$age, AvgEmpAgeCert$emp.3, type = "l", col= "dodgerblue4")
lines(AvgEmpAgeCert$age, AvgEmpAgeCert$emp.4, type = "l", col= "forestgreen")
lines(AvgEmpAgeCert$age, AvgEmpAgeCert$emp.5, type = "l", col = "darkorange" ,lwd=1)

legend(55, 0.35, legend=c("Secondary school - ", "Matriculation", "Non academic","B.A", "M.A +"),col= c("darkslateblue", "firebrick1", "dodgerblue4", "forestgreen", "darkorange") ,lty=1,cex=0.5,bg='lightblue',text.font=4)


#Q7
week_hour_age <- aggregate(ps1_new["empfull"], by = list(age = ps1_new$age, hour_full= ps1_new$pop_group), FUN = mean)
week_hour_age <-week_hour_age[order(week_hour_age$age),]
week_hour_age <- reshape(week_hour_age, v.names = "empfull", idvar = "age", timevar = "hour_full", direction= "wide")

#create a graph
plot(week_hour_age$age, week_hour_age$empfull.1, type = "l",main ="Full Employment Rate by Age and by Pop.Sector" , ylim = c(0,1), xlab = "Age", ylab = "Rate",col= "darkslateblue",lwd=3)
lines(week_hour_age$age, week_hour_age$empfull.2 , type = "l", col= "firebrick1",lwd=3)
lines(week_hour_age$age, week_hour_age$empfull.3, type = "l", col= "darkorange",lwd=3)
legend("topright", legend=c("Arab sector", "Orthodox Jews", "Others"),col = c("darkslateblue", "firebrick1", "darkorange") ,lty=1,cex=0.8, bg='lightblue',lwd=3)


pop_age_cert <- aggregate(ps1_new["emp"], by = list(age = ps1_new$age, pop_group= ps1_new$pop_group), FUN = mean)
pop_age_cert <- reshape(pop_age_cert, v.names = "emp", idvar = "age", timevar = "pop_group", direction= "wide")

plot(pop_age_cert$age, pop_age_cert$emp.1, type = "l",main ="Employment Rate by Age and by Pop.Sector", ylim = c(0,1), xlab = "Age", ylab = "Rate",col= "darkslateblue", lwd=3)
lines(pop_age_cert$age, pop_age_cert$emp.2, type = "l", col= "firebrick1", lwd=2)
lines(pop_age_cert$age, pop_age_cert$emp.3, type = "l", col= "darkorange", lwd=2)

legend("topright", legend=c("Arab sector", "Orthodox Jews", "Others"),col = c("darkslateblue", "firebrick1", "darkorange") ,lty=1,cex=0.5,bg='lightblue',text.font=4)




