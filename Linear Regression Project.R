# set the WD

setwd("D:/")

rm(list = ls())

#sset the random number generation to the fixed values

set.seed(2)

#Read the data set loss given default

lgd<- read.csv("Loss_Given_Default.csv",header = T )

lgd<- read.csv("Loss_Given_Default.csv",header = T,stringsAsFactors = F)

str(lgd)

View(lgd)

#summary of the dataset

summary(lgd)

#change the catagorical variables (Geander and Marital status)

lgd$Gender<-ifelse(lgd$Gender=="M",1,0)

lgd$Married<-ifelse(lgd$Married=="Married",1,0)

#check the gender variable has changed in numeric

str(lgd)

#plot relationship graph of all variables

pairs(lgd)

cor(lgd)

cor(lgd$Gender,lgd$Losses.in.Thousands)

cor(lgd$Age,lgd$Losses.in.Thousands)

cor(lgd$Years.of.Experience,lgd$Losses.in.Thousands)

cor(lgd$Number.of.Vehicles,lgd$Losses.in.Thousands)

cor(lgd$Married,lgd$Losses.in.Thousands)

#create training and testing dataset

nrow(lgd)

sampl<-sample(1:nrow(lgd),0.7*(nrow(lgd)))

#segregating the training dataset and the testing dataset

lgd_train<-lgd[sampl,]

nrow(lgd_train)

lgd_test<-lgd[-sampl,]

nrow(lgd_test)

#correlation check

cor_check<-cor(lgd_train)
cor_check

#install package
install.packages("corrplot")
library(corrplot)

#plot the lower trangle/upper trangle

corrplot(cor_check,type="lower",method = "circle")

corrplot(cor_check,type = "lower",method = "number")

args(lm)

#applying the linear regression
names(lgd_train)

#creating model with all the variables

lin_mod<-lm(Losses.in.Thousands ~ .,data = lgd_train)

summary(lin_mod)

#creating model without Ac_No

lin_mod1<-lm(Losses.in.Thousands ~., -Ac_No,data = lgd_train)

summary(lin_mod1)

lin_mod1<-lm(Losses.in.Thousands ~.,-c(Ac_No),data = lgd_train)

#check for multicollinearity and drop the factor which is contributing to mutlicolinearity

install.packages("car")
library(car)

vif(lin_mod1)

#residual analysis

plot(lin_mod1,1)#check for linearity assumption

plot(lin_mod1,2)#normality check

plot(lin_mod1,3)#homoscedasticity....slope

plot(lin_mod1,4)

plot(lin_mod1,5)

#sum of all residual is zero

sum(lin_mod1$residuals)

#examing univariate distribution

hist(lin_mod1$residuals)

hist(lin_mod1$residuals,col = "red",xlab="Residuals of linear regression model",main = "Distribution of residuals")

plot(density(lin_mod1$residuals))

boxplot(lin_mod1$residuals)

#prediction

pred_test<-predict(lin_mod1, newdata = lgd)

View(pred_test)

errors_pred_lin_model1<-(lgd_test$Losses.in.Thousands)-(pred_test)

View(errors_pred_lin_model1)
