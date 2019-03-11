set.seed(123)

#load the data set 

cr_train<-read.csv("Credit_Risk_Train_data.csv",na.strings = "")
cr_valid<-read.csv("Credit_Risk_Validate_data.csv",na.strings="")
cr_test<-read.csv("Credit_Risk_Test_data.csv",na.strings="")

#check the structure of the dataset

str(cr_train)
str(cr_test)
str(cr_valid)

#check the summary of the dataset

summary(cr_train)
summary(cr_valid)
summary(cr_test)

#Imputation 

#for categorical variables we will be using mode 

table(cr_train$Gender)
cr_train[which(is.na(cr_train$Gender)),"Gender"]<-"Male"

table(cr_train$Married)
cr_train[which(is.na(cr_train$Married)),"Married"]<-"Yes"

cr_train[which(is.na(cr_train$Dependents)),"Dependents"]<-"0"

cr_train[which(is.na(cr_train$Education)),"Education"]<-"Graduate"

cr_train[which(is.na(cr_train$Self_Employed)),"Self_Employed"]<-"No"

cr_train[which(is.na(cr_train$Credit_History)),"Credit_History"]<-"1"

#for numerical

cr_train[which(is.na(cr_train$LoanAmount)),"LoanAmount"]<-median(cr_train$LoanAmount,na.rm = T)

cr_train[which(is.na(cr_train$Loan_Amount_Term)),"Loan_Amount_Term"]<-median(cr_train$Loan_Amount_Term,na.rm = T)


summary(cr_train)

#for categorical variables in test data set

summary(cr_test)

cr_test[which(is.na(cr_test$Gender)),"Gender"]<-"Male"
cr_test[which(is.na(cr_test$Married)),"Married"]<-"Yes"
cr_test[which(is.na(cr_test$Dependents)),"Dependents"]<-"0"
cr_test[which(is.na(cr_test$Education)),"Education"]<-"Graduate"
cr_test[which(is.na(cr_test$Self_Employed)),"Self_Employed"]<-"No"
cr_test[which(is.na(cr_test$Credit_History)),"Credit_History"]<-"1"


#for numerical variables in test dataset
cr_test[which(is.na(cr_test$LoanAmount)),"LoanAmount"]<-median(cr_test$LoanAmount,na.rm = T)
cr_test[which(is.na(cr_test$Loan_Amount_Term)),"Loan_Amount_Term"]<-median(cr_test$Loan_Amount_Term,na.rm = T)


summary(cr_test)

#for categorical variables in validation data set
summary(cr_valid)

cr_valid[which(is.na(cr_valid$Gender)),"Gender"]<-"Male"
cr_valid[which(is.na(cr_valid$Married)),"Married"]<-"Yes"
cr_valid[which(is.na(cr_valid$Dependents)),"Dependents"]<-"0"
cr_valid[which(is.na(cr_valid$Education)),"Education"]<-"Graduate"
cr_valid[which(is.na(cr_valid$Self_Employed)),"Self_Employed"]<-"No"
cr_valid[which(is.na(cr_valid$Credit_History)),"Credit_History"]<-"1"


#for numerical variables in validation dataset
cr_valid[which(is.na(cr_valid$LoanAmount)),"LoanAmount"]<-median(cr_valid$LoanAmount,na.rm = T)
cr_valid[which(is.na(cr_valid$Loan_Amount_Term)),"Loan_Amount_Term"]<-median(cr_valid$Loan_Amount_Term,na.rm = T)

summary(cr_valid)

#plotting th relationship between the input and the output variables

names(cr_train)
plot(cr_train$Gender,cr_train$Loan_Status,main="Gender vs Loan Status",col=c("yellow","green"))

plot(cr_train$Married,cr_train$Loan_Status,main="Marital status vs Loan Status",col=c("red","green"))


plot(cr_train$Dependents,cr_train$Loan_Status,main="No of Dependents vs Loan Status",col=c("orange","blue"))
plot(cr_train$Education,cr_train$Loan_Status,main="Education vs Loan Status",col=c("red","green"))
plot(cr_train$Self_Employed,cr_train$Loan_Status,main="Self_Employed vs Loan Status",col=c("blue","orange"))
boxplot(cr_train$ApplicantIncome~cr_train$Loan_Status,main="Applicant Income vs Loan Status",col=c("red","blue"),ylim=c(1,15000))
boxplot(cr_train$CoapplicantIncome~cr_train$Loan_Status,main="Co-Applicant income vs Loan Status",col = c("red","green"))
boxplot(cr_train$LoanAmount~cr_train$Loan_Status,main="Loan Amount vs Loan Status",col=c("yellow","orange"))
plot(as.factor(cr_train$Loan_Amount_Term),cr_train$Loan_Status)
plot(cr_train$Property_Area,cr_train$Loan_Status,col=c("red","green"))

#load the e1071 library
install.packages("e1071")
library(e1071)

svm_model1<-svm(Loan_Status ~ .-Loan_ID,data = cr_train,kernel="linear")

summary(svm_model1)

#Confusion matrix
pred_1<-predict(svm_model1,newdata=cr_test)


table(pred_1,cr_valid$outcome)

#check the accuracy ratio
acc<-(58+289)/(58+1+19+289)

acc


#implementation of SVM using te caret

install.packages("caret")
library(caret)


#perform data transformation for cr_train as the caret requires the input in numericals

cr_train$Gender<-as.numeric(ifelse(cr_train$Gender=="Male",1,0))
cr_train$Married<-as.numeric(ifelse(cr_train$Married=="Yes",1,0))
cr_train$Education<-as.numeric(ifelse(cr_train$Education=="Graduate",1,0))
cr_train$Self_Employed<-as.numeric(ifelse(cr_train$Self_Employed=="Yes",1,0))
cr_train$Property_Area_Rural<-as.numeric(ifelse(cr_train$Property_Area=="Rural",1,0))
cr_train$Property_Area_Urban<-as.numeric(ifelse(cr_train$Property_Area=="Urban",1,0))
cr_train$Dependents<-as.numeric(ifelse(as.character(cr_train$Dependents)=="3+",3,as.character(cr_train$Dependents)))
cr_train$Credit_History<-as.numeric(cr_train$Credit_History)


cr_train$Loan_ID<-NULL
cr_train$Property_Area<-NULL

str(cr_train)
str(cr_test)
table(cr_train$Dependents)

#perform data transformation for cr_valid as the caret requires the input in numericals

cr_valid$Gender<-as.numeric(ifelse(cr_valid$Gender=="Male",1,0))
cr_valid$Married<-as.numeric(ifelse(cr_valid$Married=="Yes",1,0))
cr_valid$Education<-as.numeric(ifelse(cr_valid$Education=="Graduate",1,0))
cr_valid$Self_Employed<-as.numeric(ifelse(cr_valid$Self_Employed=="Yes",1,0))
cr_valid$Property_Area_Rural<-as.numeric(ifelse(cr_valid$Property_Area=="Rural",1,0))
cr_valid$Property_Area_Urban<-as.numeric(ifelse(cr_valid$Property_Area=="Urban",1,0))
cr_valid$Dependents<-as.numeric(ifelse(as.character(cr_valid$Dependents)=="3+",3,as.character(cr_train$Dependents)))
cr_valid$Credit_History<-as.numeric(cr_valid$Credit_History)


cr_valid$Loan_ID<-NULL
cr_valid$Property_Area<-NULL

str(cr_valid)

#perform data transformation for cr_test as the caret requires the input in numericals

cr_test$Gender<-as.numeric(ifelse(cr_test$Gender=="Male",1,0))
cr_test$Married<-as.numeric(ifelse(cr_test$Married=="Yes",1,0))
cr_test$Education<-as.numeric(ifelse(cr_test$Education=="Graduate",1,0))
cr_test$Self_Employed<-as.numeric(ifelse(cr_test$Self_Employed=="Yes",1,0))
cr_test$Property_Area_Rural<-as.numeric(ifelse(cr_test$Property_Area=="Rural",1,0))
cr_test$Property_Area_Urban<-as.numeric(ifelse(cr_test$Property_Area=="Urban",1,0))
cr_test$Dependents<-as.numeric(ifelse(as.character(cr_test$Dependents)=="3+",3,as.character(cr_train$Dependents)))
cr_test$Credit_History<-as.numeric(cr_test$Credit_History)

cr_test$Loan_ID<-NULL
cr_test$Property_Area<-NULL

str(cr_test)

#SVM implementation using the package caret
head(cr_train)
install.packages("caret")
library(caret)
head(cr_train)
head(cr_train[,-11])


svm_model2<-train(cr_train[,-11],cr_train$Loan_Status,method = "svmLinear")

head(cr_test)

pred_on_valid<-predict(svm_model2,newdata = cr_valid[,-11])

pred_on_valid
table(cr_valid$outcome,pred_on_valid)

pred_2<-predict(svm_model2,newdata = cr_test)
pred_2
# Confusion matrix is a function of the caret package
confusionMatrix(cr_valid$outcome,pred_2)

#final report


cr_test<-read.csv("Credit_Risk_Test_data.csv",na.strings="")

report<-data.frame(cr_test[,1],pred_2)

report
