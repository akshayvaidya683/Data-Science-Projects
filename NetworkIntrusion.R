
set.seed(123)

gc()

#read the dataset

nw_train<-read.csv("Net_train_data.csv")
nw_test<-read.csv("Net_test-data.csv")
nw-validate<-read.csv("Net-valid_data.csv")

#column 1 to 9 gives details about the features of the packet
colnames(nw_train)[1:9]

#column 10 to 11 gives details about the contents features
colnames(nw_train)[10:22]

#columns 22 to 31 employ the traffic features with 2 sec of time window
colnames(nw_train)[22:31]

#32-41 columns employ the hosy base features
colnames(nw_train)[32:41]

#check the structure
str(nw_train)
str(nw_test)
str(nw_validate)

#check Summary
summary(nw_train)
summary(nw_test)
summary(nw_validate)

#use the below function of train dataset
dim(nw_train)
head(nw_train)
str(nw_train)
summary(nw_train)

#plot the relationship between discrate variables and output variables
plot(nw_train$protocol_type,nw_train$class,main = "Protocol Type vs Class",xlab = "Protocol",ylab = "Class",col=c("red","green"))
plot(nw_train$service,nw_train$class,main = "Service vs Class",xlab = "Service",ylab = "Class",col=c("blue","grey"))
plot(nw_train$flag,nw_train$class,main = "Flag vs Class",xlab = "Flag",ylab = "Class",col=c("grey","blue"))

plot(as.factor(nw_train$logged_in),nw_train$class,xlab = "Logged in",ylab = "Class",main="Logged In vs Class",col=c("red","green"))
plot(as.factor(nw_trainis_host_login),nw_train$class,xlab = "Is Host Logged In",ylab = "Class",main = "Is Host Logged In vs Class",col=c("green","blue"))



#create the cart model using rpart
names(nw_train)
library(rpart)

cart_mod<-rpart(class ~.,data = nw_train,method = "class")

summary(cart_mod)

#plotting the decision tree
plot(cart_mod,margin=0.01)
text(cart_mod,use.n= T,pretty= T, cex=1)


#do prediction on the validation data using the above model
pred_on_valid<-predict(cart_mod,newdata = nw_validate,method="class")

#resolve the error
levels(nw_validate$service)=levels(nw_train$service)

#again performe the prediction on the validation dataset using the model decision tree
pred_on_valid<-predict(cart_mod.newdata = nw_validate,type= "class")
table(pred_on_valid)
table(nw_validate$class,pred_on_valid)

#calculate the accuracy ratio
accuracy_ratio_of_cart_mod<-(7672+9380)/(7672+5156+331+9380)
accuracy_ratio_of_cart_mod

#perform the predection for testing dataset
pred_on_nw_test<-predict(cart_mod,newdata= nw_test.type="class")
pred_on_nw_test

#rectify the levels error for the test dataset
levels(nw_test$service)=levels(nw_train$service)

#performance predction for the testing dataset
pred_on_nw_test<-predict(cart_mod,newdata = nw_test,type="class")
pred_on_nw_test
table(pred_on_nw_test)

#alternate method
library(rpart.plot)
library(RColorBrewer)
prp(cart_mod, cex=0.7)

#find out cp parameter 
printcp(cart_mod)

#prune the tree using cp parameter
cart_mod_1<-prune(cart_mod,cp=0.01)
prp(cart_mod_1)

#predection on the validation data using the pruned tree
pred_on_valid_1<-predict(cart_mod_1,newdata=nw_validate,type="class")
table(pred_on_valid_1)
table(nw_validate$class,pred_on_valid_1)

accuracy_model_1<-(7672+9380)/(7672+5161+331+9380)
accuracy_model_1

#create a new model by pruning using a different cp parameter
cart_mod_2<-prune(cart_mod,cp=0.045559)

#prediction on validation using the model 2
pred_on_valid_2<-predict(cart_mod_2,newdata=nw_validate,type="class")
table(pred_on_valid_2)
table(nw_validate$class,pred_on_valid_2)

accuracy_model_2<-(9468+9195)/(9468+3365+516+9195)
accuracy_model_2

#create new model using different cp 3
cart_mod_3<-prune(cart_mod,cp=0.029975)

pred_on_valid_3<-predict(cart_mod_3,newdata=nw_validate,type="class")
table(pred_on_valid_3)
table(nw_validate$class,pred_on_valid_3)

accuracy_model_3<-(9089+9233)+(9089+3744+478+9233)
accuracy_model_3

#create new model using cp 4
cart_mod_4<-prune(cart_mod,cp=0.019927)

pred_on_valid_4<-predict(cart_mod_4,newdata=nw_validate,type="class")
table(pred_on_valid_4)
table(nw_validate$class,pred_on_valid_4)

accuracy_model_4<-(9089+9233)/(9089+3744+478+9233)
accuracy_model_4

#create new model using cp 5
cart_mod_5<-prune(cart_mod,cp=0.010389)

pred_on_valid_5<-predict(cart_mod_5,newdata=nw_validate,type="class")
table(pred_on_valid_4)
table(nw_validate$class,pred_on_valid_5)

accuracy_model_5<-(7672+9380)/(7672+5161+331+9380)
accuracy_model_5

#plotting
prp(cart_mod_1)
prp(cart_mod_2)
prp(cart_mod_3)
prp(cart_mod_4)
prp(cart_mod_5)

#put the
result<-data.frame(Duration=nw_test$duration,Protocal_type=nw_test$protocal_type,Service=nw_test$service,
                   Flag=nw_test$flag,Predict_class_pred_on_nw_test_1)

head(results,10)

write.csv(results,"Network_Anomaly_Dection.csv",row.names = F)

#Random forest

install.packages("randomForest")
library(randomForest)

ran_forest_mod<-randomForest(class ~.,data= nw_train,method="class")

#Rectify the error
#randomForest can't handle more than 53 categorical variable
table(as.factor(nw_train$service))

str(nw_train)
nw_train$service<-as.numeric(nw_train$service)
nw_test$service<-as.numeric(nw_test$service)
nw_validate$service<-as.numeric(nw_validate$service)

ran_forest_mod<-randomForest(class ~ .,data = nw_train,method = "class")

ran_forest_mod_22<-ran_forest_mod(class ~.,data = nw_train.method = "class",ntree=50)

#apply the random forest model on the validation dataset

pred_rnd_forest<-predict(ran_forest_mod,newdata = nw_validate,type = "class")

#create the confusion matrix
table(nw_validate$class,pred_rnd_forest)


ran_forest_mod_accuracy<-(8138+9444)/(9138+4695+267+9444)

ran_forest_mod_accuracy

#identification of important variables
varImpPlot(ran_forest_mod)

ran_forest_mod

#do the predictionon the test dataset

pred_rnd_forest_test_data<-predict(ran_forest_mod,newdata = nw_test,type="class")

#store the results in new dataframe called network intrusion random forest

Network_Intrusion_RF<-data.frame(Duration=nw_test$duration,Protocal=nw_test$protocal_type,
                                 service=as.factor(nw_test$service),Flag=nw_test$flag,
                                 Predict_Class=pred_rnd_forest_test_data)

head(Network_Intrusion_RF)








