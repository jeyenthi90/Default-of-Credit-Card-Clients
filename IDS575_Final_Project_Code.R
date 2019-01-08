#Data Import

getwd()
dir_path <-"D:/MSBA/Sem1/IDS575_Advance_Statistics/Project"
setwd(dir_path)
rm(list=ls())  
infile<-"Prob_Default.csv"
PD_Taiwan=read.csv(infile, header = TRUE, sep = ",")
colnames(PD_Taiwan)
attach(PD_Taiwan)

names(PD_Taiwan)[names(PD_Taiwan) == "default.payment.next.month"] <- "default"

#Data Exploration

class(PD_Taiwan)
dim(PD_Taiwan)
names(PD_Taiwan)
str(PD_Taiwan)
attributes(PD_Taiwan)
PD_Taiwan$default <- as.factor(PD_Taiwan$default)
summary(PD_Taiwan)
fivenum(PD_Taiwan)

barplot(table(PD_Taiwan$default), main = "Distribution of Defaulters", ylab = "Frequency",
        xlab = "Default", col = "lightblue",names.arg = c("0", "1"),ylim = c(0,25000))
library(ggplot2)
a = subset(PD_Taiwan, select = c(default,LIMIT_BAL))

b<-count(a, c("default","LIMIT_BAL"))
c<-filter(b, default == 1)

d <-c[order(c$freq,decreasing = TRUE),]

filters(b)[["default"]] <- b$default == 1
library(dplyr)
detach("package:dplyr", unload=TRUE)
ggplot(b, aes(factor(default), freq,fill = default)) + 
  geom_bar(stat="identity", position = "dodge") + xlab("Default") + ylab("Count") +
  ggtitle("Distribution of Defaulters")+
  scale_fill_brewer(palette = "Set1") + scale_y_continuous(breaks=seq(0,25000,5000))


install.packages("pastecs")
library(pastecs)
stat.desc(PD_Taiwan$BILL_AMT1, basic = TRUE)
#Missing values
colSums(is.na(PD_Taiwan))

boxplot(LIMIT_BAL~default, data=PD_Taiwan, main="LIMIT BALANCE")
PD_Taiwan[1:5,]
summary(PD_Taiwan)

a = subset(PD_Taiwan, select = c(default,SEX))
b<-count(a, c("default","SEX"))
ggplot(data = b, aes(x = SEX, y = freq, fill = default)) + 
  geom_bar(stat = "identity") + ggtitle("Distribution of Defaulters by gender")

a = subset(PD_Taiwan, select = c(default,EDUCATION))
b<-count(a, c("default","EDUCATION"))
ggplot(data = b, aes(x = EDUCATION, y = freq, fill = default)) + 
  geom_bar(stat = "identity") + ggtitle("Distribution of Defaulters by education")

a = subset(PD_Taiwan, select = c(default,MARRIAGE))
b<-count(a, c("default","MARRIAGE"))
ggplot(data = b, aes(x = MARRIAGE, y = freq, fill = default)) + 
  geom_bar(stat = "identity") + ggtitle("Distribution of Defaulters by Marital Status")

a = subset(PD_Taiwan, select = c(default,MARRIAGE))
b<-count(a, c("default","MARRIAGE"))
ggplot(data = b, aes(x = MARRIAGE, y = freq, fill = default)) + 
  geom_bar(stat = "identity") + ggtitle("Distribution of Defaulters by Marital Status")

#CrossTable

library(gmodels)
CrossTable(PD_Taiwan$EDUCATION, PD_Taiwan$default, prop.r = FALSE)

CrossTable(PD_Taiwan$MARRIAGE, PD_Taiwan$default)

CrossTable(PD_Taiwan$PAY_3, PD_Taiwan$default)


library(plyr) 
var_select = c("PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6")
count_freq = count(PD_Taiwan, var_select)

#unique values
length(unique(PD_Taiwan$LIMIT_BAL, incomparables = FALSE))

#largest freq count for limit bal
PD_Taiwan_bal <- count(PD_Taiwan, c("LIMIT_BAL"))

PD_Taiwan_bal[order(PD_Taiwan_bal$freq,decreasing = TRUE),]

CrossTable(PD_Taiwan$LIMIT_BAL, PD_Taiwan$default)


#Correlaion Matrix
library(corrplot)

PD_Taiwan_num <- subset(PD_Taiwan,select = c(PAY_0,PAY_2, PAY_3, PAY_4, PAY_5, PAY_6))
PD_Taiwan_Corr <-cor(PD_Taiwan_num)
corrplot(PD_Taiwan_Corr, method = "number")

#Transformation of vars

default<-PD_Taiwan
default_credit <- subset( default, select = -ID )
attach(default_credit)
rm(default)
class(default_credit$default.payment.next.month)
default_credit$default.payment.next.month<- as.factor(default_credit$default.payment.next.month)
class(default_credit$SEX)
default_credit$SEX <- as.factor(default_credit$SEX)
class(default_credit$EDUCATION)
default_credit$EDUCATION<- as.factor(default_credit$EDUCATION)
class(default_credit$MARRIAGE)
default_credit$MARRIAGE<- as.factor(default_credit$MARRIAGE)

#Creating new variables
default_credit$PAY_AMT1_p<-(PAY_AMT1 /LIMIT_BAL)
default_credit$PAY_AMT2_p<-(PAY_AMT2 /LIMIT_BAL)
default_credit$PAY_AMT3_p<-(PAY_AMT3 /LIMIT_BAL)
default_credit$PAY_AMT4_p<-(PAY_AMT4 /LIMIT_BAL)
default_credit$PAY_AMT5_p<-(PAY_AMT5 /LIMIT_BAL)
default_credit$PAY_AMT6_p<-(PAY_AMT6 /LIMIT_BAL)

#Correlaion Matrix
library(corrplot)

default_credit_num <- subset(default_credit,select = c(PAY_0,PAY_2, PAY_3, PAY_4, PAY_5, PAY_6,
                                             PAY_AMT1_p,PAY_AMT2_p,PAY_AMT3_p,PAY_AMT4_p,
                                             PAY_AMT5_p,PAY_AMT6_p,
                                             PAY_AMT3,PAY_AMT4,PAY_AMT5,
                                             PAY_AMT6,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,
                                             BILL_AMT5,BILL_AMT6))
default_credit_corr <-cor(default_credit_num)
corrplot(default_credit_corr, method = "number")

#Creating Development and Validation dataset

set.seed(12)
indx <- sample(2, nrow(default_credit), replace = TRUE, prob = c(0.7,0.3))
train_data <- default_credit[indx == 1,]
test_data <- default_credit[indx == 2 ,]
summary(default_credit)

class(train_data$default.payment.next.month)
train_data$default.payment.next.month<- as.factor(train_data$default.payment.next.month)
class(train_data$SEX)
train_data$SEX <- as.factor(train_data$SEX)
class(train_data$EDUCATION)
train_data$EDUCATION<- as.factor(train_data$EDUCATION)
class(train_data$MARRIAGE)
train_data$MARRIAGE<- as.factor(train_data$MARRIAGE)

class(test_data$default.payment.next.month)
test_data$default.payment.next.month<- as.factor(test_data$default.payment.next.month)
class(test_data$SEX)
test_data$SEX <- as.factor(test_data$SEX)
class(test_data$EDUCATION)
test_data$EDUCATION<- as.factor(test_data$EDUCATION)
class(test_data$MARRIAGE)
test_data$MARRIAGE<- as.factor(test_data$MARRIAGE)


# create 10 equal size folds
folds <- cut(seq(1,nrow(default_credit)),breaks=10,labels=FALSE)
table(folds)
#output
folds

#Support Vector Machines
library(caret)
attach(train_data)
svm_1<- svm(default.payment.next.month~.,
            data = train_data, scale = TRUE, kernel = "linear")

predict_test1 <- predict(svm_1, newdata = test_data, type= 'class')
confusionMatrix(predict_test1, test_data$default.payment.next.month)

## Naive Bayes
library(caret)
library(e1071)
model1 <-naiveBayes(train_data$default.payment.next.month~.,train_data)
nb_Train_Accuracy = predict(model1, newdata = train_data, type = "class")
confusionMatrix(nb_Train_Accuracy, train_data$default.payment.next.month)

nb_Test_Accuracy = predict(model1, newdata = test_data, type = "class")
table(nb_Test_Accuracy, test_data$default.payment.next.month, dnn = c("Predicted","Actual"))


predict_test1 <- predict(model1, newdata = test_data, type= 'class')
confusionMatrix(predict_test1, test_data$default.payment.next.month)

#final
nb_model <- naiveBayes(train_data$default.payment.next.month~SEX+MARRIAGE+AGE++EDUCATION
                     +PAY_0+PAY_6+BILL_AMT2+BILL_AMT4 +PAY_AMT3_p+ PAY_AMT5_p,
                     train_data, laplace = 0)
nb_Test_Accuracy = predict(nb_model, newdata = test_data, type = "class")
confusionMatrix(nb_Test_Accuracy, test_data$default.payment.next.month, mode = "everything")

nb_Train_Accuracy = predict(nb_model, newdata = train_data, type = "class")
confusionMatrix(nb_Train_Accuracy, train_data$default.payment.next.month)


model2 <- naiveBayes(train_data$default.payment.next.month~SEX+MARRIAGE+AGE++EDUCATION
                     +PAY_0+PAY_6+BILL_AMT2+BILL_AMT4 +PAY_AMT3_p+ PAY_AMT5_p,
                     train_data, laplace = 1)
nb_Test_Accuracy1 = predict(model2, newdata = test_data, type = "class")
confusionMatrix(nb_Test_Accuracy1, test_data$default.payment.next.month)

# Confusion matrix for naive bayes for CV
CV_default_n <- lapply(1:10, function(x){ 
  model <-naiveBayes(default.payment.next.month~SEX+MARRIAGE+AGE+EDUCATION+PAY_0+PAY_6+BILL_AMT2+BILL_AMT4 +PAY_AMT3_p+ PAY_AMT5_p,data = default_credit[folds != x,] )
  preds <- predict(model,  default_credit[folds == x,], type= "class")
  real <- default_credit$default.payment.next.month[folds == x]
  conf <-confusionMatrix(preds, real)
  return(data.frame(preds, real))
})
CV_default_n <- do.call(rbind, CV_default_n)
confusionMatrix(CV_default_n$preds, CV_default_n$real)

## Decision Tree

#install.packages("rpart")
library(rpart)
control= rpart.control(minsplit = 0,minbucket = 0,cp=-1)
DT_model1 <- rpart(train_data$default.payment.next.month~.,data = train_data,method = "class", control=control,parms=list(split = "gini"))
Train_Accuracy = predict(DT_model1, newdata = train_data, type = "class")
confusionMatrix(Train_Accuracy, train_data$default.payment.next.month)
Test_Accuracy = predict(DT_model1, newdata = test_data, type = "class")
confusionMatrix(Test_Accuracy, test_data$default.payment.next.month)

control= rpart.control(minsplit = 10,minbucket = 0,cp=0)
DT_model2 <- rpart(train_data$default.payment.next.month~.,data = train_data,method = "class", control=control,parms=list(split = "gini"))
Test_Accuracy = predict(DT_model2, newdata = test_data, type = "class")
confusionMatrix(Test_Accuracy, test_data$default.payment.next.month)

control= rpart.control(minsplit = 10,minbucket = 10,cp=0)
DT_model3 <- rpart(train_data$default.payment.next.month~.,data = train_data,method = "class", control=control,parms=list(split = "information"))
Test_Accuracy = predict(DT_model3, newdata = test_data, type = "class")
confusionMatrix(Test_Accuracy, test_data$default.payment.next.month)

control= rpart.control(minsplit = 50,minbucket = 10,cp=0)
DT_model4 <- rpart(train_data$default.payment.next.month~.,data = train_data,method = "class", control=control,parms=list(split = "information"))
Test_Accuracy = predict(DT_model4, newdata = test_data, type = "class")
confusionMatrix(Test_Accuracy, test_data$default.payment.next.month)

control= rpart.control(minsplit = 50,minbucket = 100,cp=0)
DT_model5 <- rpart(train_data$default.payment.next.month~.,data = train_data,method = "class", control=control,parms=list(split = "information"))
Test_Accuracy = predict(DT_model5, newdata = test_data, type = "class")
confusionMatrix(Test_Accuracy, test_data$default.payment.next.month)

DT_model5$variable.importance

control= rpart.control(minsplit = 50,minbucket = 100,cp=0, maxdepth = 6)
DT_model6 <- rpart(train_data$default.payment.next.month~SEX+MARRIAGE+AGE+EDUCATION+
                     PAY_0+PAY_6+BILL_AMT2+BILL_AMT4+PAY_AMT3_p+PAY_AMT5_p,
                   data = train_data,method = "class", control=control,parms=list(split = "information"))
Train_Accuracy = predict(DT_model6, newdata = train_data, type = "class")
confusionMatrix(Train_Accuracy, train_data$default.payment.next.month, mode = "everything")

Test_Accuracy = predict(DT_model6, newdata = test_data, type = "class")
confusionMatrix(Test_Accuracy, test_data$default.payment.next.month, mode = "everything")


CV_default_tree <- lapply(1:10, function(x){ 
  model <-rpart(default.payment.next.month~SEX+MARRIAGE+AGE+EDUCATION+
                  PAY_0+PAY_6+BILL_AMT2+BILL_AMT4+PAY_AMT3_p+PAY_AMT5_p
                ,data = default_credit[folds != x,],method = "class", control=control,
                parms=list(split = "information"))
  preds <- predict(model,  default_credit[folds == x,], type="class")
  real <- default_credit$default.payment.next.month[folds == x]
  conf <-confusionMatrix(preds, real)
  return(data.frame(preds, real))
})
CV_default_tree <- do.call(rbind, CV_default_tree)
confusionMatrix(CV_default_tree$preds, CV_default_tree$real)


# Logistic Regression
library(pROC)
install.packages("AUC")
library(AUC)
logit1<-glm(default.payment.next.month~.,data=train_data,family = 'binomial')
summary(logit1)

predict_train1 <- predict(logit1, type = 'response')
pred_train1 = as.factor(ifelse(predict_train1 > 0.5, "1", "0"))
confusionMatrix(pred_train1, train_data$default.payment.next.month)

predict_test1 <- predict(logit1, newdata = test_data, type= 'response')
pred_test1 = as.factor(ifelse(predict_test1 > 0.5, "1", "0"))
confusionMatrix(pred_test1, test_data$default.payment.next.month, mode = "everything")


logit2<-glm(default.payment.next.month~SEX+MARRIAGE+AGE+EDUCATION+
              PAY_0+PAY_6+BILL_AMT2+BILL_AMT4 +PAY_AMT3_p+ PAY_AMT5_p,
            data=train_data,family = 'binomial')

predict_train2 <- predict(logit2, type = 'response')
pred_train2 = as.factor(ifelse(predict_train2 > 0.5, "1", "0"))
confusionMatrix(pred_train2, train_data$default.payment.next.month)

predict_test2 <- predict(logit2, newdata = test_data, type= 'response')
pred_test2 = as.factor(ifelse(predict_test2 > 0.5, "1", "0"))
confusionMatrix(pred_test2, test_data$default.payment.next.month, mode = "everything")

CV_default_logit <- lapply(1:10, function(x){ 
  model <-logit2<-glm(default.payment.next.month~SEX+MARRIAGE+AGE+EDUCATION+
                        PAY_0+PAY_6+BILL_AMT2+BILL_AMT4 +PAY_AMT3_p+ PAY_AMT5_p,
                      data = default_credit[folds != x,],family = 'binomial')
  preds <- predict(model,  default_credit[folds == x,], type="response")
  preds1 = as.factor(ifelse(preds > 0.5, "1", "0"))
  real <- default_credit$default.payment.next.month[folds == x]
  conf <-confusionMatrix(preds1, real)
  return(data.frame(preds1, real))
})
CV_default_logit <- do.call(rbind, CV_default_logit)
confusionMatrix(CV_default_logit$preds1, CV_default_logit$real)

#Adaboost
install.packages("adabag")
library(adabag)

model_1 <- boosting(default.payment.next.month ~.,
                      data = train_data, mfinal = 100)
boost_error <- predict.boosting(model_1, newdata = test_data)
a<-data.frame(boost_error$class, test_data$default.payment.next.month)

confusionMatrix(a$boost_error.class, a$test_data.default.payment.next.month, mode = "everything")

boost_error <- predict.boosting(model_1, newdata = train_data)
a<-data.frame(boost_error$class, train_data$default.payment.next.month)

confusionMatrix(a$boost_error.class, a$train_data.default.payment.next.month, mode = "everything")

model_ada <- boosting(default.payment.next.month ~SEX+MARRIAGE+AGE+EDUCATION+
                        PAY_0+PAY_6+BILL_AMT2+BILL_AMT4 +PAY_AMT3_p+ PAY_AMT5_p,
                      data = train_data, mfinal = 100)
model_ada

# $importance gives the variable importance

boost_error <- predict.boosting(model_ada, newdata = test_data)
a<-data.frame(boost_error$class, test_data$default.payment.next.month)

confusionMatrix(a$boost_error.class, a$test_data.default.payment.next.month, mode = "everything")
boost_error <- predict.boosting(model_ada, newdata = train_data)
a<-data.frame(boost_error$class, train_data$default.payment.next.month)

confusionMatrix(a$boost_error.class, a$train_data.default.payment.next.month, mode = "everything")


CV_boost <- lapply(1:10, function(x){ 
  model <-boosting(default.payment.next.month ~SEX+MARRIAGE+AGE+EDUCATION+PAY_0+PAY_6+BILL_AMT2+BILL_AMT4 +PAY_AMT3_p+ PAY_AMT5_p, data = default_credit[folds != x,], mfinal = 50)
  preds <- predict.boosting(model, newdata =  default_credit[folds == x,])
  real <- default_credit$default.payment.next.month[folds == x]
  #conf <-confusionMatrix(preds, real)
  print(preds$confusion)
  return(data.frame(preds$class, real))
})
CV_boost <- do.call(rbind, CV_boost)

confusionMatrix(CV_boost$preds, CV_boost$real)

#ROC 

# DT
pred_dt <- predict(DT_model6,test_data , type="prob")
pr_dt <- prediction(pred_dt[,2],test_data$default.payment.next.month)
perf_dt<-performance(pr_dt,"tpr", "fpr")
plot(perf_dt)
auc = performance(pr_dt, 'auc')
slot(auc, 'y.values')

# Naive
pred_test_naive<-predict(model2, newdata = test_data, type="raw")
p_test_naive<-prediction(pred_test_naive[,2], test_data$default.payment.next.month)
perf_naive<-performance(p_test_naive, "tpr", "fpr")
plot(perf_naive, colorize=T)
performance(p_test_naive, "auc")@y.values

# Logistic
predict_test2 <- predict(logit2, newdata = test_data, type= 'response')
pred_logit <- prediction(predict_test2, test_data$default.payment.next.month)
pr_logit <- performance(pred_logit, 'tpr','fpr')
plot(pr_logit)
auc = performance(pr_logit, 'auc')
slot(auc, 'y.values')

# AdaBoost
pred_ada <- predict.boosting(model_ada, newdata =  test_data)
pr_ada <- prediction(pred_ada$prob[,2],test_data$default.payment.next.month)
perf_ada<-performance(pr_ada,"tpr", "fpr")
plot(perf_ada)
auc = performance(pr_ada, 'auc')
slot(auc, 'y.values')

plot(perf_dt)
plot(perf_naive, add= TRUE, col="red")
plot(pr_logit, add= TRUE, col="blue")
plot(perf_ada, add= TRUE, col="cyan")
legend(0.6, 0.3, legend=c("DT", "Naive", "Logistic","AdaBoost"),
       col=c("black", "red","blue","cyan"), lty=1:2, cex=0.8)

#PR curve

# DT
PR_dt <- performance(pr_dt,'prec', 'tpr')
plot(PR_dt)
PRAUC(pred_dt,test_data$default.payment.next.month)

# Naive
PR_naive <- performance(p_test_naive,'prec', 'tpr')
plot(PR_naive)
PRAUC(pred_test_naive,test_data$default.payment.next.month)

#Logistic
PR_logit <- performance(pred_logit,'prec', 'tpr')
plot(PR_logit)
PRAUC(predict_test2,test_data$default.payment.next.month)

#AdaBoost
PR_ada <- performance(pr_ada, 'prec','tpr')
plot(PR_ada)
PRAUC(pred_ada,test_data$default.payment.next.month)

plot(PR_dt)
plot(PR_naive, add= TRUE, col="red")
plot(PR_logit, add= TRUE, col="blue")
plot(PR_ada, add= TRUE, col="cyan")
legend(0.7, 1.0, legend=c("DT", "Naive", "Logistic","AdaBoost"),
       col=c("black", "red","blue","cyan"), lty=1:2, cex=0.8)
