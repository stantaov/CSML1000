library(MLmetrics)
library(VIM)
library(ggplot2)
library(dplyr)
library(readr)
library(caTools)
library(caret)
library(ROSE)
library(DMwR)
library(data.tree)
library(party)
library(randomForest)
library(pROC)
# loading the dataset
setwd("/Curso-ML/Assignment-3/")
test  <- read.csv("classified-3.csv")
train <- read.csv("transactions-3.csv") ## Items
summary(train)
## making sure all variables are numeric or factor
test$fraud       <- as.factor(test$fraud)
train$fraud      <- as.factor(train$fraud)
test$V12         <- as.numeric(test$V12)
test$sex..       <- as.numeric(test$sex..)
test$sex.F       <- as.numeric(test$sex.F)
test$sex.M       <- as.numeric(test$sex.M)
test$income1.0.2 <- as.numeric(test$income1.0.2)
test$income1.0.4 <- as.numeric(test$income1.0.4)
test$income1.0.6 <- as.numeric(test$income1.0.6)
test$income1.0.8 <- as.numeric(test$income1.0.8)
test$income1.1   <- as.numeric(test$income1.1)
test$type.0      <- as.numeric(test$type.0)
test$type.0.25   <- as.numeric(test$type.0.25)
test$type.0.5    <- as.numeric(test$type.0.5)
test$type.0.75   <- as.numeric(test$type.0.75)
test$type.1      <- as.numeric(test$type.1)
test$same_city   <- as.numeric(test$same_city)
test$same_count  <- as.numeric(test$same_count)
str(test)
class(test$V12)
train$V12 <- as.numeric(train$V12)
class(train$V12)
str(train)
str(test)
test  <- select(test,sex..,sex.F,sex.M,income1.0.2,income1.0.4,income1.0.6,income1.0.8,income1.1,same_city,same_count,seller_sco,dispersao,type.0,type.0.25,type.0.5,type.0.75,type.1,V10,V11,V12,fraud)
train <- select(train,sex..,sex.F,sex.M,income1.0.2,income1.0.4,income1.0.6,income1.0.8,income1.1,same_city,same_count,seller_sco,dispersao,type.0,type.0.25,type.0.5,type.0.75,type.1,V10,V11,V12,fraud)
str(train)
str(test)
################ Decision Tree #####################################
train_smote <- SMOTE(fraud ~ ., train, perc.over = 100, perc.under=200)
tb_model <- ctree(fraud ~., data = train_smote)
print(tb_model)
class(test)
summary(test)
dim(test)
str(test)
summary(train)
dim(train)
str(train)
predicao <- predict(tb_model, test)
print(predicao)
confusionMatrix(predicao, test$fraud)

###############################Random Forest ########################
train_smote <- SMOTE(fraud ~ ., train, perc.over = 100, perc.under=200)
rf <- randomForest(fraud~.,data=train_smote, ntree=300, ntry=8)
print(rf)
attributes(rf)
p1 <- predict(rf, test)
head(p1)
head(train$fraud)
confusionMatrix(p1, test$fraud)

############### Random Forest + Bagging #############################
tb_model <- train(fraud ~ ., data = train_smote, method = "treebag")

pred_tree <- predict(tb_model, test)

confusionMatrix(pred_tree, test$fraud)

############## Support Vector Machine Classifier ###############
train_smote <- SMOTE(fraud ~ ., train, perc.over = 100, perc.under=200)

#library(caTools) 
library(e1071) 
set.seed(2020)

svm_model <- svm(fraud ~ ., data = train_smote, type = 'C-classification', kernel = 'linear', trControl = control) 


pred_svm <- predict(svm_model, test)

results <- pred_svm
results

lookup <- c("1" = 0, "2" = 1)
results <- lookup[results]
results <- as.numeric(results)


auc <- roc(test$fraud, results)
print(auc)

# Let's visualize AUC 
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

predicted <- as.factor(results)
actual <- as.factor(test$fraud)
confusionMatrix(predicted, actual)


f2score <- FBeta_Score(results, test$fraud, beta = 2)
cat('F2 Score:', f2score)

# F2 measure score is quite great 0.90 but the model gets a lot of false negative transactions


######################## XGBoost ######################

set.seed(2020)
library(xgboost) 
train_smote <- SMOTE(fraud ~ ., train, perc.over = 100, perc.under=200)
str(train_smote)
train_smote$fraud <- as.numeric(train_smote$fraud)-1
str(train_smote)
summary(train_smote)
train_smote$fraud <- as.factor(train_smote$fraud)
summary(train_smote)
summary(test)
data_train <- subset(train_smote, select = -c(fraud))
data_test  <- subset(test, select = -c(fraud))
dtrain <- xgb.DMatrix(data = as.matrix(data_train), label= (train_smote$fraud))
summary(dtrain)
print(nrow(train))
print(nrow(data_train))
print(nrow(train$fraud))
dtest <- xgb.DMatrix(data = as.matrix(data_test), label= (test$fraud))


xgb_model <- xgboost(data = dtrain, # the data   
                     nround = 200, # max number of boosting iterations
                     objective = "reg:linear") 


pred_xgb <- predict(xgb_model, dtest)
print(pred_xgb)

auc <- roc(test$fraud, pred_xgb)
print(auc)

# Let's visualize AUC 
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)


# Calculating F2-measure
pred_xgb_final <- round(pred_xgb,digits=0)
f2score <- FBeta_Score(pred_xgb_final, test$fraud, beta = 2)
cat('F2 Score:', f2score)


# F2 measure is 0.99 !!!!!!!!
predicted <- (round(pred_xgb,digits=0))
print(predicted)
predicted <- as.factor(predicted-1)
summary(predicted)
actual <- as.factor(test$fraud)
print(actual)
summary(actual)
confusionMatrix(predicted, actual)


# We got very accurate model!!!! it predicts 96% correctly of test frauds!!!!!









