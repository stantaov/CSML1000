library(VIM)
library(ggplot2)
library(dplyr)
library(readr)

# loading the dataset
df <- read_csv(file.choose())
df_synthetic  <- read_csv(file.choose())

# checking structure and frist 5 rows 
head(df)
str(df)

head(df_synthetic)
str(df_synthetic)

# chcking missing values
aggr(df)
sum(is.na(df))

aggr(df_synthetic)
sum(is.na(df_synthetic))


# there are 4722 missing values in sex column
# we will remove them 
df <- na.omit(df)

df_synthetic <- na.omit(df_synthetic)

####################

# converting sex, income, type into categorical data type

df$sex <- as.factor(df$sex)
df$income <- as.factor(df$income)
df$type <- as.factor(df$type)

# one hot encode categorical data 

library(caret)

target <- df$fraud # creating sepearate value with outputs 
df <- subset(df, select = -c(fraud)) # remove output column from dataset
dummy <- dummyVars(" ~ .", data = df) # encoding cotegorical columns sex, income, type
trsf <- data.frame(predict(dummy, newdata = df)) # creating transform data frame
head(trsf)

# encoding output column into binomial and adding it to the data frame

lookup <- c("N" = 0, "Y" = 1)
trsf$fraud <- lookup[target]

print(table(trsf$fraud))
print(prop.table(table(trsf$fraud)))

# At 4%, this is clearly we have skewed and imbalanced dataset.
# Imbalanced data pose classification problem for predictive modeling as most of the machine learning algorithms 
# used for classification were designed around the assumption of an equal number of examples for each class. 
# As a result, models train on imbalanced data have poor predictive performance specifically on minority class. 
# Specifically, for fraud detection where the predicting the minority class in the most important aspect of the model. 


################ Baseline Model ################ 

# We will evaluate baseline models using repeated k-fold cross-validation. 
# The k-fold cross-validation procedure provides a good general estimate of model performance that is not
# too optimistically biased, at least compared to a single train-test split. We will use k=10,

set.seed(2020) # setting seeed

# creating a 70% - 30% data split 

data_split <- createDataPartition(trsf$fraud, p = .7,
                                  list = FALSE,
                                  times = 1)

train <- trsf[ data_split,]
test <- trsf[-data_split,]

# Use cross-validation 
control <- trainControl(method="repeatedcv", number = 10, repeats = 3) # 10-fold cross-validation and repeating 3 times

# We decided to use ensemble method (Random Forest + Bagging)
# There are some advantages of using it 
# Avoid over-fitting problem
# Each bagged model has low bias but high variance
# As a result, the averaged final model has reduced variance and low bias
# Can fit non-linear fucntions
# Usually shows good accuracy

base_model <- train(fraud ~ ., data = train, method = "treebag", trControl = control) #!!!! BE AWARE IT TAKES SOME TIME TO RUN IT

# Tesging the model on the test data

predictors <- names(train)[names(train) != 'fraud']
pred_base <- predict(base_model$finalModel, test[,predictors])

# Prindting the result for Area Under The Curve
# Higher the AUC to 1, better the model predicts 0s as 0s and 1s as 1s for our output(fraud) label.

library(pROC)
auc <- roc(test$fraud, pred_base)
print(auc)

# The baseline model shows very good accuracy with 0.92 AUC coefficient 

# Let's visualize AUC 
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

# An AUC score of 0.92 is great

library(MLmetrics)

# Classification accuracy is widely used because it is one single measure used to summarize model performance. 
# F-measure provides a way to combine both precision and recall into a single measure that captures both properties.
# we need to calculate F2-measure since in our situation we need to put less weight on precision, more weight on recall.

f2score <- FBeta_Score(pred_base, test$fraud, beta = 2)
cat('F2 Score:', f2score)

# The base line model has F2-measure around 0.47 wich is not the best.


################ Treebag Model using SMOT ################ 

# Synthetic Minority Oversampling Technique makes data balanced by synthesizing new data from the existing dataset using KNN.
# The approach is effective because new synthetic examples from the minority class are created that are plausible, 
# that is, are relatively close in feature space to existing examples from the minority class.


library(DMwR)
train$fraud <- as.factor(train$fraud)
train_smote <- SMOTE(fraud ~ ., train, perc.over = 100, perc.under=200)
# perc.over - A number that drives the decision of how many extra cases from the minority class are generated (known as over-sampling).
# perc.under - A number that drives the decision of how many extra cases from the majority classes are selected for each case generated from the minority class (known as under-sampling)
train_smote$fraud <- as.numeric(train_smote$fraud)
unique(train_smote$fraud)
train_smote$fraud <- train_smote$fraud - 1
print(table(train_smote$fraud))
prop.table(table(train_smote$fraud))


# The dataset is now balanced

# Let's run treebag on balanced dataset now
tb_model <- train(fraud ~ ., data = train_smote, method = "treebag", trControl = control)


pred_tree <- predict(tb_model$finalModel, test[,predictors])


auc <- roc(test$fraud, pred_tree)
print(auc)

# The treebag model shows higher  AUC coefficient (~0.95)

# Let's visualize AUC 
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

# Calculating F2-measure
f2score <- FBeta_Score(pred_tree, test$fraud, beta = 2)
cat('F2 Score:', f2score)



# F2 measure is 0.82 very good!!!
predicted <- as.factor(round(pred_tree,digits=0))
actual <- as.factor(test$fraud)
confusionMatrix(predicted, actual)


############## Support Vector Machine Classifier ###############


#library(caTools) 
library(e1071) 
set.seed(2020)

svm_model <- svm(fraud ~ ., data = train_smote, type = 'C-classification', kernel = 'linear', trControl = control) 


pred_svm <- predict(svm_model, test[,predictors])

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


f2score <- FBeta_Score(results2, test$fraud, beta = 2)
cat('F2 Score:', f2score)

# F2 measure score is quite great 0.90 but the model gets a lot of false negative transactions


# Let's fine tune SVM model 

obj <- tune(svm, fraud~., data = train_smote, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "fix"))

summary(obj)
plot(obj)

obj$best.parameters
obj$best.performance


svm_model_best <- obj$best.model

pred_svm_best <- predict(svm_model_best, test[,predictors])

results_best <- pred_svm_best
results_best

results_best <- lookup[results_best]
results_best <- as.numeric(results_best)

auc <- roc(test$fraud, results_best)
print(auc)

# Let's visualize AUC 
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)


predicted <- as.factor(round(results_best,digits=0))

actual <- as.factor(test$fraud)
confusionMatrix(predicted, actual)
actual

######### XGBoost ######### 

set.seed(2020)
library(xgboost) 


data_train <- subset(train_smote, select = -c(fraud))
data_test <- subset(test, select = -c(fraud))
dtrain <- xgb.DMatrix(data = as.matrix(data_train), label= train_smote$fraud)

dtest <- xgb.DMatrix(data = as.matrix(data_test), label= as.numeric(test$fraud))


xgb_model <- xgboost(data = dtrain, # the data   
                 nround = 200, # max number of boosting iterations
                 objective = "binary:logistic",
                 trControl = control) 


pred_xgb <- predict(xgb_model, dtest)


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
predicted <- as.factor(round(pred_xgb,digits=0))
actual <- as.factor(test$fraud)
confusionMatrix(predicted, actual)


# We got very accurate model!!!! it predicts 96% correctly of test frauds!!!!!


my_data <- as.data.frame(cbind(predicted = predicted,
                              observed = actual))

my_data

my_data$index <- seq.int(nrow(my_data))

# Feature importance

names = dimnames(dtrain)[[2]]
importance_matrix = xgb.importance(names, model=xgb_model)
importance_matrix

fi <- xgb.plot.importance(importance_matrix, xlab = "Relative importance")
print(fi) 

ggplot(data=importance_matrix, aes(x = reorder(Feature, -Gain), y = Gain, fill = importance_matrix$Feature)) +
  geom_bar(stat="identity") + 
  labs(title = "XG Boosted Feature Importance", x = "Features", y = "Information Gain") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none")






ggplot(df, aes(sex)) +
  geom_bar(aes(fill = sex)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  ggtitle("Gender Distribution")

# There are 4722 unenditified genders

hist(df1$age, col="blue", main="Distribution of Age", xlab="Age Class", ylab="Frequency", labels=TRUE)

# There are some outliers in age column
# We will remove them 

df1 <- df %>% 
  filter(age < 95)


ggplot(df1, aes(x=age, fill=I("blue"))) +
  geom_histogram(position="identity", bins = 11) +
  theme_minimal() +
  ggtitle("Age Classes Grouped by Gender") + 
  theme(legend.position = "none")


ggplot(df, aes(income)) +
  geom_bar(aes(fill = income)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  ggtitle("Gender Distribution")


ggplot(df, aes(fraud)) +
  geom_bar(aes(fill = fraud)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  ggtitle("Gender Distribution")

# The data is very imbalanced 

ggplot(df, aes(type)) +
  geom_bar(aes(fill = type)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  ggtitle("Gender Distribution")

# Type? 

ggplot(df1, aes(x=value, fill=I("red"))) +
  geom_histogram(position="identity", bins = 11) +
  theme_minimal() +
  ggtitle("Age Classes Grouped by Gender") + 
  theme(legend.position = "none")

ggplot(df1, aes(x=seller_sco, fill=I("purple"))) +
  geom_histogram(position="identity", bins = 10) +
  theme_minimal() +
  ggtitle("Age Classes Grouped by Gender") + 
  theme(legend.position = "none")

# same_count?

ggplot(df, aes(same_count)) +
  geom_bar(aes(fill = same_count)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  ggtitle("Gender Distribution")


ggplot(df, aes(same_city)) +
  geom_bar(aes(fill = same_city)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  ggtitle("Gender Distribution")



ggplot(df1, aes(x=avr_int_go, fill=I("purple"))) +
  geom_histogram(position="identity", bins = 14) +
  theme_minimal() +
  ggtitle("Age Classes Grouped by Gender") + 
  theme(legend.position = "none")

ggplot(df1, aes(x=avr_int_se, fill=I("purple"))) +
  geom_histogram(position="identity", bins = 14) +
  theme_minimal() +
  ggtitle("Age Classes Grouped by Gender") + 
  theme(legend.position = "none")

ggplot(df1, aes(x=avr_go, fill=I("purple"))) +
  geom_histogram(position="identity", bins = 14) +
  theme_minimal() +
  ggtitle("Age Classes Grouped by Gender") + 
  theme(legend.position = "none")

ggplot(df1, aes(x=avr_servic, fill=I("purple"))) +
  geom_histogram(position="identity", bins = 14) +
  theme_minimal() +
  ggtitle("Age Classes Grouped by Gender") + 
  theme(legend.position = "none")

ggplot(df1, aes(x=avr_recur, fill=I("purple"))) +
  geom_histogram(position="identity", bins = 14) +
  theme_minimal() +
  ggtitle("Age Classes Grouped by Gender") + 
  theme(legend.position = "none")



