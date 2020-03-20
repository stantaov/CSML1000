## This program treats the trainning data (Transactions-2.csv) and the test data (classiifcados.csv) - Hotecoding, cross-validation and smoting
library(VIM)
library(ggplot2)
library(dplyr)
library(readr)
library(caTools)
library(caret)
library(ROSE)
library(DMwR)
# loading the dataset
setwd("/Curso-ML/Assignment-3/")
df <- read.csv("classified-2.csv")      ## Items to be classified
print(df)
df <- select(df,fraud,sex,income1,same_city,same_count,seller_sco,dispersao,type,V10,V11,V12)
colnames(df)<- c("fraud","sex","income1","same_city","same_count","seller_sco","dispersao","type","V10","V11","V12")
print(df)

# checking structure and frist 5 rows 
head(df)
str(df)

# chcking missing values
aggr(df)
sum(is.na(df))

# there are 4722 missing values in sex column
# we will remove them 
df <- na.omit(df)

####################
# converting sex, income, type into categorical data type

df$sex <- as.factor(df$sex)
df$income1 <- as.factor(df$income1)
df$type <- as.factor(df$type)
df$fraud <- as.factor(df$fraud)
# one hot encode categorical data 
print(df)


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
print(trsf)
str(trsf)
trsf$fraud <- as.factor(trsf$fraud)

# At 4%, this is clearly we have skewed and imbalanced dataset.
# Imbalanced data pose classification problem for predictive modeling as most of the machine learning algorithms 
# used for classification were designed around the assumption of an equal number of examples for each class. 
# As a result, models train on imbalanced data have poor predictive performance specifically on minority class. 
# Specifically, for fraud detection where the predicting the minority class in the most important aspect of the model. 


################ Trainning data ################ 

# We will evaluate baseline models using repeated k-fold cross-validation. 
# The k-fold cross-validation procedure provides a good general estimate of model performance that is not
# too optimistically biased, at least compared to a single train-test split. We will use k=10,
### Preparing the trainning data

dfx <- read.csv("transactions-2.csv") ## Items
data_sample = sample.split(dfx$fraud ,SplitRatio=0.75) ##here we separate the file to be the nodes
df1 = subset(dfx,data_sample==FALSE) ##trainning data
df1 <- select(df1,fraud,sex,income1,same_city,same_count,seller_sco,dispersao,type,V11,V12,V12.1)
print(df1)
colnames(df1)<- c("fraud","sex","income1","same_city","same_count","seller_sco","dispersao","type","V10","V11","V12")
print(df1)
summary(df1)
# checking structure and frist 5 rows 
head(df1)
str(df1)

# chcking missing values
aggr(df1)
sum(is.na(df1))

# there are 4722 missing values in sex column
# we will remove them 
df1 <- na.omit(df1)

####################

# converting sex, income, type into categorical data type

df1$sex <- as.factor(df1$sex)
df1$income1 <- as.factor(df1$income1)
df1$type <- as.factor(df1$type)
df1$fraud <- as.factor(df1$fraud)
# one hot encode categorical data 



target <- df1$fraud # creating sepearate value with outputs 
df1 <- subset(df1, select = -c(fraud)) # remove output column from dataset
dummy <- dummyVars(" ~ .", data = df1) # encoding cotegorical columns sex, income, type
trsf1 <- data.frame(predict(dummy, newdata = df1)) # creating transform data frame
head(trsf1)

# encoding output column into binomial and adding it to the data frame

lookup1 <- c("N" = 0, "Y" = 1)
trsf1$fraud <- lookup1[target]
trsf1$fraud <- as.factor(trsf1$fraud)
print(table(trsf1$fraud))
print(prop.table(table(trsf1$fraud)))

summary(df1)
summary(trsf1)


train <- trsf1
class(train)
summary(train)
test  <- trsf
table(train$fraud)


train$fraud <- as.factor(train$fraud)
train_smote <- SMOTE(fraud ~ ., train, perc.over = 800, perc.under=0)
class(train_smote)
print(train_smote)
summary(train_smote)
train1 <- rbind.data.frame(train,train_smote)
summary(train)
summary(train1)

write.csv(train1,"/Curso-ML/Assignment-3/transactions-3.csv")
write.csv(test,"/Curso-ML/Assignment-3/classified-3.csv")