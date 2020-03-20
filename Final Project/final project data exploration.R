library(VIM)
library(ggplot2)
library(dplyr)
library(readr)
library(summarytools)
library(corrplot)
library(kableExtra)
library(formattable)
library(tidyr)
library(stringr)
library(FSelect)
library(rpart.plot)
library(caret)
library(rpart)
library(rpart.plot)
library(data.tree)
library(party)
library(partykit)
library(caTools)
library(ElemStatLearn)

# Loading the dataset
df <- read_csv(file.choose())


# Checking the data structure and frist 10 rows 

head(df,10) %>%
  kable( "html", escape=F, align="c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center")

str(df) 


# Checking missing values
aggr(df)
sum(is.na(df))

# There are 4722 missing values in sex column

# We will remove them 
df <- na.omit(df)


######### Data Exploration ######### 

# Checking corrolations between features in our dataset

correlations <- cor(df[, sapply(df, is.numeric)], method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")

# As we can see that most of the features are not correlated. 
# We will check the Variable Importance for each model later.
# There is strong negative corralation between same_city and value and between same_city and type

# Before we start the data exploration we convert sex, income, type columns into categorical data type

df$sex <- as.factor(df$sex)
df$income <- as.factor(df$income)
df$type <- as.factor(df$type)

# There are statistical descriptions for each feature of the dataset 

view(dfSummary(df))


# After removaing NA values from sex columns the data set contains 285,676 transactions. 
# The mean value of all transactions is $180.88 while the largest transaction recorded in this data set amounts to $1499.98. 
# The distribution of the monetary value of all transactions is right-skewed. 

d <- density(df$value)
plot(d, main="Distribution of Values")
polygon(d, col="red", border="black")

# Let's see the number of fraudulent transactions. 
# As it can be expected, most transactions are non-fraudulent and our data is quite imbalanced. 
# In fact, 95.95% of the transactions in this data set were not fraudulent while only 4.05% were fraudulent. 
# The graph below highlights this significant contrast.

print(table(df$fraud))
print(prop.table(table(df$fraud))*100)


ggplot(df, aes(fraud)) +
  geom_bar(aes(fill = fraud)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  ggtitle("Distribution of Fraud Feature")


# Let's examine the sex column.
# We can see that gender distribution is almost balanced, there are 52% of female and 48% male customers. 

ggplot(df, aes(sex)) +
  geom_bar(aes(fill = sex)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  ggtitle("Gender Distribution")


hist(df$age, col="blue", main="Distribution of Age", xlab="Age Class", ylab="Frequency", labels=TRUE, ylim = c(0, max(df$age)))

# There are  a few outliers in age column probably related to typos
# We will remove them 

df <- df %>% 
  filter(age < 95)


ggplot(df, aes(x=age, fill=I("blue"))) +
  geom_histogram(position="identity", bins = 11) +
  theme_minimal() +
  ggtitle("Age Distribution") + 
  theme(legend.position = "none")

# Now age distribution looks more realistic, and data disctirbution looks binomial, with two spickes in 30 and 65 year age groups. 

# Let's look at income feature.

ggplot(df, aes(income)) +
  geom_bar(aes(fill = income)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  ggtitle("Income Distribution")

print(prop.table(table(df$income))*100)


# The dataset is dominated by one income group category (E - 90.64%).
# E income category is the catagory with the lowest income level. 
# There are a few customers in the highest income class (A - 1.57% and B - 0.08%)



# It’s clear from the box plot and density graph below that most of fraud transactions occured in lower rage, between 5 and 55 of transaction value. 
# However, we also see that other fraud transactions are evenly distibuted. 

ggplot(df, aes(x=fraud, y=value, color=fraud))+
  geom_boxplot() +
  ggtitle("Transaction Amount") +
  theme_minimal()


ggplot(df, aes(x = value, fill = fraud)) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, 100)) + 
  labs(title = "Transaction Amount", 
       x = "Value", 
       y = "Density", 
       col = "Class") + 
  theme_minimal() +
  scale_fill_discrete(labels = c("Fraud", "Not Fraud"))

# Let's boxplot avr_recur (average expenditure for reoccurring  transactions) against fraud

ggplot(df, aes(x=fraud, y=avr_recur, fill=fraud))+
  geom_boxplot() +
  ggtitle("Average Expenditure for Reoccurring Transactions") +
  theme_minimal()

# Interestingly to see that there are no clear differences between fraud and not fraud for this feature.

# Next we check avr_servic (average expenditure for buying services) against fraud

ggplot(df, aes(x=fraud, y=avr_servic, fill=fraud))+
  geom_boxplot() +
  ggtitle("Aaverage Expenditure for Buying Services") +
  theme_minimal()

# Similar picture as we've seen before, range of transcation is between 0 and 160 for both fraud and not fraud transactions. 

# Let's take a look at avr_go (average expenditure for buying goods) against fraud

ggplot(df, aes(x=fraud, y=avr_go, fill=fraud))+
  geom_boxplot() +
  ggtitle("Average Expenditure for Buying Goods") +
  theme_minimal()

# There is no clear difference between two (fraud, not fraud) calsses.
# We will continue checking avr_int_se (average expenditure for buying services online) and avr_int_go (average expenditure for buying goods online)

ggplot(df, aes(x=fraud, y=avr_int_se, fill=fraud))+
  geom_boxplot() +
  ggtitle("Average Expenditure for Buying Services Online") +
  theme_minimal()

# We see high variability in data for both fraudulent and not fraudulent transactions for avr_int_se feature.

ggplot(df, aes(x=fraud, y=avr_int_go, fill=fraud))+
  geom_boxplot() +
  ggtitle("Average Expenditure for Buying Goods Online") +
  theme_minimal()



# The next we explore type of transaction feature. 
# This feature describes five categories each transaction belongs to (0-recurrent, 1- goods, 2-services, 3-online goods or 4-online services)

ggplot(df, aes(type)) +
  geom_bar(aes(fill = type)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  ggtitle("Transaction Type Distribution")

# Type feature has almost uniform distribution.   

ggplot(df, aes(type)) +
  geom_bar(aes(fill = fraud)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.5)) +
  scale_fill_discrete(labels = c("Fraud", "Not Fraud")) +
  ggtitle("Transaction Type Distribution")

# It seems online services category has the highest percentage of fradulant transactions. 


hist(df$seller_sco, col="blue", main="Distribution of Seller Scrore", xlab="Seller Scrore", ylab="Frequency")

# Seller score feature has uniform distribution

ggplot(df, aes(seller_sco)) +
  geom_histogram(aes(fill = fraud), bins = 100) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 90),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Transaction Type and Fraud Distributuion")

# Interesting, only seven scores have fradulant transaction and only two of the seven have very high npercentage of fraud.


##### FEATURE ENGINNERING ######

### Creating variable dispersion which is a measure of how far form the average of the type of the item the value is

df$dispersion <- 0.00
len = nrow(df)
counter <- 1

while (counter<=len){
  if (df$type[counter] == 0){
    df$dispersion[counter] <- abs(df$avr_recur[counter]-df$value[counter])/df$avr_recur[counter]
  }
  if (df$type[counter] == 1){
    df$dispersion[counter] <- abs(df$avr_servic[counter]-df$value[counter])/df$avr_servic[counter]
  }
  if (df$type[counter] == 2){
    df$dispersion[counter] <- abs(df$avr_go-df$value)/df$avr_go[counter]
  }
  if (df$type[counter] == 3){
    df$dispersion[counter] <- abs(df$avr_int_go[counter]-df$value[counter])/df$avr_int_go[counter]
  }
  if (df$type[counter] == 4){
    df$dispersion[counter] <- abs(df$avr_int_se[counter]-df$value[counter])/df$avr_int_se[counter]
  }
  
  if (df$income[counter]=="A"){
    df$income1[counter] <- 5
  }
  if (df$income[counter]=="B"){
    df$income1[counter] <- 4
  } 
  if (df$income[counter]=="C"){
    df$income1[counter] <- 3
  }
  if (df$income[counter]=="D"){
    df$income1[counter] <- 2 
  }
  if (df$income[counter]=="E"){
    df$income1[counter] <- 1
  }
  
  counter <- counter+1
  print(df$dispersion[counter])
  print(counter)
}

str(df)

##### Clustering #######

## select features for the cluster analysis
df_claster <- select(df,fraud,sex,age,income1,same_city,same_count,seller_sco,dispersion,type)

## Making sure all features are numeric

df_claster[4]  <- lapply(df_claster[4], as.numeric)
df_claster[5]  <- lapply(df_claster[5], as.numeric)
df_claster[6]  <- lapply(df_claster[6], as.numeric)
df_claster[7]  <- lapply(df_claster[7], as.numeric)
df_claster[8]  <- lapply(df_claster[8], as.numeric)
df_claster[9]  <- lapply(df_claster[9], as.numeric)

## Normalizing the data

var4  <- summary(df_claster[4])
var5  <- summary(df_claster[5])
var6  <- summary(df_claster[6])
var7  <- summary(df_claster[7])
var8  <- summary(df_claster[8])
var9  <- summary(df_claster[9])

print( as.numeric(substr(var9[6],9,14)))
var41 <- as.numeric(substr(var4[6],9,14))
var51 <- as.numeric(substr(var5[6],9,14))
var61 <- as.numeric(substr(var6[6],9,14))
var71 <- as.numeric(substr(var7[6],9,14))
var81 <- as.numeric(substr(var8[6],9,14))
var91 <- as.numeric(substr(var9[6],9,14))

## Normalizing the value dividing by the bigger value

df_claster[4]  <- (df_claster[4])/var41
df_claster[5]  <- (df_claster[5])/var51
df_claster[6]  <- (df_claster[6])/var61
df_claster[7]  <- (df_claster[7])/var71
df_claster[8]  <- (df_claster[8])/var81
df_claster[9]  <- (df_claster[9])/var91


head(df_claster)

## Separate the data trainning data and test data
data_sample <- sample.split(df_claster$fraud ,SplitRatio=0.95) ##here we separate the file to be the nodes
trainning_cluster <- subset(df_claster,data_sample==TRUE)  ##trainning data
test_cluster <- subset(df_claster,data_sample==FALSE) ##test data

summary(test_cluster)
dim(trainning_cluster)
dim(test_cluster)

### Separate the variables to cluster

trainning_cluster_2 <- cbind(trainning_cluster[4],trainning_cluster[5],trainning_cluster[6],trainning_cluster[7],trainning_cluster[8],trainning_cluster[9])


### Creating cluster using k-means

model_clusters <- kmeans(trainning_cluster_2, 74, iter.max = 150, nstart = 10,algorithm = c("Hartigan-Wong",
                                                                     "Lloyd",
                                                                     "Forgy",
                                                                     "MacQueen"), trace=FALSE)

## cluster centers "fitted" to each obs.:
print(model_clusters$centers)
clusters <- model_clusters$centers ##  nodes
class(clusters)
print(clusters)
print(clusters[2,1])

node <- fitted(model_clusters, method = c("centers"))

print(node)
print(node[1,1])
str(node)
dim(node)
class(node)

## Generates a vector indicating the node associated to each row

sites <- fitted(model_clusters, method = c("classes"))
print(sites)
str(sites)
class(sites)
print(sites)

### Here we add the node as a column in the dataset (ssociate a node to each record)
for (i in 1:nrow(trainning_cluster)){
  trainning_cluster[i,10]<-sites[i]
}

head(trainning_cluster)

## Here we convert the Kmeans format into matrix adding the necessary columns
m <- matrix(0:0, nrow = nrow(clusters), ncol = 11)
str(m)
print(clusters[1,3])
for (ix in 1:nrow(clusters)){
  for (i in 1:6){
    ##print(trainning_cluster_2[ix,i])
    m[ix,i]<-clusters[ix,i]
  }
  m[ix,11]<-ix
}

### 
print(m) 
clusters <- m 

for (i in 1:nrow(trainning_cluster)){  ## All trainning data records
  
  for (ix in 1:nrow(clusters)){  ## all nodes 
    
    if (trainning_cluster[i,10]==clusters[ix,11]){ ## if the node is the one associates with the item
      
      if (trainning_cluster[i,1] == "Y") {
        clusters[ix,8] <- clusters[ix,8]+1 ## positive for fraud
      }
      else{
        clusters[ix,9] <- clusters[ix,9]+1 ## negative for fraud
      }
      
      clusters[ix,10] <- clusters[ix,10]+1
    } 
  } 
}

##
for (i in 1:nrow(clusters)){
  
  clusters[i,8]<- clusters[i,8]/clusters[i,10]
  clusters[i,9]<- clusters[i,9]/clusters[i,10]
  
  
}

## now we associate the percentage of fraud for each transaction in the trainning data
for (i in 1:nrow(trainning_cluster)){  ## All trainning data records
  
  for (ix in 1:nrow(clusters)){  ## all nodes 
    
    if (trainning_cluster[i,10]==clusters[ix,11]){ ## if the node is the one associates with the item
      
      trainning_cluster[i,11] <- clusters[ix,8]
      trainning_cluster[i,12] <- clusters[ix,9]
      
      
      
      ##print(ix)
    } ## if the node is the one associates with the item
  }  ## all nodes
  
  print(i)
} ## all trainning data records
#################################

print(sites)
print(clusters)
write.csv(clusters,"/Curso-ML/Assignment-3/clustered.csv")
write.csv(test_cluster,"/Curso-ML/Assignment-3/test.csv")
write.csv(trainning_cluster,"/Curso-ML/Assignment-3/transactions-2.csv")
print(test_cluster)
### Identify the variable pontos which is a composed variable 
df_clastery1 <- read.csv("transactions-2.csv") ## Items
df_clastery2 <- select(df_clastery1,fraud,sex,age,income1,same_city,same_count,seller_sco,dispersao,type,V11,V12)
print(df_clastery2)
mean4 <- mean(df_clastery2[,4])
mean5 <- mean(df_clastery2[,5])
mean6 <- mean(df_clastery2[,6])
mean7 <- mean(df_clastery2[,7])
mean8 <- mean(df_clastery2[,8])
mean9 <- mean(df_clastery2[,9])
mean10 <- mean(df_clastery2[,10])
mean11 <- mean(df_clastery2[,11])


contador2 = 1
len1 = nrow(df_clastery2)
pontos1 <-0
while (contador2<len1){
  
  if (df_clastery2[contador2,4]<= mean4){
    pontos1=pontos1+1
  }
  if (df_clastery2[contador2,5]<= mean5){
    pontos1=pontos1+1
  }
  if (df_clastery2[contador2,6]<= mean6){
    pontos1=pontos1+1
  }
  if (df_clastery2[contador2,7]<= mean7){
    pontos1=pontos1+1
  }
  if (df_clastery2[contador2,8]>= mean8){
    pontos1=pontos1+1
  }
  if (df_clastery2[contador2,9]>= mean9){
    pontos1=pontos1+1
  }
  if (df_clastery2[contador2,10]>= mean10){
    pontos1=pontos1+1
  }
  if (df_clastery2[contador2,11]<= mean11){
    pontos1=pontos1+1
  }
  df_clastery2[contador2,12] <- pontos1
  pontos1 <-0
  contador2=contador2+1
  
}









