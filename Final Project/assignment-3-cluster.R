
library(dplyr) ##library(xlsx)
library(stringr)
##library(inum)library(TH.data)library(multcomp)library(matrixStats)library(survival)library(coin)library(sandwich)library(libcoin)library(strucchange)
##library(Formula)
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
setwd("/Curso-ML/Assignment-3/")

data0x <- read.csv("transactions-1.csv") ## Items
dim(data0x)
head(data0x)
print(data0x)
## select the meaninful colunms for the analysis
data0 <- select(data0x,fraud,sex,age,income1,same_city,same_count,seller_sco,dispersao,type)
##data4 <- select(data2,fraud,sex,age,income1,same_city,same_count,seller_sco,dispersao,type)
str(data0)
print(data0)
##print(data0x)


## Making sure all are numeric
data0[4]  <- lapply(data0[4], as.numeric)
data0[5]  <- lapply(data0[5], as.numeric)
data0[6]  <- lapply(data0[6], as.numeric)
data0[7]  <- lapply(data0[7], as.numeric)
data0[8]  <- lapply(data0[8], as.numeric)
data0[9]  <- lapply(data0[9], as.numeric)

## Normalizing the data
var4  <- summary(data0[4])
var5  <- summary(data0[5])
var6  <- summary(data0[6])
var7  <- summary(data0[7])
var8  <- summary(data0[8])
var9  <- summary(data0[9])

print( as.numeric(substr(var9[6],9,14)))
var41 <- as.numeric(substr(var4[6],9,14))
var51 <- as.numeric(substr(var5[6],9,14))
var61 <- as.numeric(substr(var6[6],9,14))
var71 <- as.numeric(substr(var7[6],9,14))
var81 <- as.numeric(substr(var8[6],9,14))
var91 <- as.numeric(substr(var9[6],9,14))

print(var41)
print(var51)
print(var61)
print(var71)
print(var81)
print(var91)

class(var41)
class(data0)

## normalizing the value dividing by the bigger value

data0[4]  <- (data0[4])/var41
data0[5]  <- (data0[5])/var51
data0[6]  <- (data0[6])/var61
data0[7]  <- (data0[7])/var71
data0[8]  <- (data0[8])/var81
data0[9]  <- (data0[9])/var91
dim(data0)
print(data0)

## Separate the data trainning data and test data
data_sample = sample.split(data0$fraud ,SplitRatio=0.95) ##here we separate the file to be the nodes
data1 = subset(data0,data_sample==TRUE)  ##trainning data
data5 = subset(data0,data_sample==FALSE) ##test data
summary(data5)
dim(data1)
dim(data5)
### Separate the variables to cluster

data3 <- cbind(data1[4],data1[5],data1[6],data1[7],data1[8],data1[9])
dim(data3)
##print(data3)

### Clusterization

valor <- kmeans(data3, 74, iter.max = 150, nstart = 10,algorithm = c("Hartigan-Wong",
                                                                      "Lloyd",
                                                                      "Forgy",
                                                                      "MacQueen"), trace=FALSE)

## cluster centers "fitted" to each obs.:
print(valor$centers)
clusters <- valor$centers ##  nodes
class(clusters)
print(clusters)
print(clusters[2,1])

node1 <- fitted(valor, method = c("centers"))
print(node1)
print(node1[1,1])
str(node1)
dim(node1)
class(node1)

## Generates a vector indicating the node associated to each row
sites <- fitted(valor, method = c("classes"))
print(sites)
str(sites)
class(sites)
print(sites)

### Here we add the node as a column in the dataset (ssociate a node to each record)
for (i in 1:nrow(data1)){
  data1[i,10]<-sites[i]
}

## Here we convert the Kmeans format into matrix adding the necessary columns
m <- matrix(0:0, nrow = nrow(clusters), ncol = 11)
str(m)
print(clusters[1,3])
for (ix in 1:nrow(clusters)){
  for (i in 1:6){
    ##print(data3[ix,i])
    m[ix,i]<-clusters[ix,i]
  }
  m[ix,11]<-ix
}

### 
print(m) 
clusters <- m ## clusters is the nodes matrix where we are going to place positive and negative for reach and sell

for (i in 1:nrow(data1)){  ## All trainning data records
  
  for (ix in 1:nrow(clusters)){  ## all nodes 
    
    if (data1[i,10]==clusters[ix,11]){ ## if the node is the one associates with the item
      
      if (data1[i,1] == "Y") {
        clusters[ix,8] <- clusters[ix,8]+1 ## positive for fraud
      }
      else{
        clusters[ix,9] <- clusters[ix,9]+1 ## negative for fraud
      }
      
      clusters[ix,10] <- clusters[ix,10]+1
      
      ##print(ix)
    } ## if the node is the one associates with the item
  }  ## all nodes
  
  ##print(i)
} ## all trainning data records

##
for (i in 1:nrow(clusters)){
  
  clusters[i,8]<- clusters[i,8]/clusters[i,10]
  clusters[i,9]<- clusters[i,9]/clusters[i,10]
  

}

## now we associate the percentage of fraud for each transaction in the trainning data
for (i in 1:nrow(data1)){  ## All trainning data records
  
  for (ix in 1:nrow(clusters)){  ## all nodes 
    
    if (data1[i,10]==clusters[ix,11]){ ## if the node is the one associates with the item
      
      data1[i,11] <- clusters[ix,8]
      data1[i,12] <- clusters[ix,9]
      
      
      
      ##print(ix)
    } ## if the node is the one associates with the item
  }  ## all nodes
  
  print(i)
} ## all trainning data records
#################################

print(sites)
print(clusters)
write.csv(clusters,"/Curso-ML/Assignment-3/clustered.csv")
write.csv(data5,"/Curso-ML/Assignment-3/test.csv")
write.csv(data1,"/Curso-ML/Assignment-3/transactions-2.csv")
print(data5)
### Identify the variable pontos which is a composed variable 
data0y1 <- read.csv("transactions-2.csv") ## Items
data0y2 <- select(data0y1,fraud,sex,age,income1,same_city,same_count,seller_sco,dispersao,type,V11,V12)
print(data0y2)
mean4 <- mean(data0y2[,4])
mean5 <- mean(data0y2[,5])
mean6 <- mean(data0y2[,6])
mean7 <- mean(data0y2[,7])
mean8 <- mean(data0y2[,8])
mean9 <- mean(data0y2[,9])
mean10 <- mean(data0y2[,10])
mean11 <- mean(data0y2[,11])


contador2 = 1
len1 = nrow(data0y2)
pontos1 <-0
while (contador2<len1){
  
  if (data0y2[contador2,4]<= mean4){
    pontos1=pontos1+1
  }
  if (data0y2[contador2,5]<= mean5){
    pontos1=pontos1+1
  }
  if (data0y2[contador2,6]<= mean6){
    pontos1=pontos1+1
  }
  if (data0y2[contador2,7]<= mean7){
    pontos1=pontos1+1
  }
  if (data0y2[contador2,8]>= mean8){
    pontos1=pontos1+1
  }
  if (data0y2[contador2,9]>= mean9){
    pontos1=pontos1+1
  }
  if (data0y2[contador2,10]>= mean10){
    pontos1=pontos1+1
  }
  if (data0y2[contador2,11]<= mean11){
    pontos1=pontos1+1
  }
  data0y2[contador2,12] <- pontos1
  pontos1 <-0
  contador2=contador2+1
  
}
print(data0y2)
write.csv(data0y2,"/Curso-ML/Assignment-3/transactions-2.csv")