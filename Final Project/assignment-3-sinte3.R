library(dplyr)
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
data1y <- read.csv("classified.csv")      ## Items
print(data1y)
## normalize the trainning data
data2y <- read.csv("transactions-2.csv") ## Items
print(data2y)
summary(data2y)

prop.table(table(data2y$fraud))
prop.table(table(data2y$same_city))
### ranking the transactions
class(data2y$dispersao)
str(data2y)
data0y2 <- select(data2y,fraud,sex,age,income1,same_city,same_count,seller_sco,dispersao,type,V11,V12,V12.1)
print(data0y2)
##### end of normalizing
summary(data1y)
data0y1 <- select(data1y,fraud,sex,age,income1,same_city,same_count,seller_sco,dispersao,type,V10,V11,V12)
str(data0y1)
summary(data0y1)

mean4 <- mean(data0y2[,4])
mean5 <- mean(data0y2[,5])
mean6 <- mean(data0y2[,6])
mean7 <- mean(data0y2[,7])
mean8 <- mean(data0y2[,8])
mean9 <- mean(data0y2[,9])
mean10 <- mean(data0y2[,10])
mean11 <- mean(data0y2[,11])
print(mean4)
print(mean5)
print(mean6)
print(mean7)
print(mean8)
print(mean9)
print(mean10)
print(mean11)
contador1 = 1
len1 = nrow(data0y1)
pontos <-0
while (contador1<len1){
  
  if (data0y1[contador1,4]<= mean4){
    pontos=pontos+1
  }
  if (data0y1[contador1,5]<= mean5){
    pontos=pontos+1
  }
  if (data0y1[contador1,6]<= mean6){
    pontos=pontos+1
  }
  if (data0y1[contador1,7]<= mean7){
    pontos=pontos+1
  }
  if (data0y1[contador1,8]>= mean8){
    pontos=pontos+1
  }
  if (data0y1[contador1,9]>= mean9){
    pontos=pontos+1
  }
  if (data0y1[contador1,10]>= mean10){
    pontos=pontos+1
  }
  if (data0y1[contador1,11]<= mean11){
    pontos=pontos+1
  }
  data0y1[contador1,12] <- pontos
  pontos <-0
  contador1=contador1+1
  
}
print(data0y1)
write.csv(data0y1,"/Curso-ML/Assignment-3/classified-2.csv")